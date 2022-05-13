# # # # # # # #
# CLUSTERING  #
# # # # # # # #
rm(list=ls())
gc()

# ----------------------
# Setup and load data
# ----------------------

# control which parts of the script are executed
has_custom_fonts <- F
want_to_save     <- F
wants_lengthy_computation <- F

# libraries
library(ggplot2)
library(class)

# dataset
artist_data <- read.csv('datasets/artist_mean_data.csv', header=TRUE)
artist_data$duration_min <- artist_data$duration_ms/(1000*60)
artist_data <- artist_data[,!names(artist_data) == "duration_ms"]
song_data   <- read.csv('datasets/random_sample.csv', header=TRUE)
song_data$duration_min <- song_data$duration_ms/(1000*60)
song_data <- song_data[,!names(song_data) == "duration_ms"]
# ----------------------
# ggplot setup
# ----------------------

grayHEX  <- "#929292"
# greenHEX <- "#65d46e" # original but too light on white bg
greenHEX <- "#39c344" # darker variant

spotify_theme <- theme_minimal() + 
  theme(panel.grid.minor = element_blank(),
        axis.line.x = element_line(colour = grayHEX, size = 1),
        plot.title = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold", size = 20),
        axis.title.y = element_text(face = "bold", size = 20),
        plot.background = element_rect(colour = "#ffffff"))

# custom font (not available on all machines)
if (has_custom_fonts) {
  spotify_theme <- spotify_theme + theme(text = element_text(family = "SF Pro Display"))
}

image.dir <- "plots/s03_cls/"

# ----------------------
#  Clustering
# ----------------------

# clustering vars
cls.vars <- c(
  "base_popularity",
  "danceability",
  "energy",
  "loudness",
  "speechiness",
  "liveness",
  "valence",
  "tempo",
  "duration_min")

# function that takes any dataset and rescales it to match
# the rescaling of artist_data
cls.rescale <- function (df) {
  # note that this rescaling only guarantees outputs
  # 0<=x<=1 for the artist_data dataframe
  for (feature in cls.vars) {
    df[,feature] <- (df[,feature] - min(artist_data[,feature])) /
                    (max(artist_data[,feature]) - min(artist_data[,feature]))
  }
  df <- df
}

# rescale artist_data and compute distance
artist_rescaled <- cls.rescale(artist_data)
artist_rescaled <- artist_rescaled[,names(artist_rescaled) %in% cls.vars]
artist_dist <- dist(artist_rescaled, method='euclidean')

artist_hclust <- hclust(artist_dist, method='ward.D2')
#plot(artist_hclust, main='euclidean-ward.D2', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')

# ----------------------
#  Assign songs to clusters
# ----------------------

groups <- factor(cutree(artist_hclust, k = 5))
assign_cluster <- function(groups, k=100, test){
  test <- test[,names(test) %in% cls.vars]
  test <- cls.rescale(test)
  knnClust <- knn(train = artist_rescaled, test = test , k = k, cl = groups)
}
song_data$groups <- assign_cluster(groups, k=70, song_data)

song_data$featuring <- ifelse(song_data$n_artists > 1,1,0)

song_data$log_followers <- log(1+song_data$followers)

# We can now test popularity ~ clustering
source("scripts/permanova.R")
B <- 1000

Tp <- Tperm_anova(song_data,B)
Amdl <- summary.aov(aov(popularity ~ groups, song_data))[[1]]
T0 <- Amdl[1,4]

message("Permutational ANOVA (popularity ~ groups)")
message("=========================================")
show(c("dof"=Amdl[1,1], "Tstat"=T0, "pvalue"=sum(Tp>=T0)/B))

# boxplot
p <- ggplot(song_data, aes(x=groups,y=popularity)) + geom_boxplot(outlier.colour="black", outlier.shape=16,
             outlier.size=2, notch=F, fill=greenHEX) + spotify_theme
if (want_to_save) {
  ggsave("boxplot.png", path = image.dir, plot = p, width = 4, height = 3, units = "in", dpi = 300)
} else {
  show(p)
}

if (want_to_save) {
  save(song_data,spotify_theme,greenHEX,grayHEX,file = "stats/d03_cls.RData")
}

