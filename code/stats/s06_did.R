# # # # # # # # # # # # # # #
# DIFFERENCE IN DIFFERENCES #
# # # # # # # # # # # # # # #
rm(list = ls())
gc() # ggplot/patchwork seem to have some sort of leak
# requiring a garbage collector run after heavy plotting

set.seed(1330)


# ----------------------
# Setup and load data
# ----------------------

# control which parts of the script are executed
want_to_save     <- F
has_custom_fonts <- F
wants_lengthy_computation <- F

# libraries
library(ggplot2)
library(patchwork)
library(mgcv)

# dataset
artist_data <- read.csv('datasets/artist_mean_data.csv', header=TRUE)
artist_data$duration_min <- artist_data$duration_ms/(1000*60)
artist_data <- artist_data[,!names(artist_data) == "duration_ms"]
song_data   <- read.csv('datasets/random_sample_did_std.csv', header=TRUE)
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

image.dir <- "plots/s06_did/"

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


# ----------------------
#  Models
# ----------------------
# Split between train and test
test_idx <- sample(nrow(song_data), size=500)
test_songs <- song_data[test_idx,]
train_songs <- song_data[-test_idx,]

Ytrain <- train_songs$popularity
bsize <- 5
m0 <- gam(Ytrain ~ s(danceability, k = bsize) +
            s(energy, k = bsize) +
            s(valence, k = bsize) +
            s(duration_min, k = bsize) + featuring,
          data = train_songs)

gam(Ytrain ~ s(danceability, k = bsize) + s(energy, k = bsize) + s(valence, k = bsize) + s(duration_min, k = bsize) + featuring, data = train_songs)

if (wants_lengthy_computation) {
  library(pbapply)
  library(parallel)
  # Freedman & Lane scheme, fitting models without variable of interest
  m.danc <- gam(Ytrain ~                              s(energy, k = bsize) + s(valence, k = bsize) + s(duration_min, k = bsize) + featuring, data = train_songs)
  m.ener <- gam(Ytrain ~ s(danceability, k = bsize) +                        s(valence, k = bsize) + s(duration_min, k = bsize) + featuring, data = train_songs)
  m.vale <- gam(Ytrain ~ s(danceability, k = bsize) + s(energy, k = bsize) +                         s(duration_min, k = bsize) + featuring, data = train_songs)
  m.drtn <- gam(Ytrain ~ s(danceability, k = bsize) + s(energy, k = bsize) + s(valence, k = bsize) +                              featuring, data = train_songs)
  
  # getting residuals (to be permuted)
  e.danc <- residuals(m.danc)
  e.ener <- residuals(m.ener)
  e.vale <- residuals(m.vale)
  e.drtn <- residuals(m.drtn)
  
  # getting fitted values (NOT permuted)
  y.danc <- m.danc$fitted.values
  y.ener <- m.ener$fitted.values
  y.vale <- m.vale$fitted.values
  y.drtn <- m.drtn$fitted.values
  
  B <- 500 # the empirical CDF is well-behaved enough at B = 500
  n <- nrow(train_songs)
  
  # helper functions for pbreplicate
  Ts.danc <- function(mdl) {summary(mdl)$s.table[1,3]}
  Ts.ener <- function(mdl) {summary(mdl)$s.table[2,3]}
  Ts.vale <- function(mdl) {summary(mdl)$s.table[3,3]}
  Ts.drtn <- function(mdl) {summary(mdl)$s.table[4,3]}
  
  T0.danc <- summary(m0)$s.table[1,3]
  T0.ener <- summary(m0)$s.table[2,3]
  T0.vale <- summary(m0)$s.table[3,3]
  T0.drtn <- summary(m0)$s.table[4,3]
  runTest <- function(ys,errs,Tstat) {
    # random indices
    idxs  <- sample(n)
    # keep Ys fixed, permute errors
    # (to permute responses, the function can be called as ys = 0, errs = Y)
    Yperm <- ys + errs[idxs]
    mperm <- gam(Yperm ~ s(danceability, k = bsize) + s(energy, k = bsize) + s(valence, k = bsize) + s(duration_min, k = bsize) + featuring, data = train_songs)
    Tperm <- Tstat(mperm)
  }
  
  cl <- makeCluster(4) # change number of cores to fit your machine
  clusterExport(cl=cl, list('runTest','Ytrain',
                            'Ts.danc','Ts.ener','Ts.vale','Ts.drtn',
                            'y.danc','y.ener','y.vale','y.drtn',
                            'e.danc','e.ener','e.vale','e.drtn'))
  
  T.danc <- pbreplicate(B,runTest(y.danc,e.danc,Ts.danc))
  T.ener <- pbreplicate(B,runTest(y.ener,e.ener,Ts.ener))
  T.vale <- pbreplicate(B,runTest(y.vale,e.vale,Ts.vale))
  T.drtn <- pbreplicate(B,runTest(y.drtn,e.drtn,Ts.drtn))
  
  stopCluster(cl)
  
  p.danc <- sum(T.danc>=T0.danc)/B
  p.ener <- sum(T.ener>=T0.ener)/B
  p.vale <- sum(T.vale>=T0.vale)/B
  p.drtn <- sum(T.drtn>=T0.drtn)/B
  
  save(p.drtn,p.ener,p.vale,p.danc,T.drtn,T.ener,T.vale,T.danc, file = "stats/d06_gam_prerun.RData")
} else {
  load("stats/d06_gam_prerun.RData")
}


message("Summary gam(popularity ~ ...)")
message("=============================")
message("Residuals:")
show(quantile(residuals(m0)))
message(paste("MAE:",mean(abs(residuals(m0)))))
message("\nSignificance:")
show(data.frame("Perm.pvalue" = c(p.danc,p.ener,p.vale,p.drtn), row.names = c("s(danceability)","s(energy)","s(valence)","s(duration_min)")))
message(paste("R-sq.(adj):",summary(m0)$r.sq))

source("scripts/gamplot.R")

p <- gamplot(m0, spotify_theme, greenVars = c("energy","valence","duration_min"))
if (want_to_save) {
  ggsave("gam2.png", path = image.dir, plot = p, width = 6, height = 6, units = "in", dpi = 300)
} else {
  show(p)
}


message("Performance on test set")
message("=======================")
Ypred <- predict(m0,test_songs)
message("Residuals:")
show(quantile(test_songs$popularity - Ypred))
message(paste("MAE:",mean(abs(test_songs$popularity - Ypred))))