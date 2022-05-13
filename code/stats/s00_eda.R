# # # # # # # # # # # # # # # 
# EXPLORATORY DATA ANALYSIS #
# # # # # # # # # # # # # # #
rm(list=ls())

# ----------------------
# Setup and load data
# ----------------------

# control which parts of the script are executed
has_custom_fonts <- F
want_to_save     <- F

# libraries
library(ggplot2)
library(patchwork)
library(aplpack)

# dataset
data <- read.csv("datasets/top500.csv", header=TRUE)

# ----------------------
# ggplot setup
# ----------------------

grayHEX  <- "#929292"
# greenHEX <- "#65d46e" # original but too light on white bg
greenHEX <- "#39c344" # darker variant

polyHEX <- "#1f7dff"
A <- 0.3

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

image.dir <- "plots/s00_eda/"

# ----------------------
# Variable groups
# ----------------------

# continuous
cnt.vars <- c(
  "popularity",
  "danceability",
  "energy",
  "loudness",
  "speechiness",
  "acousticness",
  "instrumentalness",
  "liveness",
  "valence",
  "tempo",
  "duration_ms"
  )

# discrete
dsc.vars <- c(
  "key",
  "mode"
  )

discarded <- c("mode")

# ----------------------
# Distributions
# ----------------------

# plot ecdfs
first.iter <- T
for (feature in names(data)) {
  if (feature %in% discarded) {
    next
  }
  
  tmp.p <- local({
    feature <- feature
    # lower limit
    L <- min(0,min(data[,feature]))
    # upper limit
    U <- max(1, max(data[,feature]))
    
    p <- ggplot(data, aes(x = data[,feature])) +
      stat_ecdf(size = 1.5) +
      labs(x = paste(feature,"º",sep = ""), y = NULL) +
      xlim(L,U) +
      spotify_theme
  })

  if (first.iter) {
    p.final <- tmp.p
    first.iter <- F
  } else {
    p.final <- p.final + tmp.p
  }
}

if (want_to_save) {
  ggsave("facet.png", path = image.dir, plot = p.final, width = 12, height = 4, units = "in", dpi = 300)
} else {
  show(p.final)
}

# ----------------------
# Variable transformation
# ----------------------

# 0.5 threshold as advised by Spotify
# Source: https://developer.spotify.com/documentation/web-api/reference/#/operations/get-audio-features
data$instrumentalness <- data$instrumentalness > 0.5
dsc.vars <- c(dsc.vars,"instrumentalness")
cnt.vars <- cnt.vars[!cnt.vars %in% dsc.vars]

# try to lower skewness
data$speechiness      <- log(data$speechiness)
data$liveness         <- log(data$liveness)

# convert milliseconds to minutes
data$duration_min <- data$duration_ms/(60*1000)
cnt.vars <- c(cnt.vars,"duration_min")


# ----------------------
# Bagplots
# ----------------------

data$best <- T
median.pop <- quantile(data$popularity,0.5)
data$best[data$popularity < median.pop] <- F

feats <- names(data)

# Exclude these features from bagplots
feats <- feats[feats != "popularity"]
feats <- feats[feats != "duration_ms"]
feats <- feats[feats != "best"]
feats <- feats[feats != "mode"]


source("scripts/bag.R")


first.iter <- T
for (feature in feats) {
  if (feature %in% cnt.vars) {
    tmp.p <- local({
      feature <- feature
      # ggbag(data = data, x = feature, y = "popularity", polyClr = "#929292", A = 0.4)
      p <- ggplot() +
        spotify_theme + theme(legend.position = "none") + labs(x = feature, y = "popularity") +
        geom_point(data = data, mapping = aes(x = data[,feature], y = popularity, color = best), cex = 3) +
        scale_color_manual(values=c("#000000", greenHEX)) +
        geom_polygon(loop(data, x=feature, y="popularity"), mapping = aes(x,y), color = polyHEX, fill = polyHEX, alpha = A) +
        geom_polygon(bag(data, x=feature, y="popularity"), mapping = aes(x,y), color = polyHEX, fill = polyHEX, alpha = A)
    })
    if (first.iter) {
      p.final <- tmp.p
      first.iter <- F
    } else {
      p.final <- p.final + tmp.p
    }
  }
}

if (want_to_save) {
  ggsave("bags.png", path = image.dir, plot = p.final, width = 12, height = 10, units = "in", dpi = 300)
} else {
  show(p.final)
}


# ----------------------
# Save environment for next script
# ----------------------

if (want_to_save) {
  save(data, spotify_theme, cnt.vars, dsc.vars, greenHEX, grayHEX, median.pop, file = "stats/d00_eda.RData")
}
