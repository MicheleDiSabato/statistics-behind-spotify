# # # # # # # # # # # # # # # # #
# PRINCIPAL COMPONENTS ANALYSIS #
# # # # # # # # # # # # # # # # #
rm(list = ls())
gc() # ggplot/patchwork seem to have some sort of leak
     # requiring a garbage collector run after heavy plotting

set.seed(1330)

# ----------------------
# Setup and load data
# ----------------------

# control which parts of the script are executed
want_to_save <- F
has_custom_fonts <- F

# libraries
library(ggplot2)
library(patchwork)
library(splines)
library(colorspace)

# (relevant) environment from previous script
load("stats/d00_eda.RData")

# custom font (not available on all machines)
if (has_custom_fonts) {
  spotify_theme <- spotify_theme + theme(text = element_text(family = "SF Pro Display"))
}

image.dir <- "plots/s01_pca/"

# ----------------------
# Check separation between top/bottom halves
# ----------------------

# drop response variable, duplicate duration
drops <- c("popularity",
#           "danceability",
#           "energy",
#           "loudness",
#           "speechiness",
#           "acousticness",
#           "liveness",
#           "valence",
#           "tempo",
#           "duration_min",
           "duration_ms"
)

top.songs <- data[data$best,(names(data) %in% cnt.vars) & (!names(data) %in% drops)]
bot.songs <- data[!data$best,(names(data) %in% cnt.vars) & (!names(data) %in% drops)]

# custom ddplot implementation for ggplot2
source("scripts/ddplot.R")

# Use projection, Tukey has some numerical glitches on this data
p <- ddplot(x = top.songs, y = bot.songs, colorx = greenHEX, colory = "#000000", method = "Projection") +
  spotify_theme + labs(x="Top half",y="Bottom half") +
  theme(axis.line.x = element_blank())

if (want_to_save) {
  ggsave("ddhalves.png", path = image.dir, plot = p, width = 4, height = 4, units = "in", dpi = 300)
} else {
  show(p)
}


# ----------------------
# PCA
# ----------------------
# only select 5 promising variables
pca.feats <- c(
  "danceability",
  "loudness",
  "valence",
  "tempo",
  "duration_min"
  )

data.pca <- data[,names(data) %in% pca.feats]

# drop response + duration_ms (we already have duration_min)
data.pca <- data.pca[,!names(data.pca) %in% drops]

# rescale and run PCA
data.pca <- data.frame(lapply(data.pca, function(x) (x - min(x))/(max(x)-min(x))))
pc.data <- princomp((data.pca), scores=T)

message("PCA output")
message("==========")
show(summary(pc.data))

# store loadings in dataframe for plotting
pc.loads <- pc.data$loadings
pc.loads <- data.frame(t(pc.loads[,]),row.names = NULL)

# consider first 3 components
pc.loads <- pc.loads[1:3,]

# custom barplot implementation for ggplot2
source("scripts/applypca.R")

data.pca      <- apply.pca(pc.loads, data.pca)
data.pca$best <- data$best

# plotting loadings
p1 <- plot.pc(pc.loads, 1, greenHEX, grayHEX, spotify_theme)
p2 <- plot.pc(pc.loads, 2, greenHEX, grayHEX, spotify_theme)
p3 <- plot.pc(pc.loads, 3, greenHEX, grayHEX, spotify_theme)

p <- p1 + p2 + p3

if (want_to_save) {
  ggsave("pcloads.png", path = image.dir, plot = p, width = 18, height = 3, units = "in", dpi = 300)
} else {
  show(p)
}


# plotting scatterplots
p12 <- ggplot(data.pca, aes(x=PC1,y=PC2,color=best)) + geom_point(cex=3) +
  spotify_theme + theme(legend.position = "none") + scale_color_manual(values=c("#000000", greenHEX))
p23 <- ggplot(data.pca, aes(x=PC2,y=PC3,color=best)) + geom_point(cex=3) +
  spotify_theme + theme(legend.position = "none") + scale_color_manual(values=c("#000000", greenHEX))
p31 <- ggplot(data.pca, aes(x=PC3,y=PC1,color=best)) + geom_point(cex=3) +
  spotify_theme + theme(legend.position = "none") + scale_color_manual(values=c("#000000", greenHEX))

p <- p12 + p23 + p31

if (want_to_save) {
  ggsave("pcpairs.png", path = image.dir, plot = p, width = 12, height = 4, units = "in", dpi = 300)
} else {
  show(p)
}

# ----------------------
# Pairplot
# ----------------------

source("scripts/pairplot.R")

cln.vars <- c("danceability", "energy", "loudness", "valence")
data.test <- data[,names(data) %in% cln.vars]
p <- pairplot(data.test, spotify_theme,data$best, F)

if (want_to_save) {
  ggsave("pairs.png", path = image.dir, plot = p, width = 12, height = 8, units = "in", dpi = 300)
} else {
  show(p)
}


# ----------------------
# Permutational linear model (valence ~ danceability)
# ----------------------

B <- 1000
n <- nrow(data.test)

Y <- data.test$valence
X <- data.test$danceability

m0 <- lm(Y ~ X)
T0 <- abs(summary(m0)$coefficients[2,3])

Tp <- numeric(B)

for (perm in 1:B) {
  idxs <- sample(n)
  
  Yperm <- Y[idxs]
  mperm <- lm(Yperm ~ X)
  
  Tp[perm] <- abs(summary(mperm)$coefficients[2,3])
}
pvalue <- sum(Tp>=T0)/B

message("\nSummary lm(valence ~ danceability)")
message("==================================")
message("Residuals:")
show(quantile(summary(m0)$residuals))
message("\nCoefficients:")
coeffs.perm = data.frame("Estimate"=summary(m0)$coefficients[1:2,1],"Perm.pvalue"=c(NaN,pvalue),row.names = c("(Intercept)","danceability"))
show(coeffs.perm)
message(paste("\nMultiple R-squared:",summary(m0)$r.squared))


# ----------------------
# Permutational b-spline (loudness ~ energy)
# ----------------------

Y <- data.test$loudness
X <- data.test$energy

m0 <- lm(Y ~ bs(X))
T0 <- summary(m0)$fstatistic[1]

Tp <- numeric(B)

for (perm in 1:B) {
  idxs <- sample(n)
  
  Yperm <- Y[idxs]
  mperm <- lm(Yperm ~ bs(X))
  
  Tp[perm] <- summary(mperm)$fstatistic[1]
}
pvalue <- sum(Tp>=T0)/B

message("\nSummary lm(loudness ~ bs(energy))")
message("===================================")
message("Residuals:")
show(quantile(summary(m0)$residuals))
message(paste("\nGlobal permutational p-value:",pvalue))
message(paste("\nMultiple R-squared:",summary(m0)$r.squared))

# Plot model results
xx <- seq(min(X),max(X), length.out=500)
ypred <- predict(m0, data.frame(X=xx), se = T)
yy <- ypred$fit
yd <- ypred$se.fit

p <- ggplot(data.test, aes(x=energy, y=loudness)) + geom_point(cex=3) +
  spotify_theme +
  geom_line(mapping = aes(x=xx,y=yy), color = greenHEX, size=2) +
  geom_line(mapping = aes(x=xx,y=(yy + yd)), color = greenHEX, lty=2) +
  geom_line(mapping = aes(x=xx,y=(yy - yd)), color = greenHEX, lty=2)

if (want_to_save) {
  ggsave("bspline.png", path = image.dir, plot = p, width = 6, height = 6, units = "in", dpi = 300)
} else {
  show(p)
}
