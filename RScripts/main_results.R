# WARNING: to run this script the functions "plot_binning" and "plot_kernel_estimation" are needed

rm(list=ls())
graphics.off()
setwd("E:/NPS_project")
library(MASS)
library(rgl)
library(DepthProc)
library(hexbin)
library(packagefinder)
library(aplpack)
library(robustbase)
library(roahd)
library(progress)
library(pbapply)
library(car)
library(mgcv)
library(rgl)
library(splines)
library(pbapply)
library(np)
library(splines)
library(fda)

hiphop_clean <- read.csv("hiphop_clean.csv",header=TRUE)

hiphop_v2 <- read.csv("hiphop_v2.csv",header=TRUE)

hiphop <- read.csv("hiphop_complete.csv",header=TRUE)

spotify <- read.csv("spotify.csv",header=TRUE)
spotify = spotify[,-1]

spotify_v2 <- read.csv("spotify_v2.csv",header=TRUE)
#spotify_v2_uri_timedignature = spotify_v2[,c(1,17)]
#spotify_v2 = spotify_v2[,c(-1,-17)]


data = spotify
#data = data[-which(data$popularity == 0),]
#data$popularity = log(data$popularity)
# lambda.energy <- powerTransform(data$energy)
# lambda.energy
# data$energy <- bcPower(data$energy, lambda.energy$lambda)
# un-comment to resize everything to the range (0, 1)
# pop = data$popularity
# for (i in 3:dim(data)[2])
# {
#   data[,i] = as.double(data[,i])
# }
# base_pop = data$base_popularity
# data <- data.frame(lapply(data[,c(-1,-2)], function(x) (x - min(x))/(max(x)-min(x))))
# data$popularity = pop
# data$base_popularity = base_pop

index_popularity  = which(names(data)=="popularity")
index_danceability  = which(names(data)=="danceability")
index_energy = which(names(data)=="energy")
index_loudness = which(names(data)=="loudness")
index_speechiness = which(names(data)=="speechiness")
index_acousticness = which(names(data)=="acousticness")
index_instrumentalness = which(names(data)=="instrumentalness")
index_valence = which(names(data)=="valence")
index_tempo = which(names(data)=="tempo")
index_duration = which(names(data)=="duration_ms")
indici = c(index_popularity,
           index_danceability,
           index_energy,
           index_loudness,
           index_speechiness,
           index_acousticness,
           index_instrumentalness,
           index_valence,
           index_tempo,
           index_duration)

# focus on danceability, energy, loudness, tempo, duration_ms and valence

colore = rep(1,500)
pop = data$popularity
i1 = which(pop <= quantile(pop, c(0.3,0.6))[1])
i2 = which(pop > quantile(pop, c(0.3,0.6))[2])
colore[i1] = 2
colore[i2] = 3
length(i1) ; length(i2)
# the number of elements in each group seems to be kind of balances

x11()
par(mfrow = c(2,3))
plot(data$danceability, data$popularity, col = colore, pch = 19, cex = 2)
bagplot_ <- bagplot(data.frame(data$danceability, data$popularity), xlab = "danceability", transparency = TRUE, add=TRUE)
plot(data$energy, data$popularity, col = colore, pch = 19, cex = 2)
bagplot_ <- bagplot(data.frame(data$energy, data$popularity), xlab = "energy", transparency = TRUE, add=TRUE)
plot(data$loudness, data$popularity, col = colore, pch = 19, cex = 2)
bagplot_ <- bagplot(data.frame(data$loudness, data$popularity), xlab = "loudness", transparency = TRUE, add=TRUE)
plot(data$valence, data$popularity, col = colore, pch = 19, cex = 2)
bagplot_ <- bagplot(data.frame(data$valence, data$popularity), xlab = "valence", transparency = TRUE, add=TRUE)
plot(data$tempo, data$popularity, col = colore, pch = 19, cex = 2)
bagplot_ <- bagplot(data.frame(data$tempo, data$popularity), xlab = "tempo", transparency = TRUE, add=TRUE)
plot(data$duration_ms, data$popularity, col = colore, pch = 19, cex = 2)
bagplot_ <- bagplot(data.frame(data$duration_ms, data$popularity), xlab = "duration", transparency = TRUE, add=TRUE)
par(mfrow = c(1,1))

# we hoped to find major differences among the 3 groups, but in fact the distributions appear to be the same
# the only notable detail is that the distribution of the popularity of the least popular
# songs seems to be more spread than the ohers

x11()
par(mfrow = c(3,3))
plot(data$danceability, data$popularity, col = colore, pch = 19)
plot(data$energy, data$popularity, col = colore, pch = 19)
plot(data$loudness, data$popularity, col = colore, pch = 19)
plot((data$speechiness), data$popularity, col = colore, pch = 19)
plot((data$acousticness), data$popularity, col = colore, pch = 19)
plot(data$instrumentalness, data$popularity, col = colore, pch = 19)
plot(data$valence, data$popularity, col = colore, pch = 19)
plot(data$tempo, data$popularity, col = colore, pch = 19)
plot(data$duration_ms, data$popularity, col = colore, pch = 19)
par(mfrow = c(1,1))

x11()
par(mfrow = c(2,3))
hist(data$danceability, breaks = 50)
hist(data$energy, breaks = 50)
hist(data$loudness, breaks = 50)
hist(data$valence, breaks = 50)
hist(data$tempo, breaks = 50)
hist(data$duration_ms, breaks = 50)
par(mfrow = c(1,1))
x11()
hist(data$popularity, breaks = 50)

# tolgo popularity (1), energy (3), speechiness (5), acousticness (6), instrumentalness (7)
pc.data <- princomp(scale(data[,indici[c(-1,-3, -5, -6, -7)]]), scores=T)
summary(pc.data)
load.data <- pc.data$loadings
x11()
par(mfcol = c(1,3))
for(i in 1:3) barplot(load.data[,i], ylim = c(-1, 1), main=paste("PC",i))

x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
plot(pc.data, las=2, main='Principal Components', ylim=c(0,7))
abline(h=1, col='blue')
barplot(sapply(as.data.frame(scale(data[,indici[c(-1,-3, -5, -6, -7)]])),sd)^2, las=2, main='Original Variables', ylim=c(0,7), ylab='Variances')
plot(cumsum(pc.data$sde^2)/sum(pc.data$sde^2), type='b', axes=F, xlab='Number of components', ylab='Contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.9, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(scale(data[,indici[c(-1,-3, -5, -6, -7)]])),labels=1:ncol(scale(data[,indici[c(-1,-3, -5, -6, -7)]])),las=2)

x11()
plot(pc.data$scores[,1], pc.data$scores[,2], col = colore, pch = 19)

plot3d(pc.data$scores[,1], pc.data$scores[,2], pc.data$scores[,3],col = colore, pch = 19,size = 10)

# pca yields similar results, so a possible development could be to test whether the mean
# features of the low-popular songs are different wrt the medium-high-popular ones


# WIP


########## LINEAR REGRESSION

which.min(abs(cor(data[,indici])[1,]))

# tempo and popularity are not correlated at all
# hist(instrumentalness) shows that the majority of its values are near 0
hist(log(data$instrumentalness))
cor(data[,indici])[1,]
cor(data$popularity[which(data$instrumentalness>0)], log(data$instrumentalness[which(data$instrumentalness>0)]))
plot(data$popularity[which(data$instrumentalness>0)], log(data$instrumentalness[which(data$instrumentalness>0)]))

# in general we see that popularity does not seem to depend on any of the regressors
# indeed the following plots show that most smoothing techniques have a bad 
# perforormance on our data.

source("E:/NPS_project/plot_binning.R")
plot_binning(data.frame(data[,indici]), "pop", layout_pic = c(3,3))


source("E:/NPS_project/plot_kernel_estimation.R")
# c(0.03,0.03,5,0.03,0.03,0.03,0.03,10,100000)
bw_computed = plot_kernel_estimation(data = data.frame(data[,indici[c(1,9)]]), 
                                     popularity_choice = "popularity",
                                     kernel = "truncated gaussian",
                                     bw_vector = c(10), 
                                     cv_method = 2,
                                     layout_pic = c(1,1))
