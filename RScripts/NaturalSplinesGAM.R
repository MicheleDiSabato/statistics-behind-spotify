########### SET WORKING DIRECTORY ###########
rm(list = ls())
setwd("~/nps-project")

########### LIBRERIE ########################
library(ISLR2)
library(car)
library(mgcv)
library(rgl)
library(splines)
library(pbapply)

###### IMPORTO I DATI ######
data <- read.csv("spotify.csv", header=TRUE)
data = data[,-1]   # elimino la prima colonna perchè contiene
                   # solo gl'indici delle osservazioni
data$duration_min = data$duration_ms/(1000*60)
data = data[, -which(names(data) == "duration_ms")]
data_transform <- spotify_transform(data)

# ANALISI PRELIMINARE DEI DATI
    # scatterplot Matrix alla Cappozzo maniera
x11(title = " Scatter Plot Dati Originali")
scatterplotMatrix(data[, - which( names(data) == "mode")])
x11(title = "Scatter Plot Dati Trasformati")
scatterplotMatrix(data_transform[, - which( names(data_transform) == "mode")])
graphics.off()
    
    # plot popularity VS variabili da includere nel modello NON trasformate
x11(title = "Scatter Plot Variabili per GAM - Dati Originali")
par(mfrow = c(2,3))
plot(data$loudness, data$popularity)
plot(data$valence, data$popularity)
plot(data$tempo, data$popularity)
plot(data$duration_min, data$popularity)
plot(data$danceability, data$popularity)
plot(data$acousticness, data$popularity)
    # plot popularity VS variabili da includere nel modello TRASFORMATE
x11(title = "Scatter Plot Variabili per GAM - Dati Trasformati")
par(mfrow = c(2,3))
plot(data_transform$loudness, data_transform$popularity)
plot(data_transform$valence, data_transform$popularity)
plot(data_transform$tempo, data_transform$popularity)
plot(data_transform$duration_min, data_transform$popularity)
plot(data_transform$danceability, data_transform$popularity)
plot(data_transform$acousticness, data_transform$popularity)

graphics.off()
# GAM con natural splines su dati SENZA TRASFORMAZIONI #
data_ns = data
ns_model <- lm(popularity ~ ns(loudness, df = 3) + ns(valence, df = 3) +
                 ns(tempo, df = 3) + ns(duration_min, df = 3) +
                 ns(danceability, df = 3) + ns(acousticness, df = 3),
               data = data_ns)
summary(ns_model)
    
    # qqplot of the residuals
x11(title = "Residui GAM con Natural Splines - Dati Originali")
qqnorm(ns_model$residuals)
abline(0,1, col = 2)
    # shapiro test of residuals
shapiro.test(ns_model$residuals)

# GAM con natural splines su dati SENZA TRASFORMAZIONI #
data_ns_tr= data_transform
ns_model_tr <- lm(popularity ~ ns(loudness, df = 3) + ns(valence, df = 3) +
                    ns(tempo, df = 3) + ns(duration_min, df = 3) +
                    ns(danceability, df = 3) + ns(acousticness, df = 3),
                  data = data_ns_tr)
summary(ns_model_tr)
    # qqplot of the residuals
x11(title = "Residui GAM con Natural Splines - Dati Trasformati")
qqnorm(ns_model_tr$residuals)
abline(0,1, col = 2)
    # shapiro test of residuals
shapiro.test(ns_model_tr$residuals)
