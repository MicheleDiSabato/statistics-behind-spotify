library(mgcv)
library(robustbase)
library(RobStatTM)
library(MASS)
songs_complete <- read.csv("additional_songs_clean.csv", sep = ",")

log_followers = log(pmax(songs_complete$followers_0, songs_complete$followers_1, 
                           songs_complete$followers_2,songs_complete$followers_3,
                           songs_complete$followers_4, songs_complete$followers_5,
                           songs_complete$followers_6) +1)
data = songs_complete
data$log_followers = log_followers
data$duration_min = data$duration_ms/1000/60
data$featuring = ifelse(data$n_artists > 1, 1, 0)

ind = c("base_popularity", "log_followers", "duration_min")
data_MCD <- data[,names(data) %in% ind]

fit_MCD <- covMcd(data_MCD, alpha = 0.5, nsamp = "best")
ind_best = fit_MCD$best
data_clean = data[ind_best,]

quartz()
par(mfrow = c(2,2))
plot(fit_MCD)

ind_reg = c("popularity", "base_popularity", "log_followers", "duration_min", "danceability", "loudness", "featuring")
data_reg <- data_clean[,names(data_clean) %in% ind_reg]

quartz()
plot(data_reg[,-length(data_reg[1,])])

model_gam1 = mgcv::gam(popularity ~ s(duration_min, bs='cr') + s(base_popularity, bs='cr') + 
                       s(log_followers,bs='cr') + featuring, data = data_reg)
summary(model_gam1)
quartz()
par(mfrow= c(1,3))
plot(model_gam1)

lm_robust = lm(popularity ~ base_popularity, data = data_reg)
summary(lm_robust)
shapiro.test(lm_robust$residuals)
plot(lm_robust)

data_rob <- data[,names(data) %in% ind_reg]
fit_lms <- lmsreg(popularity ~ base_popularity, data = data_rob)
fit_lms

fit_lts <- ltsReg(popularity ~ base_popularity , alpha=.75, mcd=TRUE, data = data_rob)
shapiro.test(fit_lts$residuals[fit_lts$best])


