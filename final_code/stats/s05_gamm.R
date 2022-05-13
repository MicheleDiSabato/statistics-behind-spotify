# # # # # # # # # # #
# GAM MIXED EFFECTS #
# # # # # # # # # # #
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

# libraries
library(ggplot2)
library(patchwork)
library(mgcv)

# (relevant) environment from previous script
load("stats/d04_gam.RData")

# custom font (not available on all machines)
if (has_custom_fonts) {
  spotify_theme <- spotify_theme + theme(text = element_text(family = "SF Pro Display"))
}

image.dir <- "plots/s05_gamm/"

source("scripts/errfuncs.R")
source("scripts/gamplot.R")

# ----------------------
# Random effects on groups
# ----------------------
m0 <- gam(popularity ~ s(duration_min, k=bsize) + 
            s(exc_popularity, k=bsize) + 
            s(gen_popularity, k=bsize) + 
            s(groups, bs="re") + featuring, 
          data = train_songs)
show(summary(m0))

p <- gamplot(m0,spotify_theme,greenVars = c("gen_popularity","exc_popularity","duration_min"))
if (want_to_save) {
  ggsave("gamm0.png", path = image.dir, plot = p, width = 8, height = 3, units = "in", dpi = 300)
} else {
  show(p)
}


message("Performance on test set")
message("=======================")
Ypred <- predict(m0,test_songs)
MSE_m0  <- mse_func(Ypred, test_songs$popularity)
MAE_m0  <- mae_func(Ypred, test_songs$popularity)
MAPE_m0 <- mape_func(Ypred, test_songs$popularity)
show(c("MSE.test" = MSE_m0,"MAE.test" = MAE_m0,"MAPE.test" = MAPE_m0))
  

# ----------------------
# Random effects on groups and on featuring
# ----------------------
m1 <- gam(popularity ~ s(duration_min, k=bsize) + 
            s(exc_popularity, k=bsize) + 
            s(gen_popularity, k=bsize) + 
            s(groups, bs="re") + s(featuring, bs="re"),
          data = train_songs)
show(summary(m1))

p <- gamplot(m1,spotify_theme,greenVars = c("gen_popularity","exc_popularity","duration_min"))
if (want_to_save) {
  ggsave("gamm1.png", path = image.dir, plot = p, width = 8, height = 3, units = "in", dpi = 300)
} else {
  show(p)
}


message("Performance on test set")
message("=======================")
Ypred <- predict(m1,test_songs)
MSE_m1  <- mse_func(Ypred, test_songs$popularity)
MAE_m1  <- mae_func(Ypred, test_songs$popularity)
MAPE_m1 <- mape_func(Ypred, test_songs$popularity)
show(c("MSE.test" = MSE_m1,"MAE.test" = MAE_m1,"MAPE.test" = MAPE_m1))