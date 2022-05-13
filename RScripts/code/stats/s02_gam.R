# # # # # # # # # # # # # # # #
# GENERALIZED ADDITIVE MODELS #
# # # # # # # # # # # # # # # #
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
wants_lengthy_computation <- F # permutation tests

# libraries
library(ggplot2)
library(patchwork)
library(mgcv)

# (relevant) environment from previous script
load("stats/d00_eda.RData")

# custom font (not available on all machines)
if (has_custom_fonts) {
  spotify_theme <- spotify_theme + theme(text = element_text(family = "SF Pro Display"))
}

image.dir <- "plots/s02_gam/"

source("scripts/gamplot.R")

# ----------------------
# First GAM
# ----------------------
Y <- data$popularity
bsize <- 5 # size of basis for smooth terms
# between danceability and valence we keep danceability (but they are equivalent)
# between energy and loudness we keep energy because it is in [0,1]
# see script s01_pca.R
m0 <- gam(Y ~ s(danceability, k = bsize) +
            s(energy, k = bsize) +
            s(speechiness, k = bsize) +
            s(liveness, k = bsize) +
            s(tempo, k = bsize) +
            s(duration_min, k = bsize),
          data = data)

# ----------------------
# Tests
# ----------------------

if (wants_lengthy_computation) {
  library(pbapply)
  library(parallel)
  # Freedman & Lane scheme, fitting models without variable of interest
  m.live <- gam(Y ~ s(danceability, k = bsize) + s(energy, k = bsize) + s(speechiness, k = bsize)                          + s(tempo, k = bsize) + s(duration_min, k = bsize), data = data)
  m.drtn <- gam(Y ~ s(danceability, k = bsize) + s(energy, k = bsize) + s(speechiness, k = bsize) + s(liveness, k = bsize) + s(tempo, k = bsize)                             , data = data)
  
  # getting residuals (to be permuted)
  e.live <- residuals(m.live)
  e.drtn <- residuals(m.drtn)
  
  # getting fitted values (NOT permuted)
  y.live <- m.live$fitted.values
  y.drtn <- m.drtn$fitted.values
  
  B <- 1000 # the empirical CDF is well-behaved enough at B = 1000
  n <- nrow(data)
  
  # helper functions for pbreplicate
  Ts.glob <- function(mdl) {sum(summary(mdl)$s.table[,3])}
  Ts.live <- function(mdl) {summary(mdl)$s.table[4,3]}
  Ts.drtn <- function(mdl) {summary(mdl)$s.table[6,3]}
  T0.glob <- sum(summary(m0)$s.table[,3])
  T0.live <- summary(m0)$s.table[4,3]
  T0.drtn <- summary(m0)$s.table[6,3]
  runTest <- function(ys,errs,Tstat) {
    # random indices
    idxs  <- sample(n)
    # keep Ys fixed, permute errors
    # (to permute responses, the function can be called as ys = 0, errs = Y)
    Yperm <- ys + errs[idxs]
    
    mperm <- gam(Yperm ~ s(danceability, k = bsize) + s(energy, k = bsize) + s(speechiness, k = bsize) + s(liveness, k = bsize) + s(tempo, k = bsize) + s(duration_min, k = bsize), data = data)
    Tperm <- Tstat(mperm)
  }
  
  cl <- makeCluster(3) # change number of cores to fit your machine
  clusterExport(cl=cl, list('runTest','Y','Ts.glob','Ts.live','Ts.drtn','e.live','y.live','e.drtn','y.drtn'))
  
  T.glob <- pbreplicate(B,runTest(0*Y,Y,Ts.glob))
  T.live <- pbreplicate(B,runTest(y.live,e.live,Ts.live))
  T.drtn <- pbreplicate(B,runTest(y.drtn,e.drtn,Ts.drtn))
  
  stopCluster(cl)
  
  p.glob <- sum(T.glob>=T0.glob)/B
  p.live <- sum(T.live>=T0.live)/B
  p.drtn <- sum(T.drtn>=T0.drtn)/B
  
  save(m0,p.drtn,p.glob,p.live,T.drtn,T.glob,T.live,T0.drtn,T0.glob,T0.live, file = "stats/d02_gam_prerun.RData")
} else {
  load("stats/d02_gam_prerun.RData")
}

message("Summary gam(popularity ~ ...)")
message("=============================")
message("Residuals:")
show(quantile(residuals(m0)))
message(paste("MAE:",mean(abs(residuals(m0)))))
message("\nSignificance:")
show(data.frame("Perm.pvalue" = c(p.live, p.drtn), row.names = c("s(liveness)","s(duration_min)")))
message(paste("\nGlobal permutational p-value:", p.glob))

# ----------------------
# Plots
# ----------------------

par(mfrow = c(2,3))
p <- gamplot(m0,spotify_theme,greenVars="duration_min",yellowVars="liveness")
if (want_to_save) {
  ggsave("gam1.png", path = image.dir, plot = p, width = 8, height = 3, units = "in", dpi = 300)
} else {
  show(p)
}