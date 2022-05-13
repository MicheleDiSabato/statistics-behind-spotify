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
wants_lengthy_computation <- F

# libraries
library(ggplot2)
library(patchwork)
library(mgcv)

# (relevant) environment from previous script
load("stats/d03_cls.RData")

# custom font (not available on all machines)
if (has_custom_fonts) {
  spotify_theme <- spotify_theme + theme(text = element_text(family = "SF Pro Display"))
}

image.dir <- "plots/s04_gam/"

source("scripts/gamplot.R")


# ----------------------
#  Models
# ----------------------
# Split between train and test
test_idx <- sample(nrow(song_data), size=500)
test_songs <- song_data[test_idx,]
train_songs <- song_data[-test_idx,]

Ytrain <- train_songs$popularity
bsize <- 5
m0 <- gam(Ytrain ~ s(base_popularity, k = bsize) +
            s(energy, k = bsize) +
            s(log_followers, k = bsize) +
            s(liveness, k = bsize) +
            s(tempo, k = bsize) +
            s(duration_min, k = bsize) +
            groups + featuring,
          data = train_songs)

# base_popularity is linear, liveness and tempo are 0
# we don't need the smoothing term in base_popularity
# and we drop liveness+tempo

m1 <- gam(Ytrain ~ base_popularity +
            s(energy, k = bsize) +
            s(log_followers, k = bsize) +
            s(duration_min, k = bsize) +
            groups + featuring,
          data = train_songs)

if (wants_lengthy_computation) {
  library(pbapply)
  library(parallel)
  # Freedman & Lane scheme, fitting models without variable of interest
  m.ener <- gam(Ytrain ~ base_popularity +                        s(log_followers, k = bsize) + s(duration_min, k = bsize) + groups + featuring, data = train_songs)
  m.logf <- gam(Ytrain ~ base_popularity + s(energy, k = bsize) +                               s(duration_min, k = bsize) + groups + featuring, data = train_songs)
  m.drtn <- gam(Ytrain ~ base_popularity + s(energy, k = bsize) + s(log_followers, k = bsize) +                              groups + featuring, data = train_songs)
  
  # getting residuals (to be permuted)
  e.ener <- residuals(m.ener)
  e.logf <- residuals(m.logf)
  e.drtn <- residuals(m.drtn)
  
  # getting fitted values (NOT permuted)
  y.ener <- m.ener$fitted.values
  y.logf <- m.logf$fitted.values
  y.drtn <- m.drtn$fitted.values
  
  B <- 100 # the empirical CDF is well-behaved enough at B = 100
  n <- nrow(train_songs)
  
  # helper functions for pbreplicate
  Ts.ener <- function(mdl) {summary(mdl)$s.table[1,3]}
  Ts.logf <- function(mdl) {summary(mdl)$s.table[2,3]}
  Ts.drtn <- function(mdl) {summary(mdl)$s.table[3,3]}
  
  T0.ener <- summary(m1)$s.table[1,3]
  T0.logf <- summary(m1)$s.table[2,3]
  T0.drtn <- summary(m1)$s.table[3,3]
  runTest <- function(ys,errs,Tstat) {
    # random indices
    idxs  <- sample(n)
    # keep Ys fixed, permute errors
    # (to permute responses, the function can be called as ys = 0, errs = Y)
    Yperm <- ys + errs[idxs]
    
    mperm <- gam(Yperm ~ base_popularity + s(energy, k = bsize) + s(log_followers, k = bsize) + s(duration_min, k = bsize) + groups + featuring, data = train_songs)
    Tperm <- Tstat(mperm)
  }
  
  cl <- makeCluster(4) # change number of cores to fit your machine
  clusterExport(cl=cl, list('runTest','Ytrain',
                            'Ts.ener','Ts.logf','Ts.drtn',
                            'y.ener','y.logf','y.drtn',
                            'e.ener','e.logf','e.drtn'))
  
  T.ener <- pbreplicate(B,runTest(y.ener,e.ener,Ts.ener))
  T.logf <- pbreplicate(B,runTest(y.logf,e.logf,Ts.logf))
  T.drtn <- pbreplicate(B,runTest(y.drtn,e.drtn,Ts.drtn))
  
  stopCluster(cl)
  
  p.ener <- sum(T.ener>=T0.ener)/B
  p.logf <- sum(T.logf>=T0.logf)/B
  p.drtn <- sum(T.drtn>=T0.drtn)/B
  
  save(p.drtn,p.ener,p.logf,T.drtn,T.ener,T.logf, file = "stats/d04_gam_prerun.RData")
} else {
  load("stats/d04_gam_prerun.RData")
}


message("Summary gam(popularity ~ ...)")
message("=============================")
message("Residuals:")
show(quantile(residuals(m1)))
message(paste("MAE:",mean(abs(residuals(m1)))))
message("\nSignificance:")
show(data.frame("Perm.pvalue" = c(p.ener,p.logf,p.drtn), row.names = c("s(energy)","s(log_followers)","s(duration_min)")))
message(paste("R-sq.(adj):",summary(m1)$r.sq))

source("scripts/gamplot.R")

p <- gamplot(m1, spotify_theme, greenVars = c("energy","log_followers","duration_min"))
if (want_to_save) {
  ggsave("gam2.png", path = image.dir, plot = p, width = 8, height = 3, units = "in", dpi = 300)
} else {
  show(p)
}

# ----------------------
#  Final model with new features
# ----------------------
train_songs$basepop_scaled <- (train_songs$base_popularity - min(train_songs$base_popularity))/
                            (max(train_songs$base_popularity) - min(train_songs$base_popularity))
train_songs$logfoll_scaled <- (train_songs$log_followers - min(train_songs$log_followers))/
  (max(train_songs$log_followers) - min(train_songs$log_followers))

#                                                     this is supposed to be train_songs
# Exact same transformation on test set                       vvvvvvvvvvv
test_songs$basepop_scaled <- (test_songs$base_popularity - min(train_songs$base_popularity))/
  (max(train_songs$base_popularity) - min(train_songs$base_popularity))
test_songs$logfoll_scaled <- (test_songs$log_followers - min(train_songs$log_followers))/
  (max(train_songs$log_followers) - min(train_songs$log_followers))

# average between logfollowers and popularity -> generalized popularity
train_songs$gen_popularity <- train_songs$basepop_scaled + train_songs$logfoll_scaled
test_songs$gen_popularity <- test_songs$basepop_scaled + test_songs$logfoll_scaled

# difference between popularity and logfollowers -> popularity "in excess" of followers
train_songs$exc_popularity <- train_songs$basepop_scaled - train_songs$logfoll_scaled
test_songs$exc_popularity <- test_songs$basepop_scaled - test_songs$logfoll_scaled

m2 <- gam(popularity ~ s(duration_min, k=bsize) + 
                       s(exc_popularity, k=bsize) + 
                       s(gen_popularity, k=bsize) + 
                       groups + featuring, 
                     data = train_songs)

p <- gamplot(m2, spotify_theme, greenVars = c("gen_popularity","exc_popularity","duration_min"))
if (want_to_save) {
  ggsave("gam3.png", path = image.dir, plot = p, width = 8, height = 3, units = "in", dpi = 300)
} else {
  show(p)
}

if (wants_lengthy_computation) {
  x1 = train_songs$duration_min
  x2 = train_songs$exc_popularity
  x3 = train_songs$gen_popularity
  x4 = train_songs$groups
  x5 = train_songs$featuring
  y = train_songs$popularity
  data = data.frame(x1,x2,x3,x4,x5,y)
  get_formula = function(i)
  {
    if(i==0)
      f = y ~ s(x1,  k = bsize) + s(x2,  k = bsize) + s(x3,  k = bsize) + x4 + x5
    if(i==1)
      f = y ~ s(x2,  k = bsize) + s(x3,  k = bsize) + x4 + x5
    if(i==2)
      f = y ~ s(x1,  k = bsize) + s(x3,  k = bsize) + x4 + x5
    if(i==3)
      f = y ~ s(x1,  k = bsize) + s(x2,  k = bsize) + x4 + x5
    if(i==4)
      f = y ~ s(x1,  k = bsize) + s(x2,  k = bsize) + s(x3,  k = bsize) + x5
    if(i==5)
      f = y ~ s(x1,  k = bsize) + s(x2,  k = bsize) + s(x3,  k = bsize) + x4
    return(f)
  }
  set.seed(123)
  B = 100
  pval = numeric(5)
  for(i in 1:5)
  {
    print(i)
    model_gam_reduced = mgcv::gam(get_formula(i), data=data, family = gaussian)
    fitted.obs = model_gam_reduced$fitted.values
    residuals.obs = data$y - fitted.obs
    Tperm = numeric(B)
    if(i<=3)
      T0 = summary(mgcv::gam(get_formula(0), data=data, family = gaussian))$s.table[i,3]
    else
      T0 = summary(mgcv::gam(get_formula(0), data=data, family = gaussian))$pTerms.chi.sq[i-3]
    for(b in 1:B)
    {
      residuals.perm = sample(residuals.obs, replace = FALSE)
      y.perm = fitted.obs + residuals.perm
      gam.perm = mgcv::gam(y.perm ~ s(x1,  k = bsize) + s(x2, k = bsize) + s(x3, k = bsize) + x4 + x5, data = data, family=gaussian)
      if(i<=3)
        Tperm[b] = summary(gam.perm)$s.table[i,3]
      else
        Tperm[b] = summary(gam.perm)$pTerms.chi.sq[i-3]
    }
    pval[i] = sum(Tperm>=T0)/B
  }
  save(pval,file = "stats/d04_gam_prerun_2.RData")
} else {
  load("stats/d04_gam_prerun_2.RData")
}

message("Summary gam(popularity ~ ...)")
message("=============================")
message("Residuals:")
show(quantile(residuals(m2)))
message(paste("MAE:",mean(abs(residuals(m2)))))
message("\nSignificance:")
show(data.frame("Perm.pvalue" = pval, row.names = c("s(duration_min)","s(exc_popularity)","s(gen_popularity)","groups","featuring")))
message(paste("R-sq.(adj):",summary(m2)$r.sq))


message("Performance on test set")
message("=======================")
Ypred <- predict(m2,test_songs)
message("Residuals:")
show(quantile(test_songs$popularity - Ypred))
message(paste("MAE:",mean(abs(test_songs$popularity - Ypred))))

if (want_to_save) {
  save(train_songs,bsize,test_songs,spotify_theme,greenHEX,grayHEX,file = "stats/d04_gam.RData")
}

