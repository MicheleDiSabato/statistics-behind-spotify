Tperm_anova <- function(data,B) {
  G <- data$groups
  Tp <- numeric(B)
  
  n <- nrow(data)
  for (perm in 1:B) {
    idxs <- sample(n)
    Tp[perm] <- summary.aov(aov(popularity ~ G[idxs], data = data))[[1]][1,4]
  }
  Tp <- Tp
}