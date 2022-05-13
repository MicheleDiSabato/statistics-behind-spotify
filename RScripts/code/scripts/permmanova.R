# Recall that p-values with a Wilk statistic of W0
# are as follows:
#    p = Pr(W<=W0)
# because small values represent extreme results

Tperm_wilk <- function(kvec) {
  # Tp[[i-th k]][perm-th value]
  Tp <- rep(list(numeric(B)),length(kvec))
  groups <- vector(mode = "list",length = length(kvec))

  for(i in 1:length(kvec)) {
    groups[[i]] <- factor(cutree(artist_hclust, k = kvec[i]))
  }
  
  n <- nrow(artist_rescaled)
  for (perm in 1:B) {
    idxs <- sample(n)
    for(i in 1:length(kvec)) {
      groups[[i]] <- groups[[i]][idxs]
      Tp[[i]][perm] <- -summary.manova(
        manova(as.matrix(artist_rescaled) ~ groups[[i]]),
        test="Wilks")$stats[1,2]
      }
  }
  Tp <- Tp
}

T0_wilk <- function(kvec) {
  T0 <- 0*kvec
  groups <- vector(mode = "list",length = length(kvec))
  for(i in 1:length(kvec)) {
    groups[[i]] <- factor(cutree(artist_hclust, k = kvec[i]))
    T0[i] <- -summary.manova(
      manova(as.matrix(artist_rescaled) ~ groups[[i]]),
      test="Wilks")$stats[1,2]
  }
  T0 <- T0
}