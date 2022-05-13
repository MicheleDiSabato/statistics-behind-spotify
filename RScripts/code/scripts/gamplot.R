gamplot <- function (mdl, thm, greenVars = c(), yellowVars = c()) {
  oldplot <- plot.gam(mdl)
  
  first_iter <- T
  for (plt in oldplot) {
    p <- local({
      xx <- plt$x
      yy <- plt$fit
      yd <- plt$se
      df <- data.frame(X=xx,Y=yy,SE=yd)
      if (plt$xlab %in% greenVars) {
        p <- ggplot(df, aes(x=X, y=Y)) + spotify_theme + geom_line(size=1) +
          geom_line(mapping = aes(y = Y+SE), lty=2, size = 0.75, color = greenHEX) + 
          geom_line(mapping = aes(y = Y-SE), lty=2, size = 0.75, color = greenHEX) +  labs(y=NULL, x=plt$xlab)
      } else if (plt$xlab %in% yellowVars) {
        p <- ggplot(df, aes(x=X, y=Y)) + spotify_theme + geom_line(size=1) +
          geom_line(mapping = aes(y = Y+SE), lty=2, size = 0.75, color = "#D5BD5E") + 
          geom_line(mapping = aes(y = Y-SE), lty=2, size = 0.75, color = "#D5BD5E") +  labs(y=NULL, x=plt$xlab)
      } else if (plt$xlab == "Gaussian quantiles") {
        p <- "DISCARD"
      } else {
        p <- ggplot(df, aes(x=X, y=Y)) + spotify_theme + geom_line(size=1) +
          geom_line(mapping = aes(y = Y+SE), lty=2, size = 0.75, color = grayHEX) + 
          geom_line(mapping = aes(y = Y-SE), lty=2, size = 0.75, color = grayHEX) +  labs(y=NULL, x=plt$xlab)
      }
      
    })
    
    if (min(p != "DISCARD")) {
      if (first_iter) {
        pfinal <- p
        first_iter <- F
      } else {
        pfinal <- pfinal + p
      }
    }
  }
  pfinal <- pfinal
}