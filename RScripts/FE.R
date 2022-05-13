# Variable transformation only for:
##    danceability
##    speechiness
##    liveness
##    duration_min
# Update comment after adding transformations.

spotify_transform <- function(raw.df) {
  # ~ Normal with low variance
  unimodal  <- function(x,L,k,m){L/(1+exp((m-x)/k))}
  vunimodal <- Vectorize(unimodal, vectorize.args = "x")
  
  # ~ Exponential
  exponential  <- function(x,a,b,c){
    if (x<=(b*c-1)/(a*c)) {
      return(0)
    } else {
      return(1/(a*x - b) + c)
    }
  }
  vexponential <- Vectorize(exponential, vectorize.args = "x")
  
  ##------------
  ## danceability
  raw.df$danceability <- vunimodal(raw.df$danceability,1.06,0.12,0.61)
  
  ##------------
  ## speechiness
  raw.df$speechiness <- vexponential(raw.df$speechiness,-54.47,-0.47,1.02)
  
  ##------------
  ## liveness
  raw.df$liveness <- vexponential(raw.df$liveness,-7.43,0.48,1.21)
  
  ##------------
  ## duration_min
  raw.df$duration_min <- vunimodal(raw.df$duration_min,0.94,0.68,3.86)
  
  return(raw.df)
}