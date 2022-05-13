library(dplyr)

d <- read.csv("genres.csv")

# by.artists <- d %>% group_by(Artist)
# by.style   <- d %>% group_by(Style)
# 
# genres <- tally(by.style) %>% arrange(desc(n))

#-------------------------#
# SUBSTITUTE WEIRD GENRES #
#-------------------------#

d[d$Style == "new wave pop", "Style"]        <- "pop"
d[d$Style == "brill building pop", "Style"]  <- "pop"
d[d$Style == "sophisti-pop", "Style"]        <- "pop"
d[d$Style == "synthpop", "Style"]            <- "pop"
d[d$Style == "power pop", "Style"]           <- "pop"
d[d$Style == "indie pop", "Style"]           <- "pop"
d[d$Style == "dance pop", "Style"]           <- "pop"
d[d$Style == "k-pop", "Style"]               <- "pop"
d[d$Style == "europop", "Style"]             <- "pop"
d[d$Style == "bubblegum pop", "Style"]       <- "pop"
d[d$Style == "singer-songwriter", "Style"]   <- "pop"
d[d$Style == "latin pop", "Style"]           <- "latin"
d[d$Style == "classic rock", "Style"]        <- "rock"
d[d$Style == "glam rock", "Style"]           <- "rock"
d[d$Style == "samba-rock", "Style"]          <- "rock"
d[d$Style == "rock drums", "Style"]          <- "rock"
d[d$Style == "rockabilly", "Style"]          <- "rock"
d[d$Style == "soft rock", "Style"]           <- "rock"
d[d$Style == "hip pop", "Style"]             <- "pop"
d[d$Style == "art pop", "Style"]             <- "pop"
d[d$Style == "country", "Style"]             <- "folk"
d[d$Style == "country rock", "Style"]        <- "folk"
d[d$Style == "outlaw country", "Style"]      <- "folk"
d[d$Style == "electro", "Style"]             <- "disco"
d[d$Style == "classic house", "Style"]       <- "disco"
d[d$Style == "rock-and-roll", "Style"]       <- "rock"
d[d$Style == "funk rock", "Style"]           <- "funk"
d[d$Style == "atl hip hop", "Style"]         <- "hip hop"
d[d$Style == "bronx hip hop", "Style"]       <- "hip hop"
d[d$Style == "riot grrrl", "Style"]          <- "punk"
d[d$Style == "doo-wop", "Style"]             <- "blues"
d[d$Style == "vocal jazz", "Style"]          <- "jazz"
d[d$Style == "neo soul", "Style"]            <- "blues"

#-------------------------#

final.genres <- c("latin","blues","reggae","jazz","metal","punk","disco","folk","rap","pop","hip hop","funk","soul","rock")

assign_genre <- function(g,a) {
  error <- TRUE
  for (genre in final.genres) {
    if (genre %in% g) {
      error <- FALSE
      break      
    }
  }
  if (error) {
    stop(print(g))
  }
  genre
}

artists <- unique(d$Artist)

df <- data.frame("A","B")
names(df) <- c("Artist","Genre")

for (artist in artists) {
  new.row <- data.frame(artist, assign_genre(d[d$Artist == artist,"Style"], artist))
  names(new.row) <- c("Artist","Genre")
  df <- rbind(df,new.row)
}

df <- df[2:359,]