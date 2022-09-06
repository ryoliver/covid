red_color <- "#ee7674ff"
yellow_color <- "#f4d5a4ff"
blue_color <- "#82b3c4ff"

#color19 <- "#6184D8"
#color20 <- "#50C5B7"

color19 <- "#98C3A0"
color20 <- "#007991"


every_nth <- function(x, nth, empty = TRUE, inverse = FALSE) 
{
  if (!inverse) {
    if(empty) {
      x[1:nth == 1] <- ""
      x
    } else {
      x[1:nth != 1]
    }
  } else {
    if(empty) {
      x[1:nth != 1] <- ""
      x
    } else {
      x[1:nth == 1]
    }
  }
}