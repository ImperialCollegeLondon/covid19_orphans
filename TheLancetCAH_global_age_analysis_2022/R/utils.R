round.choose <- function(x, roundTo, dir = 1) {
  if(dir == 1) {  ##ROUND UP
    x + (roundTo - x %% roundTo)
  } else {
    if(dir == 0) {  ##ROUND DOWN
      x - (x %% roundTo)
    }
  }
}