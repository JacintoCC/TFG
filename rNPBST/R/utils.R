heaviside <- function(x, a=0){
  return( (sign(x-a) + 1) / 2 )
}

locate.max <- function(x){
  vec <- sapply(x, function(y){y == max(x)})
  return(vec/sum(vec))
}
