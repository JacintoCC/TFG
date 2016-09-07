locate.max <- function(x){
  vec <- sapply(x, function(y){y == max(x)})
  return(vec/sum(vec))
}

bayesianSign.CMC.test <- function(x, y = NULL, s = 0.5, z_0 = 0,
                             rope.min = -0.01, rope.max = 0.01,
                             weights = c(0.5, rep(1, length(x))),
                             samples = 30000){
    # Check if the data corresponds with a pair of
    # observations or the difference between observations
    if(is.null(y)){
      diff <- x
    }
    else if(length(x) != length(y)){
      stop("X and Y must be of the same length")
    }
    else{
      diff <- x - y
    }
    if(rope.min > rope.max){
      stop("rope.min should be smaller than rope.min")
    }

    # Creation of the vector with the pseudo-observation
    diff <- c(z_0, diff)
    num.elements <- length(diff)

    # Generate the sampled weights
    sampled.weights <- rdirichlet(samples, weights)

    # Belonging of an interval
    belongs.left <- diff < rope.min
    belongs.rope <- diff > rope.min & diff < rope.max
    belongs.right <- diff > rope.max

    # Selects the max according to sampled weights
    #   In case of a tie, the score is divided
    winners <- apply(sampled.weights, 1,
                     function(x){
                       locate.max(c(x %*% belongs.left,
                                    x %*% belongs.rope,
                                    x %*% belongs.right))
                                }
                    )

    # Compute of the probabilities
    prob <- apply(winners, 1, mean)

    return(list(left = prob[1], rope = prob[2], right = prob[3]))
}

bayesianSignedRank.test <- function(x, y = NULL, s = 0.5, z_0 = 0,
                                    rope.min = -0.01, rope.max = 0.01,
                                    weights = c(0.5, rep(1, length(x))),
                                    samples = 30000){

  # Check if the data corresponds with a pair of
  # observations or the difference between observations
  if(is.null(y)){
    diff <- x
  }
  else if(length(x) != length(y)){
    stop("X and Y must be of the same length")
  }
  else{
    diff <- x - y
  }
  if(rope.min > rope.max){
    stop("rope.min should be smaller than rope.min")
  }

  # Creation of the vector with the pseudo-observation
  diff <- c(z_0, diff)
  num.elements <- length(diff)

  # Generate the sampled weights
  sampled.weights <- rdirichlet(samples, weights)

  # Belonging of an interval
  belongs.left <- sapply(diff, FUN = function(x) x+diff < 2 * rope.min)
  belongs.rope <- sapply(diff, FUN = function(x) (x+diff > 2 * rope.min) & (x+diff < 2 * rope.max))
  belongs.right <- sapply(diff, FUN = function(x) x+diff > 2 * rope.max)

  # Selects the max according to sampled weights
  #   In case of a tie, the score is divided
  winners <- apply(sampled.weights, 1,
                   function(x){
                     matrix.prod <- sapply(x, FUN = function(y) x*y)
                     locate.max(c(sum(matrix.prod * belongs.left),
                                  sum(matrix.prod * belongs.rope),
                                  sum(matrix.prod * belongs.right)))
                              }
                  )

  # Compute of the probabilities
  prob <- apply(winners, 1, mean)

  return(list(left = prob[1], rope = prob[2], right = prob[3]))
}


correlatedBayesianT.test <- function(x, y = NULL, rho = ncol(x),
                                    rope.min = -0.01, rope.max = 0.01){
  # Check if the data corresponds with a pair of
  # observations or the difference between observations
  if(is.null(y)){
    diff <- x
  }
  else if(length(x) != length(y)){
    stop("X and Y must be of the same length")
  }
  else{
    diff <- x - y
  }
  if(rope.min > rope.max){
    stop("rope.min should be smaller than rope.min")
  }

  delta <- mean(diff)
  n <- ncol(diff)*nrow(diff)
  df <- n-1
  stdX <- sd(diff)
  sp <- sd(stdX)*sqrt(1/n + rho/(1-rho))
  p.left <- pt((rope.min - delta)/sp, df)
  p.rope <- pt((rope.max - delta)/sp, df) - p.left
  results <- list('left' = p.left, 'rope' = p.rope, 'right'= 1 - p.left-p.rope)
  return (results)
}
