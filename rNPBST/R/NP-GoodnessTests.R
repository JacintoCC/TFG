#' @title Kolmogorov-Smirnov test for goodness of fit
#'
#' @export
#' @description This function performs the Kolmogorov-Smirnov test
#' @param sequence Secuence of data
#' @param distribution Distribution name to perform test
#' @return A htest object with pvalues and statistics
ks.test <- function(sequence, distribution = "NORMAL", ...){
  # Order sequence
  sequence <- sequence[order(sequence)]

  cumulative.probability.fun <- getCumulativeProbabilityFunction(distribution, ...)

  n <- length(sequence)
  Sn <- (1:n)/n
  F0 <- cumulative.probability.fun(sequence)

  abs.dif <- abs(Sn - F0)
  abs.dif.2 <- abs(c(0, Sn[-n]) - F0)

  Dn <- max(c(abs.dif, abs.dif.2))
  pvalue <- pkolmogorov(n, Dn)

  htest <- list(data.name = deparse(substitute(sequence)),
                statistic = Dn,
                p.value = pvalue,
                method = "Kolmogorov-Smirnov")
  return(htest)
}

#' @title Lilliefors test for goodness of fit
#'
#' @export
#' @description This function performs the Lilliefors test
#' @param sequence Secuence of data
#' @param distribution Distribution name to perform test
#' @return A htest object with pvalues and statistics
lilliefors.test <- function(sequence, distribution = "NORMAL"){
  # Order sequence
  sequence <- sequence[order(sequence)]

  n <- length(sequence)
  Sn <- (1:n)/n

  if(distribution == "NORMAL"){
    mean <- mean(sequence)
    sd <- sd(sequence)
    parameters <- c(mean = mean, sd = sd)
    cumulative.probability.fun <- function(x) pnorm(x, mean = mean,
                                                    sd = sd)
  }
  else if(distribution == "EXPONENTIAL"){
    mean <- mean(sequence)
    parameters <- c(mean = mean)
    cumulative.probability.fun <- function(x) pexp(x, mean = mean)
  }
  else{
    stop("Not supported distribution in this test")
  }

  F0 <- cumulative.probability.fun(sequence)

  abs.dif <- abs(Sn - F0)
  abs.dif.2 <- abs(c(0, Sn[-n]) - F0)

  Dn <- max(c(abs.dif, abs.dif.2))
  pvalue <- pkolmogorov(n, Dn)

  htest <- list(data.name = deparse(substitute(sequence)),
                parameters = parameters,
                statistic = Dn,
                p.value = pvalue,
                method = "Lilliefors")
  return(htest)
}
