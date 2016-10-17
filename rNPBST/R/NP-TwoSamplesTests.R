#############
# KOLMOGOROV - SMIRNOV TWO samples
#############


#' @title Exact p values for Kolmogorov-Smirnov test for compare two samples
#'
#' @export
#' @description This function compute the exact pvalue for the Kolmogorov-Smirnov test for two samples
#' @param n Size of distribution
#' @param Dn Statistic
#' @return computed pvalue
computeKolmogorovExactProbability <- function(n, Dn){
  if(n < 9){
    data(KolmogorovTwoSample)
    nmDn <- round(n * n * Dn)
    pvalue <- KolmogorovTwoSample$distribution[KolmogorovTwoSample$x == n &
                                               KolmogorovTwoSample$y == n &
                                               KolmogorovTwoSample$Dn == nmDn]
    pvalue <- ifelse(pvalue == -1, 1, pvalue)
    return(pvalue)
  }
  else
    return(1)
}

#' @title Asymptotic p values for Kolmogorov-Smirnov test for compare two samples
#'
#' @export
#' @description This function compute the asymptotic pvalue for the Kolmogorov-Smirnov test for two samples
#' @param n Size of distribution
#' @param Dn Statistic
#' @return computed pvalue
computeKolmogorovAsymptoticProbability <- function(n, Dn){
  if(n < 21){
    data(KolmogorovTwoSampleAsymptotic)
    nmDn <- round(n * n * Dn)
    pvalue <- KolmogorovTwoSampleAsymptotic
    pvalue <- ifelse(pvalue == -1, 1, pvalue)
    return(pvalue)
  }
  else{
    size <- sqrt(2*n)/n
    v <- Dn >= c(1.07,1.22,1.36,1.52,1.63)/size
    if(length(v) > 0){
      pvalue <- colnames(KolmogorovTwoSampleAsymptotic)[v[length(v)]]
      return(pvalue)
    }
    else
      return(1)
  }
}

#' @title Kolmogorov-Smirnov test for compare two samples
#'
#' @export
#' @description This function performs the Kolmogorov-Smirnov test for two samples
#' @param matrix Matrix of data
#' @return A htest object with pvalues and statistics
ksTwoSamples.test <- function(matrix){
  if(ncol(matrix) != 2)
    stop("Kolmogorov-Smirnov test only can be employed with two samples")

  sample1 <- matrix[ ,1]
  sample2 <- matrix[ ,2]
  combined <- c(sample1, sample2)
  n <- nrow(combined)

  # Order data
  sample1 <- sample1[order(sample1)]
  sample2 <- sample2[order(sample2)]
  combined <- combined[order(combined)]

  # Distict values
  distinct <- length(unique(combined))
  increment <- 1 / distinct

  combinedCdf <- vector("numeric", length = 2* n)

  for(i in 2:(2*n))
    combinedCdf[i] <- ifelse(combined[i] == combined[i-1], combinedCdf[i-1] + increment)

  # Sn and Sm arrays

  Sn <- combinedCdf[combined %in% sample1]
  Sm <- combinedCdf[combined %in% sample2]

  # Diff
  diff <- Sm - Sn

  DnPos <- max(diff)
  DnNeg <- min(diff)
  Dn <- max(abs(c(DnPos, DnNeg)))

  pvalue <- c("Exact Left Tail" = computeKolmogorovExactProbability(n, abs(DnNeg)),
              "Exact Right Tail" = computeKolmogorovExactProbability(n, abs(DnPos)),
              "Exact Double Tail" = computeKolmogorovExactProbability(n, Dn),
              "Asymptotic Left Tail" = computeKolmogorovAsymptoticProbability(n, abs(DnNeg)),
              "Asymptotic Left Tail" = computeKolmogorovAsymptoticProbability(n, abs(DnPos)),
              "Asymptotic Left Tail" = computeKolmogorovAsymptoticProbability(n, Dn))

  htest <- list(data.name = deparse(substitute(sequence)),
                statistic = c("DnPos" = DnPos, "DnNeg" = DnNeg, "Dn" = Dn),
                p.value = pvalue,
                method = "Kolmogorov-Smirnov")
  return(htest)
}


############
# WALD WOLFOWITZ TEST
############
#' @title Wald-Wolfowitz char vector construction
#'
#' @description Get sequence for Wald-Wolfowitz test
#' @param sample1 First sample
#' @param sample2 Second sample
#' @return Sequence to apply Wald-Wolfowitz test
waldWolfowitzSequence <- function(sample1, sample2){
  # Sort
  sample1 <- sample1[order(sample1)]
  sample2 <- sample2[order(sample2)]

  n <- length(sample1) + length(sample2)

  combined <- c(sample1, sample2)
  combined <- combined[combined]
  sequence <- sapply(combined, function(i){
      if(i %in% sample1 & !(i %in% sample2))
        return(1)
      else if(!(i %in% sample1) & i %in% sample2)
        return(-1)
      else
        return(0)
    })

  for(i in 1:n){
    if(sequence[i] == 0){
      sequence[i] <- ifelse(sequence[i-1] == 1, -1, 1)
    }
  }

  return(sequence)
}

#' @title Wald-Wolfowitz test for compare two samples
#'
#' @export
#' @description This function performs the Wald-Wolfowitz test for two samples
#' @param matrix Matrix of data
#' @return A htest object with pvalues and statistics
waldWolfowitz.test <- function(matrix){
  if(ncol(matrix) != 2)
    stop("Wald-Wolfowitz test only can be employed with two samples")

  # Remove NA numbers
  sample1 <- matrix[1, !is.NA(matrix[1, ])]
  sample2 <- matrix[2, !is.NA(matrix[2, ])]

  sequence <- waldWolfowitzSequence(sample1, sample2)
  rle <- rle(sequence)
  runs <- length(rle$lengths)

  exact.pvalue <- computeNumberOfRunsLeftTailProbability(length(sample1),
                                                         length(sample2), runs)
  asymptotic.pvalue <- computeNumberOfRunsAsymptoticProbability(length(sample1),
                                  length(sample2), runs)["Asymptotic Left Tail"]
  pvalue <- c("Exact p-value" = exact.pvalue,
              "Asymptotic p-value" = asymptotic.pvalue)

  htest <- list(data.name = deparse(substitute(sequence)),
                statistic = runs, p.value = pvalue, method = "Wald-Wolfowitz")
  return(htest)
}
