#' @title Compute exact probability of distribution
#'
#' @export
#' @description Function to get the exact probability given a distribution table
#' @param N number of columns
#' @param k number of rows
#' @param L Page statistic
#' @return Exact p-value computed
computePageExactProbability <- function(N, k, L){
  if(N <= 8 & N >= 3 & k <= 12 & k >= 2){
    data(PageTable)page
    pvalues <- PageTable$p[PageTable$N == N &
                           PageTable$k == m &
                           PageTable$L == L]
    return(pvalues[1])
  }

  return(-1)
}

#' @title Compute asymptotic probability of distribution
#'
#' @export
#' @description Function to get the asymptotic probability given a distribution table
#' @param N number of columns
#' @param k number of rows
#' @param L Page statistic
#' @return Exact p-value computed
computePageExactProbability <- function(N, k, L){

  numerator <- 12 * (L - 0.5) - 3 * k * N * (N + 1) * (N + 1)
  denominator <- N * (N + 1) * sqrt(k * (N - 1))
  Z <- numerator / denominator

  return(pnorm(Z))
}

#' @title Page test for multiple comparisons
#'
#' @export
#' @description This function performs the Page test
#' @param matrix Matrix of data
#' @return A htest object with pvalues and statistics
page.test <- function(matrix){
  if(ncol(matrix) < 3)
    stop("Extended median test only can be employed with more than two samples")

  if(anyNA(matrix))
    stop("No null values allowed in this test.")

  ranks <- t(apply(matrix, 1, rank))
  sumRanks <- apply(ranks, 2, sum)
  L <- sum(sumRanks * 1:ncol(matrix))

  exact.pvalue <- computePageExactProbability(ncol(matrix), nrow(matrix), L)
  asymptotic.pvalue <- computePageAsymptoticProbability(ncol(matrix), nrow(matrix), L)
  pvalues <- c("Exact pvalue" = exact.pvalue,
               "Asymtotic pvalue" = asymptotic.pvalue)

  htest <- list(data.name = deparse(substitute(matrix)),
                statistic = L, p.value = pvalues,
                method = "Page")
  return(htest)
}
