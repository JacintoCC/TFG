#' @title Daniel Trend test for bivariated samples
#'
#' @export
#' @description This function performs the Daniel Trend test
#' @param matrix Matrix of data
#' @return A htest object with pvalues and statistics
danielTrend.test <- function(matrix){

  # Checks
  if(ncol(matrix) != 2)
    stop("This test only can be employed with two samples")

  n <- nrow(matrix)

  if(n < 3)
    stop("This test need samples of size 3 or more")

  if(anyNA(matrix))
    stop("No null values allowed in this test.")

  # Ascending rank for both columns
  rank.first <- rank(matrix[ ,1])
  rank.second <- rank(matrix[ ,2])

  # Compute sum D statistic
  sumD <- sum( (rank.first - rank.second)^2 )

  # Compute R statistic
  R <- 1 - (6 * sumD) / (n * (n*n - 1))

  # Compute P-Values
  if(n <= 10){
    data(SpearmanExactTable)
    pvalue <- computeExactProbability(SpearmanExactTable, n, R)
  }
  else if(n <= 30){
    data(SpearmanQuantileTable)
    pvalue <- computeAproximatedProbability(SpearmanQuantileTable, n, R)
  }

  # Compute asymptotic p-value
  Z <- R * sqrt(n-1)

  positive.dependence.pvalue <- pnorm(Z)
  negative.dependence.pvalue <- 1 - pnorm(Z)
  no.dependence.pvalue <- 2 * min(positive.dependence.pvalue,
                                  negative.dependence.pvalue)

  statistic <- list(D = sumD, R = R, Z = Z)
  pvalues <- ifelse(n <= 30, list(pvale = pvalue,
                                  pos.dep.pvalue = positive.dependence.pvalue,
                                  neg.dep.pvalue = negative.dependence.pvalue,
                                  no.dependence.pvalue = no.dependence.pvalue),
                             list(pos.dep.pvalue = positive.dependence.pvalue,
                                  neg.dep.pvalue = negative.dependence.pvalue,
                                  no.dependence.pvalue = no.dependence.pvalue))

  htest <- make.htest(data.name = deparse(substitute(matrix)),
                      statistic = statistic, p.value = pvalues,
                      method = "Daniel Trend")
  return(htest)
}
