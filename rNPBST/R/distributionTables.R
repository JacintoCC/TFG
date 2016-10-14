##################################
# READ XML DISTRIBUTION METHODS
##################################

#' @title Parse XML attributes to a data frame
#'
#' @description Parse XML Attributes of a row
#' @param x Row
#' @return Row with values and attribute names
attParse <- function(x){
  xattrs <- xmlAttrs(x)
  c(sapply(xmlChildren(x), xmlValue), xattrs)
}

#' @title Parse XML distribution to a data frame
#'
#' @description Parse XML distribution to a data frame
#' @param xml.file XML file with the distribution
#' @return Distribution data frame
xmlDistributionToDataFrame <-function(xml.file){
  parsed.file <- xmlParse(xml.file)
  dist.data.frame <- as.data.frame(t(xpathSApply(parsed.file, "//*/element", attParse)),
                                   stringsAsFactors = FALSE)
  colnames(dist.data.frame)[which(colnames(dist.data.frame) == "text")] <- "distribution"
  dist.data.frame <- transform(dist.data.frame,  distribution = as.numeric(distribution))

  if(ncol(dist.data.frame) == 3){
    dist.data.frame <- transform(dist.data.frame,  n = as.integer(n))
    dist.data.frame <- transform(dist.data.frame,  T = as.numeric(T))
  }
  else{
    dist.data.frame <- transform(dist.data.frame,  x = as.integer(x))
    dist.data.frame <- transform(dist.data.frame,  y = as.integer(y))
    dist.data.frame <- transform(dist.data.frame,  z = as.integer(z))
  }

  return(dist.data.frame)
}

#' @title Parse XML quantile distribution to a data frame
#'
#' @description Parse XML distribution to a data frame
#' @param xml.file XML file with the distribution
#' @param pvalues p-values of the distribution of the statistic
#' @return Distribution data frame
xmlQuantileDistributionToMatrix <-function(xml.file, pvalues){
  parsed.file <- xmlParse(xml.file)
  matrix <- type.convert(t(xpathSApply(parsed.file, "//*/row", attParse)))
  rownames(matrix) <- matrix[ ,"n"]
  matrix <- matrix[ ,-which(colnames(matrix) == "n")]
  colnames(matrix) <- pvalues
  return(matrix)
}

##################################
# GET VALUE OF STATISTIC DISTRIBUTION
##################################

#' @title Get value from distribution table
#'
#' @export
#' @description Function to get the exact probability given a distribution table
#' @param table Distribution table
#' @param n Size of the exact distribution
#' @param T Value of the statistic
#' @param epsilon Threshold for compare the statistic
#' @return p-value computed
getFromDistributionTable <- function(table, n, T, epsilon = 0.002){
  value <- table[which(table$n == n) && which(abs(table$T - T) < epsilon),
                 "distribution"]
  return(value)
}

#' @title Compute exact probability of distribution
#'
#' @export
#' @description Function to get the exact probability given a distribution table
#' @param table Distribution table
#' @param n Size of the exact distribution
#' @param T Value of the statistic
#' @return Exact p-value computed
computeExactProbability <- function(table, n, T){
  value <- getFromDistributionTable(table, n, abs(T))
  return(value)
}

#' @title Compute exact probability of distribution
#'
#' @description Function to get the exact probability given a distribution table
#' @param table Distribution table
#' @param n Size of the exact distribution
#' @param T Value of the statistic
#' @return p-value computed
computeAproximatedProbability <- function(table, n, T){

  for(i in 1:ncol(table)){
    if(T >= table[as.character(n),i])
      return(colnames(table)[i])
  }

  return(1.0)
}

#' @title Kolmogorov probability
#'
#' @description Computes p-value of the Kolmogorov distribution
#' @param n Size of the population
#' @param Dn Kolmogorov statistic
#' @return p-value computed
pkolmogorov <- function(n, Dn){
  data(KolmogorovTable)
  asymptoticValues <- c(1.07,1.22,1.36,1.52,1.63)

  if(n <= 40){
    for(i in ncol(KolmogorovTable):1){
      if(Dn >= table[as.character(n),i])
        return(colnames(table)[i])
    }
  }
  else{
    size <- sqrt(n)
    for(i in ncol(KolmogorovTable):1){
      if(Dn >= asymptoticValues[i]/size)
        return(colnames(table)[i])
    }
  }

  return(1.0)
}

#' @title Get cumulative probability function
#'
#' @description  Get cumulative probability function according to distribution
#' @param distribution Distribution name
#' @return Cumulative probability function
getCumulativeProbabilityFunction <- function(distribution, ...){
    switch(distribution,
      "NORMAL" = function(x) pnorm(q = x, ...),
      "UNIFORM" = function(x) punif(q = x, ...),
      "CHI_SQUARE" =function(x) pchisq(q = x, ...) ,
      "EXPONENTIAL" = function(x) pexp(q = x, ...),
      "GAMMA" = function(x) pgamma(q = x, ...),
      "LAPLACE" = function(x) plaplace(q = x, ...),
      "LOGISTIC" = function(x) plogis(q = x, ...),
      "WEIBULL" = function(x) pweibull(q = x, ...),
      "KOLMOGOROV" = function(x) pkolmogorov(Dn = x, ...)
    )
}
