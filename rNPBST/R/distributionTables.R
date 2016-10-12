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
getFromDistributionTable <- function(table, n.value, T.value, epsilon = 0.002){
  value <- table[which(table$n == n.value) && which(abs(table$T - T.value) < epsilon),
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
