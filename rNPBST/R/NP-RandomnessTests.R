################
# TOTAL NUMBER OF RUNS TEST
################

#' @title Exact Left tail probability fo number of runs distribution
#'
#' @description Computes left tail p-value of the Total number of runs distribution
#' @param a Number of elements of first class
#' @param b Number of elements of second class
#' @param runs Number of runs
#' @return pvalue computed
computeNumberOfRunsLeftTailProbability <- function(a, b, runs){
  n1 <- min(a,b)
  n2 <- max(a,b)

  if(n1 > 12 | (n1 < 9 & n1+n2 > 20) | (n1 > 8 & n2 >12))
    return(-1)

  data(TotalNumberOfRuns.Left)
  left.tail <- TotalNumberOfRuns.Left$distribution[TotalNumberOfRuns.Left$x == n1 &
                                                   TotalNumberOfRuns.Left$y == n2 &
                                                   TotalNumberOfRuns.Left$z == runs]
  right.tail <- TotalNumberOfRuns.Right$distribution[TotalNumberOfRuns.Right$x == n1 &
                                                     TotalNumberOfRuns.Right$y == n2 &
                                                     TotalNumberOfRuns.Right$z == runs+1]
  if(length(left.tail) == 0 || is.null(left.tail) || is.na(left.tail))
    left.tail <- -1
  if(length(right.tail) == 0 || is.null(right.tail) || is.na(right.tail))
    right.tail <- -1

  if(left.tail == -1 & right.tail == -1)
    return(-1)

  if(left.tail == -1)
    left.tail <- 1 - right.tail

  return(left.tail)
}


#' @title Exact Left tail probability fo number of runs distribution
#'
#' @description Computes left tail p-value of the Total number of runs distribution
#' @param a Number of elements of first class
#' @param b Number of elements of second class
#' @param runs Number of runs
#' @return pvalue computed
computeNumberOfRunsRightTailProbability <- function(a, b, runs){
  n1 <- min(a,b)
  n2 <- max(a,b)

  if(n1 > 12 | (n1 < 10 & n1+n2 > 20) | (n1 > 9 & n2 >12))
    return(-1)

  data(TotalNumberOfRuns.Left)
  left.tail <- TotalNumberOfRuns.Left$distribution[TotalNumberOfRuns.Left$x == n1 &
                                                   TotalNumberOfRuns.Left$y == n2 &
                                                   TotalNumberOfRuns.Left$z == runs-1]
  right.tail <- TotalNumberOfRuns.Right$distribution[TotalNumberOfRuns.Right$x == n1 &
                                                     TotalNumberOfRuns.Right$y == n2 &
                                                     TotalNumberOfRuns.Right$z == runs]
   if(length(left.tail) == 0 || is.null(left.tail) || is.na(left.tail))
     left.tail <- -1
   if(length(right.tail) == 0 || is.null(right.tail) || is.na(right.tail))
     right.tail <- -1

  if(left.tail == -1 & right.tail == -1)
    return(-1)

  if(right.tail == -1)
    right.tail <- 1 - left.tail

  return(right.tail)
}


#' @title Asymptotic values
#'
#' @description Computes asymptotic p-values of the Total number of runs distribution
#' @param a Number of elements of first class
#' @param b Number of elements of second class
#' @param runs Number of runs
#' @return pvalues computed
computeNumberOfRunsAsymptoticProbability <- function(a, b, runs){
  n1 <- a
  n2 <- b
  n <- n1 + n2

  denominator <- sqrt(2 * n1 * n2 * (2* n1 * n2 - n) / (n * n * (n-1)))
  numerator <- runs - 0.5 - 1 - 2 * n1 * n2 / n
  z <- numerator / denominator
  right.pvalue <- 1 - pnorm(z)

  numerator <- runs + 0.5 - 1 - 2 * n1 * n2 / n
  z <- numerator / denominator
  left.pvalue <- pnorm(z)

  double.pvalue <- doubleTailProbability(left.pvalue,right.pvalue)

  return(c("Asymptotic Left Tail" = left.pvalue,
           "Asymptotic Right Tail" = right.pvalue,
           "Asymptotic Double Tail" = double.pvalue))
}


#' @title Number of runs test for randomness
#'
#' @export
#' @description This function performs the Number of runs
#' @param sequence Sequence of data
#' @return A htest object with pvalues and statistics
numberRuns.test <- function(sequence){
  data.name <- deparse(substitute(sequence))
  sequence <- as.factor(sequence)

  if(length(levels(sequence)) != 2)
    stop("Total number of runs test only can be employed with binary sequences")

  n <- length(sequence)
  n1 <- sum(sequence == levels(sequence)[1])
  n2 <- sum(sequence == levels(sequence)[2])

  runs <- length(rle(as.numeric(sequence))$lengths)

  exact.left.tail <- computeNumberOfRunsLeftTailProbability(n1, n2, runs)
  exact.right.tail <- computeNumberOfRunsRightTailProbability(n1, n2, runs)
  exact.double.tail <- doubleTailProbability(exact.left.tail,exact.right.tail)

  asymptotic.values <- computeNumberOfRunsAsymptoticProbability(n1, n2, runs)

  pvalues <- c("Exact Left Tail" = exact.left.tail,
               "Exact Right Tail" = exact.right.tail,
               "Exact Double Tail" = exact.double.tail,
               asymptotic.values)

  htest <- list(data.name = data.name,
                statistic = c("Runs" = runs), p.value = pvalues,
                method = "Number of runs")
  return(htest)
}

################
# NUMBER OF RUNS UP AND DOWN MEDIAN TEST
################

#' @title Number of runs up and down median test for randomness
#'
#' @export
#' @description This function performs the Number of runs up and down median test
#' @param sequence Sequence of data
#' @return A htest object with pvalues and statistics
numberRunsUpDownMedian.test <- function(sequence){
  n <- length(sequence)

  median <- median(sequence[order(sequence)])
  n1 <- sum(sequence > median)
  n2 <- sum(sequence < median)

  sign.median <- sign(sequence - median)
  rle <- rle(sign.median)
  runs <- length(rle$values[rle$values != 0])

  exact.left.tail <- computeNumberOfRunsLeftTailProbability(n1, n2, runs)
  exact.right.tail <- computeNumberOfRunsRightTailProbability(n1, n2, runs)

  if(exact.left.tail == -1)
    exact.left.tail <- 1
  if(exact.right.tail == -1)
    exact.right.tail <- 1

  exact.double.tail <- doubleTailProbability(exact.left.tail,exact.right.tail)

  asymptotic.values <- computeNumberOfRunsAsymptoticProbability(n1, n2, runs)

  pvalues <- c("Exact Left Tail" = exact.left.tail,
               "Exact Right Tail" = exact.right.tail,
               "Exact Double Tail" = exact.double.tail,
               asymptotic.values)

  htest <- list(data.name = deparse(substitute(sequence)),
                statistic = c("Number of elements" = n,
                              "Number of runs" = runs,
                              "Median" = median),
                p.value = pvalues,
                method = "Runs Up and Down With respect of the median")
  return(htest)
}

################
# NUMBER OF RUNS UP AND DOWN TEST
################


#' @title Exact tail probability fo number of runs distribution
#'
#' @description  Computes exact p-value of the Runs up down distribution
#' @param n Number of elements
#' @param R Runs up down statistic
#' @return pvalue computed
computeRunsUpDownExactProbability <- function(n, R){

  if(n < 3 | n > 25 | R >= n | R < 1)
    return(c("Exact Left Tail" = -1,
             "Exact Right Tail" = -1,
             "Exact Double Tail" = -1))


  left.limits <- c(0,0,0,1,1,2,3,3,4,5,5,6,7,7,8,9,9,10,11,11,12,13,13,14,15,15)

  data(RunsUpDown)
  if(R > left.limits[n]){
    left <- 1
    right <- RunsUpDown$distribution[RunsUpDown$x == n &
                                     RunsUpDown$y == R]
  }
  else{
    left <- RunsUpDown$distribution[RunsUpDown$x == n &
                                    RunsUpDown$y == R]
    right <- 1
  }

  return(c("Exact Left Tail" = left,
           "Exact Right Tail" = right,
           "Exact Double Tail" = doubleTailProbability(left, right)))
}


#' @title Asymptotic values of runs up and down test
#'
#' @description Computes asymptotic p-values of the Total number of runs up and down distribution
#' @param n Number of elements
#' @param R Runs up down statistic
#' @return Asymptotic pvalues computed
computeNumberOfRunsAsymptoticProbability <- function(n, R){
  denominator <- sqrt((16 * n - 29) / 90)
  numerator <- R - 0.5 - (2 * n - 1) / 3
  z <- numerator / denominator
  right.pvalue <- pnorm(z)

  numerator <- R + 0.5 - (2 * n - 1) / 3
  z <- numerator / denominator
  left.pvalue <- pnorm(z)

  double.pvalue <- doubleTailProbability(left.pvalue,right.pvalue)

  return(c("Asymptotic Left Tail" = left.pvalue,
           "Asymptotic Right Tail" = right.pvalue,
           "Asymptotic Double Tail" = double.pvalue))
}

#' @title Number of runs up and down median test for randomness
#'
#' @export
#' @description This function performs the Number of runs up and down  test
#' @param sequence Sequence of data
#' @return A htest object with pvalues and statistics
numberRunsUpDown.test <- function(sequence){
  n <- length(sequence)

  sign.trend <- sign(sequence - c(0, sequence[-n]))
  rle <- rle(sign.trend)
  runs <- length(rle$values[rle$values != 0])

  exact.values <- computeRunsUpDownExactProbability(n, runs)
  asymptotic.values <- computeNumberOfRunsAsymptoticProbability(n, runs)

  pvalues <- c(exact.values, asymptotic.values)

  htest <- list(data.name = deparse(substitute(sequence)),
                statistic = runs, p.value = pvalues,
                method = "Number of runs up and down")
  return(htest)
}




################
# VON NEUMANN TEST
################

#' @title Exact tail probability for Von Neumann distribution
#'
#' @description  Computes exact p-value of the Von Neumann distribution
#' @param n Number of elements
#' @param NM NM statistic
#' @param RVN RVN statistic
#' @return pvalue computed
computeVonNewmannExactProbability <- function(n, NM, RVN){

  if(n < 10){
    data(NMRanksLeft)
    data(NMRanksRight)

    int.NM <- ceil(NM)
    left <- NMRanksLeft$distribution[NMRanksLeft$x == n & NMRanksLeft$y == int.NM]

    if(left == -1){
      probs <- NMRanksLeft$distribution[NMRanksLeft$x == n & NMRanksLeft$y > int.NM]
      if(length(probs[probs != -1]) > 0)
        left <- 1
    }

    int.NM <- floor(NM)
    right <- NMRanksRight$distribution[NMRanksRight$x == n & NMRanksRight$y == int.NM]

    if(right == -1){
      probs <- NMRanksRight$distribution[NMRanksRight$x == n & NMRanksRight$y > int.NM]
      if(length(probs[probs != -1]) > 0)
        right <- 1
    }
  }
  else{
    data(RanksVonNeumann)

    row <- RanksVonNeumann[n, ]
    left <- as.numeric(names(row[row >= RVN])[1])
    right <- as.numeric(names(row[4 - row <= RVN])[1])

    if(is.null(left) || is.na(left))
      left <- 1
    if(is.null(right) || is.na(right))
      right <- 1
  }

  return(c("Exact Left Tail" = left,
           "Exact Right Tail" = right,
           "Double Tail" = doubleTailProbability(left, right)))
}

#' @title Von Neumann test for randomness
#'
#' @export
#' @description This function performs the Von Neumann
#' @param sequence Sequence of data
#' @return A htest object with pvalues and statistics
vonNeumann.test <- function(sequence){
  n <- length(sequence)
  ranks <- rank(sequence)

  #Compute NM statistic
  NM.vector <- sapply(1:(n-1), function(i) (ranks[i]-ranks[i+1])*(ranks[i]-ranks[i+1]))
  NM <- sum(NM.vector)

  # Compute RVN statistic
  denominator <- sum((ranks - (n+1) / 2) * (ranks - (n+1) / 2))
  RVN <- NM / denominator

  exact.values <- computeVonNewmannExactProbability(n, NM, RVN)

  variance <- 4 * (n-2) * (5*n*n - 2*n - 9) / (5 * n * (n+1) * (n-1) * (n-1))
  z <- (RVN - 2) / sqrt(variance)


  asymptotic.values <- c("Asymptotic Left Tail" = pnorm(z),
                         "Asymptotic Right Tail" = 1 - pnorm(z),
                         "Asymptotic Double Tail" = doubleTailProbability(pnorm(z), 1 - pnorm(z)))

  pvalues <- c(exact.values, asymptotic.values)

  htest <- list(data.name = deparse(substitute(sequence)),
                statistic = c("NM" = NM, "RVN" = RVN), p.value = pvalues,
                method = "Von Neumann")
  return(htest)
}
