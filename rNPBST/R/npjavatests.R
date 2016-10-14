#' @title Execution of a StatisticalTest object
#'
#' @export
#' @description Wrapper to run a StatisticalTest object.
#' @param java.test A StatisticalTest Java object
#' @return The function returns the report of the test in a string
runTest <- function(java.test.object){
  # Run test
  .jcall(java.test.object, "V", "doTest")
  out <- .jcall(java.test.object, "S", "printReport")
  return(out)
}

#' @title Carry out a test introducing name and data
#'
#' @description Function to make a non-parametric test
#' @param test.name Name of the test.
#' @return The function returns the report of the test in a string
doTest <- function(test.name, ...){
  java.t.classes <- c("javanpst.tests.countData.contingencyCoefficient.ContingencyCoefficient",
                      "javanpst.tests.countData.multinomialEqualityTest.MultinomialEqualityTest",
                      "javanpst.tests.countData.orderedEqualityTest.OrderedEqualityTest",
                      "javanpst.tests.equality.extendedMedianTest.ExtendedMedianTest",
                      "javanpst.tests.equality.JTTest.JTTest",
                      "javanpst.tests.equality.kruskalWallisTest.KruskalWallisTest",
                      "javanpst.tests.goodness.A_DTest.A_DTest",
                      "javanpst.tests.goodness.chiSquareTest.ChiSquareTest",
                      "javanpst.tests.goodness.K_STest.K_STest",
                      "javanpst.tests.goodness.lillieforsTest.LillieforsTest",
                      "javanpst.tests.location.normalScoresTest.NormalScoresTest",
                      "javanpst.tests.location.wilcoxonRankSumTest.WilcoxonRankSumTest",
                      "javanpst.tests.multiple.concordanceCoefficient.ConcordanceCoefficient",
                      "javanpst.tests.multiple.friedmanTest.FriedmanTest",
                      "javanpst.tests.multiple.incompleteConcordance.IncompleteConcordance",
                      "javanpst.tests.multiple.pageTest.PageTest",
                      "javanpst.tests.multiple.partialCorrelationTest.PartialCorrelationTest",
                      "javanpst.tests.oneSample.confidenceQuantile.ConfidenceQuantile",
                      "javanpst.tests.oneSample.populationQuantile.PopulationQuantile",
                      "javanpst.tests.oneSample.signTest.SignTest",
                      "javanpst.tests.oneSample.wilcoxonTest.WilcoxonTest",
                      "javanpst.tests.randomness.numberRunsTest.NumberRunsTest",
                      "javanpst.tests.randomness.runsUpDownMedianTest.RunsUpDownMedianTest",
                      "javanpst.tests.randomness.runsUpDownTest.RunsUpDownTest",
                      "javanpst.tests.randomness.vonNeumannTest.VonNeumannTest",
                      "javanpst.tests.scale.david_BartonTest.David_BartonTest",
                      "javanpst.tests.scale.freund_Ansari_BradleyTest.Freund_Ansari_BradleyTest",
                      "javanpst.tests.scale.klotzTest.KlotzTest",
                      "javanpst.tests.scale.moodTest.MoodTest",
                      "javanpst.tests.scale.siegel_TukeyTest.Siegel_TukeyTest",
                      "javanpst.tests.scale.sukhatmeTest.SukhatmeTest",
                      "javanpst.tests.twoSample.controlMedianTest.ControlMedianTest",
                      "javanpst.tests.twoSample.K_STest.K_STest",
                      "javanpst.tests.twoSample.medianTest.MedianTest",
                      "javanpst.tests.twoSample.wald_WolfowitzTest.Wald_WolfowitzTest")

  names(java.t.classes) <-  c("kendall",
                              "contingency coeff",
                              "multinomial equality",
                              "ordered equality",
                              "extended median",
                              "JT",
                              "kruskal",
                              "AD",
                              "chi square",
                              "KS",
                              "lilliefors",
                              "normal scores",
                              "wilcoxon rank sum",
                              "concordance coeff",
                              "friedman",
                              "incomplete concordance",
                              "page",
                              "partial correlation",
                              "confidence quantile",
                              "population quantile",
                              "sign",
                              "wilcoxon",
                              "number runs",
                              "runs median",
                              "runs up down",
                              "vonNeumann",
                              "david barton",
                              "freud ansari bradley",
                              "klotz",
                              "mood",
                              "siegel tukey",
                              "sukhatme",
                              "control median",
                              "KS",
                              "median",
                              "wald wolfowitz")

  if(test.name %in% names(java.t.classes)){
    # Create Java object
    java.test.object <- .jnew(java.t.classes[test.names], ...)
    return(runTest(java.test.object))
  }
  else{
    return("Test name is not supported\n")
  }
}


#' @title Make a htest object
#'
#' @export
#' @description This function takes the arguments, makes a list and assign the list the htest class
#' @return htest object
make.htest <- function(...){
  htest <- list(...)
  class(htest) <- "htest"
  return(htest)
}

#' @title Contingency Coefficient test for count data
#'
#' @export
#' @description This function performs the Contingency Coefficient test
#' @param matrix Matrix of data
#' @return A htest object with pvalues and statistics
contingency.coeff.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.countData.contingencyCoefficient.ContingencyCoefficient",
                            dataTable(matrix))
  report <- runTest(java.test.object)
  q <- .jcall(java.test.object, "D", "getQ")
  coefficients <- list(c = .jcall(java.test.object, "D", "getC"),
                       phi = .jcall(java.test.object, "D", "getPhi"))
  pvalue <- .jcall(java.test.object, "D", "getPValue")
  htest <- make.htest(data.name = deparse(substitute(matrix)),
                      statistic = q, p.value = pvalue,
                      coefficients = coefficients,
                      method = "contingency coeff")
  return(htest)
}

#' @title Multinomial equality test for count data
#'
#' @export
#' @description This function performs the Multinomial equality test
#' @param matrix Matrix of data
#' @return A htest object with pvalues and statistics
multinomialEq.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.countData.multinomialEqualityTest.MultinomialEqualityTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
  q <- .jcall(java.test.object, "D", "getQ")
  pvalue <- .jcall(java.test.object, "D", "getPValue")
  htest <- make.htest(data.name = deparse(substitute(matrix)),
                      statistic = q, p.value = pvalue,
                      method = "multinomial equality")
  return(htest)
}

#' @title Ordered equality test for count data
#'
#' @export
#' @description This function performs the Ordered equality test
#' @param matrix Matrix of data
#' @return A htest object with pvalues and statistics
orderedEq.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.countData.orderedEqualityTest.OrderedEqualityTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
  statistic <- c(Wx = .jcall(java.test.object, "D", "getWx"),
                    Wy = .jcall(java.test.object, "D", "getWy"))
  pvalue <- c(right = .jcall(java.test.object, "D", "getRightPValue"),
                 left = .jcall(java.test.object, "D", "getLeftPValue"),
                 double = .jcall(java.test.object, "D", "getDoublePValue"))
  htest <- make.htest(data.name = deparse(substitute(matrix)),
                      statistic = statistic, p.value = pvalue,
                      method = "ordered equality")
  return(htest)
}


#' @title Extended Median test for equality
#'
#' @export
#' @description This function performs the extended median test
#' @param matrix Matrix of data
#' @return A htest object with pvalues and statistics
extendedMedian.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.equality.extendedMedianTest.ExtendedMedianTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
  statistic <- c(median = .jcall(java.test.object, "D", "getMedian"),
                    q = .jcall(java.test.object, "D", "getQ"),
                    improved.q = .jcall(java.test.object, "D", "getImprovedQ"))
  pvalue <- .jcall(java.test.object, "D", "getPValue")
  htest <- make.htest(data.name = deparse(substitute(matrix)),
                      statistic = statistic, p.value = pvalue,
                      method = "exteded median")
  return(htest)
}

#' @title Jonckheere and Terpstra test for equality
#'
#' @export
#' @description This function performs the Jonckheere and Terpstra test
#' @param matrix Matrix of data
#' @return A htest object with pvalues and statistics
jt.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.equality.JTTest.JTTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
  statistic <- c(b = .jcall(java.test.object, "D", "getB"),
                 z = .jcall(java.test.object, "D", "getZ"))
  pvalue <- .jcall(java.test.object, "D", "getPValue")
  htest <- make.htest(data.name = deparse(substitute(matrix)),
                      statistic = statistic, p.value = pvalue,
                      method = "JT")
  return(htest)
}

#' @title Kruskal-Wallis est for equality
#'
#' @export
#' @description This function performs the Kruskal-Wallis test
#' @param matrix Matrix of data
#' @return A htest object with pvalues and statistics
kruskalWallis.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.equality.kruskalWallisTest.KruskalWallisTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
  htest <- make.htest(data.name = deparse(substitute(matrix)),
                      method = "Kruskal Wallis")
  return(htest)
}

#' @title Anderson-Darling test for goodness of fit
#'
#' @export
#' @description This function performs the Anderson-Darling test
#' @param matrix Matrix of data
#' @return A htest object with pvalues and statistics
ad.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.goodness.A_DTest.A_DTest",
                            numericSequence(matrix))
  report <- runTest(java.test.object)
  a <- .jcall(java.test.object, "D", "getA")
  pvalue <- .jcall(java.test.object, "D", "getPValue")
  htest <- make.htest(data.name = deparse(substitute(matrix)),
                      statistic = a, p.value = pvalue,
                      method = "AD")
  return(htest)
}

#' @title Chi square test for goodness of fit
#'
#' @export
#' @description This function performs the Chi square test
#' @param sequence Sequence of data
#' @return A htest object with pvalues and statistics
chiSquare.test <- function(sequence){
  java.test.object <- .jnew("javanpst.tests.goodness.chiSquareTest.ChiSquareTest",
                            numericSequence(sequence))
  report <- runTest(java.test.object)
  q <- .jcall(java.test.object, "D", "getQ")
  pvalue <- .jcall(java.test.object, "D", "getPValue")
  htest <- make.htest(data.name = deparse(substitute(sequence)),
                      statistic = q, p.value = pvalue,
                      method = "chi square")
  return(htest)
}


#' @title Normal scores test for location
#'
#' @export
#' @description This function performs the normal scores test
#' @param matrix Matrix of data
#' @return A htest object with pvalues and statistics
normalScores.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.location.normalScoresTest.NormalScoresTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
  statistic <- c(normalStatistic1 = .jcall(java.test.object, "D", "getNormalStatistic1"),
                    normalStatistic2 = .jcall(java.test.object, "D", "getNormalStatistic2"))
  pvalue <- c(left = .jcall(java.test.object, "D", "getLeftPValue"),
                 right = .jcall(java.test.object, "D", "getRightPValue"),
                 double = .jcall(java.test.object, "D", "getDoublePValue"))
  htest <- make.htest(data.name = deparse(substitute(matrix)),
                      statistic = statistic, p.value = pvalue,
                      method = "normal scores")
  return(htest)
}

#' @title Wilcoxon Rank Sum test for location
#'
#' @description This function performs the Wilcoxon Rank Sum test
#' @param matrix Matrix of data
#' @return A htest object with pvalues and statistics
wilcoxonRankSum.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.location.wilcoxonRankSumTest.WilcoxonRankSumTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
  w <- .jcall(java.test.object, "D", "getStatistic1")
  pvalue <- c(exact.left = .jcall(java.test.object, "D", "getExactLeftPValue"),
                 exact.right = .jcall(java.test.object, "D", "getExactRightPValue"),
                 exact.double = .jcall(java.test.object, "D", "getExactDoublePValue"),
                 exact.double.90 = .jcall(java.test.object, "D", "getExactConfidence90"),
                 exact.double.95 = .jcall(java.test.object, "D", "getExactConfidence95"),
                 asymptotic.left = .jcall(java.test.object, "D", "getAsymptoticLeftPValue"),
                 asymptotic.right = .jcall(java.test.object, "D", "getAsymptoticRightPValue"),
                 asymptotic.double = .jcall(java.test.object, "D", "getExactDoublePValue"))
  confidenceInterval <- list(confidenceInterval95 = .jcall(java.test.object, "S", "printConfidenceInterval95"),
                             confidenceInterval90 = .jcall(java.test.object, "S", "printConfidenceInterval90"))
  htest <- make.htest(data.name = deparse(substitute(matrix)),
                      statistic = w, p.value = pvalue,
                      method = "wilcoxon rank sum", report = report,
                      confidenceInterval = confidenceInterval)
  return(htest)
}

#' @title Concordance Coefficient test for multiple comparisons
#'
#' @export
#' @description This function performs the Concordance Coefficient test
#' @param matrix Matrix of data
#' @return A htest object with pvalues and statistics
concordanceCoeff.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.multiple.concordanceCoefficient.ConcordanceCoefficient",
                            dataTable(matrix))
  report <- runTest(java.test.object)
  statistic <- c(s = .jcall(java.test.object, "D", "getS"),
                    q = .jcall(java.test.object, "D", "getQ"),
                    w = .jcall(java.test.object, "D", "getW"))
  pvalue <- .jcall(java.test.object, "D", "getPValue")
  htest <- make.htest(data.name = deparse(substitute(matrix)),
                      statistic = statistic, p.value = pvalue,
                     method = "concordance coeff")
  return(htest)
}

#' @title Friedman test for multiple comparisons
#'
#' @export
#' @description This function performs the Friedman test
#' @param matrix Matrix of data
#' @return A htest object with pvalues and statistics
friedman.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.multiple.friedmanTest.FriedmanTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
  statistic <- c(s = .jcall(java.test.object, "D", "getS"),
                 q = .jcall(java.test.object, "D", "getQ"))
  pvalue <- .jcall(java.test.object, "D", "getPValue")
  htest <- make.htest(data.name = deparse(substitute(matrix)),
                      statistic = statistic, p.value = pvalue,
                      method = "Friedman test")
  return(htest)
}

#' @title Incomplete Concordance test for multiple comparisons
#'
#' @export
#' @description This function performs the Incomplete Concordance test
#' @param matrix Matrix of data
#' @param lambda Parameter of the test
#' @return A htest object with pvalues and statistics
incompleteConcordance.test <- function(matrix, lambda){
  java.test.object <- .jnew("javanpst.tests.multiple.incompleteConcordance.IncompleteConcordance",
                            dataTable(matrix), lambda)
  report <- runTest(java.test.object)
  statistic <- c(q = .jcall(java.test.object, "D", "getQ"),
                    w = .jcall(java.test.object, "D", "getW"))
  pvalue <- .jcall(java.test.object, "D", "getPValue")
  htest <- make.htest(data.name = deparse(substitute(matrix)),
                      statistic = statistic, p.value = pvalue,
                      method = "incomplete concordance")
  return(htest)
}

#' @title Page test for multiple comparisons
#'
#' @description This function performs the Page test
#' @param matrix Matrix of data
#' @return A htest object with pvalues and statistics
page.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.multiple.pageTest.PageTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
  l <- .jcall(java.test.object, "D", "getL")
  pvalue <- c(exact = .jcall(java.test.object, "D", "getExactPValue"),
                 asymptotic = .jcall(java.test.object, "D", "getAsymptoticPValue"))
  htest <- make.htest(data.name = deparse(substitute(matrix)),
                      statistic = l, p.value = pvalue,
                      method = "page")
  return(htest)
}

#' @title Partial correlation test for multiple comparisons
#'
#' @description This function performs the partial correlation test
#' @param matrix Matrix of data
#' @return A htest object with pvalues and statistics
partialCorrelation.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.multiple.partialCorrelationTest.PartialCorrelationTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
  tau <- .jcall(java.test.object, "D", "getTau")
  pvalue <- .jcall(java.test.object, "D", "getPValue")
  htest <- make.htest(data.name = deparse(substitute(matrix)),
                      statistic = tau, p.value = pvalue,
                      method = "partial correlation")
  return(htest)
}

#' @title Confidence Quantile for one sample
#'
#' @export
#' @description This function performs the confidence quantile test
#' @param n Parameter of the test
#' @param p Parameter of the test
#' @param q Parameter of the test
#' @return Report of the test in a string
confidenceQuantile.test <- function(n, p, q){
  java.test.object <- .jnew("javanpst.tests.oneSample.confidenceQuantile.ConfidenceQuantile",
                            n, p, q)
  report <- runTest(java.test.object)
  return(report=report)
}

#' @title Population quantile for one sample
#'
#' @export
#' @description This function performs the confidence quantile test
#' @param sequence  Parameter of the test
#' @param quantile Parameter of the test
#' @param value Parameter of the test
#' @return Report of the test in a string
populationQuantile.test <- function(sequence, quantile, value){
  java.test.object <- .jnew("javanpst.tests.oneSample.populationQuantile.PopulationQuantile",
                            numericSequence(sequence), quantile, value)
  report <- runTest(java.test.object)
  k <- .jcall(java.test.object, "D", "getK")
  pvalue <- c(left = .jcall(java.test.object, "D", "getExactLeftPValue"),
                 right = .jcall(java.test.object, "D", "getExactRightPValue"),
                 double = .jcall(java.test.object, "D", "getExactDoublePValue"))
  htest <- make.htest(data.name = deparse(substitute(sequence)),
                      statistic = statistic, p.value = pvalue,
                      method = "population quantile")
  return(htest)
}

#' @title Sign test for one sample
#'
#' @export
#' @description This function performs the Sign test
#' @param sequence Sequence of data
#' @return A htest object with pvalues and statistics
sign.test <- function(sequence){
  java.test.object <- .jnew("javanpst.tests.oneSample.signTest.SignTest",
                            numericSequence(sequence))
  report <- runTest(java.test.object)
  statistic <- c(k = .jcall(java.test.object, "D", "getK"),
                    k2 = .jcall(java.test.object, "D", "getK2"))
  pvalue <- c(exact.left = .jcall(java.test.object, "D", "getExactLeftPValue"),
                 exact.right = .jcall(java.test.object, "D", "getExactRightPValue"),
                 exact.double = .jcall(java.test.object, "D", "getExactDoublePValue"),
                 asymptotic.left = .jcall(java.test.object, "D", "getLeftPValue"),
                 asymptotic.right = .jcall(java.test.object, "D", "getRightPValue"),
                 asymptotic.double = .jcall(java.test.object, "D", "getDoublePValue"))
  htest <- make.htest(data.name = deparse(substitute(sequence)),
                      statistic = statistic, p.value = pvalue,
                      method = "sign")
  return(htest)
}

#' @title Wilcoxon test for one sample
#'
#' @description This function performs the Wilcoxon test
#' @param matrix Matrix of data
#' @return A htest object with pvalues and statistics
wilcoxon.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.oneSample.wilcoxonTest.WilcoxonTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
  statistic <- c(r.plus = .jcall(java.test.object, "D", "getRPlus"),
                    r.minus = .jcall(java.test.object, "D", "getRMinus"))
  pvalue <- c(exact.left = .jcall(java.test.object, "D", "getExactLeftPValue"),
                 exact.right = .jcall(java.test.object, "D", "getExactRightPValue"),
                 exact.double = .jcall(java.test.object, "D", "getExactDoublePValue"),
                 asymptotic.left = .jcall(java.test.object, "D", "getLeftPValue"),
                 asymptotic.right = .jcall(java.test.object, "D", "getRightPValue"),
                 asymptotic.double = .jcall(java.test.object, "D", "getDoublePValue"))
  htest <- make.htest(data.name = deparse(substitute(matrix)),
                      statistic = statistic, p.value = pvalue,
                      method = "wilcoxon")
  return(htest)
}

#' @title Number of Runs test for randomness
#'
#' @description This function performs the Number Of Runs test
#' @param sequence Sequence of data
#' @return A htest object with pvalues and statistics
numberRuns.test <- function(sequence){
  java.test.object <- .jnew("javanpst.tests.randomness.numberRunsTest.NumberRunsTest",
                            stringSequence(sequence))
  report <- runTest(java.test.object)
  runs <- .jcall(java.test.object, "D", "getRuns")
  pvalue <- c(exact.left = .jcall(java.test.object, "D", "getExactLeftPValue"),
                 exact.right = .jcall(java.test.object, "D", "getExactRightPValue"),
                 exact.double = .jcall(java.test.object, "D", "getExactDoublePValue"),
                 asymptotic.left = .jcall(java.test.object, "D", "getLeftPValue"),
                 asymptotic.right = .jcall(java.test.object, "D", "getRightPValue"),
                 asymptotic.double = .jcall(java.test.object, "D", "getDoublePValue"))
  htest <- make.htest(data.name = deparse(substitute(sequence)),
                      statistic = runs, p.value = pvalue,
                      method = "number runs")
  return(htest)
}

#' @title Number of Runs Up and Down Median test for randomness
#'
#' @description This function performs the Number of Runs Up and Down Median test
#' @param sequence Sequence of data
#' @return A htest object with pvalues and statistics
numberRunsUpDownMedian.test <- function(sequence){
  java.test.object <- .jnew("javanpst.tests.randomness.runsUpDownMedianTest.RunsUpDownMedianTest",
                            numericSequence(sequence))
  report <- runTest(java.test.object)
  statistic <- c(runs = .jcall(java.test.object, "D", "getRuns"),
                    median = .jcall(java.test.object, "D", "getMedian"))
  pvalue <- c(exact.left = .jcall(java.test.object, "D", "getExactLeftPValue"),
                 exact.right = .jcall(java.test.object, "D", "getExactRightPValue"),
                 exact.double = .jcall(java.test.object, "D", "getExactDoublePValue"),
                 asymptotic.left = .jcall(java.test.object, "D", "getLeftPValue"),
                 asymptotic.right = .jcall(java.test.object, "D", "getRightPValue"),
                 asymptotic.double = .jcall(java.test.object, "D", "getDoublePValue"))
  htest <- make.htest(data.name = deparse(substitute(sequence)),
                      statistic = statistic, p.value = pvalue,
                      method = "number runs median")
  return(htest)
}

#' @title Number of Runs Up and Down test for randomness
#'
#' @description This function performs the Number of Runs Up and Down test
#' @param sequence Sequence of data
#' @return A htest object with pvalues and statistics
numberRunsUpDown.test <- function(sequence){
  java.test.object <- .jnew("javanpst.tests.randomness.runsUpDownTest.RunsUpDownTest",
                            numericSequence(sequence))
  report <- runTest(java.test.object)
  runs <- .jcall(java.test.object, "D", "getRuns")
  pvalue <- c(exact.left = .jcall(java.test.object, "D", "getExactLeftPValue"),
                 exact.right = .jcall(java.test.object, "D", "getExactRightPValue"),
                 exact.double = .jcall(java.test.object, "D", "getExactDoublePValue"),
                 asymptotic.left = .jcall(java.test.object, "D", "getLeftPValue"),
                 asymptotic.right = .jcall(java.test.object, "D", "getRightPValue"),
                 asymptotic.double = .jcall(java.test.object, "D", "getDoublePValue"))
  htest <- make.htest(data.name = deparse(substitute(sequence)),
                      statistic = runs, p.value = pvalue,
                      method = "number runs up down")
  return(htest)
}

#' @title Von Neumann test for randomness
#'
#' @description This function performs the Von Neumann test
#' @param sequence Sequence of data
#' @return A htest object with pvalues and statistics
vonNeumann.test <- function(sequence){
  java.test.object <- .jnew("javanpst.tests.randomness.vonNeumannTest.VonNeumannTest",
                            numericSequence(sequence))
  report <- runTest(java.test.object)
  statistic <- c(NM = .jcall(java.test.object, "D", "getNM"),
                    RVN = .jcall(java.test.object, "D", "getRVN"))
  pvalue <- c(exact.left = .jcall(java.test.object, "D", "getExactLeftPValue"),
                 exact.right = .jcall(java.test.object, "D", "getExactRightPValue"),
                 exact.double = .jcall(java.test.object, "D", "getExactDoublePValue"),
                 asymptotic.left = .jcall(java.test.object, "D", "getLeftPValue"),
                 asymptotic.right = .jcall(java.test.object, "D", "getRightPValue"),
                 asymptotic.double = .jcall(java.test.object, "D", "getDoublePValue"))
  htest <- make.htest(data.name = deparse(substitute(sequence)),
                      statistic = statistic, p.value = pvalue,
                      method = "vonNeumann")
  return(htest)
}

#' @title David Barton test for scale
#'
#' @export
#' @description This function performs the David Barton test
#' @param matrix Matrix of data
#' @return A htest object with pvalues and statistics
davidBarton.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.scale.david_BartonTest.David_BartonTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
  statistic <- c(MNx = .jcall(java.test.object, "D", "getTestStatistic1"),
                    MNy = .jcall(java.test.object, "D", "getTestStatistic2"))
  pvalue <- c(asymptotic.left = .jcall(java.test.object, "D", "getLeftPValue"),
                 asymptotic.right = .jcall(java.test.object, "D", "getRightPValue"),
                 asymptotic.double = .jcall(java.test.object, "D", "getDoublePValue"))
  htest <- make.htest(data.name = deparse(substitute(matrix)),
                      statistic = statistic, p.value = pvalue,
                      method = "david barton")
  return(htest)
}

#' @title Freund Ansari Bradley test for scale
#'
#' @export
#' @description This function performs the Freund Ansari Bradley test
#' @param matrix Matrix of data
#' @return A htest object with pvalues and statistics
freundAnsariBradley.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.scale.freund_Ansari_BradleyTest.Freund_Ansari_BradleyTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
  statistic <- c(MNx = .jcall(java.test.object, "D", "getTestStatistic1"),
                    MNy = .jcall(java.test.object, "D", "getTestStatistic2"))
  pvalue <- c(asymptotic.left = .jcall(java.test.object, "D", "getLeftPValue"),
                 asymptotic.right = .jcall(java.test.object, "D", "getRightPValue"),
                 asymptotic.double = .jcall(java.test.object, "D", "getDoublePValue"))
  htest <- make.htest(data.name = deparse(substitute(matrix)),
                      statistic = statistic, p.value = pvalue,
                      method = "freund ansari bradley")
  return(htest)
}

#' @title Klotz test for scale
#'
#' @export
#' @description This function performs the Klotz test
#' @param matrix Matrix of data
#' @return A htest object with pvalues and statistics
klotz.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.scale.klotzTest.KlotzTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
  statistic <- c(MNx = .jcall(java.test.object, "D", "getTestStatistic1"),
                    MNy = .jcall(java.test.object, "D", "getTestStatistic2"))
  pvalue <- c(x.pvalue = .jcall(java.test.object, "D", "getPValue1"),
                 y.pvalue = .jcall(java.test.object, "D", "getPValue2"))
  htest <- make.htest(data.name = deparse(substitute(matrix)),
                      statistic = statistic, p.value = pvalue,
                      method = "klotz")
  return(htest)
}

#' @title Mood test for scale
#'
#' @export
#' @description This function performs the Mood test
#' @param matrix Matrix of data
#' @return A htest object with pvalues and statistics
mood.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.scale.moodTest.MoodTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
  statistic <- c(MNx = .jcall(java.test.object, "D", "getTestStatistic1"),
                    MNy = .jcall(java.test.object, "D", "getTestStatistic2"))
  pvalue <- c(x.pvalue = .jcall(java.test.object, "D", "getPValue1"),
                 y.pvalue = .jcall(java.test.object, "D", "getPValue2"))
  htest <- make.htest(data.name = deparse(substitute(matrix)),
                      statistic = statistic, p.value = pvalue,
                      method = "mood")
  return(htest)
}

#' @title Siegel Tukey test for scale
#'
#' @description This function performs the Siegel Tukey test
#' @param matrix Matrix of data
#' @return A htest object with pvalues and statistics
siegelTukey.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.scale.siegel_TukeyTest.Siegel_TukeyTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
  statistic <- c(ST1 = .jcall(java.test.object, "D", "getTestStatistic1"),
                    ST2 = .jcall(java.test.object, "D", "getTestStatistic2"))
  pvalue <- c(exact.left = .jcall(java.test.object, "D", "getExactLeftPValue"),
                 exact.right = .jcall(java.test.object, "D", "getExactRightPValue"),
                 exact.double = .jcall(java.test.object, "D", "getExactDoublePValue"),
                 asymptotic.left = .jcall(java.test.object, "D", "getLeftPValue"),
                 asymptotic.right = .jcall(java.test.object, "D", "getRightPValue"),
                 asymptotic.double = .jcall(java.test.object, "D", "getDoublePValue"))
  htest <- make.htest(data.name = deparse(substitute(matrix)),
                      statistic = statistic, p.value = pvalue,
                      method = "siegel tukey")
  return(htest)
}

#' @title Sukhatme test for scale
#'
#' @export
#' @description This function performs the Sukhatme test
#' @param matrix Matrix of data
#' @return A htest object with pvalues and statistics
sukhatme.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.scale.sukhatmeTest.SukhatmeTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
  sukhatme <- .jcall(java.test.object, "D", "getTestStatistic")
  pvalue <- c(asymptotic.left = .jcall(java.test.object, "D", "getLeftPValue"),
                 asymptotic.right = .jcall(java.test.object, "D", "getRightPValue"),
                 asymptotic.double = .jcall(java.test.object, "D", "getDoublePValue"))
  htest <- make.htest(data.name = deparse(substitute(matrix)),
                      statistic = sukhatme, p.value = pvalue,
                      method = "sukhatme")
  return(htest)
}

#' @title Control Median test for two samples
#'
#' @export
#' @description This function performs the Control Median test
#' @param matrix Matrix of data
#' @return A htest object with pvalues and statistics
controlMedian.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.twoSample.controlMedianTest.ControlMedianTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
  statistic <- c(u = .jcall(java.test.object, "D", "getU"),
                    v = .jcall(java.test.object, "D", "getV"),
                    median = .jcall(java.test.object, "D", "getMedian"),
                    median2 = .jcall(java.test.object, "D", "getMedian2"))
  pvalue <- c(exact.left = .jcall(java.test.object, "D", "getExactLeftPValue"),
                 exact.right = .jcall(java.test.object, "D", "getExactRightPValue"),
                 exact.double = .jcall(java.test.object, "D", "getExactDoublePValue"),
                 asymptotic.left = .jcall(java.test.object, "D", "getLeftPValue"),
                 asymptotic.right = .jcall(java.test.object, "D", "getRightPValue"),
                 asymptotic.double = .jcall(java.test.object, "D", "getDoublePValue"))
  htest <- make.htest(data.name = deparse(substitute(matrix)),
                      statistic = statistic, p.value = pvalue,
                      method = "control median")
  return(htest)
}

#' @title  Kolmogorov-Smirnov test for two samples
#'
#' @description This function performs the Kolmogorov-Smirnov test for two samples
#' @param matrix Matrix of data
#' @return A htest object with pvalues and statistics
ks.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.twoSample.K_STest.K_STest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
  statistic <- c(DnPos = .jcall(java.test.object, "D", "getDnPos"),
                    DnNeg = .jcall(java.test.object, "D", "getDnNeg"),
                    Dn = .jcall(java.test.object, "D", "getDn"))
  pvalue <- c(exact.left = .jcall(java.test.object, "D", "getExactLeftPValue"),
                 exact.right = .jcall(java.test.object, "D", "getExactRightPValue"),
                 exact.double = .jcall(java.test.object, "D", "getExactDoublePValue"),
                 asymptotic.left = .jcall(java.test.object, "D", "getLeftPValue"),
                 asymptotic.right = .jcall(java.test.object, "D", "getRightPValue"),
                 asymptotic.double = .jcall(java.test.object, "D", "getDoublePValue"))
  htest <- make.htest(data.name = deparse(substitute(matrix)),
                      statistic = statistic, p.value = pvalue,
                      method = "ks")
  return(htest)
}

#' @title Median test for two samples
#'
#' @export
#' @description This function performs the Median test
#' @param matrix Matrix of data
#' @return A htest object with pvalues and statistics
median.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.twoSample.medianTest.MedianTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
  statistic <- c(u = .jcall(java.test.object, "D", "getU"),
                    v = .jcall(java.test.object, "D", "getV"),
                    median = .jcall(java.test.object, "D", "getMedian"))
  pvalue <- c(exact.left = .jcall(java.test.object, "D", "getExactLeftPValue"),
                 exact.right = .jcall(java.test.object, "D", "getExactRightPValue"),
                 exact.double = .jcall(java.test.object, "D", "getExactDoublePValue"),
                 asymptotic.left = .jcall(java.test.object, "D", "getLeftPValue"),
                 asymptotic.right = .jcall(java.test.object, "D", "getRightPValue"),
                 asymptotic.double = .jcall(java.test.object, "D", "getDoublePValue"))
  htest <- make.htest(data.name = deparse(substitute(matrix)),
                      statistic = statistic, p.value = pvalue,
                      method = "median")
  return(htest)
}

#' @title Wald-Wolfowitz for two samples
#'
#' @description This function performs the Wald-Wolfowitz test
#' @param matrix Matrix of data
#' @return A htest object with pvalues and statistics
waldWolfowitz.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.twoSample.wald_WolfowitzTest.Wald_WolfowitzTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
  r <- .jcall(java.test.object, "D", "getR")
  pvalue <- c(exact = .jcall(java.test.object, "D", "getExactPValue"),
                 asymptotic = .jcall(java.test.object, "D", "getAsymptoticPValue"))
  htest <- make.htest(data.name = deparse(substitute(matrix)),
                      statistic = statistic, p.value = pvalue,
                      method = "median")
  return(htest)
}
