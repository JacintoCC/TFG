#' @title Execution of a StatisticalTest object
#'
#' @description Wrapper to run a StatisticalTest object.
#' @param java.test A StatisticalTest Java object
#' @return The function returns the report of the test in a string
runTest <- function(java.test){
  # Run test
  .jcall(java.test.object, "V", "doTest")
  out <- .jcall(java.test.object, "S", "printReport")
  return(out)
}

#' @title Carry out a test introducing name and data
#'
#' @description Function to make a non-parametric test
#' @param test.name Name of the test.
#' \itemize{
#'   \item{\code{'daniel trend'}} - Daniel Trend test
#'   \item{\code{'kendall'}} - Kendall test
#'   \item{\code{'contingency coeff'}
#'   \item{\code{'fisher'}}
#'   \item{\code{'mcNemar'}}
#'   \item{\code{'multinomial equality'}}
#'   \item{\code{'ordered equality'}}
#'   \item{\code{'CD'}}
#'   \item{\code{'extended median'}}
#'   \item{\code{'JT'}}
#'   \item{\code{'kruskal'}}
#'   \item{\code{'AD'}}
#'   \item{\code{'chi square'}}
#'   \item{\code{'KS'}}
#'   \item{\code{'lilliefors'}}
#'   \item{\code{'normal scores'}}
#'   \item{\code{'wilcoxon rank sum'}}
#'   \item{\code{'concordance coeff'}}
#'   \item{\code{'friedman'}}
#'   \item{\code{'incomplete concordance'}}
#'   \item{\code{'page'}}
#'   \item{\code{'partial correlation'}}
#'   \item{\code{'confidence quantile'}}
#'   \item{\code{'population quantile'}}
#'   \item{\code{'sign'}}
#'   \item{\code{'wilcoxon'}}
#'   \item{\code{'number runs'}}
#'   \item{\code{'runs median'}}
#'   \item{\code{'runs up down'}}
#'   \item{\code{'vonNeumann'}}
#'   \item{\code{'david barton'}}
#'   \item{\code{'freud ansari bradley'}}
#'   \item{\code{'klotz'}}
#'   \item{\code{'mood'}}
#'   \item{\code{'siegel tukey'}}
#'   \item{\code{'sukhatme'}}
#'   \item{\code{'control median'}}
#'   \item{\code{'KS'}}
#'   \item{\code{'median'}}
#'   \item{\code{'wald wolfowitz'}}
#' @return The function returns the report of the test in a string
doTest <- function(test.name, ...){
  java.t.classes <- c("javanpst.tests.bivariate.danielTrendTest.DanielTrendTest",
                      "javanpst.tests.bivariate.kendallTest.KendallTest",
                      "javanpst.tests.countData.contingencyCoefficient.ContingencyCoefficient",
                      "javanpst.tests.countData.fisherTest.FisherTest",
                      "javanpst.tests.countData.mcNemarTest.McNemarTest",
                      "javanpst.tests.countData.multinomialEqualityTest.MultinomialEqualityTest",
                      "javanpst.tests.countData.orderedEqualityTest.OrderedEqualityTest",
                      "javanpst.tests.equality.CDTest.CDTest",
                      "javanpst.tests.equality.extendedMedianTest.ExtendedMedianTest",
                      "javanpst.tests.equality.JTTest.JTTest",
                      "javanpst.tests.equality.kruskalWallisTest.KruskalWallisTest",
                      "javanpst.tests.goodness.A_DTest.A_DTest",
                      "javanpst.tests.goodness.chiSquareTest.ChiSquareTest",
                      "javanpst.tests.goodness.K_STest.K_STest",
                      "javanpst.tests.goodness.lillieforsTest.LillieforsTest"
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

  names(java.t.classes) <-  c("daniel trend",
                              "kendall",
                              "contingency coeff",
                              "fisher",
                              "mcNemar",
                              "multinomial equality",
                              "ordered equality",
                              "CD",
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

make.htest <- function(...){
  htest <- list(...)
  class(htest) <- "htest"
  return(htest)
}

danielTrend.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.bivariate.danielTrendTest.DanielTrendTest",
                            dataTable(matrix))

  report <- runTest(java.test.object)
  statistic <- list(r = .jcall(java.test.object, "D", "getR"),
                    z = .jcall(java.test.object, "D", "getZ"))
  pvalue <- .jcall(java.test.object, "D", "getExactPValue")
  htest <- make.htest(statistic = statistic, p.value = pvalue,
                      method = "daniel trend", report = report)
  return(htest)
}

kendall.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.bivariate.kendallTest.KendallTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
  statistic <- list(t = .jcall(java.test.object, "D", "getT"),
                    z = .jcall(java.test.object, "D", "getZ"))
  pvalue <- .jcall(java.test.object, "D", "getExactPValue")
  htest <- make.htest(statistic = statistic, p.value = pvalue,
                      method = "kendall", report = report)
  return(htest)
}

contingency.coeff.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.countData.contingencyCoefficient.ContingencyCoefficient",
                            dataTable(matrix))
  report <- runTest(java.test.object)
  q <- .jcall(java.test.object, "D", "getQ")
  coefficients <- list(c = .jcall(java.test.object, "D", "getC"),
                       phi = .jcall(java.test.object, "D", "getPhi"))
  pvalue <- .jcall(java.test.object, "D", "getPValue")
  htest <- make.htest(statistic = q, p.value = pvalue,
                      coefficients = coefficients,
                      method = "contingency coeff", report = report)
  return(htest)
}

fisher.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.countData.fisherTest.FisherTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
  q <- .jcall(java.test.object, "D", "getQ")
  pvalue <- .jcall(java.test.object, "D", "getAsymptoticPValue")
  exact.left.p.value <- .jcall(java.test.object, "D", "getExactLeftPValue")
  exact.right.p.value <- .jcall(java.test.object, "D", "getExactRightPValue")
  htest <- make.htest(statistic = q, p.value = pvalue,
                      exact.left.p.value = exact.left.p.value,
                      exact.right.p.value = exact.right.p.value,
                      method = "fisher", report = report)
  return(htest)
}

mcNemar.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.countData.mcNemarTest.McNemarTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
  statistic <- list(s = .jcall(java.test.object, "D", "getS"),
                    z = .jcall(java.test.object, "D", "getZ"),
                    t = .jcall(java.test.object, "D", "getT"))
  pvalue <- list(exact = .jcall(java.test.object, "D", "getExactPValue"),
                 asymptotic.normal = .jcall(java.test.object, "D", "getAsymptoticNormalPValue"),
                 asymptotic.chi = .jcall(java.test.object, "D", "getAsymptoticChiPValue"))
  htest <- make.htest(statistic = statistic, p.value = pvalue,
                      method = "fisher", report = report)
  return(htest)
}

multinomialEq.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.countData.multinomialEqualityTest.MultinomialEqualityTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
  q <- .jcall(java.test.object, "D", "getQ")
  pvalue <- .jcall(java.test.object, "D", "getPValue")
  htest <- make.htest(statistic = q, p.value = pvalue,
                      method = "multinomial equality", report = report)
  return(htest)
}

orderedEq.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.countData.orderedEqualityTest.OrderedEqualityTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
  statistic <- list(Wx = .jcall(java.test.object, "D", "getWx"),
                    Wy = .jcall(java.test.object, "D", "getWy"))
  pvalue <- list(right = .jcall(java.test.object, "D", "getRightPValue"),
                 left = .jcall(java.test.object, "D", "getLeftPValue"),
                 double = .jcall(java.test.object, "D", "getDoublePValue"))
  htest <- make.htest(statistic = statistic, p.value = pvalue,
                      method = "ordered equality", report = report)
  return(htest)
}

cd.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.equality.CDTest.CDTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
  statistic <- list(median = .jcall(java.test.object, "D", "getZ"),
                    W = .jcall(java.test.object, "D", "getW"))
  pvalue <- list(exact = .jcall(java.test.object, "D", "getExactPValue"),
                 asymptotic = .jcall(java.test.object, "D", "getAsymptoticPValue"))
  htest <- make.htest(statistic = statistic, p.value = pvalue,
                      method = "CD", report = report)
  return(htest)
}

extendedMedian.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.equality.extendedMedianTest.ExtendedMedianTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
  statistic <- list(median = .jcall(java.test.object, "D", "getMedian"),
                    q = .jcall(java.test.object, "D", "getQ"),
                    improved.q = .jcall(java.test.object, "D", "getImprovedQ"))
  pvalue <- .jcall(java.test.object, "D", "getPValue")
  htest <- make.htest(statistic = statistic, p.value = pvalue,
                      method = "exteded median", report = report)
  return(htest)
}

jt.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.equality.JTTest.JTTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
  statistic <- list(b = .jcall(java.test.object, "D", "getB"),
                    z = .jcall(java.test.object, "D", "getZ"),
  pvalue <- .jcall(java.test.object, "D", "getPValue")
  htest <- make.htest(statistic = statistic, p.value = pvalue,
                      method = "JT", report = report)
  return(htest)
}

kruskalWallis.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.equality.kruskalWallisTest.KruskalWallisTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
  htest <- make.htest(method = "kruskal wallis", report = report)
  return(htest)
}

ad.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.goodness.A_DTest.A_DTest",
                            numericSequence(matrix))
  report <- runTest(java.test.object)
  a <- .jcall(java.test.object, "D", "getA")
  pvalue <- .jcall(java.test.object, "D", "getPValue")
  htest <- make.htest(statistic = a, p.value = pvalue,
                      method = "AD", report = report)
  return(htest)
}

chiSquare.test <- function(sequece){
  java.test.object <- .jnew("javanpst.tests.goodness.chiSquareTest.ChiSquareTest",
                            numericSequence(sequece)
  report <- runTest(java.test.object)
  q <- .jcall(java.test.object, "D", "getQ")
  pvalue <- .jcall(java.test.object, "D", "getPValue")
  htest <- make.htest(statistic = q, p.value = pvalue,
                      method = "chi square", report = report)
  return(htest)
}

ks.test <- function(sequece){
  java.test.object <- .jnew("javanpst.tests.goodness.K_STest.K_STest",
                            numericSequence(sequece)
  report <- runTest(java.test.object)
  Dn <- .jcall(java.test.object, "D", "getDn")
  pvalue <- .jcall(java.test.object, "D", "getPValue")
  htest <- make.htest(statistic = Dn, p.value = pvalue,
                      method = "KS", report = report)
  return(htest)
}

lilliefors.test <- function(sequece){
  java.test.object <- .jnew("javanpst.tests.goodness.lillieforsTest.LillieforsTest"
                            numericSequence(sequence))
  report <- runTest(java.test.object)
  Dn <- .jcall(java.test.object, "D", "getDn")
  pvalue <- .jcall(java.test.object, "D", "getPValue")
  htest <- make.htest(statistic = Dn, p.value = pvalue,
                      method = "lilliefors", report = report)
  return(htest)
}

normalScores.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.location.normalScoresTest.NormalScoresTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
  Dn <- .jcall(java.test.object, "D", "getDn")
  pvalue <- .jcall(java.test.object, "D", "getPValue")
  htest <- make.htest(statistic = Dn, p.value = pvalue,
                      method = "KS", report = report)
  return(htest)
}

wilcoxonRankSum.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.location.wilcoxonRankSumTest.WilcoxonRankSumTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
}

concordanceCoeff.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.multiple.concordanceCoefficient.ConcordanceCoefficient",
                            dataTable(matrix))
  report <- runTest(java.test.object)
}

friedman.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.multiple.friedmanTest.FriedmanTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
}

incompleteConcordance.test <- function(matrix, lambda){
  java.test.object <- .jnew("javanpst.tests.multiple.incompleteConcordance.IncompleteConcordance",
                            dataTable(matrix), lambda)
  report <- runTest(java.test.object)
}

page.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.multiple.pageTest.PageTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
}

partialCorrelation.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.multiple.partialCorrelationTest.PartialCorrelationTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
}

confidenceQuantile.test <- function(n, p, q){
  java.test.object <- .jnew("javanpst.tests.oneSample.confidenceQuantile.ConfidenceQuantile",
                            n, p, q)
  report <- runTest(java.test.object)
}

confidenceQuantile.test <- function(sequence, quantile, value){
  java.test.object <- .jnew("javanpst.tests.oneSample.populationQuantile.PopulationQuantile",
                            numericSequence(sequence), quantile, value)
  report <- runTest(java.test.object)
}

sign.test <- function(sequence){
  java.test.object <- .jnew("javanpst.tests.oneSample.signTest.SignTest",
                            numericSequence(sequence))
  report <- runTest(java.test.object)
}

wilcoxon.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.oneSample.wilcoxonTest.WilcoxonTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
}

numberRuns.test <- function(sequence){
  java.test.object <- .jnew("javanpst.tests.randomness.numberRunsTest.NumberRunsTest",
                            stringSequence(sequence))
  report <- runTest(java.test.object)
}

numberRunsUpDownMedian.test <- function(sequence){
  java.test.object <- .jnew("javanpst.tests.randomness.runsUpDownMedianTest.RunsUpDownMedianTest",
                            numericSequence(sequence))
  report <- runTest(java.test.object)
}

numberRunsUpDown.test <- function(sequence){
  java.test.object <- .jnew("javanpst.tests.randomness.runsUpDownTest.RunsUpDownTest",
                            numericSequence(sequence))
  report <- runTest(java.test.object)
}

vonNeumann.test <- function(sequence){
  java.test.object <- .jnew("javanpst.tests.randomness.vonNeumannTest.VonNeumannTest",
                            numericSequence(sequence))
  report <- runTest(java.test.object)
}

davidBarton.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.scale.david_BartonTest.David_BartonTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
}

freundAnsariBradley.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.scale.freund_Ansari_BradleyTest.Freund_Ansari_BradleyTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
}

klotz.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.scale.klotzTest.KlotzTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
}
mood.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.scale.moodTest.MoodTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
}
siegelTukey.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.scale.siegel_TukeyTest.Siegel_TukeyTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
}
sukhatme.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.scale.sukhatmeTest.SukhatmeTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
}

controlMedian.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.twoSample.controlMedianTest.ControlMedianTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
}

ks.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.twoSample.K_STest.K_STest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
}

median.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.twoSample.medianTest.MedianTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
}

waldWolfowitz.test <- function(matrix){
  java.test.object <- .jnew("javanpst.tests.twoSample.wald_WolfowitzTest.Wald_WolfowitzTest",
                            dataTable(matrix))
  report <- runTest(java.test.object)
}
