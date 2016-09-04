danielTrendTest <- function(data){
  # create instance of FriedmanTest class
  dtt <- .jnew("javanpst.tests.bivariate.danielTrendTest.DanielTrendTest",
              dataTable(data))
  # run test
  .jcall(dtt, "V", "doTest")
  out <- .jcall(dtt, "S", "printReport")
  cat(out)
}

danielTrendTest <- function(data){
  # create instance of FriedmanTest class
  dtt <- .jnew("javanpst.tests.bivariate.danielTrendTest.DanielTrendTest",
              dataTable(data))
  # run test
  .jcall(dtt, "V", "doTest")
  out <- .jcall(dtt, "S", "printReport")
  cat(out)
}
