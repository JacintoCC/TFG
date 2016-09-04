friedmanTest <- function(data){
  # create instance of FriedmanTest class
  ft <- .jnew("javanpst.tests.multiple.friedmanTest.FriedmanTest",
              dataTable(data))
  # run test
  .jcall(ft, "V", "doTest")
  out <- .jcall(ft, "S", "printReport")
  cat(out)
}
