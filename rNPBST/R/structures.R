#' @title Java DataTable object
#'
#' @export
#' @description Returns a DataTable object
#' @param matrix Matrix to create the object
#' @return Java DataTable object
dataTable <- function(matrix){
  # Create the DataTable object
  table <- .jnew("javanpst.data.structures.dataTable.DataTable")
  #
  nrow <- nrow(matrix)
  ncol <- ncol(matrix)

  # Set number of rows and cols to the object
  .jcall(table, "V", "setDimensions", as.integer(nrow), as.integer(ncol))

  # Set values
  for( i in 1:nrow ){
    for( j in 1:ncol ){
      .jcall(table, "V", "setValue", as.integer(i-1),
             as.integer(j-1), as.double(matrix[i,j]))
    }
  }
  return(table)
}

#' @title Create a string sequence
#'
#' @export
#' @description Function to create a string sequence
#' @param array Array with the data to fill the sequence up
#' @return Java String sequece object
stringSequence <- function(array){
  sequence <- .jnew("javanpst.data.structures.sequence.StringSequence")

  for( s in array ){
    o <- .jnew("java.lang.String", s)
    .jcall(sequence, "V", "append", .jcast(o))
  }

  return(sequence)
}

#' @title Create a numeric sequence
#'
#' @export
#' @description Function to create a numeric sequence
#' @param array Array with the data to fill the sequence up
#' @return Java String sequece object
numericSequence <- function(array){
  sequence <- .jnew("javanpst.data.structures.sequence.NumericSequence")
  for( s in array ){
    o <- .jnew("java.lang.Double", s)
    .jcall(sequence, "V", "append", .jcast(o))
  }

  return(sequence)
}
