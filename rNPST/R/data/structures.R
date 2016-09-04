dataTable <- function(matrix){
  # Create the DataTable object
  table <- .jnew("javanpst.data.structures.dataTable.DataTable")

  #
  nrow <- nrow(matrix)
  ncol <- ncol(matrix)

  # Set number of rows and cols to the object
  .jcall(table, "V", "setDimensions", nrow, ncol)

  # Set values
  for( i in 1:nrow ){
    for( j in 1:ncol ){
      .jcall(table, "V", "setValue", as.integer(i-1),
             as.integer(j-1), as.double(matrix[i,j]))
    }
  }
  return(table)
}

fillSequence <- function(sequece, array){
  for( s in array ){
    .jcall(sequence, "V", "append", s)
  }

  return(sequence)
}

stringSequence <- function(array){
  sequence <- j.new("javanpst.data.structures.sequence.StringSequence")
  fillSequence(sequence, array)

  return(sequence)
}

numericSequence <- function(array){
  sequence <- j.new("javanpst.data.structures.sequence.NumericSequence")
  fillSequence(sequence, array)

  return(sequence)
}
