#' Generates block indices. NOTE: assumes all blocks are of EQUAL size. Test
#' 
#' @export
#' @title Generates indices of blocks.
#' @name BlockIndices
#' @param B Number of blocks (of equal size) which the covariate matrix is split into
#' @param P Number of SNPs per block (can be left as NULL if V is supplied)
#' @param V Total number of all SNPs (can be left as NULL if P is supplied)
#' 
#' @return A vector of block start indices. Note that the final index does not exist
#' (it will always be used with -1)
BlockIndices <- function(
  B,
  P=NULL,
  V=NULL) {
  
  if (is.null(P)) {
    P <- V/B
  }
  if (is.null(V)) {
    V <- B*P
  }

  block.indices <- seq(from=1,by=P,length.out=(B+1) )
  
  return(block.indices)
}
