#' Generates indices of SNPs simulated as causal within each block.
#' 
#' @export
#' @title Generates indices of SNPs simulated as causal within each block.
#' @name CausalIndices
#' @param B Number of blocks (of equal size) which the covariate matrix is split into
#' @param P Number of SNPs per block
#' @param betas Vector of effects used to simulate causal effects within each block.
#' 
#' @return A vector of indices of the location of SNPs simulated as causal
CausalIndices <- function(
  B=1,
  P,
  betas) {
  
  n.effects <- length(betas)
  causal.indices <- NULL
  for (b in 1:B) {
    causal.indices <- c(causal.indices, (b-1)*P+c(1:n.effects) )
  }
  
  return(causal.indices)
}
