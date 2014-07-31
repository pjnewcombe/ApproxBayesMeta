#' Generates the plug-in estimate of X'X required for the marginal
#' meta-analysis model. If a covariate matrix, W, is available from
#' an external population of identical size, W'W would simply be used
#' as the plug-in estimate for X'X. However, if W is from a population
#' of a different size, then it must be scaled accordingly, to create
#' `B' as described by Yang and Visscher.
#' 
#' @export
#' @title Generates the plug-in estimate of the unobserved X'X (or `B' in Yang and Visscher)
#' @name GenerateXtxPlugin
#' @param n Number of individuals the marginal statistics are calculated from
#' @param W Covariate matrix from external data
#' @param mafs Vector of Minor Allele Frequencies. If left as NULL, these are
#' estimated from W
#' @param B The number of covariate blocks
#' 
#' @return A list of block plug-in estimates for X'X
#' @author Paul Newcombe
GenerateXtxPlugin <- function(
  n,
  W,
  B=1,
  mafs=NULL) {
  
  # Generate mafs from W if not provided
  if (is.null(mafs)) {
    mafs <- apply(W, MAR=2, mean)/2
  }
  
  # Normalise W since will use in an intercept free model
  W <- NormaliseCovariates(W)
  
  xTx <- list()
  block.indices <- BlockIndices(B=B,V=ncol(W))
  for (b in 1:B) {
    covs.b <- c(block.indices[b]:(block.indices[b+1]-1) )
    cov.names.b <- colnames(W)[covs.b]
    W.b <- W[,cov.names.b]
    
    # Generate D_w
    D_w <- diag(length(covs.b))
    diag(D_w) <- apply(W.b^2,MAR=2,sum)
    
    # Generate D
    D <- diag(length(covs.b))
    diag(D) <- unlist(lapply(mafs[covs.b], function(p) 2*p*(1-p)*n))
    
    # Calculate B
    xTx[[b]] = sqrt(D) %*% solve(sqrt(D_w)) %*% t(W.b) %*% W.b %*% solve(sqrt(D_w)) %*% sqrt(D)
    rownames(xTx[[b]]) <- cov.names.b
    colnames(xTx[[b]]) <- cov.names.b
  }
  
  return(xTx)
}
