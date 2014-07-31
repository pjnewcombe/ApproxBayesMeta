#' This function simulates a continuous outcome according to a covariate matrix and
#' vector of betas.
#' 
#' @export
#' @title Simulates continuous outcomes
#' @name SimContinuousOutcome
#' @param seed Random number seed
#' @param X A covariate matrix (rows individuals, columns covariates)
#' @param B Number of blocks (of equal size) which the covariate matrix is split into
#' @param betas Vector of effects to simulate for causal SNP(s) within each block.
#' Within each block the causal location(s) will be arbitrarily counted from the first
#' SNP onward.
#' @param sd Remaining residual error in outcome
#' @param normalise Center the outcome around it's mean for use with an
#' intercept-free model
#' 
#' @return A N by P+1 data.frame of the outcome Y, and genotypes
SimContinuousOutcome <- function(
  seed=1,
  X,
  B=1,
  betas,
  sd,
  normalise=F) {
  
  # Figure out block setup
  causal.indices <- CausalIndices(B=B,P=(ncol(X)/B),betas=betas)
  
  # Simulate outcomes
  cat("Simulating outcome vector\n")
  set.seed(seed)
  Y <- rnorm(nrow(X), mean= X[,causal.indices] %*% as.matrix(rep(betas,B),ncol=1), sd=sd)
  
  # Normalise covariates and outcome, for intercept-free model
  if (normalise) {
    cat("Normalising for intercept-free model\n")
    Y <- Y - mean(Y)    
  }
  
  return(Y)
}
