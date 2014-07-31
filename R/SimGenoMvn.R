#' Simulates a matrix of genotypes utilising a latent multivariate normal 
#' distribution. All `SNPs' have the same specified MAF, and the same
#' (also specified) pairwise correlations. Therehsolds consistent with
#' HWE for the specified MAF are applied on the resulting continuous
#' covariates, to create categorical `genotypes'.
#' 
#' @export
#' @title Simulates a matrix of genotypes using a latent multivariate normal
#' @name SimGenoMvn
#' @param seed Random number seed
#' @param N number of people
#' @param B number of snp-blocks
#' @param P number of SNPs
#' @param MAF Minor Allele Frequency of the SNPs
#' @param rSq pair-wise correlation between the latent normal variables
#' 
#' @return A N by P matrix of 0/1/2 genotype codes
SimGenoMvn <- function(
  seed=1,
  N,
  B=1,
  P,
  MAF,
  rSq,
  normalise=F) {
  # Setup
  set.seed(seed)
  sigma <- matrix(rSq, P, P)
  diag(sigma) <- 1
  
  # Simulate X matrix
  X <- NULL
  for (b in 1:B) {
    cat("Simulating",P,"covariates for",N,"people from a multivariate normal - block",b,"\n")
    X.block <- rmvnorm(N, mean=rep(0,P), sigma=sigma)
    X <- cbind(X, X.block)
  }
  colnames(X) <- paste("V",c(1:ncol(X)),sep="")
  
  # Convert to genotypes at breaks consistent with HWE given the MAF
  cat("Converting continuous X matrix to genotypes under HWE\n")
  geno.p.breaks <- c( (1-MAF)^2, (1-MAF)^2 + 2*MAF*(1-MAF) )
  geno.q.breaks <- c(-Inf,qnorm(geno.p.breaks),Inf)
  X <- apply(X, MAR=2, function(x) cut(x,breaks=geno.q.breaks,labels=F)-1 )
  
  # Normalise
  if (normalise) {
    cat("Mean-centring covariates for intercept-free model\n")
    X <- NormaliseCovariates(X)    
  }
  
  return(X)
}