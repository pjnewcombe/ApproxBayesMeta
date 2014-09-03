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
#' @param P number of SNPs in each block
#' @param MAF Minor Allele Frequency of the SNPs in each block (vector of same length
#' as the number of blocks)
#' @param rSq pair-wise correlation between the latent normal variables (vector of
#' same length as the number of blocks)
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
  
  # Simulate X matrix
  X <- NULL
  for (b in 1:B) {
    cat("Simulating",P,"covariates for",N,"people from a multivariate normal - block",b,"\n")
    
    # Draw from a multivariate normal
    sigma <- matrix(rSq[b], P, P)
    diag(sigma) <- 1
    X.block <- rmvnorm(N, mean=rep(0,P), sigma=sigma)
    
    # Convert to genotypes at breaks consistent with HWE given the MAF
    geno.p.breaks <- c( (1-MAF[b])^2, ((1-MAF[b])^2 + 2*MAF[b]*(1-MAF[b])) )
    geno.q.breaks <- c(-Inf,qnorm(geno.p.breaks),Inf)
    X.block <- apply(X.block, MAR=2, function(x) cut(x,breaks=geno.q.breaks,labels=F)-1 )
    
    # Merge with the other blocks
    X <- cbind(X, X.block)
  }
  
  # Name the variables
  colnames(X) <- paste("V",c(1:ncol(X)),sep="")
  
  # Normalise
  if (normalise) {
    cat("Mean-centring covariates for intercept-free model\n")
    X <- NormaliseCovariates(X)    
  }
  
  return(X)
}