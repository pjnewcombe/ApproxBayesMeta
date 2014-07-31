#' Generates a simulation scenario name string  to be used in results file etc.
#' @export
#' @title Creates simulation scenario name string
#' @name Namer
#' @param n.int Number of people in analysis data
#' @param n.ext Number of people in analysis data
#' @param n.blocks Number of blocks
#' @param n.snps Number of SNPs in each block
#' @param maf Minor allele frequenct of each SNP
#' @param rsq Pairwise correlation between SNPs
#' @param betas Vector of effects
#' @param which.analysis If generating a name to associate with results
#' this provides a short handle for the analysis type. E.g. "Uni", "RJ", "MAMS".
#' @param n.mil Number of iterations of a bayesian analysis
#' @param seed MCMC seed
#' 
#' @return A character string
#' @author Paul Newcombe
Namer <- function(
  n.int=10000,
  n.ext,
  n.blocks,
  n.snps=1000,
  maf=0.1,
  rsq,
  betas,
  which.analysis=NULL,
  n.mil=NULL,
  seed=1
  ) {
  
  name.str <- paste(
    "MVN",
    "_n",n.int,"ext",n.ext,
    "_",n.blocks,"b",n.snps,"P",
    "_fq",maf,"r",rsq,
    "_b",paste(betas,collapse="and"),
    sep=""
    )
  
  if (!is.null(which.analysis)) {
    name.str <- paste(name.str,"_",which.analysis,sep="")    
    if (!is.null(n.mil)) {
      name.str <- paste(name.str,"_",n.mil,"mItsSeed",seed,sep="")
    }
  }
  
  return(name.str)
}
