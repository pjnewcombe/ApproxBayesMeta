#' Generates the t vector required by the marginal meta-analysis method described by
#' Verzilli et al (AJHG, 2008). Note that this only requires univariate effect estimates,
#' MAFs (and the sample size from which the effect estimates came). HWE is assumed in
#' order to generate all quantities necessary (outcome means by genotype group) to
#' construct t. NOTE: Conceptually the group means are mean-centred, so these
#' t's are for use in an intercept-free model
#' 
#' @export
#' @title Generate t vector for marginal meta-analysis model.
#' @name GenerateTs
#' @param n Number of individuals the marginal statistics are calculated from
#' @param uni.res data.frame containing univariate results from UnivariateLinReg
#' @param mafs Vector of Minor Allele Frequencies
#' 
#' @return A vector of t's
GenerateTs <- function(
  n,
  uni.res,
  mafs) {
  
  uni.res <- rbind(uni.res, "MAF"=mafs)
  
  ts <- apply(uni.res, MAR=2, function(x) {
    # Group counts under HWE
    n0 <- round(n*(1-x["MAF"])^2)
    n1 <- round(n*(1-x["MAF"])*x["MAF"])
    n2 <- n-n1-n0
    
    # Group means from beta-hat (mean-centred)
    y0 <- - (n1*x["Estimate"] + n2*2*x["Estimate"])/n
    y1 <- y0 + x["Estimate"]
    y2 <- y0 + 2*x["Estimate"]
    
    return(y1*n1 + 2*y2*n2)
  } )
  
  return(ts)
}
