#' Generates the (true) t vector required by the marginal meta-analysis method described by
#' Verzilli et al (AJHG, 2008). Note: Y is normalised such that the t's 
#' produced are for use in an intercept-free model.
#' 
#' @export
#' @title Generate t true vector (from IPD) for marginal meta-analysis model.
#' @name GenerateTs
#' @param y Normalised vector of outcome values.
#' @param X Covariate matrix
#' 
#' @return A vector of t's
GenerateTs <- function(
  y,
  X) {
  
  # Mean center y
  y <- y - mean(y)
  
  # Calculate ts
  ts <- apply(X, MAR=2, function(x) {
    # Group counts under HWE
    n1 <- sum(x==1)
    n2 <- sum(x==2)
    
    # Group means from beta-hat (mean-centred)
    y1 <- mean(y[x==1])
    y2 <- mean(y[x==2])
    
    return( y1 * n1 + 2 * y2 * n2 )
  } )
  
  return(ts)
}
