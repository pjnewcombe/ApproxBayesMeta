#' Given a matrix of covariates (rows individuals, columns variables), this function
#' centres each covariate on it's mean for use in an intercept free model.
#' 
#' @export
#' @title Mean centers covariates in an `X' matrix
#' @name NormaliseCovariates
#' @param X A covariate matrix (rows individuals, columns covariates)
#' 
#' @return A N by P data.frame
NormaliseCovariates <- function(X) {
  X <- apply(X, MAR=2, function(x) x-mean(x) )
  return(X)
}
