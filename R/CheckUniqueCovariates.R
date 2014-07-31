#' This function checks whether all covariates in an X-matrix are unique.
#' 
#' @export
#' @title Checks whether all covariates in a covariate matrix are unique
#' @name CheckUniqueCovariates
#' @param X a covariate matrix
#' 
#' @return Boolean set to TRUE if all covariates are unique
#' @author Paul Newcombe
CheckUniqueCovariates <- function(
  X) {
  
  # Run unique function on the transpose
  unique.covs <- unique(t(X))
  if (nrow(unique.covs)==ncol(X)) {
    all.covs.unique <- TRUE
  } else {
    all.covs.unique <- FALSE    
  }
  
  return(all.covs.unique)
}
