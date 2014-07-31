#' Performs univariate analysis of simulated data, with or without confounders
#' 
#' @export
#' @title Univariate analysis of simulated data
#' @name UnivariateLinReg
#' @param data data.frame Consisting of covariates to analyse, and 
#' the outcome
#' @param outcome Label of the outcome in `data'. By default is assumed to be "Y".
#' @param confounders Vector of labels of any confounders to adjust for in the analyses.
#' 
#' @return A N by P+1 data.frame of the outcome Y, and genotypes
UnivariateLinReg <- function(
  data,
  outcome="Y",
  confounders=NULL
  ) {
  
  ### --- Setup for any confounders
  if (!is.null(confounders)) {
    CONF <- data[,confounders]
    data <- data[,!colnames(data)%in%confounders]
    reg.form <- "Y ~ x + CONF"
  } else {
    reg.form <- "Y ~ x"
  }
  
  ### --- Seperate outcome from covariates, and confounders
  Y <- data[,outcome]
  X <- data[,!colnames(data)%in%outcome]
  
  ### --- Perform linear regression using the apply statement
  results <- apply(X, MAR = 2, function(x) summary(lm(formula(reg.form)))$coefficients["x",] )
  
  ### --- Return results
  return(results)
}
