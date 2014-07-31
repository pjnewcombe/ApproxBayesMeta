\name{NormaliseCovariates}
\alias{NormaliseCovariates}
\title{Mean centers covariates in an `X' matrix}
\usage{
  NormaliseCovariates(X)
}
\arguments{
  \item{X}{A covariate matrix (rows individuals, columns
  covariates)}
}
\value{
  A N by P data.frame
}
\description{
  Given a matrix of covariates (rows individuals, columns
  variables), this function centres each covariate on it's
  mean for use in an intercept free model.
}

