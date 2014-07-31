\name{GenerateXtxPlugin}
\alias{GenerateXtxPlugin}
\title{Generates the plug-in estimate of the unobserved X'X (or `B' in Yang and Visscher)}
\usage{
  GenerateXtxPlugin(n, W, B = 1, mafs = NULL)
}
\arguments{
  \item{n}{Number of individuals the marginal statistics
  are calculated from}

  \item{W}{Covariate matrix from external data}

  \item{mafs}{Vector of Minor Allele Frequencies. If left
  as NULL, these are estimated from W}

  \item{B}{The number of covariate blocks}
}
\value{
  A list of block plug-in estimates for X'X
}
\description{
  Generates the plug-in estimate of X'X required for the
  marginal meta-analysis model. If a covariate matrix, W,
  is available from an external population of identical
  size, W'W would simply be used as the plug-in estimate
  for X'X. However, if W is from a population of a
  different size, then it must be scaled accordingly, to
  create `B' as described by Yang and Visscher.
}
\author{
  Paul Newcombe
}

