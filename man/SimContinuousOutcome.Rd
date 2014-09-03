\name{SimContinuousOutcome}
\alias{SimContinuousOutcome}
\title{Simulates continuous outcomes}
\usage{
  SimContinuousOutcome(seed = 1, X, betas, sd,
    normalise = F)
}
\arguments{
  \item{seed}{Random number seed}

  \item{X}{A covariate matrix (rows individuals, columns
  covariates)}

  \item{betas}{A named vector of effects, named with the
  covariates the effects correspond to in X.}

  \item{sd}{Remaining residual error in outcome}

  \item{normalise}{Center the outcome around it's mean for
  use with an intercept-free model}
}
\value{
  A N by P+1 data.frame of the outcome Y, and genotypes
}
\description{
  This function simulates a continuous outcome according to
  a covariate matrix and vector of betas.
}

