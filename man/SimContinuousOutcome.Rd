\name{SimContinuousOutcome}
\alias{SimContinuousOutcome}
\title{Simulates continuous outcomes}
\usage{
  SimContinuousOutcome(seed = 1, X, B = 1, betas, sd,
    normalise = F)
}
\arguments{
  \item{seed}{Random number seed}

  \item{X}{A covariate matrix (rows individuals, columns
  covariates)}

  \item{B}{Number of blocks (of equal size) which the
  covariate matrix is split into}

  \item{betas}{Vector of effects to simulate for causal
  SNP(s) within each block. Within each block the causal
  location(s) will be arbitrarily counted from the first
  SNP onward.}

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

