\name{Namer}
\alias{Namer}
\title{Creates simulation scenario name string}
\usage{
  Namer(n.int = 10000, n.ext, n.blocks, n.snps = 1000,
    maf = 0.1, rsq, betas, which.analysis = NULL,
    n.mil = NULL, seed = 1)
}
\arguments{
  \item{n.int}{Number of people in analysis data}

  \item{n.ext}{Number of people in analysis data}

  \item{n.blocks}{Number of blocks}

  \item{n.snps}{Number of SNPs in each block}

  \item{maf}{Minor allele frequenct of each SNP}

  \item{rsq}{Pairwise correlation between SNPs}

  \item{betas}{Vector of effects}

  \item{which.analysis}{If generating a name to associate
  with results this provides a short handle for the
  analysis type. E.g. "Uni", "RJ", "MAMS".}

  \item{n.mil}{Number of iterations of a bayesian analysis}

  \item{seed}{MCMC seed}
}
\value{
  A character string
}
\description{
  Generates a simulation scenario name string to be used in
  results file etc.
}
\author{
  Paul Newcombe
}

