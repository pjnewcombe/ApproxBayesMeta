\name{GenerateTs}
\alias{GenerateTs}
\title{Generate t true vector (from IPD) for marginal meta-analysis model.}
\usage{
  GenerateTs(n, uni.res, mafs)

  GenerateTs(n, uni.res, mafs)
}
\arguments{
  \item{y}{Normalised vector of outcome values.}

  \item{X}{Covariate matrix}

  \item{n}{Number of individuals the marginal statistics
  are calculated from}

  \item{uni.res}{data.frame containing univariate results
  from UnivariateLinReg}

  \item{mafs}{Vector of Minor Allele Frequencies}
}
\value{
  A vector of t's

  A vector of t's
}
\description{
  Generates the (true) t vector required by the marginal
  meta-analysis method described by Verzilli et al (AJHG,
  2008). Note: Y is normalised such that the t's produced
  are for use in an intercept-free model.

  Generates the t vector required by the marginal
  meta-analysis method described by Verzilli et al (AJHG,
  2008). Note that this only requires univariate effect
  estimates, MAFs (and the sample size from which the
  effect estimates came). HWE is assumed in order to
  generate all quantities necessary (outcome means by
  genotype group) to construct t. NOTE: Conceptually the
  group means are mean-centred, so these t's are for use in
  an intercept-free model
}

