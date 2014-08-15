\name{GenerateTrueTs}
\alias{GenerateTrueTs}
\title{Generate true t vector (from IPD) for marginal meta-analysis model.}
\usage{
  GenerateTrueTs(y, X)
}
\arguments{
  \item{y}{Normalised vector of outcome values.}

  \item{X}{Covariate matrix}
}
\value{
  A vector of t's
}
\description{
  Generates the (true) t vector required by the marginal
  meta-analysis method described by Verzilli et al (AJHG,
  2008). Note: Y is normalised such that the t's produced
  are for use in an intercept-free model.
}

