\name{BlockIndices}
\alias{BlockIndices}
\title{Generates indices of blocks.}
\usage{
  BlockIndices(B, P = NULL, V = NULL)
}
\arguments{
  \item{B}{Number of blocks (of equal size) which the
  covariate matrix is split into}

  \item{P}{Number of SNPs per block (can be left as NULL if
  V is supplied)}

  \item{V}{Total number of all SNPs (can be left as NULL if
  P is supplied)}
}
\value{
  A vector of block start indices. Note that the final
  index does not exist (it will always be used with -1)
}
\description{
  Generates block indices. NOTE: assumes all blocks are of
  EQUAL size.
}

