\name{myclt}
\alias{myclt}
\title{Central Limit Theorem Simulation}
\description{
  This function simulates the Central Limit Theorem by generating 'iter' samples of 'n' random numbers from a uniform distribution between 0 and 5,
  summing each sample, and plotting the histogram of the sums.
}
\usage{
  myclt(n, iter)
}
\arguments{
  \item{n}{
    Number of random numbers in each sample.
  }
  \item{iter}{
    Number of samples to generate.
  }
}
\value{
  Vector of sum values for each sample.
}
