#'
#' @title
#'   Calculate a diversity index
#'
#' @description
#'   Calculate a diversity index for a community.
#'
#' @param clusters
#'   A set of cluster identities (cluster memberships) for each member in a
#'   community.
#'
#' @param base
#'   The log-base.
#'
#' @details
#'   From dorbarker's nAWC \url{https://github.com/dorbarker/nAWC}.
#'
#' @return
#'   The diversity index.
#'
#' @name diversityindex
#'

NULL



#' @rdname diversityindex

shannon <- compiler::cmpfun(function(clusters, base=2) {
  # Calculates the Shannon Entropy for a group of clusters
  # uses base 2, returning bits by default

  p_i <- function(i) {
    sum(clusters == i) / length(clusters)
  }

  N <- unique(clusters)

  -sum(sapply(N, function(i) p_i(i) * log(p_i(i), base = base)))
})



#' @rdname diversityindex

simpsons <- function(clusters)
{

  # Compute the Simpson's index of diversity for a list of classifications.
  # Simpson, E. H. 1949. Measurement of species diversity. Nature. 163:688.
  #
  # Args:
  #   clusters: List of classifications.
  #
  # Returns:
  #   Simpson's index of diversity and 95% confidence interval values and the
  #   number of partitions.


  n <- length(clusters)
  d <- table(clusters)
  sid <- 1.0
  if (n * (n - 1) > 0)
  {
    sid <- 1.0 - (sum(d * (d - 1)) / (n * (n - 1)))
  }

  s2 <- 4 / n * (sum((d/n)^3) - sum((d/n)^2)^2)

  low <- sid - 2 * sqrt(s2)
  high <- sid + 2 * sqrt(s2)

  return(list(sid=sid,low=low,high=high,n=n))
}

