#'
#' @title
#'   Calculate a partition coefficient
#'
#' @description
#'   Calculate a partition coefficient from a 'mismatch' matrix; the agreement/
#'   disagreement between two clustering schemes.
#'
#' @param a,b,c,d
#'   The parameters of a 'mismatch' matrix
#'
#' @details
#'   From dorbarker's nAWC \url{https://github.com/dorbarker/nAWC}.
#'
#' @return
#'   The partition coefficient.
#'
#' @name partitioncoefficient
#'

NULL



#' @rdname partitioncoefficient
#' @export

rand <- function(a,b,c,d)
{
  # Compute the Rand coefficient from a mismatch matrix from 2
  # classifications of a dataset.
  # Rand, W. M. 1971. Objective criteria for the evaluation of clustering
  # methods. J. Am. Stat. Assoc. 66:846-850.
  #
  # Args:
  #   a: a variable in formula to calculate the Rand coefficient.
  #   b: b variable in formula to calculate the Rand coefficient.
  #   c: c variable in formula to calculate the Rand coefficient.
  #   d: d variable in formula to calculate the Rand coefficient.
  #
  # Returns:
  #   Rand coefficient value.
  rand <- (a + d) / (a + b + c + d)
  return(rand)
}



#' @rdname partitioncoefficient
#' @export

wallace <- function(a,b,c)
{
  # Compute the Wallace coefficients from a mismatch matrix from 2
  # classifications of a dataset.
  # Wallace, D. L. 1983. A method for comparing two hierarchical clusterings:
  # comment. J. Am. Stat. Assoc. 78:569-576.
  #
  # Args:
  #   a: a variable in formula to calculate the Wallace coefficient.
  #   b: b variable in formula to calculate the Wallace coefficient.
  #   c: c variable in formula to calculate the Wallace coefficient.
  #
  # Returns:
  #   Wallace coefficients for clustering 1 to 2 and 2 to 1.
  w1 = 0
  w2 = 0
  if ((a + b) > 0)
  {
    w1 = a / (a + b)
  }
  if ((a + c) > 0)
  {
    w2 = a / (a + c)
  }
  return(list(w1vs2=w1,w2vs1=w2))
}


