#'
#' @title
#'   Calculate the adjusted Wallace coefficient
#'
#' @description
#'   Calculate the adjusted Wallace coefficient.
#'
#' @param clusters_A,clusters_B
#'   A set of cluster identities (cluster memberships) for each member in a
#'   community.
#'
#' @details
#'   From dorbarker's nAWC \url{https://github.com/dorbarker/nAWC}.
#'
#' @return
#'   The adjusted Wallace coefficient.
#'
#' @export
#'

adj_wallace <- function(clusters_A, clusters_B)
{
  # Compute the Adjusted Wallace coefficients with 95% confidence intervals.
  # Severiano A., F. R. Pinto, M. Ramirez and J. Carriço. 2011. Adjusted
  #   Wallace as a Measure of Congruence between Typing Methods. J. Clin.
  #   Microbiol. doi:10.1128/JCM.00624-11
  # Pinto, F.R., J. Melo-Cristino, M. Ramirez. 2008. A Confidence Interval
  #   for the Wallace Coefficient of Concordance and Its Application to
  #   Microbial Typing Methods. PLoS ONE 3(11):e3696.
  #   doi:10.1371/journal.pone.0003696.
  #
  # Args:
  #   clusters_A: Array/list of one set of classifications.
  #   clusters_B: Array/list of another set of classifications.
  #
  # Returns:
  #   Adjusted Wallace coefficients with 95% confidence intervals for
  #   classifications 1 to 2 and 2 to 1.
  # print('adj_wallace')
  ct <- as.matrix(table(clusters_A, clusters_B))
  # print(ct)
  a <- sum(ct * (ct-1)/2)
  # print(a)
  sum_col <- colSums(ct)
  # print(sum_col)
  sum_row <- rowSums(ct)
  # print(sum_row)
  a1 <- sum(sum_col * (sum_col -1 )/2)
  # print(a1)
  b <- a1 - a
  c <- sum(sum_row * (sum_row -1)/2) - a

  n <- sum(sum_row)
  d <- ((n * (n - 1)) / 2) - a1 - c


  sum_row_2 <- sum_row[sum_row > 1]
  sum_col_2 <- sum_col[sum_col > 1]
  csumFc2 <- colSums(sweep(ct[,names(sum_col_2), drop = FALSE], 2, sum_col_2, FUN=function(x,y){(x/y)^2}))
  # print("csumFc2")
  # print(csumFc2)
  csumFc3 <- colSums(sweep(ct[,names(sum_col_2), drop = FALSE], 2, sum_col_2, FUN=function(x,y){(x/y)^3}))
  # print('csumFc3')
  # print(csumFc3)

  # print('names(sum_row_2)')
  # print(names(sum_row_2))

  rsumFc2 <- rowSums(sweep(ct[names(sum_row_2), , drop = FALSE], 1, sum_row_2, FUN=function(x,y){(x/y)^2}))
  # print('rsumFc2')
  # print(rsumFc2)
  rsumFc3 <- rowSums(sweep(ct[names(sum_row_2), , drop = FALSE], 1, sum_row_2, FUN=function(x,y){(x/y)^3}))
  # print('rsumFc3')
  # print(rsumFc3)

  rSumW1 <- sum((sum_row_2 * (sum_row_2 -1))^2 * (((4.0 * sum_row_2 * (sum_row_2 - 1.0) * (sum_row_2 - 2.0) * rsumFc3) + (2.0 * sum_row_2 * (sum_row_2 - 1.0) * rsumFc2) - (2.0 * sum_row_2 * (sum_row_2 - 1.0) * (2.0 * sum_row_2 - 3.0) * (rsumFc2^2.0))) / ((sum_row_2 * (sum_row_2 - 1.0))^2.0)))
  rSumW2 <- sum(sum_row * (sum_row - 1))
  cSumW1 <- sum((sum_col_2 * (sum_col_2 -1))^2 * (((4.0 * sum_col_2 * (sum_col_2 - 1.0) * (sum_col_2 - 2.0) * csumFc3) + (2.0 * sum_col_2 * (sum_col_2 - 1.0) * csumFc2) - (2.0 * sum_col_2 * (sum_col_2 - 1.0) * (2.0 * sum_col_2 - 3.0) * (csumFc2^2.0))) / ((sum_col_2 * (sum_col_2 - 1.0))^2.0)))
  cSumW2 <- sum(sum_col * (sum_col - 1))

  # get variance of Wallace 1vs2 (varW1) and 2vs1 (varW2)

  varW1 <- 0.0
  varW2 <- 0.0
  if (rSumW2 > 0)
  {
    varW1 <- (rSumW1 / ((rSumW2) ^ 2.0))
  }
  if (cSumW2 > 0)
  {
    varW2 <- (cSumW1 / ((cSumW2) ^ 2.0))
  }
  # get mismatch matrix
  wallace <- wallace(a, b, c)
  w1 <- wallace$w2vs1  # check this
  w2 <- wallace$w1vs2
  sid1 <- simpsons(clusters_A)
  sid2 <- simpsons(clusters_B)

  wi1 <- 1 - sid1$sid
  wi2 <- 1 - sid2$sid
  aw1 <- (w1 - wi2) / (1 - wi2)
  aw2 <- (w2 - wi1) / (1 - wi1)
  aw1CI <- 2.0 * (1.0 / (1.0 - wi2)) * sqrt(varW1)
  aw1Low <- aw1 - aw1CI
  if (aw1Low < 0.0)
  {
    aw1Low <- 0.0
  }
  aw1High <- aw1 + aw1CI
  if (aw1High > 1.0)
  {
    aw1High <- 1.0
  }
  aw2CI <- 2.0 * (1.0 / (1.0 - wi1)) * sqrt(varW2)
  aw2Low <- aw2 - aw2CI
  aw2High <- aw2 + aw2CI
  if (aw2Low < 0.0)
  {
    aw2Low <- 0.0
  }
  if (aw2High > 1.0)
  {
    aw2High <- 1.0
  }
  return(list(Wallace_A_vs_B=w1, Wallace_B_vs_A=w2, Simpsons_A=sid1, Simpsons_B=sid2, Adjusted_Wallace_A_vs_B=aw1,Adjusted_Wallace_A_vs_B_low=aw1Low, Adjusted_Wallace_A_vs_B_high=aw1High, Adjusted_Wallace_B_vs_A=aw2, Adjusted_Wallace_B_vs_A_low=aw2Low, Adjusted_Wallace_B_vs_A_high=aw2High))
}


