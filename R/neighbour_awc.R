#'
#' @title
#'   Calculate the neighbour-adjusted Wallace coefficient
#'
#' @description
#'   Calculate the neighbour-adjusted Wallace coefficient (nAWC).
#'
#' @param clusters_df
#'   A set of cluster identities (cluster memberships) for each member in a
#'   community.
#'
#' @details
#'   From dorbarker's nAWC \url{https://github.com/dorbarker/nAWC}.
#'
#' @return
#'   The diversity index.
#'
#' @export
#'


neighbour_awc <- compiler::cmpfun(function(clusters, i) {

  # Calculates the Adjusted Wallace Coefficent of the
  # ith column versus the i-1th column

  cur <- clusters[ , i]

  if ((i - 1) == 0 || length(unique(cur)) == 1) {
    result <- NA

  } else {

    suppressWarnings({
      result <- adj_wallace(cur, clusters[, i-1]) %>%
        use_series("Adjusted_Wallace_A_vs_B")                         # MNagrittr
    })
  }

  result
})



#' @rdname neighbour_awc
#' @export

nawc_shannon <- function(clusters) {
  # Calculates various statistics for clusters,
  # and binds them together in a data.frame

  thresholds <-
    clusters %>%
    colnames %>%
    as.integer

  entropy <- sapply(clusters, shannon)

  nawc <-
    clusters %>%
    seq_along %>%
    sapply(neighbour_awc, clusters=clusters)

  # n_clusts <- sapply(clusters, function(x) length(unique(x)))

  # p_singletons <- sapply(clusters, singleton_proportion)

  df <- data.frame("threshold"    = thresholds,
                   "nawc"         = nawc,
                   "shannon_bits" = entropy,

                   # "Number of Clusters" = n_clusts,
                   # "Proportion of Singleton Clusters" = p_singletons,
                   check.names = FALSE)
  df
}



