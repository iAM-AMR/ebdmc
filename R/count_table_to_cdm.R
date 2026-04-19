#'
#' @title
#'    Create a community data matrix (CIM) from a table of cluster-group counts
#'
#' @description
#'    Create a community data matrix (CDM) from a table of counts by cluster and group.
#'
#' @param .counts
#'   dataframe: a table of counts by group.
#'
#' @param .clstr_col
#'   unquoted expression: the column with cluster identities.
#'
#' @param .count_col
#'   unquoted expression: the column with the counts.
#'
#' @return
#'    a community data matrix (CDM).
#'
#' @importFrom dplyr relocate
#' @importFrom tibble column_to_rownames
#' @importFrom tidyselect all_of
#' @importFrom tidyr pivot_wider
#'
#' @export
#'


count_table_to_cdm <-

  function(

      .counts
    , .clstr_col = clstr
    , .count_col = n

  ) {

  # Pivot to wide, and fill NAs with 0 to create a matrix-like structure.
  .out <- tidyr::pivot_wider(.counts, names_from = {{.clstr_col}}, values_from = {{.count_col}}, values_fill = 0)

  # Move group names to rownames.
  .out <- tibble::column_to_rownames(.out, var = "group")

  # Get clstr columns in ascending, numeric order, and order in dataframe.
  .cols <- sort(as.numeric(colnames(.out)))
  .out <- dplyr::relocate(.out, tidyselect::all_of(as.character(.cols)))

  # Coerce to matrix.
  .out <- as.matrix(.out)

  return(.out)

}


