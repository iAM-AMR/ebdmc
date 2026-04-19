#'
#' @title
#'   Create a list of tables of counts by group and cluster
#'
#' @description
#'   Create a list (indexed by schema) of tables of counts of cluster-group
#'   pairs, from a community identity matrix.
#'
#' @param .cim
#'   table: a community identity matrix (CIM) with a group column (.group), and one
#'          or more schema columns (.cols).
#'
#' @param .group
#'   unquoted expression: the group column name.
#'
#' @param .cols
#'   tidyeval expression: the schema column name(s), in tidyeval notation.
#'
#' @importFrom dplyr count mutate group_split
#' @importFrom tidyselect all_of eval_select starts_with
#' @importFrom tidyr pivot_longer
#'
#' @return
#'   a list of tibbles (by schema), of counts of objects in each cluster by group.
#'
#' @export
#'


cim_to_count_table_ls <-

  function(

      .cim
    , .group = group
    , .cols  = tidyselect::starts_with("schema")

  ) {

    # Get schema cols names using tidyselect semantics.
    schema_col_names <- names(tidyselect::eval_select(.cols, data = .cim))

    # Pivot to long.
    .out <- tidyr::pivot_longer(.cim, tidyselect::all_of(schema_col_names), names_to = "schema", values_to = "clstr")

    # Coerce schema to a factor.
    # By default, count() returns a tibble ordered alphabetically. To maintain the
    # order of the schema in .cim in count() and group_split(), coerce schema to
    # a factor.
    .out <- dplyr::mutate(.out, schema = factor(.out$schema, schema_col_names))

    # Count by schema, group, clstr.
    .out <- dplyr::count(.out, schema, {{.group}}, clstr, name = "n")

    # Split into list of tibbles by schema.
    # The schema column is dropped, and the tibble is named (below) by position.
    # To validate positional rename is correct, set .keep = TRUE.
    .out <- dplyr::group_split(.out, schema, .keep = FALSE)

    # Set tibble names by position.
    names(.out) <- schema_col_names

    return(.out)

  }


