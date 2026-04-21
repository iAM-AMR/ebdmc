#'
#' @title
#'   Replace unsafe names of measures of beta diversity
#'
#' @description
#'   Replace unsafe (e.g., for column) names of measures of beta diversity
#'   in betadiver() with safe names.
#'
#' @param .measure
#'   a string or vector: the unsafe name of the measure(s).
#'
#' @returns
#'   a string or vector: the safe name of the measure(s), where applicable.
#'
#' @importFrom dplyr case_match
#'
#' @export
#'





# Some measures as parameters in betadiver() are unsafe column/selection names.
# Replace unsafe names with safe column/selection names.


msafely <- function(.measures) {
  # Prefix numbers and symbols with 'x'.
  dplyr::case_match(.measures,
                    "-1" ~ "x1",
                    "-2" ~ "x2",
                    "-3" ~ "x3",
                    "19" ~ "x19",
                    .default = .measures)
}

