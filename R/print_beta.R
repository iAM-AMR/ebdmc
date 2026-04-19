#'
#' @title
#'   Print the name of a measure of beta diversity in-text using a beta symbol
#'
#' @description
#'   Print the name of a measure of beta diversity using a beta symbol in-text
#'   (e.g., for RMD/QMD documents), in the format beta subscript measure.
#'
#' @param .measure
#'   string: the name of the measure.
#'
#' @returns
#'   a string: .measure in the format b_.measure (beta subscript .measure).
#'
#' @export
#'


print_beta <- function(.measure) {
  paste0("$\\beta$~", .measure, "~")
}


