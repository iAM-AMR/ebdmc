#'
#' @title
#'    Plot a comparison between beta measures for abundance data and their presence-absence forms
#'
#' @description
#'    Compare measures of beta diversity for abundance (count) measures and their
#'    presence-absence (binary) forms.
#'
#' @param fndata
#'    A table of measures of beta diversity in long format, with columns: `schema_order`,
#'    `measure`, `measure_type`, `value`, `comparison`.
#'
#' @param .measure
#'    A vector of the name(s) of measure(s) to plot. If length > 1, plot will be
#'    faceted by .measure. If .measure is not specified, facet across all measures
#'    in fndata.
#'
#' @param .comparison
#'    A vector of the name(s) of comparison(s) to plot. If length > 1, plot will be
#'    faceted by .comparison If .comparison is not specified, facet across all
#'    comparison in fndata.
#'
#' @details
#'    fndata must be in long format, with columns:  `schema_order`,
#'    `measure`, `measure_type`, `value`, `comparison`.
#'
#' @return
#'    A step plot of diversity.
#'
#' @importFrom dplyr filter
#' @importFrom ggplot2 aes ggplot geom_step labs facet_grid
#'
#' @export
#'


make_plot_compare_forms <- function(fndata, .measure, .comparison) {

  # Set Defaults --------------------------------------------------------------

  # If .measure is not set, facet by all .measure.
  if  (missing(.measure)) {
    .measure <- unique(fndata$measure)
  }

  # If .comparison is not set, facet by all .comparison
  if  (missing(.comparison)) {
    .comparison  <- unique(fndata$comparison)
  }


  # Create Plot ---------------------------------------------------------------

  plot_out <-

    fndata %>%

    # Filter by .schema and .measure; %in% supports strings or a vector of
    # strings.
    dplyr::filter(measure    %in% .measure,
                  comparison %in% .comparison) %>%

    ggplot2::ggplot() +

    geom_step(mapping = aes(x      = schema_order,
                            y      = value,
                            colour = measure_type)) +

    labs(x = "Schema order",
         y = "Beta diversity",
         colour = "Measure type") +

    # This structure is necessary to supply the correct labels.
    # The criteria are verbose in an 'else if', but are specified for clarity.

    if (length(.measure) > 1 && length(.comparison) > 1) {
      list(
        facet_grid(comparison ~ measure),
        labs(title = "Beta diversity")
      )
    } else if (length(.measure) == 1 && length(.comparison)  > 1) {
      list(
        facet_wrap(vars(comparison)),
        labs(title = paste0("Beta diversity (measure: ", .measure, ")"))
      )
    } else if (length(.measure)  > 1 && length(.comparison) == 1) {
      list(
        facet_wrap(vars(measure)),
        labs(title = paste0("Beta diversity (comparison: ", .comparison, ")"))
      )
    } else { # length(.measure) == 1 && length (.comparison) == 1
      list(
        labs(title = paste0("Beta diversity (measure: ", .measure,
                            ", comparison: ", .comparison, ")"))
      )
    }


  # Return Plot ---------------------------------------------------------------

  return(plot_out)

}
