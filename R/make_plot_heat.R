#'
#' @title
#'    Plot measures of beta diversity for multiple comparisons as a heat map
#'
#' @description
#'    make_plot_heat() creates a heat map of diversity between from a long
#'    data frame of diversity measures.
#'
#' @param fndata
#'    A table of measures of beta diversity in long format, with columns: `schema`,
#'    `measure`, `value`, `group_from`, `group_to`.
#'
#' @param .measure
#'    A vector of the name(s) of measure(s) to plot. If length > 1, plot will be
#'    faceted by .measure. If .measure is not specified, facet across all measures
#'    in fndata.
#'
#' @param .schema
#'    A vector of the name(s) of schema(s) to plot. If length > 1, plot will be
#'    faceted by .schema If .schema is not specified, facet across all schema
#'    in fndata.
#'
#' @param .lblsize
#'    A label size (numeric) for numeric labels.
#'
#' @details
#'    fndata must be in long format, with columns:  'schema', 'measure',
#'    'value', 'group_from`, `group_to`.
#'
#'    This visualization is useful where contiguous comparisons are on the
#'    diagonal. They will be on the diagonal if they are adjacent in order of
#'    appearance in 'group_from'.
#'
#' @return
#'    A heat map of diversity between stages.
#'
#' @importFrom dplyr filter
#' @importFrom forcats fct_relevel
#' @importFrom ggplot2 ggplot
#'
#' @export
#'


make_plot_heat <-

  function(

      fndata
    , .measure
    , .schema
    , .lblsize = 5

  ) {

  # Set Defaults --------------------------------------------------------------

  # If .measure is not set, facet by all .measure.
  if  (missing(.measure)) {
    .measure <- unique(fndata$measure)
  }

  # If .schema is not set, facet by all .schema.
  if  (missing(.schema)) {
    .schema  <- unique(fndata$schema)
  }


  # Set Orientation -----------------------------------------------------------

  # Get groups in fndata, in order of appearance.
  # Order affects plotting, but not computation.
  lgrps <- unique(c(fndata$group_from, fndata$group_to))

  # Coerce group_to and group_from to factors to set order in plots.
  # Note: fct_relevel() will ignore missing levels and warn of extra levels in grps.
  # Suppress warnings; tail(grps) and head(grps) never appear in to and from respectively.
  suppressWarnings({
    fndata$group_from <- forcats::fct_relevel(fndata$group_from, lgrps)
    fndata$group_to   <- forcats::fct_relevel(fndata$group_to,   lgrps)
  })


  # Create Plot ---------------------------------------------------------------

  plot_out <-

    fndata %>%

    # Filter by .schema and .measure; %in% supports strings or a vector of
    # strings.
    dplyr::filter(measure %in% .measure,
                  schema  %in% .schema) %>%

    ggplot2::ggplot(aes(x = group_to, y = group_from)) +

    geom_tile(aes(fill = value))  +

    # Lock scale between 0 and 1, not to min/max of the facet.
    scale_fill_continuous(limits = c(0, 1)) +

    geom_text(aes(label  = round(value, 2),
                  colour = I("white"),
                  size   = I(.lblsize))) +

    labs(x = "Compare to",
         y = "Compare from",
         fill = "Beta diversity") +

    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +

    # This structure is necessary to supply the correct labels.
    # The criteria are verbose in an 'else if', but are specified for clarity.

    if (length(.measure) > 1 && length(.schema) > 1) {
      list(
        facet_wrap(vars(measure, schema)),
        labs(title = "Beta diversity")
      )
    } else if (length(.measure) == 1 && length(.schema)  > 1) {
      list(
        facet_wrap(vars(schema)),
        labs(title = paste0("Beta diversity (measure: ", .measure, ")"))
      )
    } else if (length(.measure)  > 1 && length(.schema) == 1) {
      list(
        facet_wrap(vars(measure)),
        labs(title = paste0("Beta diversity (schema: ", .schema, ")"))
      )
    } else { # length(.measure) == 1 && length (.schema) == 1
      list(
        labs(title = paste0("Beta diversity (measure: ", .measure,
                            ", schema: ", .schema, ")"))
      )
    }


  # Return Plot ---------------------------------------------------------------

  return(plot_out)

}


