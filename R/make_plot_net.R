#'
#' @title
#'    Plot measures of beta diversity for multiple comparisons as a network diagram
#'
#' @description
#'    Create a network diagram of similarity between groups from a long data frame
#'    of diversity measures.
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
#' @param .lnsize
#'    A vector c(low, high) of the range of edge (line) widths to plot. Set smaller
#'    values (e.g., c(1, 3)) for plots with many facets to avoid overlapping edges.
#'
#' @details
#'    NOTE: THIS FUNCTION PLOTS SIMILARITY (1 - DIVERSITY).
#'
#'    This function returns a network diagram of similarity among all unique groups
#'    found in the 'group_from' and 'group_to' columns. If more than one measure
#'    or schema are specified, the returned plot is faceted. If .measure or .schema
#'    are not specified, the returned plot is faceted across all measures and schema
#'    in fndata. fndata must be in long format, with columns:  'schema', 'measure',
#'    'value', 'group_from`, `group_to`.
#'
#'    Why plot similarity and not diversity?
#'
#'    A network plot demonstrates magnitude through colour and edge (line) width.
#'    The natural interpretation of colour intensity and edge width is of a measure
#'    of the similarity between nodes (i.e., where an increased value corresponds
#'    to increased similarity). Plotting diversity necessitates inverting each
#'    scale, and makes interpretation more difficult.
#
#' @return
#'    A network plot of similarity between groups.
#'
#' @importFrom tibble tibble
#' @importFrom dplyr filter mutate
#' @importFrom ggraph ggraph
#'
#' @export


make_plot_net <-

  function(

      fndata
    , .measure
    , .schema
    , .lnsize = c(1, 5)

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


  # Setup Nodes and Edges -----------------------------------------------------

  # Get groups in fndata, in order of appearance.
  # Order affects plotting, but not computation.
  # Contiguous comparisons should be adjacent in fndata to ensure they are
  # positioned adjacent in the plot.
  lgrps <- unique(c(fndata$group_from, fndata$group_to))

  # Create a data frame of nodes (groups), with a numeric id.
  nodes_df  <- tibble::tibble(id = 1:length(lgrps), label = lgrps)

  # Create a mapping between the nodes (groups) and id in a named vector.
  nodes_map <- set_names(nodes_df$id, nodes_df$label) # MAGRITTR


  # Create Plot ---------------------------------------------------------------

  plot_out <-

    fndata %>%

    # Filter by .schema and .measure; %in% supports strings or a vector of
    # strings.
    dplyr::filter(schema  %in% .schema,
                  measure %in% .measure) %>%

    # Add 'from' and 'to' cols, as id of group.
    # Calculate similarity.
    dplyr::mutate(to               = dplyr::recode(group_to,   !!!nodes_map),
                  from             = dplyr::recode(group_from, !!!nodes_map),
                  similarity       = 1 - value) %>%

    # Create graph object.
    {tidygraph::tbl_graph(nodes    = nodes_df,
                          edges    = .,
                          directed = FALSE)} %>%

    # Create plot object.
    # Note, ggplot/gggraph functions are not scoped (::) here -- do
    # library('ggplot') and library('ggraph').
    ggraph::ggraph(layout = 'circle') +

    geom_node_point(size = .lnsize[[2]],
                    show.legend = FALSE) +

    # Plot node names.
    geom_node_text(mapping      = aes(label = label),
                   repel        = FALSE,
                   nudge_y      = 0.2) +

    # Plot edges. Set width and color to similarity.
    geom_edge_link(mapping = aes(color = similarity,
                                 width = similarity)) +

    # Set color gradient scale.
    scale_edge_color_continuous(low    = "#56B1F7",
                                high   = "#132B43",
                                name   = "Similarity",  # Set name in legend.
                                limits = c(0, 1)) +     # Set uniform limits.

    # Set width gradient scale.
    # Reverse the scale order in legend to greatest width at top, to match color.
    scale_edge_width_continuous(range  = .lnsize,       # Set c(low, high) plotted edge width.
                                guide  = guide_legend(title  = "Similarity",
                                                      reverse = TRUE),
                                limits = c(0, 1)) +     # Set uniform limits.

    # These criteria are verbose for an else if structure, but are specified
    # here for reference.
    if (       length(.measure)  > 1 && length(.schema)  > 1) {
      # If multiple values are passed to .measure, and .schema, facet by
      # .measure and .schema, and use a generic title, because measure
      # and schema are already reported in facet labels.
      list(
        facet_edges(vars(measure, schema)),
        labs(title = paste("Similarity among groups"))
      )
    } else if (length(.measure)  > 1 && length(.schema) == 1) {
      # If multiple values are passed to .measure and a single value is
      # passed to .schema, facet only by .measure and report schema in
      # the title.
      list(
        facet_edges(vars(measure)),
        labs(title = paste("Similarity among groups (schema:", .schema, ")"))
      )
    } else if (length(.measure) == 1 && length(.schema)  > 1) {
      # If a single value is passed to .measure and multiple values are
      # passed to .schema, facet only by .schema and report measure in
      # the title.
      list(
        facet_edges(vars(schema)),
        labs(title = paste("Similarity among groups (measure:", .measure, ")"))
      )
    } else { # length(.measure) == 1 && length (.schema) == 1
      # If a single value is passed to .measure and to .schema, do not facet.
      # Report measure and schema in the title.
      list(labs(title = paste("Similarity among groups (measure:", .measure,
                              ", schema:", .schema, ")")))
    }


  # Return Plot ---------------------------------------------------------------

  return(plot_out)

}


