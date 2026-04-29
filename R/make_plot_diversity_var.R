#'
#' @title
#'    Plot the variability in measures between comparisons
#'
#' @description
#'    Plot the variability in measures between comparisons (between groups).
#'
#' @param fndata
#'    A table of measures of beta diversity in long format, with columns: `schema_order`,
#'    `measure`, `measure_type`, `value`, `comparison`.
#'
#' @details
#'    fndata must be in long format, with columns:  `schema_order`,
#'    `measure`, `measure_type`, `value`, `comparison`.
#'
#' @return
#'    A box plot of variability by schema.
#'
#' @importFrom ggplot2 aes element_text geom_boxplot ggplot facet_wrap labs scale_y_continuous theme
#'
#' @export
#'


make_plot_diversity_var <- function(fndata) {

  ggplot2::ggplot(data = fndata) +

    geom_boxplot(mapping = aes(x = as.factor(schema_order),
                               y = value)) +

    scale_y_continuous(limits = c(0, 1)) +

    theme(axis.text.x = element_text(angle = 90,
                                     vjust = 0.5,
                                     hjust = 1)) +

    labs(x     = "Stringency level",
         y     = "Beta diversity",
         title = paste("Variation in measures of beta diversity across comparisons")) +

    # facet_wrap(vars(measure), scales = "free")
    facet_wrap(vars(measure))

}


