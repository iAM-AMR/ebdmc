#'
#' @title
#'   Create an example ebdmc matrix
#'
#' @description
#'   Create an example matrix for documentation in the ebdmc project.
#'
#' @param .nrow
#'   integer: number of rows to generate
#'
#' @param .ncol
#'   integer: number of columns to generate
#'
#' @param .type
#'   string: one of: 'cim', 'im', or 'cdm'
#'
#' @param .knit
#'   boolean: use knitr::kable() to style the output
#'
#' @return
#'   an example matrix
#'
#' @importFrom knitr kable
#' @importFrom stringr str_pad
#'
#' @export
#'


mkmatrix <-

  function(

      .nrow = 5
    , .ncol = 10
    , .type = c("cim", "im", "cdm")
    , .knit = TRUE

  ) {

    .type <- match.arg(.type)

    # ID/SA is a random number between 1 and nrow.
    # group is a random number between 1 and nrow/2, to ensure multiple rows
    # per group.

    if (.type == "cim") {

      out <- data.frame(c(paste0("ID", stringr::str_pad(1:.nrow, 3, pad = "0"))),
                        c(paste0("group", sample(1:ceiling(.nrow/2), .nrow, replace = TRUE))),
                        replicate(.ncol, c(sample(x = 1:.nrow, size = .nrow, replace = TRUE))))

      names(out) <- c("id", "group", paste0("schema_", stringr::str_pad(1:.ncol, 3, pad = "0")))

    }

    if (.type == "im") {

      out <- data.frame(c(paste0("SA", stringr::str_pad(1:.nrow, 3, pad = "0"))),
                        replicate(.ncol, c(sample(1:.nrow))),
                        row.names = 1)

      names(out) <- c(stringr::str_pad(1:.ncol, 3, pad = "0"))

    }

    if (.type == "cdm") {

      out <- data.frame(c(paste0("group", 1:.nrow)),
                        replicate(.ncol, c(sample(1:(.nrow*5), size = .nrow))),
                        row.names = 1)
      names(out) <- paste0("type_", c(stringr::str_pad(1:.ncol, 3, pad = "0")))

    }

    if (.knit) {
      knitr::kable(out)
    } else {
      return(out)
    }

  }


