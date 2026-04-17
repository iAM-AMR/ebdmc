#'
#'
#' @title
#'    \emph{Salmonella} Heidelberg isolates typed using a SNP-based scheme at different levels of stringency
#'
#' @description
#'    Isolates of \emph{Salmonella} Heidelberg collected in 2013 from broiler
#'    chickens in Ontario, Canada, through the Canadian Integrated Program for
#'    Antimicrobial Resistance Surveillance (CIPARS) and the Canadian Food
#'    Inspection Agency's (CFIA's) National Microbiological Baseline Study in
#'    broiler chickens were typed using a single-nucleotide polymorphism (SNP)
#'    -based scheme at different levels of stringency.
#'
#'    See: Hetman, B. M., Pearl, D. L., Barker, D. O. R., Robertson, J.,
#'         Nash, J. H. E., Reid-Smith, R., Agunos, A., Carrillo, C., Topp, E.,
#'         Van Domselaar, G., Parmley, E. J., Bharat, A., Mulvey, M., Allen, V.,
#'         & Taboada, E. N. (2022). Combining analytical epidemiology and genomic
#'         surveillance to identify risk factors associated with the spread of
#'         antimicrobial resistance in \emph{Salmonella enterica} subsp.
#'         \emph{Enterica} serovar Heidelberg. Microbial Genomics, 8(11), 000891.
#'         \url{https://doi.org/10.1099/mgen.0.000891}

#'
#' @format
#'    A data frame (tibble) of 257 rows and 43 columns:
#'    \describe{
#'      \item{id}{an anonymized identifier.}
#'      \item{group}{the production stage from which the isolate was sampled.}
#'      \item{snp_###}{the cluster/type membership using a stringency of ###,
#'                     where ### is the threshold number of SNP difference
#'                     used to establish a new type. For example, where 000,
#'                     isolates which differ at any SNP are considered a new
#'                     type. Where 001, isolates which differ at more than one
#'                     SNP are considered a new type.}
#'    }

"SHCKON2013_SNP"
