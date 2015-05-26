#' Salary growth history dataframe for public pension plan prototypes
#'
#' Dataframe with information on salary growth rates.
#' \itemize{
#'   \item{Each record will have age and the historical growth rate of salary from this age to the next.}
#'   \item{"Every age from 20 through 70, no gaps"}
#' }
#'
#' @source Rockefeller Institute analysis of public pension plan data.
#' @format Data frame with 1 row per prototype per age 20-70, no gaps.
#' \describe{
#' \item{planname}{Identifies a specific prototype, character}
#' \item{age}{20:70, no gaps, integer}
#' \item{sscale.hist.rate}{Growth rate from this age to the next, as a decimal (not as percent), numeric}
#' }
#' @examples
#' library(dplyr)
#' glimpse(salgrowth.hist)
"salgrowth.hist"
