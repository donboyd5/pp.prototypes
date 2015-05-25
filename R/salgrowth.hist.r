#' Salary growth history dataframe for public pension plan prototypes
#'
#' Dataframe with information on initial retirees.
#' \itemize{
#'   \item{Each record will have age and the historical growth rate of salary from this age to the next.}
#'   \item{"Empty" cells are excluded}
#'   \item{Cells that are not allowed under a given benefit structure MAY be included.
#'   For example, there could be rows for age 50 even if a plan does not allow retirement until age 62.}
#' }
#'
#' @source Rockefeller Institute analysis of public pension plan data.
#' @format Data frame with 1 row per prototype per age 20-70, no gaps.
#'    each record will have the number of retirees and the average annual benefit
#' \describe{
#' \item{planname}{Identifies a specific prototype, character}
#' \item{age}{20:70, no gaps, integer}
#' \item{sscale.hist.rate}{Growth rate from this age to the next, as a decimal (not as percent), numeric}
#' }
#' @examples
#' library(dplyr)
#' glimpse(salgrowth.hist)
"salgrowth.hist"
