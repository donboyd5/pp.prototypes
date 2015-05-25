#' Retirees dataframe for public pension plan prototypes
#'
#' Dataframe with information on initial retirees.
#' \itemize{
#'   \item{Each record will have the number of retirees and the average annual benefit for a particular age.}
#'   \item{"Empty" cells are excluded}
#'   \item{Cells that are not allowed under a given benefit structure MAY be included.
#'   For example, there could be rows for age 50 even if a plan does not allow retirement until age 62.}
#' }
#'
#' @source Rockefeller Institute analysis of public pension plan data.
#' @format Data frame with 1 row per prototype per non-empty age.
#'    each record will have the number of retirees and the average annual benefit
#' \describe{
#' \item{planname}{Identifies a specific prototype, character}
#' \item{age}{Allowable range 50:120; only includes records that have actual data for nretirees and benefit, integer}
#' \item{nretirees}{Number of retirees in this row, can be noninteger or zero, numeric}
#' \item{benefit}{Average annual benefit in $, numeric}
#' }
#' @examples
#' library(dplyr)
#' glimpse(retirees)
"retirees"
