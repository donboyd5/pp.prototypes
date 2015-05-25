#' Actives dataframe for public pension plan prototypes
#'
#' Dataframe with information on initial actives.
#' \itemize{
#'   \item{Each record will have the number of actives and the average salary in an age-ea cell.}
#'   \item{"Empty" cells are excluded}
#'   \item{Physically impossible cells are not included (e.g., there won't be a cell with age=25 and ea=30)}
#'   \item{Cells that are not allowed under a given benefit structure MAY be included.
#'   For example, there could be rows for ages beyond a plan's allowable retirement age.}
#' }
#'
#' @source Rockefeller Institute analysis of public pension plan data.
#' @format Data frame with 1 row per prototype per non-empty age category per non-empty ea category.
#'    each record will have the number of actives and the average salary in the cells
#' \describe{
#' \item{planname}{Identifies a specific prototype, character}
#' \item{age}{Allowable range 20:70; only includes records that have actual data for nactives and salary, integer}
#' \item{ea}{Allowable range 20:70; only includes records that have actual data for nactives and salary, not less than age, integer}
#' \item{nactives}{Number of actives in this cell, can be noninteger or zero, numeric}
#' \item{salary}{Amount in $, numeric}
#' }
#' @examples
#' library(dplyr)
#' glimpse(actives)
"actives"
