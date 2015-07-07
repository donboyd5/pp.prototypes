
# See below for:
# - loads
# - globals
# - overview documentation

# CRR locations for CAFRs and AVs
#   for all     --	http://publicplansdata.org/reports/
#   menu system --	http://publicplansdata.org/resources/download-avs-cafrs/



#****************************************************************************************************
#                    Load packages ####
#****************************************************************************************************

# packages I always want loaded
library("plyr") # always load BEFORE loading dplyr
library("dplyr")
options(dplyr.print_min = 60) # default is 10
options(dplyr.print_max = 60) # default is 20
library("foreign") # various import and export routines - e.g., for reading Stata files
library("gdata") # for reading spreadsheets
library("knitr")
library("lubridate")
library("ggplot2")
library("magrittr")
library("readr")
library("readxl")
library("stringr")
library("tidyr")

library("XLConnect") # slow but convenient because it reads ranges
library("xlsx")

library("devtools")

# load my packages last
# devtools::install_github("donboyd5/apitools")
# devtools::install_github("donboyd5/bdata")
# devtools::install_github("donboyd5/btools")
# devtools::install_github("donboyd5/pdata")
library("apitools")
library("bdata")
library("btools")
# library("pdata")


#****************************************************************************************************
#                    Global variables and directories ####
#****************************************************************************************************
draw <- "./data-raw/"

protofn <- "PrototypeAnalysis(14).xlsx"

active_ages <- list(); active_ages$min=20; active_ages$max=64
retiree_ages <- list(); retiree_ages$min=65; retiree_ages$max=120



#****************************************************************************************************
#                    Functions ####
#****************************************************************************************************

actives_ageea_adj <- function(df, minage, maxage) {
  # force age and ea for actives to fall in the allowable range by putting any violations into the boundary case
  # and then collapsing any duplicate age-ea combinations, getting weighted average salary
  df2 <- df %>% mutate(age=ifelse(age < minage, minage, age),
                       ea=ifelse(ea < minage, minage, ea),
                       age=ifelse(age > maxage, maxage, age),
                       ea=ifelse(ea > maxage, maxage, ea)) %>%
    group_by(age, ea) %>%
    summarise(salary=sum(salary*nactives) / sum(nactives), # CAUTION: this must be done BEFORE changing the meaning of nactives
              nactives=sum(nactives))
  return(df2)
}

retirees_age_adj <- function(df, minage, maxage) {
  # force age for retirees to fall in the allowable range by putting any violations into the boundary case
  # and then collapsing any duplicate ages, getting weighted average benefit
  df2 <- df %>% mutate(age=ifelse(age < minage, minage, age),
                       age=ifelse(age > maxage, maxage, age)) %>%
    group_by(age) %>%
    summarise(benefit=sum(benefit*nretirees) / sum(nretirees), # CAUTION: this must be done BEFORE changing the meaning of nretirees
              nretirees=sum(nretirees))
  return(df2)
}


splong <- function(df, fillvar, fitrange=NULL, method = "natural"){
  ## spline smoothing
  # df should have only 3 columns: fillvar, nonfillvar [in either order], and value
  # or just 2 columns, with no nonfillvar
  # last column ALWAYS must be the value var
  valvar <- names(df)[length(names(df))]
  nonfillvar <- setdiff(names(df), c(fillvar, valvar))
  f <- function(x) {
    if(is.null(fitrange)) fitrange <- min(x[, fillvar]):max(x[, fillvar])
    spl <- spline(x[,fillvar], x[,valvar], xout=fitrange, method = method)
    dfout <- data.frame(x=spl$x, y=spl$y)
    names(dfout) <- c(fillvar,valvar)
    return(dfout)
  }
  if(length(nonfillvar) > 0) dfl2 <- plyr::ddply(df, c(nonfillvar), f) else dfl2 <- f(df)
  return(dfl2)
}


splong2 <- function(df, fillvar, fitrange=NULL, method = "natural"){
  ## spline smoothing
  # df should have only 2 columns: fillvar, and valvar, where:
  #   valvar is the variable to be interpolated - the y value
  #   fillvar is the variable it corresponds to - the x value
  #   fitrange is the set of NEW x values (fillvar values) for which we will get NEW interpolated y values (valvar)
  # note the double-bracket indexing - i.,e., [[]] - a change from before. This ensures that the column selected is
  # greated as a vector, which spline needs, rather than as a one-column data frame; dplyr's tbl_df "broke" the previous version
  # so this is now better
  valvar <- names(df)[length(names(df))]
  nonfillvar <- setdiff(names(df), c(fillvar, valvar))
  f <- function(x) {
    if(is.null(fitrange)) fitrange <- min(x[, fillvar]):max(x[, fillvar])
    spl <- spline(x[[fillvar]], x[[valvar]], xout=fitrange, method = method) # double brackets ensure we return a vector rather than 1-column dataframe
    dfout <- data.frame(x=spl$x, y=spl$y)
    names(dfout) <- c(fillvar,valvar)
    return(dfout)
  }
  dfl2 <- f(df)
  return(dfl2)
}


make_plist <- function(protoname, actives, retirees, salgrowth.hist, salgrowth.assume) {
  plist <- list()
  plist$actives <- actives
  plist$retirees <- retirees
  plist$salgrowth.hist <- salgrowth.hist
  plist$salgrowth.assume <- salgrowth.assume
  saveRDS(plist, paste0(draw, protoname, ".rds"))
  return(NULL)
}




#****************************************************************************************************
#                    Overview ####
#****************************************************************************************************

# A prototype will have 3 elements:
#
# 1) Initial actives table -- number of actives and average salary, by age and ea, normalized to 1,000 actives
# 2) Initial retirees table -- number of retirees and average benefit, by age
# 3) Historical salary - to be used to construct salary history of the initial actives
# 4) Assumed salary growth - to be used for estimating future salaries

# This program creates a set of prototype .rda files in the data-raw directory. Each such prototype contains an
# actives df, a retirees df, and a salary growth df.

# A master program gathers these separate prototypes into a single actives df, retirees df, and salgrowth df.


#****************************************************************************************************
#                    Actives ####
#****************************************************************************************************

# Dataframe with information on initial actives, structured as follows:
#   1 record per prototype per non-empty age category per non-empty ea category
#   each record will have the number of actives and the average salary in the cells
#   "empty" cells will be excluded
#   physically impossible cells will not be included (e.g., there won't be a cell with age=25 and ea=30)
#   cells that are not allowed under a given benefit structure MAY be included. For example, there could be
#     cells for people of age 66 even if retirement at 65 is mandatory. The model will have to deal with this.

# Variables are:
#   planname	character	identifies a specific prototype (later in the project it could identify a specific plan or system)
#   age	integer	range 20:70; only will include records that have actual data for nactives and salary
#   ea	integer	range 20:70; only will include records that have actual data for nactives and salary
#   nactives	double	number of actives in this cell; note that it could be noninteger; it could be zero, rather than strictly positive
#   salary	double	amount in $;  note that it could be noninteger; it could be zero, rather than strictly positive

# Example
# planname	age	ea	nactives	salary
# underfunded	20	20	20	 20,000
# underfunded	21	20	10	 30,000
# underfunded	24	21	7	 30,000
# underfunded	……………………………………………………………………………….
# underfunded	64	40	100	 70,000
# underfunded	65	50	23	 72,000
# underfunded	66	50	12	 74,000
# underfunded	67	50	10	 76,000
# underfunded	68	50	9	 78,000
# underfunded	69	60	8	 80,000
# underfunded	70	60	7	 82,000
# wellfunded	21	20	30	 30,000
# wellfunded	23	20	12	 30,000
# wellfunded	24	21	15	 30,000
# wellfunded	……………………………………………………………………………….
# wellfunded	64	40	80	 70,000
# wellfunded	65	50	23	 75,000
# wellfunded	66	50	12	 80,000
# wellfunded	67	50	9	 82,000
# wellfunded	68	60	8	 84,000
# wellfunded	69	60	7	 86,000
# wellfunded	70	20	30	 88,000


#****************************************************************************************************
#                    Retirees ####
#****************************************************************************************************

# Dataframe with information on initial retirees, structured as follows:
#   1 record per prototype per non-empty age category in the allowable range (which I proposed to be 50-120)
#   each record will have the number of  retirees and the average annual benefit in the cells
#   "empty" cells will be excluded
#   physically impossible cells will not be included (e.g., there won't be a cell with age=25 and ea=30)
#   cells that are not allowed under a given benefit structure MAY be included. For example, there could
#     be cells for peole of age 66 even if retirement at 65 is mandatory. The model will have to deal with this.

# Variables are:
#   planname	character	identifies a specific prototype (later in the project it could identify a specific plan or system)
#   age	integer	range 50:120; only will include records that have actual data for nretirees and benefit
#   nretirees	double	number of retirees in this cell; note that it could be noninteger; it could be zero, rather than strictly positive
#   benefit	double	amount in $;  note that it could be noninteger; it could be zero, rather than strictly positive

# Example
# planname	age	nretirees	benefit
# underfunded	52	10	 12,000
# underfunded	54	12	 20,000
# underfunded	55	23	 24,000
# underfunded	56	44	 28,000
# underfunded	…………………………………………………
# underfunded	81	90	 27,000
# underfunded	82	80	 25,000
# underfunded	99	70	 12,000
# underfunded	105	60	 8,000
# wellfunded	61	30	 35,000
# wellfunded	62	100	 37,000
# wellfunded	64	200	 39,000
# wellfunded	65	350	 41,000
# wellfunded	66	400	 43,000
# wellfunded	67	520	 45,000
# wellfunded	…………………………………………………
# wellfunded	87	12	 25,000
# wellfunded	99	7	 12,000
# wellfunded	102	5	 8,000
# wellfunded	120	1	 5,000


#****************************************************************************************************
#                    Historical salary growth rates by age ####
#****************************************************************************************************

# Dataframe with information on historical salary growth rates by age, to be used in the model to reconstruct salary history
#   A single dataframe will store information on all prototypes
#   1 record per prototype per age, 20-70, WITH NO GAPS
#   each record will have age and the salary growth rate from that age to the next

# Variable	type
# planname	character	identifies a specific prototype (later in the project it could identify a specific plan or system)
# age	integer	range 20:70 with no gaps
# sscale.hist.rate	double	total nominal salary (merit + productivity + inflation) indexed to age 20=1


# Example
# planname	age	sscale.hist.rate
# underfunded	20	0.0450
# underfunded	21	0.0440
# underfunded	22	0.0431
# underfunded	23	0.0422
# underfunded	24
# underfunded	…………………………………
# underfunded	62	0.0029
# underfunded	63	0.0018
# underfunded	64	0.0005
# underfunded	65	0.0005
# underfunded	66	0.0005
# underfunded	67	0.0005
# underfunded	68	0.0005
# underfunded	69	0.0005
# underfunded	70	-
# wellfunded	20	0.0300
# wellfunded	21	0.0300
# wellfunded	22	0.0300
# wellfunded	23	0.0300
# wellfunded	24
# wellfunded	…………………………………
# wellfunded	62	0.0005
# wellfunded	63	0.0005
# wellfunded	64	0.0005
# wellfunded	65	0.0005
# wellfunded	66	0.0005
# wellfunded	67	0.0005
# wellfunded	68	0.0005
# wellfunded	69	0.0005
# wellfunded	70


#****************************************************************************************************
#                    Assumed salary growth rates by age ####
#****************************************************************************************************
# Same format and concepts as the historical data
# For now, the data also will be the same



