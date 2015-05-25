
# test our ability to use NLP to hit targets

# goal is to build unknown DETAILED age x yos WF and avg pay matrix consistent
# with known SUMMARY age by yos WF and avgpay matrix
# and with known DETAILED WF and avg pay amounts by age (row sums or avgs) and yos (col sums or avgs)

library(plyr)
library(dplyr)
library(lubridate)
library(magrittr)
library(readr)
library(stringr)
library(tidyr)
library(XLConnect) # slow but convenient because it reads ranges
library(nloptr)

library(btools)

ddir <- "./Data/"

fn <- "MakePrototypes(4).xlsx"
sheetname <- "TestProbWFAndPay"


# get TRUE detailed matrices - normally we won't have these
wf.d.true <- as.matrix(readWorksheetFromFile(paste0(ddir, fn), sheet=sheetname, header=FALSE, region="B14:h20"))
avgpay.d.true <- as.matrix(readWorksheetFromFile(paste0(ddir, fn), sheet=sheetname, header=FALSE, region="B35:h41"))
totpay.d.true <- wf.d.true * avgpay.d.true
sum(wf.d.true)
sum(totpay.d.true)
sum(totpay.d.true) / sum(wf.d.true) # grand average pay from details



rnames.d <- 20:26
cnames.d <- 0:6

# read true known SUMMARY wf matrix and avgpay matrix
wf.s <- as.matrix(readWorksheetFromFile(paste0(ddir, fn), sheet=sheetname, header=FALSE, region="B5:D7"))
avgpay.s <- as.matrix(readWorksheetFromFile(paste0(ddir, fn), sheet=sheetname, header=FALSE, region="B26:D28"))
totpay.s <- wf.s * avgpay.s
sum(wf.s)
sum(totpay.s)
sum(totpay.s) / sum(wf.s) # grand average pay from details
nrows.s <- nrow(wf.s)
ncols.s <- ncol(wf.s)

# get known DETAILED wf rowsums by age, colsums by yos
wf.age.d <- as.vector(t(readWorksheetFromFile(paste0(ddir, fn), sheet=sheetname, header=FALSE, region="i14:i20"))) # must transpose
wf.yos.d <- as.vector(readWorksheetFromFile(paste0(ddir, fn), sheet=sheetname, header=FALSE, region="b21:h21"))
sum(wf.s) == sum(wf.age.d)
sum(wf.s) == sum(wf.yos.d)
nrows.d <- length(wf.age.d)
ncols.d <- length(wf.yos.d)

# get known DETAILED avgpay averages by age, averages by yos
avgpay.s.age.d <- as.vector(readWorksheetFromFile(paste0(ddir, fn), sheet=sheetname, header=FALSE, region="i35:i41"))
avgpay.s.yos.d <- as.vector(readWorksheetFromFile(paste0(ddir, fn), sheet=sheetname, header=FALSE, region="b42:h42"))

# create initial guesses 
wf.d.guess <- matrix(nrow=nrows.d, ncol=ncols.d)
avgpay.d.guess <- matrix(nrow=nrows.d, ncol=ncols.d)
# for now, use averages in each cell to be collapsed

# we need a crosswalk between the row-col indexes of the summary matrices and of the detailed matrices
# this is it for now, but should be improved upon
recipe.s <- "dimtype, sindex, dindex.first, dindex.last
row, 1, 1, 3
row, 2, 4, 5
row, 3, 6, 7
col, 1, 1, 3
col, 2, 4, 5
col, 3, 6, 7"
recipe <- read_csv(str_replace_all(recipe.s, " ", ""))

# get dimensions
getdim <- function(row, col) {
  row.first <- as.numeric(recipe[recipe$dimtype=="row" & recipe$sindex==row, "dindex.first"])
  row.last <- as.numeric(recipe[recipe$dimtype=="row" & recipe$sindex==row, "dindex.last"])
  col.first <- as.numeric(recipe[recipe$dimtype=="col" & recipe$sindex==col, "dindex.first"])
  col.last <- as.numeric(recipe[recipe$dimtype=="col" & recipe$sindex==col, "dindex.last"])
  
  dimlist <- list()
  dimlist$rows <- row.first : row.last
  dimlist$cols <- col.first : col.last
  dimlist$len <- length(dimlist$rows) * length(dimlist$cols)
  return(dimlist)
}
getdim(3, 2)

fillguess <- function(dmat, smat, nrows.s, ncols.s, avg=FALSE){
  # detailed matrix, summary matrix, n summary rows, n summary columns
  for(srow in 1:nrows.s) {
    for(scol in 1:ncols.s) {
      diminfo <- getdim(srow, scol)
      if(avg==TRUE) dmat[diminfo$rows, diminfo$cols] <- smat[srow, scol] / diminfo$len else
        dmat[diminfo$rows, diminfo$cols] <- smat[srow, scol]
    }
  }
  return(dmat)
}

wf.d.guess <- fillguess(wf.d.guess, wf.s, nrows.s, ncols.s, avg=TRUE)
wf.d.guess
sum(wf.d.guess)

avgpay.d.guess <- fillguess(avgpay.d.guess, avgpay.s, nrows.s, ncols.s)
avgpay.d.guess

# now we are ready to set up the NLP




















# set up an NLP to get an age by yos matrix of actives, by individual years, that makes sense relative to other data we have
# the x's to search for are the individual-year cells in the age x yos matrix
# the constraints that must be satisfied (within a tolerance) are all of the known aggregates, including:
#   grouped cell-totals of # actives in the age x yos matrix
#   row totals and column totals for that matrix (whicy by definition will be satisfied, so we won't use them)
#   totals for individual ages and individual yos's, from elsewhere in the av
# I'll start out with an objective function where we want to minimize the squared difference of calculated total employees vs. known total emps
# but we know we will satisfy this

# slsqp allows equality constraints (convenient) but requires gradients, allows nonlinear constraints

cells <- merge(select(agedf, age), select(yosdf, yos)) %>%
  mutate(x=0) # cartesion product - sparse representation of the age x yos matrix
glimpse(cells)

# define constraint vector - each must be less than zero
evalgeq <- function(x, cells) {
  # equality constraints
  # start by setting the 
  h <- numeric(6)
  # all of these must be <=0
  h[1] <- 127 - 2*x[1]^2 - 3*x[2]^4 - x[3] - 4*x[4]^2 - 5*x[5]
  h[2] <- 282 - 7*x[1] - 3*x[2] - 10*x[3]^2 - x[4] + x[5]
  h[3] <- 196 - 23*x[1] - x[2]^2 - 6*x[6]^2 + 8*x[7]
  h[4] <- -4*x[1]^2 - x[2]^2 + 3*x[1]*x[2] -2*x[3]^2 - 5*x[6]	+11*x[7]
  
  # x1+x2~ 4.282 -> (a) x1+x2 < 1.1*4.282, (b) x1+x2 > .9*4.282
  tol <- .01
  val <- 5
  h[5] <- x[1] + x[2] - (1+tol) * val
  h[6] <- -x[1] - x[2] + (1-tol) * val
  return(-h) # note the minus sign
}


# define objective function 267428



fn.hs100 <- function(x) {
  (x[1]-10)^2 + 5*(x[2]-12)^2 + x[3]^4 + 3*(x[4]-11)^2 + 10*x[5]^6 +
    7*x[6]^2 + x[7]^4 - 4*x[6]*x[7] - 10*x[6] - 8*x[7]
}
hin.hs100 <- function(x) {
  h <- numeric(4)
  h[1] <- 127 - 2*x[1]^2 - 3*x[2]^4 - x[3] - 4*x[4]^2 - 5*x[5]
  h[2] <- 282 - 7*x[1] - 3*x[2] - 10*x[3]^2 - x[4] + x[5]
  h[3] <- 196 - 23*x[1] - x[2]^2 - 6*x[6]^2 + 8*x[7]
  h[4] <- -4*x[1]^2 - x[2]^2 + 3*x[1]*x[2] -2*x[3]^2 - 5*x[6]	+11*x[7]
  return(h)
}

x0.hs100 <- c(1, 2, 0, 4, 0, 1, 1)









