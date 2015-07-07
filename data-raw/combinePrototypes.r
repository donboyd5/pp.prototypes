

# Each prototype is a list with 4 data frames. Create 4 data frames, each of which has data for all prototypes.
protonames <- c("average", "underfunded", "underfundedTeachers", "oldplan", "highabratio", "youngplan")

getproto <- function(proto) readRDS(paste0(draw, paste0(proto, ".rds")))
biglist <- llply(protonames, getproto)

actives <- ldply(1:length(biglist), function(lnum) return(biglist[[lnum]]$actives))
retirees <- ldply(1:length(biglist), function(lnum) return(biglist[[lnum]]$retirees))
salgrowth.hist <- ldply(1:length(biglist), function(lnum) return(biglist[[lnum]]$salgrowth.hist))
salgrowth.assume <- ldply(1:length(biglist), function(lnum) return(biglist[[lnum]]$salgrowth.assume))

count(actives, planname)
count(retirees, planname)
count(salgrowth.hist, planname)
count(salgrowth.assume, planname)

#****************************************************************************************************
#                    Now save the combined data frames ####
#****************************************************************************************************
use_data(actives, overwrite = TRUE)
use_data(retirees, overwrite = TRUE)
use_data(salgrowth.hist, overwrite = TRUE)
use_data(salgrowth.assume, overwrite = TRUE)


# library(pp.prototypes)
# actives
# salgrowth.hist
# load("./data/salgrowth.assume.rda")


#****************************************************************************************************
#                    Compare prototypes ####
#****************************************************************************************************

load("./data/actives.rda")
load("./data/retirees.rda")

# actives
ht(actives)
tmp <- actives %>% group_by(planname, age) %>%
  summarise(nactives.sum=sum(nactives),
            salary.avg=sum(salary*nactives) / nactives.sum,
            yos.avg=sum(nactives * (age - ea)) / nactives.sum)
qplot(age, nactives.sum, data=tmp, colour=planname, geom=c("point", "line"))
qplot(age, salary.avg, data=tmp, colour=planname, geom=c("point", "line"))
qplot(age, yos.avg, data=tmp, colour=planname, geom=c("point", "line"))
# plan level summaries
actives %>% group_by(planname) %>%
  summarise(nactives.sum=sum(nactives),
            salary.avg=sum(salary*nactives) / nactives.sum,
            yos.avg=sum(nactives * (age - ea)) / nactives.sum,
            age.avg=sum(age*nactives) / nactives.sum)


# retirees
ht(retirees)
qplot(age, nretirees, data=retirees, colour=planname, geom=c("point", "line"))
qplot(age, benefit, data=retirees, colour=planname, geom=c("point", "line"))
# get plan level summaries
retirees %>% group_by(planname) %>%
  summarise(nretirees.sum=sum(nretirees),
            benefit.avg=sum(benefit*nretirees) / nretirees.sum,
            age.avg=sum(age*nretirees) / nretirees.sum)




