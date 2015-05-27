
# basic checks, and code to share with Yimeng

# devtools::install_github("donboyd5/pp.prototypes")
library("pp.prototypes")
library("plyr")
library("dplyr")
library("tidyr")
library("ggplot2")

#data(package="pp.prototypes")
?actives
?retirees
?salgrowth.hist
?salgrowth.assume

glimpse(actives)
count(actives, planname)
count(actives, age)
count(actives, ea) %>% data.frame
actives %>% group_by(planname) %>%
  summarise(age=sum(age*nactives) / sum(nactives),
            nactives.sum=sum(nactives), # rename so that it does not mess up next calculation
            salary=sum(salary*nactives) / sum(nactives))
tmp <- actives %>% group_by(planname, age) %>%
  summarize(nactives.sum=sum(nactives), salary=sum(nactives*salary) / nactives.sum)
qplot(age, nactives.sum, data=tmp, colour=planname, geom=c("point", "line"))
qplot(age, salary, data=tmp, colour=planname, geom=c("point", "line")) # a little odd


glimpse(retirees)
count(retirees, planname)
count(retirees, age)
retirees %>% group_by(planname) %>%
  summarise(age=sum(age*nretirees) / sum(nretirees),
            nretirees.sum=sum(nretirees), # rename so that it does not mess up next calculation
            benefit=sum(benefit*nretirees) / sum(nretirees))
tmp <- retirees %>% group_by(planname, age) %>%
  summarize(nretirees.sum=sum(nretirees), benefit=sum(nretirees*benefit) / nretirees.sum)
qplot(age, nretirees.sum, data=tmp, colour=planname, geom=c("point", "line"))
qplot(age, benefit, data=tmp, colour=planname, geom=c("point", "line")) # a little odd

glimpse(salgrowth.hist)
qplot(age, sscale.hist.rate, data=salgrowth.hist, colour=planname, geom=c("point", "line"))

glimpse(salgrowth.assume)
qplot(age, sscale.assume.rate, data=salgrowth.assume, colour=planname, geom=c("point", "line"))


