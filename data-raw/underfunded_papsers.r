
# Construct underfunded high outflow prototype, based on PA-SERS
# http://publicplansdata.org/reports/PA_PA-PSERS_AV_2013_92.pdf


# based on data in the AV at

baseplan <- "PA-PSERS"
protoname <- "underfunded"

totactives <- 1000
abratio <- 1.6
totretirees <- totactives / abratio


#****************************************************************************************************
#                    Actives ####
#****************************************************************************************************
# actives - numbered p.34 ####
# convert to the ea x age format
range <- "A5:M25"  # include column headers, but NOT row or column totals
(df <- readWorksheetFromFile(paste0(draw, protofn), sheet=paste0(baseplan, ".Actives"), header=TRUE, region=range, colTypes="character"))

df2 <- df %>% gather(yos, value, -order, -type, -midage, -agegrp) %>%
  mutate(age=as.integer(midage),
         yos=as.integer(gsub("[^0-9]", "", yos)),
         ea=as.integer(age - yos),
         value=cton(value)) %>%
  select(age, ea, value, type) %>%
  spread(type, value) %>%
  filter(nactives>0)
# check total n actives and avg salary here before scaling
sum(df2$nactives)
sum(df2$nactives * df2$salary) / sum(df2$nactives) # grand average salary

# good - now finish up
actives <- df2 %>% mutate(planname=protoname,
                          nactives=nactives / sum(nactives) * totactives) %>% # totactives is a parameter above
  select(planname, age, ea, nactives, salary) %>%
  arrange(ea, age)

glimpse(actives)
filter(actives, age<ea) # should be zero rows
sum(actives$nactives)
sum(actives$nactives * actives$salary) / sum(actives$nactives) # grand average salary


#****************************************************************************************************
#                    Retirees ####
#****************************************************************************************************

# repeat for retirees p.35 ####
# NOTE THAT WE ONLY NEED THE TOTALS COLUMN
range <- "A4:N24"  # include column headers; for retirees, include column totals but not row totals
(df <- readWorksheetFromFile(paste0(draw, protofn), sheet=paste0(baseplan, ".Retirees"), header=TRUE, region=range, colTypes="character"))

df2 <- df %>% select(type, age=midage, value=total) %>%
  mutate(age=as.integer(age),
         value=cton(value)) %>%
  spread(type, value) %>%
  filter(nretirees>0)
# check total n actives and avg salary here before scaling
sum(df2$nretirees)
sum(df2$nretirees * df2$benefit) / sum(df2$nretirees) # grand average benefit

# good - now finish up
retirees <- df2 %>% mutate(planname=protoname,
                           nretirees=nretirees / sum(nretirees) * totretirees) %>% # totactives is a parameter above
  select(planname, age, nretirees, benefit) %>%
  arrange(age)

glimpse(retirees)
sum(retirees$nretirees)
sum(retirees$nretirees * retirees$benefit) / sum(retirees$nretirees) # grand average benefit


#****************************************************************************************************
#                    Historical salary growth ####
#****************************************************************************************************
range <- "A4:B12"  # include column headers
(df <- readWorksheetFromFile(paste0(draw, protofn), sheet=paste0(baseplan, ".SalGrowHist"), header=TRUE, region=range, colTypes="character"))

# we can see that the rate is 0.0375 from age 55 on, so force that rather than let the spline make it different
(df2 <- rbind(filter(df, age<55), data.frame(age=55:70, rate=0.0375)))


salgrowth.hist <- splong(df2, "age", fitrange=20:70, method = "natural") %>%
  mutate(planname=protoname,
         age=as.integer(age)) %>%
  select(planname, age, sscale.hist.rate=rate) %>%
  arrange(age)

qplot(age, sscale.hist.rate, data=salgrowth.hist, geom=c("point", "line"))


#****************************************************************************************************
#                    Assumed salary growth ####
#****************************************************************************************************
salgrowth.assume <- salgrowth.hist %>% rename(sscale.assume.rate=sscale.hist.rate)


#****************************************************************************************************
#                    Make the list and save ####
#****************************************************************************************************

underfunded <- list()
underfunded$actives <- actives
underfunded$retirees <- retirees
underfunded$salgrowth.hist <- salgrowth.hist
underfunded$salgrowth.assume <- salgrowth.assume
underfunded

saveRDS(underfunded, paste0(draw, "underfunded.rds"))


