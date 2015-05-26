
# Construct average prototype, based on AZ-SERS
# http://publicplansdata.org/reports/AZ_AZ-SRS_AV_2013_6.pdf

baseplan <- "AZ-SERS"
protoname <- "average"

totactives <- 1000
abratio <- 2.1
totretirees <- totactives / abratio


#****************************************************************************************************
#                    Actives ####
#****************************************************************************************************
# actives - numbered ####
# convert to the ea x age format
range <- "A8:L32"  # include column headers, but NOT row or column totals
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

# repeat for retirees ####
# NOTE THAT WE ONLY NEED THE TOTALS COLUMN
range <- "A9:O29"  # include column headers; for retirees, include column totals but not row totals
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
range <- "B4:C16"  # include column headers
(df <- readWorksheetFromFile(paste0(draw, protofn), sheet=paste0(baseplan, ".SalGrowHist"), header=TRUE, region=range, colTypes="character"))

# These data are on a yos basis. Conver to age basis (20:70) assuming entrance at 20
df2 <- df %>% mutate(age=as.integer(c(20:29, 35, 40)),
                     rate=cton(rate)) %>%
  select(age, rate)
(df3 <- rbind(df2, data.frame(age=41:70, rate=0.03)))

salgrowth.hist <- splong(df3, "age", fitrange=20:70, method = "natural") %>%
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

average <- list()
average$actives <- actives
average$retirees <- retirees
average$salgrowth.hist <- salgrowth.hist
average$salgrowth.hist <- salgrowth.assume
average

saveRDS(average, paste0(draw, "average.rds"))







