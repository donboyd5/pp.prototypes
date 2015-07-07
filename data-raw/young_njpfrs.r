
# Construct old (mature) prototype, based primarily on MI-MPSERS
#


# based on data in the AV at

baseplan <- "NJ-PFRS"
protoname <- "youngplan"

totactives <- 1000
abratio <- 1.69
totretirees <- totactives / abratio


#****************************************************************************************************
#                    Actives ####
#****************************************************************************************************
# actives - numbered p. ####
# convert to the ea x age format
range <- "A5:N27"  # include column headers, but NOT row or column totals
(df <- readWorksheetFromFile(paste0(draw, protofn), sheet=paste0(baseplan, ".Actives"), header=TRUE, region=range, colTypes="character"))

df2 <- df %>% gather(yos, value, -order, -midage, -type, -agegrp) %>%
  mutate(age=as.integer(midage),
         yos=as.integer(gsub("[^0-9]", "", yos)),
         ea=as.integer(age - yos),
         value=cton(value)) %>%
  filter(!is.na(age), !is.na(ea)) %>%
  select(age, ea, value, type) %>%
  spread(type, value) %>%
  filter(nactives>0)

# check total n actives and avg salary here to be sure we hit published numbers, before adjusting and scaling
sum(df2$nactives)
sum(df2$nactives * df2$salary) / sum(df2$nactives) # grand average salary

# Now, adjust ages and entry ages as needed so that each fits in 20:70
df3 <- actives_ageea_adj(df2, active_ages$min, active_ages$max)

# check nactives and tot salary
sum(df2$nactives); sum(df3$nactives)
sum(df2$nactives * df2$salary) / sum(df2$nactives); sum(df3$nactives * df3$salary) / sum(df3$nactives)


# good - now finish up
actives <- df3 %>% ungroup %>% # ungroup, just to be safe
  mutate(planname=protoname,
         nactives=nactives / sum(nactives) * totactives) %>% # totactives is a parameter above
  select(planname, age, ea, nactives, salary) %>%
  arrange(ea, age)

glimpse(actives)
filter(actives, age<ea) # should be zero rows
sum(actives$nactives)
sum(actives$nactives * actives$salary) / sum(actives$nactives) # grand average salary
sum(actives$nactives * actives$age) / sum(actives$nactives) # grand average age
actives %>% mutate(yos=age-ea) %>% summarize(avgyos=sum(yos*nactives) / sum(nactives)) # grand average yos

actives %>% select(age, ea, nactives) %>% spread(ea, nactives) %>% kable(digits=2)
actives %>% select(age, ea, salary) %>% spread(ea, salary) %>% kable(digits=2)



#****************************************************************************************************
#                    Retirees ####
#****************************************************************************************************

# repeat for retirees p. ####
# NOTE THAT WE ONLY NEED THE TOTALS COLUMN
range <- "A5:D117"  # include column headers; for retirees, include column totals but not row totals
(df <- readWorksheetFromFile(paste0(draw, protofn), sheet=paste0(baseplan, ".Retirees"), header=TRUE, region=range, colTypes="character"))

df2 <- df %>% select(type, age=midage, value=total) %>%
  mutate(age=as.integer(age),
         value=cton(value)) %>%
  filter(!is.na(age)) %>%
  spread(type, value) %>%
  filter(nretirees>0)
# check total n retirees and avg benefit here before scaling
sum(df2$nretirees)
sum(df2$nretirees * df2$benefit) / sum(df2$nretirees) # grand average benefit

# Now, adjust ages and entry ages as needed so that each fits in the allowable range
df3 <- retirees_age_adj(df2, retiree_ages$min, retiree_ages$max)

# check nretirees and average benefit
sum(df2$nretirees); sum(df3$nretirees)
sum(df2$nretirees * df2$benefit) / sum(df2$nretirees); sum(df3$nretirees * df3$benefit) / sum(df3$nretirees)

# good - now finish up
retirees <- df3 %>% mutate(planname=protoname,
                           nretirees=nretirees / sum(nretirees) * totretirees) %>% # totretirees is a parameter above
  select(planname, age, nretirees, benefit) %>%
  arrange(age)

glimpse(retirees)
sum(retirees$nretirees)
sum(retirees$nretirees * retirees$benefit) / sum(retirees$nretirees) # grand average benefit


#****************************************************************************************************
#                    Historical salary growth ####
#****************************************************************************************************
range <- "A5:B14"  # include column headers
(df <- readWorksheetFromFile(paste0(draw, protofn), sheet=paste0(baseplan, ".SalGrowHist"), header=TRUE, region=range, colTypes="character"))

# we can see that the rate is 0.0395 from age 45 on, so force that rather than let the spline make it different
(df2 <- rbind(filter(df, age<45), data.frame(age=45:70, rate=0.0395)))


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

make_plist(protoname, actives, retirees, salgrowth.hist, salgrowth.assume)

# check
tmp <- readRDS(paste0(draw, protoname, ".rds"))
str(tmp)

