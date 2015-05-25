
# Construct underfunded prototype, based on PA-SERS
# http://publicplansdata.org/reports/PA_PA-PSERS_AV_2013_92.pdf

# Gather some input assumptions from PA-PSERS, as a rough example of a
# high outflow poorly funded prototype

# based on data in the AV at

totactives <- 1000
abratio <- 1.6
totretirees <- totactives / abratio



#****************************************************************************************************
#                    Actives ####
#****************************************************************************************************
# actives - numbered p.34 ####
# age x yos
# age 25 25-29 30-34 35-39 40-44 45-49 50-54 55-59 60-64 Over 64
# yos 0-4	 5-9	 10-14	15-19	20-24	25-29	30-34	35-39	40+
# midpoints of the age and yos ranges
age.mid <- c(25, seq(27, 62, 5), 66)
yos.mid <- c(seq(2, 37, 5), 42)

# convert to the ea x age format
df <- readWorksheetFromFile(paste0(draw, protofn), sheet="PA-PSERSActives", header=FALSE, region="A5:L24")
names(df) <- c("order", "tabletype", "agegrp", yos.mid)


wf <- df %>% filter(tabletype=="workforce") %>%
  arrange(order) %>%
  mutate(age=as.integer(age.mid)) %>%
  select(-order, -tabletype, -agegrp) %>%
  gather(yos, nworkers, -age) %>%
  filter(nworkers>0) %>%
  mutate(ea=as.integer(age - cton(as.character(yos)))) %>%
  select(-yos) %>%
  spread(age, nworkers, fill=0)
rownames(wf) <- wf$ea
wfl <- wf %>% gather(age, nactives, -ea, convert=TRUE) # long data frame; convert will create integer ea

avgpay <- df %>% filter(tabletype=="avgpay") %>%
  arrange(order) %>%
  mutate(age=as.integer(age.mid)) %>%
  select(-order, -tabletype, -agegrp) %>%
  gather(yos, avgpay, -age) %>%
  filter(avgpay>0) %>%
  mutate(ea=as.integer(age - cton(as.character(yos)))) %>%
  select(-yos) %>%
  spread(age, avgpay, fill=0)
rownames(avgpay) <- avgpay$ea
avgpayl <- avgpay %>% gather(age, salary, -ea, convert=TRUE) # long data frame


# create actives data frame
actives <- left_join(wfl, avgpayl) %>%
  filter(nactives>0) %>%
  mutate(planname="underfunded",
         nactives=nactives / sum(nactives) * totactives) %>% # totactives is a parameter above
  select(planname, age, ea, nactives, salary)

glimpse(actives)
filter(actives, age<ea) # should be zero rows
sum(actives$nactives)


#****************************************************************************************************
#                    Retirees ####
#****************************************************************************************************

# repeat for retirees p.35 ####
# age x yos
# age has different groupings than for actives, of course; yos groupings are the same
# age <50 50-54 55-59 60-64 65-69 70-74 75-79 80-84 85-89 Over 89
# yos 0-4	 5-9	 10-14	15-19	20-24	25-29	30-34	35-39	40+
age.mid <- c(48, seq(52, 87, 5), 90)
# yos.mid <- c(seq(2, 37, 5), 42)

# NOTE THAT WE ONLY NEED THE TOTALS COLUMN

# convert to the ea x age format
df <- readWorksheetFromFile(paste0(draw, protofn), sheet="PA-PSERSRetirees", header=FALSE, region="A4:M25")
# we only need the totals column
df <- df %>% select(Col1, Col2, Col3, Col13)
names(df) <- c("type", "order", "agegrp", "total")
retirees <- df %>% spread(type, total) %>%
  filter(agegrp!="Total") %>%
  mutate(planname="underfunded",
         age=age.mid[order],
         nretirees=num / sum(num) * totretirees) %>%
  select(planname, age, benefit=bens, nretirees)

retirees
totretirees

sum(retirees$nretirees) # good, matches
sum(retirees$nretirees*retirees$benefit) / 1e6 # good, ~ matches p.33 (I think it's close enough)
sum(retirees$nretirees*retirees$benefit) / sum(retirees$nretirees) # good, matches


#****************************************************************************************************
#                    Salary growth ####
#****************************************************************************************************
df <- readWorksheetFromFile(paste0(draw, protofn), sheet="PA-PSERSSalGrowth.Hist", header=FALSE, region="A5:B12")
names(df) <- c("age", "sscale.hist.rate")
df

salgrowth.hist <- splong(df, "age", fitrange=20:70, method = "natural") %>%
  mutate(planname="underfunded",
         age=as.integer(age),
         sscale.hist.rate=sscale.hist.rate / 100) %>%
  select(planname, age, sscale.hist.rate) %>%
  arrange(age)



#****************************************************************************************************
#                    Make the list and save ####
#****************************************************************************************************

underfunded <- list()
underfunded$actives <- actives
underfunded$retirees <- retirees
underfunded$salgrowth.hist <- salgrowth.hist
underfunded

saveRDS(underfunded, paste0(draw, "underfunded.rds"))







