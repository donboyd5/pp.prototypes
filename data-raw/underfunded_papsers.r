
# Construct underfunded prototype, based on PA-SERS

# Gather some input assumptions from PA-PSERS, as a rough example of a
# high outflow poorly funded prototype

# based on actives table in the AV at
# http://publicplansdata.org/reports/PA_PA-PSERS_AV_2013_92.pdf


fn <- "Prototype analysis(1).xlsx"
sheetname <- "PA-PSERS"

library(XLConnect) # slow but convenient because it reads ranges

# actives - numbered p.34 ####
# age x yos
# age 25 25-29 30-34 35-39 40-44 45-49 50-54 55-59 60-64 Over 64
# yos 0-4	 5-9	 10-14	15-19	20-24	25-29	30-34	35-39	40+
# midpoints of the age and yos ranges
age.mid <- c(25, seq(27, 62, 5), 66)
yos.mid <- c(seq(2, 37, 5), 42)

# convert to the ea x age format
df <- readWorksheetFromFile(paste0(draw, fn), sheet=sheetname, header=FALSE, region="A5:L24")
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
nactives.target <- 1000
actives <- left_join(wfl, avgpayl) %>%
  filter(nactives>0) %>%
  mutate(planname="underfunded",
         nactives=nactives / sum(nactives) * nactives.target) %>%
  select(planname, age, ea, nactives, salary)

glimpse(actives)
filter(actives, age<ea) # should be zero rows
sum(actives$nactives)


# Now make the list and save
underfunded <- list()
underfunded$actives <- actives

saveRDS(underfunded, paste0(draw, "underfunded.rds"))






apm <- as.matrix(avgpay[, -1])



wfm <- as.matrix(wf[, -1])
sum(wfm) # good

# adjust
awfm <- wfm



totpay <- wfm * apm
sum(totpay) / 1e9 # good, same as numbered p.39

sum(totpay) / sum(wfm) # good, ~matches p.34



# repeat for retirees p.35 ####
# age x yos
# age has different groupings than for actives, of course; yos groupings are the same
# age <50 50-54 55-59 60-64 65-69 70-74 75-79 80-84 85-89 Over 89
# yos 0-4	 5-9	 10-14	15-19	20-24	25-29	30-34	35-39	40+
age.v <- c(48, seq(52, 87, 5), 90)
yos.v <- c(seq(2, 37, 5), 42)

# convert to the ea x age format
df <- readWorksheetFromFile(fn, sheet=sheetname, header=FALSE, region="A30:L49")
names(df) <- c("order", "tabletype", "agegrp", yos.v)

ret <- df %>% filter(tabletype=="num") %>%
  arrange(order) %>%
  mutate(age=age.v) %>%
  select(-order, -tabletype, -agegrp) %>%
  gather(yos, nretirees, -age) %>%
  filter(nretirees>0) %>%
  mutate(ea=age - cton(as.character(yos))) %>%
  select(-yos) %>%
  spread(age, nretirees, fill=0)
rownames(ret) <- ret$ea
retm <- as.matrix(ret[, -1])
sum(retm) # good 189170

avgben <- df %>% filter(tabletype=="bens") %>%
  arrange(order) %>%
  mutate(age=age.v) %>%
  select(-order, -tabletype, -agegrp) %>%
  gather(yos, avgben, -age) %>%
  filter(avgben>0) %>%
  mutate(ea=age - cton(as.character(yos))) %>%
  select(-yos) %>%
  spread(age, avgben, fill=0)
rownames(avgben) <- avgben$ea
avgbenm <- as.matrix(avgben[, -1])

totben <- retm * avgbenm
sum(totben) / 1e6 # good, ~ matches p.33 (I think it's close enough)

sum(totben) / sum(retm) # good, matches p.35



