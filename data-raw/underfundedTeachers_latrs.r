
# Construct an underfunded teachers prototype, based on LA-TRS
# http://publicplansdata.org/reports/LA_LA-TRSL_AV_2013_45.pdf

# This is designed to represent group 4 in recent clustering - 32 plans with declining actives, moderately rising retirees
# low actives ratio, substantial 5% outflows, poor funding - summary of groups below
# group	n	activepch	retireepch	xcfpct2	abratio	ActiveAge_avg	MktAssets_net	ActFundedRatio_GASB	PercentReqContPaid
# 1	84	-0.424	4.126	-2.098	1.939	45.975	10.142	0.738	1.000
# 4	32	-1.554	2.473	-5.038	1.533	45.850	10.200	0.595	0.999
# 3	19	-0.572	2.926	-3.083	2.009	40.500	7.396	0.797	0.970
# 5	7	-0.749	7.108	-0.188	4.473	42.500	4.168	0.733	1.000
# 2	3	-6.818	3.345	-4.844	0.765	49.220	6.694	0.545	0.902
# 8	3	-0.013	19.898	2.071	8.340	NA	7.637	1.049	0.990
# 6	1	-0.921	2.160	-15.634	1.454	43.500	3.275	0.258	0.608
# 7	1	-20.000	-3.339	-12.706	0.033	60.700	0.869	0.744	1.658



baseplan <- "LA-TRS"
protoname <- "underfundedTeachers"

totactives <- 1000
abratio <- 1.6
totretirees <- totactives / abratio


#****************************************************************************************************
#                    Actives ####
#****************************************************************************************************
# there are some important differences in formatting between the LA data and other data - totals rather than averages
# for salary, blank rows, etc.

# actives - numbered ####
# convert to the ea x age format
range <- "A11:N58"  # include column headers, but NOT row or column totals
(df <- readWorksheetFromFile(paste0(draw, protofn), sheet=paste0(baseplan, ".Actives"), header=TRUE, region=range, colTypes="character"))

df2 <- df %>% filter(!is.na(as.numeric(order))) %>% # note extra step here
  select(-agegrp1, -agegrp2) %>% # get rid of unneeded stubs
  gather(yos, value, -order, -type, -midage) %>%
  mutate(age=as.integer(midage),
         yos=as.integer(gsub("[^0-9]", "", yos)),
         ea=as.integer(age - yos),
         value=cton(value)) %>%
  select(age, ea, value, type) %>%
  spread(type, value) %>%
  filter(nactives>0) %>%
  mutate(salary=totpay / nactives) %>% # special step for LA-TRS
  select(-totpay)
# check total n actives and avg salary here to be sure we hit published numbers, before adjusting and scaling
sum(df2$nactives)
sum(df2$nactives * df2$salary) / sum(df2$nactives) # grand average salary
# good. proceed.

# Now, adjust ages and entry ages as needed so that each fits in the allowable range
df3 <- actives_ageea_adj(df2, active_ages$min, active_ages$max)

# check nactives and average salary
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
sum(actives$nactives * actives$age) / sum(actives$nactives) # grand average age - about 0.6 years younger than the true plan
actives %>% mutate(yos=age-ea) %>% summarize(avgyos=sum(yos*nactives) / sum(nactives)) # about 0.5 years less yos than the true plan

actives %>% select(age, ea, nactives) %>% spread(ea, nactives) %>% kable(digits=2)
actives %>% select(age, ea, salary) %>% spread(ea, salary) %>% kable(digits=2)


#****************************************************************************************************
#                    Retirees ####
#****************************************************************************************************

# repeat for retirees ####
# NOTE THAT WE ONLY NEED THE TOTALS COLUMN
range <- "A5:O51"  # include column headers; for retirees, include column totals but not row totals
(df <- readWorksheetFromFile(paste0(draw, protofn), sheet=paste0(baseplan, ".Retirees"), header=TRUE, region=range, colTypes="character"))

df2 <- df %>%  filter(!is.na(as.numeric(order))) %>% # note extra step here to get rid of blank rows
  select(type, age=midage, value=total) %>%
  mutate(age=as.integer(age),
         value=cton(value)) %>%
  spread(type, value) %>%
  filter(nretirees>0) %>%
  mutate(benefit=totbenefit / nretirees) %>% # special step for LA-TRS
  select(-totbenefit)
# check total nretirees and avg benefit here before scaling and multiplying by 12
sum(df2$nretirees)
sum(df2$nretirees * df2$benefit) / sum(df2$nretirees) # grand average benefit
# good. proceed.

# Now, adjust ages and entry ages as needed so that each fits in the allowable range
df3 <- retirees_age_adj(df2, retiree_ages$min, retiree_ages$max)

# check nretirees and average benefit
sum(df2$nretirees); sum(df3$nretirees)
sum(df2$nretirees * df2$benefit) / sum(df2$nretirees); sum(df3$nretirees * df3$benefit) / sum(df3$nretirees)

# good - now finish up
retirees <- df3 %>% mutate(planname=protoname, # do NOT need to convert monthly to annual - data already are annual
                           nretirees=nretirees / sum(nretirees) * totretirees) %>% # totactives is a parameter above
  select(planname, age, nretirees, benefit) %>%
  arrange(age)

glimpse(retirees)
sum(retirees$nretirees)
sum(retirees$nretirees * retirees$benefit) / sum(retirees$nretirees) # grand average benefit
sum(retirees$nretirees * retirees$age) / sum(retirees$nretirees) # grand average age - about 1.2 years older than real plan


#****************************************************************************************************
#                    Historical salary growth ####
#****************************************************************************************************
# note that this plan has growth by yos so we must convert to age
range <- "A6:B63"  # include column headers
(df <- readWorksheetFromFile(paste0(draw, protofn), sheet=paste0(baseplan, ".SalGrowHist"), header=TRUE, region=range, colTypes="character"))
glimpse(df)

# These data are on a yos basis. Conver to age basis (20:70) assuming entrance at 20
df2 <- df %>% mutate(age=as.integer(cton(yos) + 20),
                     rate=cton(rate)) %>%
  select(age, rate) %>%
  filter(age %in% 20:70)
# note that we don't need to fill in missing ages with this plan

salgrowth.hist <- df2 %>% mutate(planname=protoname) %>%
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



