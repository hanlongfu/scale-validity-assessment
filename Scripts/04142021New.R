setwd("Y:/SAB-RESEARCH-UNIT/02Work/ResearchProjects/Validation/Sample Data")

library("data.table")
library("lubridate")
library("RcppRoll")
library("dplyr")
library("zoo")

# extract month and year from case grant date and sup closing date from case files 

cases$Month_grant <- month(as.POSIXlt(cases$case_grant_date, format="%m/%d/%Y"))
cases$Year_grant <-  year(as.POSIXlt(cases$case_grant_date, format="%m/%d/%Y"))
cases$Month_closing <- month(as.POSIXlt(cases$sup_closing_date, format="%m/%d/%Y"))
cases$Year_closing <-  year(as.POSIXlt(cases$sup_closing_date, format="%m/%d/%Y"))

# there are five cases where closing date happened before grant date

which(grepl(2009, cases$Year_closing)) # 35595
which(grepl(2010, cases$Year_closing)) # 18759 44830 46973 84594

## those cases are removed from the analytical data 
cases1 <- cases[cases$Year_closing >= 2011,]
cases1 <- na.omit(cases1, cols="xnmbr")

#################Rewrite Recode#################################
cases1$year_month <- paste(cases1$Year_grant, cases1$Month_grant, sep = "_")

cases1$countingmo <- cases1$year_month

#################Rewrite Recode#################################
# create a lookup vector
x <- rep(2011:2020, each = 12)
y <- rep(2021, each = 2)
xplusy <- c(x,y)
z <- rep(1:12,11,length.out=122)
xplusy_z <- paste(xplusy, z, sep="_")

# create a list
lookup_list <- 1:122
names(lookup_list) <- xplusy_z

#recode countingmo 
cases1$countingmo <- do.call(dplyr::recode, c(list(cases1$countingmo), lookup_list))

#################Rewrite Recode#################################

#there were 3509 unique cases (court case number) with the same case_grant_date
cases1[,.N, by=.(xnmbr,countingmo)][N > 1] 
cases1[, .N,by=.(xnmbr)]   # there were 143798 individuals 
cases1[, .N,by=.(crt_case_nmbr)] # there were 183546 unique court cases

# there were 6564 cases that have two or more cases with the same crt_case_nmbr but different xnmbrs
# this implies that one case could involve multiple individuals
cases1[, .N,by=crt_case_nmbr][N>1] -> crt_case_repeat 
table(crt_case_repeat$N)   # among the 6564 cases, 87% involve two people, 10% involve three people

# there were 109927 individuals (76.44% pf 143798) without recidivism
cases1[, .N,by=.(xnmbr)][N <= 1,] 
cases1[, .N,by=.(xnmbr)][N > 1,] 
# overall the data is unbalanced - unequal number of measurements per person
# and not time-structured - measurements do not occur at identical times
# multilevel models could accommodate such data structure (see Singer & Willett, 2003)

#there were 2119 unique combinations of xnmbr, case_grant_date, sup_closing_dates
# that have two or more cases
cases1[, .N, by=.(xnmbr, case_grant_date, sup_closing_date)][N > 1] 

#there were 2713 unique combinations of xnmbr, case_grant_date that have two or more cases
cases1[, .N, by=.(xnmbr, case_grant_date)][N > 1]


###########################################
## remove duplicate rows on the same day 
# 5680 cases where an individual have multiple 
# cases on the same case_grant_date
###########################################

# keep one case on the same day per individual by selecting the first case
temp1<- cases1 %>%  group_by(xnmbr, case_grant_date) %>% mutate(dupe = n()>1) %>% filter(dupe == TRUE) %>% filter(row_number() == 1)
temp2 <- cases1 %>% group_by(xnmbr, case_grant_date) %>% mutate(dupe = n()>1) %>% filter(dupe == FALSE) 

finalcases <- rbind(temp1, temp2)

finalcases$dupe <- NULL

#create a recidivism indicator variable
cases1[, recid := 1:.N - 1, by=xnmbr]

###########################################################
## join data
##########################################################

# filter out assessments done on the same day to the same individual
assmt_nona1 <- assmt_nona %>% group_by(xnmbr, assmnt_date) %>% mutate(dupe = n()>1) %>% filter(dupe == FALSE) %>% select(-dupe)
assmt_nona2 <- assmt_nona %>% group_by(xnmbr, assmnt_date) %>% mutate(dupe = n()>1) %>% filter(dupe == TRUE) %>% filter(row_number() == 1) %>% select(-dupe)
assmt_nona <- rbind(assmt_nona1, assmt_nona2)

# join the data
finalDT <- assmt_nona[finalcases, .(i.xnmbr, i.idx, i.recid, i.countingmo, x.assmnt_date, i.case_grant_date, i.sup_closing_date, i.def_stts_code, i.primary_chrg_code, i.primary_chrg_statute, i.primary_chrg_level, i.crt_case_nmbr, i.case_stts_code, i.race, i.cnt_grant_typ_ind, i.drug, i.sex_registration, i.arson, i.cjis_chrg_code,x.total_score, x.countingmo, x.criminal_history, x.assmt_id, x.education_employment, x.family_marital, x.leisure_recreation, x.companions, x.alcohol_drug, x.procriminal_attitude, x.antisocial_pattern), on = .(xnmbr = xnmbr, assmnt_date <= sup_closing_date, assmnt_date >= case_grant_date)][,.(xnmbr = i.xnmbr, assmnt_date = x.assmnt_date, case_grant_date = i.case_grant_date, recid = i.recid, idx = i.idx, countingmo_case = i.countingmo, countingmo_asst = x.countingmo, sup_closing_date = i.sup_closing_date,def_stts_code = i.def_stts_code, primary_chrg_code = i.primary_chrg_code, primary_chrg_statute = i.primary_chrg_statute,  primary_chrg_level = i.primary_chrg_level, crt_case_nmbr = i.crt_case_nmbr, case_stts_code = i.case_stts_code, race = i.race, cnt_grant_typ_ind = i.cnt_grant_typ_ind, drug = i.drug, sex_reg = i.sex_registration, arson = i.arson, cjis_chrg_code = i.cjis_chrg_code,total_score = x.total_score, assmt_id = x.assmt_id, criminal_hist = x.criminal_history, edu_emp = x.education_employment, fam_marital = x.family_marital, leisure_rec = x.leisure_recreation, companion =  x.companions, alcohol_drug = x.alcohol_drug, procriminal_att = x.procriminal_attitude, anti_social = x.antisocial_pattern )]


# one assessment matched to multiple cases within individuals
finalDT[, .N, by=.(xnmbr, assmnt_date)][N>1 & !is.na(assmnt_date)]

# select the first case per xnmbr and assmt_id combination
finalDT[, head(.SD, 1), by=.(xnmbr, assmt_id)] -> temp

###########################################################
## data analysis
##########################################################

#15.67% of the variation in recidivism was between individuals (intraclass correlation)
model0_fit <- lmer(formula = recid ~ 1 + (1|xnmbr), data=cases1,na.action=na.exclude)
summary(model0_fit)
RandomEffects <- as.data.frame(VarCorr(model0_fit))
ICC_between <- RandomEffects[1,4]/(RandomEffects[1,4]+RandomEffects[2,4]) 
ICC_between  

##########################################################
## because there's no significant difference between individuals in terms of 
## rate of change (growth rate). There's only significant difference between individuals
## in terms of initial status. 
##########################################################

# unconditional growth model (random intercept model)
# there's an issue of convergence
model1_fit <- lmer(formula = recid ~ countingmo + (countingmo |xnmbr), data=cases1,na.action=na.exclude, REML=FALSE)
summary(model1_fit)
RandomEffects1 <- as.data.frame(VarCorr(model1_fit))
ICC_between1 <- RandomEffects1[1,4]/(RandomEffects1[1,4]+RandomEffects1[2,4]) 
ICC_between1 

# alternative: nlme package to fit the model
model1.alt <- lme(recid~countingmo, cases1, random= ~countingmo | xnmbr, method="ML")
summary(model1.alt)

##################################################################

model2_fit <- lmer(formula = recid ~ 1 + countingmo + (1|xnmbr), data=cases1,na.action=na.exclude)
summary(model2_fit)
RandomEffects2 <- as.data.frame(VarCorr(model2_fit))
ICC_between <- RandomEffects2[1,4]/(RandomEffects2[1,4]+RandomEffects2[2,4]) 
ICC_between  

# individual growth curves (takes time)
gg2 <- ggplot(cases1,aes(x = countingmo, y = recid, group = xnmbr)) +  
  geom_point() + stat_smooth(method = "lm", se = FALSE)   
gg3 <- gg2 +stat_smooth(data = cases1, aes(x = countingmo, y = recid, group = 1, colour="#990000"), method = "lm", size = 3, se=FALSE)
print(gg3)

# recode race variable
cases1<- cases1 %>% mutate(race1 = recode(race, B = 0, H = 1, W = 1, .default = 1))

# race predicting varying intercepts
model3_fit <- lmer(formula = recid ~ 1 + countingmo + race1 + (1|xnmbr), data=cases1,na.action=na.exclude)
summary(model3_fit)
RandomEffects3 <- as.data.frame(VarCorr(model3_fit))
ICC_between <- RandomEffects3[1,4]/(RandomEffects3[1,4]+RandomEffects3[2,4]) 
ICC_between  

##### analyze the relationship between ls/cmi total score and recid
modelx_fit <- lmer(formula = recid ~ 1 + countingmo_case + as.numeric(total_score) + (1|xnmbr), data=finalDT,na.action=na.exclude)
summary(modelx_fit)
RandomEffects <- as.data.frame(VarCorr(modelx_fit))
ICC_between <- RandomEffects[1,4]/(RandomEffects[1,4]+RandomEffects[2,4]) 
ICC_between  

# nlme approach
modelx_fit <- lme(recid ~ 1 + countingmo_case + as.numeric(total_score), random = ~1|xnmbr, data=finalDT, na.action=na.omit)
summary(modelx_fit)
anova(modelx_fit)


###########################################
## A whole new approach inspired by
## BOS study prepared by Irene Vidyanti, Ph.D
###########################################

# Split sentencing individual by year
cases1[Year_grant == 2015 & cnt_grant_typ_ind == 'MANDATORY SUPERVISION',.N, by=xnmbr]







###########################################
## expand grid
###########################################

# all_combinations <- expand.grid(xplusy_z, xnmbr=unique(df4$xnmbr))
# 
# # Sort both datasets to prepare for joining
# df4 <- df4[order(df4$xnmbr, df4$countingmo),]
# all_combinations <- all_combinations[order(all_combinations$xnmbr, all_combinations$countingmo),]
# 
# # convert data types 
# all_combinations$countingmo <- as.integer(all_combinations$countingmo)
# all_combinations$xnmbr <- as.character(all_combinations$xnmbr)
# 
# # join datasets
# df5 <- df4[all_combinations, on=.(countingmo, xnmbr)]
# 
# #remove assessment done outside 01/02/2011 - 02/02/2021 range
# assmt_nona <- assmt[!is.na(assmt$countingmo),]
# 
# # join assessment with expand grid
# setDT(df5)
# setDT(assmt_nona)
# assmt_panel <- assmt_nona[df5, on=.(xnmbr,countingmo)]


# ###########################################
# ## add countingmo to assessment data
# ###########################################
# 
# # extra year and month from assessment date
# assmt$month_assmnt <- month(as.POSIXlt(assmt$assmnt_date, format="%m/%d/%Y"))
# assmt$year_assmnt <-  year(as.POSIXlt(assmt$assmnt_date, format="%m/%d/%Y"))
# 
# #concatenate year and month
# assmt$year_month <- paste(assmt$year_assmnt, assmt$month_assmnt, sep="_")
# 
# #recode year_month according to lookup list
# assmt$countingmo <- assmt$year_month
# assmt$countingmo <- do.call(dplyr::recode, c(list(assmt$countingmo), lookup_list))
# 
# ###########################################
# ## add countingmo to case file
# ###########################################
# # extra year and month from assessment date
# cases_uniq$month_grant <- month(as.POSIXlt(cases_uniq$case_grant_date, format="%m/%d/%Y"))
# cases_uniq$year_grant <-  year(as.POSIXlt(cases_uniq$case_grant_date, format="%m/%d/%Y"))
# 
# #concatenate year and month
# cases_uniq$year_month <- paste(cases_uniq$year_grant, cases_uniq$month_grant, sep="_")
# 
# #recode year_month according to lookup list
# cases_uniq$countingmo <- cases_uniq$year_month
# cases_uniq$countingmo <- do.call(dplyr::recode, c(list(cases_uniq$countingmo), lookup_list))
# 
# 
# ###########################################
# ## merge cases_uniq with assmt_panel
# ###########################################
# 
# finalDT <- cases_uniq[assmt_panel, on =.(xnmbr, countingmo)]
# finalDT <- unique(finalDT)
# 
# ###### Experiment on temporary data ######################
# # examine the data
# finalDT_temp <- head(finalDT, 1220)
# 
# # fill the NAs in some cols
# finalDT_temp <- finalDT_temp %>% group_by(xnmbr) %>% fill(race, drug, sex_registration, arson, .direction = "downup")
# 
# # calculate difference in months between case grant and case closing dates
# setDT(finalDT_temp)[, interval := (interval(case_grant_date, sup_closing_date) %/% months(1))]








