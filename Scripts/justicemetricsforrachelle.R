library(data.table)
library(lubridate)
library(zoo)

# load assessment and case data files (skipped)


# merge when assessment date falls between case grant and sup closing dates
# above logic in data.table syntax:  on = .(xnmbr = xnmbr, assmnt_date <= sup_closing_date, assmnt_date >= case_grant_date)
setDT(assmt)
setDT(cases)
lj <- assmt[cases, .(i.xnmbr, x.assmnt_date, i.case_grant_date, i.sup_closing_date, i.def_stts_code, i.primary_chrg_code, i.primary_chrg_statute, 
                     i.primary_chrg_level, i.crt_case_nmbr, i.case_stts_code, i.race, i.cnt_grant_typ_ind, i.drug, i.sex_registration, i.arson, i.cjis_chrg_code,
                     x.total_score, x.criminal_history, x.education_employment, x.family_marital, x.leisure_recreation, x.companions, x.alcohol_drug, x.procriminal_attitude, x.antisocial_pattern), on = .(xnmbr = xnmbr, assmnt_date <= sup_closing_date, assmnt_date >= case_grant_date)][,.(xnmbr = i.xnmbr, assmnt_date = x.assmnt_date, case_grant_date = i.case_grant_date, sup_closing_date = i.sup_closing_date,def_stts_code = i.def_stts_code, primary_chrg_code = i.primary_chrg_code, primary_chrg_statute = i.primary_chrg_statute,  primary_chrg_level = i.primary_chrg_level, crt_case_nmbr = i.crt_case_nmbr, case_stts_code = i.case_stts_code, race = i.race, cnt_grant_typ_ind = i.cnt_grant_typ_ind, drug = i.drug, sex_reg = i.sex_registration, arson = i.arson, cjis_chrg_code = i.cjis_chrg_code,total_score = x.total_score, criminal_hist = x.criminal_history, edu_emp = x.education_employment, fam_marital = x.family_marital, leisure_rec = x.leisure_recreation, companion =  x.companions, alcohol_drug = x.alcohol_drug, procriminal_att = x.procriminal_attitude, anti_social = x.antisocial_pattern )]


# remove redundant columns
lj_unique <- unique(lj)

# calculate the time lapse since previous recidivism and index recidivism within individuals
lj_unique$case_grant_date <- as.Date(lj_unique$case_grant_date, '%Y-%m-%d')
lj_unique1 <- lj_unique[order(case_grant_date), months_passed := (as.yearmon(case_grant_date) - as.yearmon(lag(case_grant_date,1)))*12, by=xnmbr][, reci_nmbr := 1:.N, by=xnmbr]

# create dummy variable for recidivism within 6 months, 1 year, 18 months, 2 years, 3 years, and 3 years above
lj_unique[, sixmo := ifelse(!is.na(months_passed) & months_passed <=6, 1, 0), by=xnmbr]
lj_unique[, oneyear := ifelse(!is.na(months_passed) & months_passed <=12 & months_passed > 6, 1, 0), by=xnmbr]
lj_unique[, eighteenmo := ifelse(!is.na(months_passed) & months_passed <=18 & months_passed > 12, 1, 0), by=xnmbr]
lj_unique[, twoyears := ifelse(!is.na(months_passed) & months_passed <=24 & months_passed > 18, 1, 0), by=xnmbr]
lj_unique[, threeyears := ifelse(!is.na(months_passed) & months_passed <=36 & months_passed > 24, 1, 0), by=xnmbr]
lj_unique[, gethree := ifelse(!is.na(months_passed) & months_passed > 36, 1, 0), by=xnmbr]

# export data as csv
fwrite(lj_unique, "lj_unique.csv")