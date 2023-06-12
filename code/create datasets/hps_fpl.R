library(here)
library(data.table)
library(tidyverse)
library(magrittr)
library(lubridate)

###load in data & subset cols----
dfp<-fread(here("trunk","derived","Gold Files","Household Pulse Survey Data Tables","hps_final.txt"))
dir.create(here("trunk","derived","Analytic Tables"),showWarnings = F)
dir.create(here("trunk","derived","Analytic Tables","Household Pulse Survey"),showWarnings = F)
###weight and merge----
####Wrangle Cols----
dfp <-
  fastDummies::dummy_cols(dfp,
                          select_columns = c("marital_status",
                                             "race",
                                             "education",
                                             "income",
                                             "age_g",
                                             "hh_size_g"))
  #create dummies

dfp %<>%
  rename(
    ms_d = marital_status_Divorced,
    ms_m = marital_status_Married,
    ms_nm = `marital_status_Never Married`,
    ms_s = marital_status_Separated,
    ms_w = marital_status_Widowed,
    ms_na = marital_status_,
    asian = race_Asian,
    black = race_Black,
    other = race_Other,
    white = race_White,
    education_HS = `education_HS or eq`,
    education_LT_HS = `education_Less than HS`,
    education_Some_College = `education_Some college`,
    education_Some_HS = `education_Some HS`,
    income_25k = `income_Less than $25,000`,
    income_35k = `income_$25,000 - $34,999`,
    income_50k = `income_$35,000 - $49,999`,
    income_75k = `income_50,000 - $74,999`,
    income_100k = `income_$75,000 - $99,999`,
    income_150k = `income_$100,000 - $149,999`,
    income_200k = `income_$150,000 - $199,999`,
    income_mt200k = `income_≥$200,000`,
    ag_18to35 = `age_g_18 to 35`,
    ag_36to50 = `age_g_36 to 50`,
    ag_51to65 = `age_g_51 to 65`,
    ag_66to80 = `age_g_66 to 80`,
    ag_81to90 = `age_g_81 to 90`
  )
#rename dummies

dfp[,gender_b_M := case_when(gender_b == "M" ~ 1, T ~ 0)]
#gender binary

fwrite(dfp,here("trunk","derived","Analytic Tables","Household Pulse Survey","pre-agg_hps.txt"))

rm(list=ls())
