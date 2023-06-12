library(here)
library(data.table)
library(tidyverse)
library(magrittr)
library(lubridate)

dfp<-fread(here("trunk","derived","Analytic Tables","Household Pulse Survey","pre-agg_hps.txt"))

#Vars----

covars <-
  c(
    "snap",
    "hispanic",
    "ms_d",
    "ms_m",
    "ms_nm",
    "ms_s",
    "ms_w",
    "ms_na",
    "asian",
    "black",
    "other",
    "white",
    "education_AA",
    "education_BA",
    "education_Grad",
    "education_HS",
    "education_LT_HS",
    "education_Some_College",
    "education_Some_HS",
    "income_",
    "income_200k",
    "income_mt200k",
    "income_35k",
    "income_50k",
    "income_100k",
    "income_150k",
    "income_75k",
    "income_25k",
    "ag_18to35",
    "ag_36to50",
    "ag_51to65",
    "ag_66to80",
    "ag_81to90",
    "gender_b_M",
    "hh_size_g_1_to_3",
    "hh_size_g_4_to_6",
    "hh_size_g_7_to_10",
    "gen_health_good"
  )

outcomes <-
  c(
    "food_insec"
  )

#Reweight----

df<-dfp[,lapply(.SD,weighted.mean,w=pweight,na.rm=T),
        .SDcols=c(outcomes,covars),
        by=.(survey_date_end,state)]

df[,ym:=format(survey_date_end, "%Y-%m")]

df<-df[,lapply(.SD,mean),
       .SDcols=c(outcomes,covars),
       .(ym,state)]
rm(dfp,covars,outcomes)
#COVID----
dfc<-fread(here("trunk","raw","COVID-19","covid_state.txt"))
dfc<-
  dfc[,.(
    state=usdata::abbr2state(state),
    mdy=mdy(submission_date),
    year=year(mdy(submission_date)),
    ym=format(lubridate::ymd(mdy(submission_date)), "%Y-%m"),
    new_case,
    new_death
  )]

dfc<-dfc[!is.na(state),.(
  new_case=sum(new_case),
  new_death=sum(new_death)
),.(state,ym,year)]
  #monthly cases and deaths

dfp<-fread(here("trunk","raw","PEP","pep.txt"))
df%<>%merge(dfc,by=c("state","ym"))
df%<>%merge(dfp,by=c("state","year"))
rm(dfp,dfc)
#Hospital Outcomes----
dfh<-fread(here("trunk","raw","COVID-19","hosp_imp.txt"))
dfh[,`:=`(date=lubridate::ymd(date),
          ym=format(lubridate::ymd(date), "%Y-%m"),
          state=cdlTools::fips(state,to="Name"))]

dfhh<-dfh[,
          .(ym,
            state,
            
            inpatient_beds,
            inpatient_beds_used,
            inpatient_beds_used_covid,
            
            inpatient_beds_utilization,
            inpatient_bed_covid_utilization,
            
            percent_of_inpatients_with_covid,
            deaths_covid)]

dfh2<-dfhh[,.(inpatient_beds=sum(inpatient_beds,na.rm=T),
              inpatient_beds_used=sum(inpatient_beds_used,na.rm=T),
              inpatient_beds_used_covid=sum(inpatient_beds_used_covid,na.rm=T),
        
              inpatient_beds_utilization=mean(inpatient_beds_utilization,na.rm=T),
              inpatient_bed_covid_utilization=mean(inpatient_bed_covid_utilization,na.rm=T),
              
              percent_of_inpatients_with_covid=mean(percent_of_inpatients_with_covid,na.rm=T),
              deaths_hosp=sum(deaths_covid,na.rm=T)),
           .(ym,state)]

df%<>%merge(dfh2,by=c("state","ym"))
#flag policy intervention----
dfe<-fread(here("trunk","raw","Emergency Allotments","EA_by_month.csv"))
dfe%<>%pivot_longer(!Date)
dfe%<>%as.data.table()
dfe[value==0]$value<-NA
dfe[,Date:=parse_date_time(Date,orders = c("m y"))]
dfe<-dfe[!is.na(value)]
  #toss out all NAs
dfe=dfe[,.(state=gsub(name,pattern = "\\.",replacement = " "),ym=format(Date, "%Y-%m"),EA=1)]

df%<>%merge(dfe,by=c("state","ym"),all.x=T)
df[is.na(EA)]$EA<-0
  #make all missing EAs 0s
#outcomes scaling----

df[,`:=`(
  food_insec=food_insec*100,
  
  cases_per_100k=(new_case/population)*10^5,
  death_per_100k=(new_death/population)*10^5,
  inpatient_beds_per_100k=(inpatient_beds/population)*10^5,
  inpatient_beds_used_per_100k=(inpatient_beds_used/population)*10^5,
  inpatient_beds_used_covid_per_100k=(inpatient_beds_used_covid/population)*10^5,
  inpatient_beds_used_covid_per_case=(inpatient_beds_used_covid/new_case)*100,
  
  inpatient_beds_utilization=inpatient_beds_utilization*100,
  inpatient_bed_covid_utilization=inpatient_bed_covid_utilization*100,
  percent_of_inpatients_with_covid=percent_of_inpatients_with_covid*100
  
)]
#save----
fwrite(df,here("trunk","derived","Analytic Tables","Household Pulse Survey","hps_fs.txt"))
rm(list=ls())
