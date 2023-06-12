library(data.table)
library(tidyverse)
library(magrittr)
library(ggthemes)
library(here)
library(openxlsx)
library(lubridate)
library(tidysynth)
library(patchwork)
library(flextable)

#load in & set up----
dir.create(here("trunk","analysis","Synthetic Control"),showWarnings = F)
df<-fread(here("trunk","derived","Analytic Tables","Household Pulse Survey","hps_fs.txt"))

df[,`:=`(time_var=(interval(ym("2020-08"),ym(ym)))%/%months(1),
         date=lubridate::ymd(paste0(ym,"-01")))]

df<-df[time_var<8]
outcomes<-c("food_insec",
            
            "inpatient_beds_per_100k",
            "inpatient_beds_used_per_100k",
            "inpatient_beds_used_covid_per_100k",
            "inpatient_beds_used_covid_per_case",
            
            "inpatient_beds_utilization",
            "inpatient_bed_covid_utilization",
            "percent_of_inpatients_with_covid")
#synth ----
for (i in outcomes) {
  
  synth<-df%>%
    synthetic_control(outcome = !!sym(i),
                      unit = state,
                      time = time_var, 
                      i_unit = "Nebraska",
                      i_time = 0,
                      generate_placebos=T)%>%
    generate_predictor(time_window = -3:-1,
                       white_m=mean(white),
                       asian_m=mean(asian),
                       black_m=mean(black),
                       other_m=mean(other),
                       hispanic_m=mean(hispanic),
                       male_m=mean(gender_b_M),
                       
                       ag18to35=mean(ag_18to35),
                       ag36to50=mean(ag_36to50),
                       ag51to65=mean(ag_51to65),
                       ag66to80=mean(ag_66to80),
                       
                       education_LT_HS_m=mean(education_LT_HS),
                       education_Some_HS_m=mean(education_Some_HS),
                       education_HS_m=mean(education_HS),
                       education_Some_College_m=mean(education_Some_College),
                       education_BA_m=mean(education_BA),
                       education_Grad_m=mean(education_Grad),
                       
                       income_m=mean(income_),
                       income_25k_m=mean(income_25k),
                       income_35k_m=mean(income_35k),
                       income_50k_m=mean(income_50k),
                       income_75k_m=mean(income_75k),
                       income_100k_m=mean(income_100k),
                       income_150k_m=mean(income_150k),
                       income_200k_m=mean(income_200k),
                       income_mt200k_m=mean(income_mt200k),
                       
                       hh_size_g_1_to_3=mean(hh_size_g_1_to_3),
                       hh_size_g_4_to_6=mean(hh_size_g_4_to_6),
                       hh_size_g_7_to_10=mean(hh_size_g_7_to_10))%>%
    generate_predictor(time_window = -3:-2,
                       gen_health_good=mean(gen_health_good),
                       outcome_m=mean(!!sym(i)))%>%
    generate_weights(optimization_window = -3:-2,
                     optimization_method="All",
                     margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6)%>%
    generate_control()
  
  assign(paste0("synth_",i),synth)
}
#save----
rm(df,outcomes,i)
save(list=ls(),file=here("trunk","analysis","Synthetic Control","tidysynths_reestimate.RData"))
#create table----
obj_list<-ls()%>%str_subset(pattern="synth_")

grab_new_table <- function(x,r) {
  may<-(eval(as.name(x))%>%
        grab_synthetic_control()%>%
        filter(time_unit==-3)%>%
        summarise(y=mean(real_y-synth_y)))[[1]]%>%round(r)
  
  june<-(eval(as.name(x))%>%
          grab_synthetic_control()%>%
          filter(time_unit==-2)%>%
          summarise(y=mean(real_y-synth_y)))[[1]]%>%round(r)
  
  july<-(eval(as.name(x))%>%
          grab_synthetic_control()%>%
          filter(time_unit==-1)%>%
          summarise(y=mean(real_y-synth_y)))[[1]]%>%round(r)
  
  p<-(eval(as.name(x))%>%
        grab_signficance()%>%
        filter(type=="Treated"))[[7]]%>%round(r)
  
  out<-x
  
  tibble(out,may,june,july,p)
}

dfc<-lapply(obj_list, grab_new_table,r=2)
dfc%<>%bind_rows()
dfc%<>%as.data.table()

dfc%>%
  mutate(out=case_when(out=="synth_food_insec"~"Percent food insecure",
                       out=="synth_inpatient_beds_used_per_100k"~"Number of inpatient beds\nfilled per 100,000 people",
                       out=="synth_inpatient_beds_used_covid_per_100k"~"Number of inpatient beds\nfilled by COVID-19 patients\nper 100,000 people",
                       out=="synth_inpatient_beds_used_covid_per_case"~"Number of inpatient beds\nfilled by COVID-19 patients\nper 100 cases",
                       out=="synth_inpatient_beds_per_100k"~"Number of inpatient beds\n per 100,000 people",
                       out=="synth_inpatient_beds_utilization"~"Percent of inpatient beds\nfilled",
                       out=="synth_inpatient_bed_covid_utilization"~"Percent of inpatient beds\nfilled by COVID-19 patients",
                       out=="synth_percent_of_inpatients_with_covid"~"Percent of inpatients with COVID-19"
  ))%>%
  select(Outcome=out,May=may,June=june,July=july,`P-Value`=p)
