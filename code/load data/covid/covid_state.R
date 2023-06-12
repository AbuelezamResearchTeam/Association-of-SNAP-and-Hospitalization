library(here)
library(data.table)
library(tidyverse)

df<-fread("https://data.cdc.gov/api/views/9mfq-cb36/rows.csv?accessType=DOWNLOAD")
  
dir.create(here("trunk","raw","COVID-19"),showWarnings = F)
fwrite(df,here("trunk","raw","COVID-19","covid_state.txt"))
#source: United States COVID-19 Cases and Deaths by State over Time - ARCHIVED
#https://data.cdc.gov/Case-Surveillance/United-States-COVID-19-Cases-and-Deaths-by-State-o/9mfq-cb36

rm(list=ls())
