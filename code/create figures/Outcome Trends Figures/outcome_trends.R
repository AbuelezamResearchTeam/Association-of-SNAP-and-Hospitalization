library(data.table)
library(tidyverse)
library(magrittr)
library(ggthemes)
library(here)
library(openxlsx)
library(lubridate)
library(patchwork)

#load in & set up----
dir.create(here("trunk","analysis","Outcome Trends"),showWarnings = F)
df<-fread(here("trunk","derived","Analytic Tables","Household Pulse Survey","hps_fs.txt"))
#Nebraska trends----

v<-c("food_insec","likely_snap_food_insec","n_cases_k","n_deaths_k")
df1<-
  df[state=="Nebraska"&substr(ym,1,4)%in%c("2020","2021")]%>%
  pivot_longer(cols=all_of(v),names_to="var",values_to="val")
df1$var<-factor(df1$var,levels = v)
  #make Nebraska only & subset to 2020/2021

p1<-df1%>%
  mutate(date=lubridate::ymd(paste0(ym,"-01")))%>%
  ggplot()+
    geom_line(aes(x=date,y=val,group=var))+
    facet_wrap(~var,scales="free",labeller = as_labeller(c(food_insec="Percent Food Insecure",
                                                           likely_snap_food_insec="Percent SNAP Eligible Food Insecure",
                                                           covid_vax_perc="Percent Vaccinated",
                                                           n_cases_k="Monthly Case Rate (Per 100k)",
                                                           n_deaths_k="Monthly Mortality Rate (Per 100k)")))+
      #plot the raw rates for outcomes
    geom_rect(xmin=lubridate::ymd("2020-08-01"),xmax=lubridate::ymd("2020-11-01"),ymin=-Inf,ymax=Inf,alpha=.05)+
      #shade first gap in coverage (Aug 2020 to Nov 2020)
    geom_rect(xmax=Inf,xmin=lubridate::ymd("2021-08-01"),ymin=-Inf,ymax=Inf,alpha=.05)+
      #shade final cancellation (Aug 2021 onwards)
    labs(x="Year-Month",
         y="",
         title="Figure X: Nebraska (2020-2022)",
         caption="Shaded regions represent time when Nebraska opted out of emergency allotment support.")+
    theme_clean()+
    theme(legend.position = "none",
          axis.text.x = element_text(angle=45,hjust=1),
          plot.caption=element_text(hjust=0),
          plot.title.position = "plot",
          plot.caption.position = "plot",
          plot.background = element_rect(color = "white"))+
  scale_x_date(date_breaks = "2 month", date_labels =  "%b %Y")
p1

ggsave(p1,file=here("trunk","analysis","Outcome Trends","Nebraska_trends.png"),
       height=7.54,width=9.06,units=c("in"))
knitr::plot_crop(here("trunk","analysis","Outcome Trends","Nebraska_trends.png"))

# National Trends----
df2<-
  df[substr(ym,1,4)%in%c("2020","2021")]%>%
  pivot_longer(cols=all_of(v),names_to="var",values_to="val")
df2$var<-factor(df2$var,levels = v)
#subset to 2020/2021

p2<-df2%>%
  mutate(date=lubridate::ymd(paste0(ym,"-01")),
         isNeb=case_when(state=="Nebraska"~"Nebraska",T~"Not Nebraska"))%>%
  ggplot()+
  geom_line(aes(x=date,y=val,group=state,col=isNeb))+
  facet_wrap(~var,scales="free_y",labeller = as_labeller(c(food_insec="Percent Food Insecure",
                                                         likely_snap_food_insec="Percent SNAP Eligible Food Insecure",
                                                         covid_vax_perc="Percent Vaccinated",
                                                         n_cases_k="Monthly Case Rate (Per 100k)",
                                                         n_deaths_k="Monthly Mortality Rate (Per 100k)")))+
  gghighlight::gghighlight(state=="Nebraska",calculate_per_facet=T)+
  #plot the raw rates for outcomes
  geom_rect(xmin=lubridate::ymd("2020-08-01"),xmax=lubridate::ymd("2020-11-01"),ymin=-Inf,ymax=Inf,alpha=.05)+
  #shade first gap in coverage (Aug 2020 to Nov 2020)
  geom_rect(xmax=Inf,xmin=lubridate::ymd("2021-08-01"),ymin=-Inf,ymax=Inf,alpha=.05)+
  #shade final cancellation (Aug 2021 onwards)
  labs(x="Year-Month",
       y="",
       title="Figure X: Nebraska (2020-2022)",
       caption="Shaded regions represent time when Nebraska opted out of emergency allotment support.")+
  theme_clean()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle=45,hjust=1),
        plot.caption=element_text(hjust=0),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.background = element_rect(color = "white"))+
  scale_x_date(date_breaks = "2 month", date_labels =  "%b %Y")
p2
fwrite(df2,here("trunk","analysis","Outcome Trends","national_trends.txt"))
ggsave(p2,file=here("trunk","analysis","Outcome Trends","National_trends.png"),
       height=7.54,width=9.06,units=c("in"))
knitr::plot_crop(here("trunk","analysis","Outcome Trends","National_trends.png"))

rm(list=ls())
