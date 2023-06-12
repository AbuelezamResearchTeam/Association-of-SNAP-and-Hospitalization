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
library(stringi)
#set up----
load(here("trunk","analysis","Synthetic Control","tidysynths.RData"))
pval_cor <- function(p) {
  p=case_when(p<0.001~"P<0.001",
              p>=0.001&p<0.01~stri_pad_right(round(p,3), 3, 0),
              p<.99&p>=0.01~stri_pad_right(round(p,2), 2, 0),
              p>.99~"P>.99")
}
grab_sc <- function(x,f) {
  dfx<-grab_synthetic_control(eval(as.name(x)),placebo = f)
  dfx$outcome<-x
  print(dfx)
}

grab_bt <- function(x) {
  grab_balance_table(eval(as.name(x)))%>%
    mutate(Nebraska=round(Nebraska,2),
           synthetic_Nebraska=round(synthetic_Nebraska,2),
           donor_sample=round(donor_sample,2))%>%
    flextable()%>%
    add_header_lines(print(x))
}

grab_rt <- function(x,r) {
  e<-(eval(as.name(x))%>%
        grab_synthetic_control()%>%
        filter(time_unit%in%0:3)%>%
        summarise(y=mean(real_y-synth_y)))[[1]]%>%round(r)
  #get synthetic estimate
  
  p<-(eval(as.name(x))%>%
        grab_signficance()%>%
        filter(type=="Treated"))[[7]]%>%round(r)
  #get synth p-val
  
  pre_mean<-(eval(as.name(x))$.original_data[1]%>%
               as.data.table()%>%
               filter(ym(ym)%within%interval(ym("2020-05"),ym("2020-07")))%>%
               summarise(m=mean(eval(as.name(gsub(pattern="synth_",x=x,""))))%>%round(r),
                         sd=sd(eval(as.name(gsub(pattern="synth_",x=x,""))))%>%round(r)))
  
  pre_mean1<-paste0(pre_mean$m," (",pre_mean$sd,")")
  #get nebraska mean and sd of outcome in pre-intervention
  
  pre_mean_n<-(eval(as.name(x))$.original_data[2]%>%
                 as.data.table()%>%
                 filter(ym(ym)%within%interval(ym("2020-05"),ym("2020-07")))%>%
                 summarise(m=mean(eval(as.name(gsub(pattern="synth_",x=x,""))))%>%round(r),
                           sd=sd(eval(as.name(gsub(pattern="synth_",x=x,""))))%>%round(r)))
  pre_mean_n1<-paste0(pre_mean_n$m," (",pre_mean_n$sd,")")
  #get national average and sd of outcome in pre-intervention
  
  pre_mean_s<-(eval(as.name(x))$.original_data[2]%>%
                 as.data.table()%>%
                 merge(eval(as.name(x))%>%grab_unit_weights(),by.x="state",by.y="unit")%>%
                 filter(ym(ym)%within%interval(ym("2020-05"),ym("2020-07")))%>%
                 mutate(n=weight*eval(as.name(gsub(pattern="synth_",x=x,""))))%>%
                 group_by(ym)%>%
                 summarise(n=sum(n))%>%
                 summarise(m=mean(n)%>%round(r),
                           sd=sd(n)%>%round(r)))
  pre_mean_s1<-paste0(pre_mean_s$m," (",pre_mean_s$sd,")")
  #get synthetic values of outcome in pre-intervention
  
  out<-x
  post_mean<-(eval(as.name(x))$.original_data[1]%>%
                as.data.table()%>%
                filter(ym(ym)%within%interval(ym("2020-12"),ym("2021-03")))%>%
                summarise(m=mean(eval(as.name(gsub(pattern="synth_",x=x,""))))%>%round(r),
                          sd=sd(eval(as.name(gsub(pattern="synth_",x=x,""))))%>%round(r)))
  
  
  tibble(out,pre_mean_n1,pre_mean_s1,pre_mean1,s=paste0(e," (",p,")"),diff_mean=(((post_mean$m-pre_mean$m)/pre_mean$m)*100)%>%round(r))
}

obj_list<-ls()%>%str_subset(pattern="synth_")

# tab2: outcome vals ----
dfc<-lapply(obj_list, grab_rt,r=2)
dfc%<>%bind_rows()
dfc%<>%as.data.table()

dfc[,`:=`(p=as.numeric(gsub(pattern="\\).*","",gsub(pattern=".*\\(","",s))),
          e=gsub(pattern="\\(.*","",s))]
dfc1<-dfc[out%in%c("synth_food_insec","synth_inpatient_bed_covid_utilization","synth_inpatient_beds_utilization","synth_percent_of_inpatients_with_covid")]
dfc2<-dfc[!out%in%c("synth_food_insec","synth_inpatient_bed_covid_utilization","synth_inpatient_beds_utilization","synth_percent_of_inpatients_with_covid","synth_inpatient_beds_used_covid_per_case")]
dfc1[,adj_p:=p.adjust(p,method="BH")]
dfc2[,adj_p:=p.adjust(p,method="BH")]
dfc1[,`:=`(
  p=pval_cor(p),
  adj_p=pval_cor(adj_p))]
dfc2[,`:=`(
  p=pval_cor(p),
  adj_p=pval_cor(adj_p))]
dfc<-dfc1[,.(out,pre_mean_n1,pre_mean_s1,pre_mean1,s=paste0(e," (",p,"; ",adj_p,")"),diff_mean)]
save(dfc,file=here("trunk","analysis","Synthetic Control","main_outcome_tables.RData"))
rm(dfc)
dfc<-dfc2[,.(out,pre_mean_n1,pre_mean_s1,pre_mean1,s=paste0(e," (",p,"; ",adj_p,")"),diff_mean)]
save(dfc,file=here("trunk","analysis","Synthetic Control","appendix_outcome_tables.RData"))
rm(dfc)
#fig 1: counterfactuals----
dfc<-lapply(obj_list, grab_sc,f=F)
dfc%<>%bind_rows()
dfc%<>%as.data.table()

dfc%<>%
  pivot_longer(cols = c("real_y","synth_y"))%>%
  mutate(name=case_when(name=="real_y"~"Nebraska",T~"Synthetic Nebraska"),
         ym=case_when(time_unit==-3~ymd(20200501),
                      time_unit==-2~ymd(20200601),
                      time_unit==-1~ymd(20200701),
                      time_unit==0~ymd(20200801),
                      time_unit==1~ymd(20200901),
                      time_unit==2~ymd(20201001),
                      time_unit==3~ymd(20201101),
                      time_unit==4~ymd(20201201),
                      time_unit==5~ymd(20210101),
                      time_unit==6~ymd(20210201),
                      time_unit==7~ymd(20210301)),
         outcome_o=factor(outcome,levels=c("synth_food_insec",
                                           "synth_inpatient_beds_used_per_100k",
                                           "synth_inpatient_beds_used_covid_per_100k",
                                           "synth_inpatient_beds_used_covid_per_case",
                                           "synth_inpatient_beds_per_100k","synth_inpatient_bed_covid_utilization",
                                           "synth_inpatient_beds_utilization",
                                           "synth_percent_of_inpatients_with_covid")))

outcome_labs<-c(
  "Percent food insecure",
  "Percent of inpatient beds\nfilled by COVID-19 patients",
  "Number of inpatient beds\n per 100,000 people",
  "Number of inpatient beds\nfilled by COVID-19 patients\nper 100,000 people",
  "Number of inpatient beds\nfilled by COVID-19 patients\nper 100 cases",
  "Number of inpatient beds\nfilled per 100,000 people",
  "Percent of inpatient beds\nfilled",
  "Percent of inpatients with COVID-19"
)
names(outcome_labs)<-obj_list
  
p1<-dfc%>%
  filter(outcome%in%c("synth_food_insec","synth_inpatient_bed_covid_utilization","synth_inpatient_beds_utilization","synth_percent_of_inpatients_with_covid"))%>%
  ggplot()+
  geom_line(aes(x=ym,y=value,col=name,linetype=name))+
  facet_wrap(~outcome_o,
             scales="free_y",
             labeller = labeller(outcome_o=outcome_labs))+
  geom_rect(xmin=ymd(20200801),xmax=ymd(20201101),ymin=-Inf,ymax=Inf,alpha=.01)+
  scale_x_date(date_breaks = "2 month", date_labels =  "%b %Y")+
  theme_clean()+
  theme(legend.position ="bottom",
        plot.title.position = "plot",
        axis.text.x=element_text(angle=90),
        legend.background = element_rect(color=NA))+
  labs(x="",
       y="",
       title="Figure 1: Nebraska & Synthetic Nebraska trends in food insecurity, mental health, and\nhospital capacity indicators")+
  scale_colour_manual(values = c("#8a100b","#b29d6c"),
                      name="",
                      labels=c("Nebraska","Synthetic Nebraska"))+
  scale_linetype_manual(name="",
                        labels=c("Nebraska","Synthetic Nebraska"),
                        values=c(1,2))+
  guides(color=guide_legend(nrow=2,
                            byrow=T,
                            override.aes = list(fill=NA)))

p1

ggsave(p1,file=here("trunk","analysis","Synthetic Control","counterfactual_plot.png"),
       height=7.54,width=9.06,units=c("in"))
ggsave(p1,file=here("trunk","analysis","Synthetic Control","counterfactual_plot.pdf"),
       height=7.54,width=9.06,units=c("in"))
knitr::plot_crop(here("trunk","analysis","Synthetic Control","counterfactual_plot.png"))
rm(dfc,p1,outcome_labs)
#ref table: states contributions----

df_h<-synth_food_insec%>%
  grab_unit_weights()%>%
  select(unit)

for (x in obj_list) {
  dfx<-grab_unit_weights(eval(as.name(x)))
  dfx%<>%mutate(weight=paste0(round(weight*100,0),"%"))
  names(dfx)[2]<-x
  df_h%<>%merge(dfx,by="unit")
}
fwrite(df_h,here("trunk","analysis","Synthetic Control","states_table.txt"))
rm(df_h,i)
#fig 2: placebos ----
dfc<-lapply(obj_list, grab_sc,f=T)
dfc%<>%bind_rows()

dfc%<>%
  mutate(diff=real_y-synth_y)%>%
  select(-c(real_y,synth_y))%>%
  mutate(ym=case_when(time_unit==-3~ymd(20200501),
                      time_unit==-2~ymd(20200601),
                      time_unit==-1~ymd(20200701),
                      time_unit==0~ymd(20200801),
                      time_unit==1~ymd(20200901),
                      time_unit==2~ymd(20201001),
                      time_unit==3~ymd(20201101),
                      time_unit==4~ymd(20201201),
                      time_unit==5~ymd(20210101),
                      time_unit==6~ymd(20210201),
                      time_unit==7~ymd(20210301)))

p2<-dfc%>%
  ggplot()+
  geom_line(aes(x=ym,y=diff,group=.id))+
  facet_wrap(~outcome,scales="free_y")+
  gghighlight::gghighlight(.id=="Nebraska",
                           calculate_per_facet=T,
                           use_direct_label = T)+
  geom_vline(xintercept = ymd(20200701),linetype=3)+
  scale_x_date(date_breaks = "2 month", date_labels =  "%b %Y")+
  theme_clean()+
  theme(legend.position = "bottom",
        axis.text.x=element_text(angle=90))+
  labs(x="",
       y="",
       title="Fig X: Difference between Real and Synthetic outcomes for Nebraska & Placebos")

ggsave(p2,file=here("trunk","analysis","Synthetic Control","plot_placebos.png"),
       height=7.54,width=9.06,units=c("in"))
knitr::plot_crop(here("trunk","analysis","Synthetic Control","plot_placebos.png"))
rm(dfc,p2)

#fig 3: balance tables ----
dfc<-lapply(obj_list, grab_bt)
save(dfc,file=here("trunk","analysis","Synthetic Control","balance_tables.RData"))
rm(dfc) 