library(data.table)
library(tidyverse)
library(magrittr)
library(ggthemes)
library(here)
library(openxlsx)
library(lubridate)
library(Synth)
library(patchwork)
library(flextable)

#load in & set up----
dir.create(here("trunk","analysis","Synthetic Control"),showWarnings = F)
df<-fread(here("trunk","derived","Analytic Tables","Household Pulse Survey","hps_fs.txt"))

df[,`:=`(unit_var=cdlTools::fips(state,to="FIPS"),
         time_var=(interval(ym("2020-05"),ym(ym)))%/%months(1),
         date=lubridate::ymd(paste0(ym,"-01"))
         )]

#create synths ----
outcomes<-c("n_cases_k","n_deaths_k","likely_snap_food_insec","food_insec")

pred<-c(names(df)[grepl(names(df),pattern="income|hispanic|ms|black|asian|other|education|ag")],"likely_snap")

df<-df[time_var%in%0:6]
for (i in 1:4) {
  dp<-dataprep(foo=df,
               predictors=pred,
               dependent=outcomes[i],
               unit.variable="unit_var",
               time.variable = "time_var",
               treatment.identifier = 31,
               controls.identifier = df[unit_var!=31,.N,unit_var]$unit_var,
               time.predictors.prior = c(0:2),
               time.optimize.ssr = c(0:2),
               time.plot = c(0:6)
  )
  assign(paste0(outcomes[i],"_synth"),synth(dp))
    #create synth
  
  hold<-cbind(
    get(paste0(outcomes[i],"_synth"))$solution.w%>%as.data.table(),
    unit_var=as.integer(rownames(get(paste0(outcomes[i],"_synth"))$solution.w))
    )
    #export weights solution into dataframe for merging
  
  names(hold)[1]<-paste0(outcomes[i],"_weights")
  df%<>%merge(hold,by="unit_var",all.x=T)
    #rename and merge to original df
}

#make dfs ----
for (i in outcomes) {
  df[[paste0(i,"_w")]]<-df[[i]]*df[[paste0(i,"_weights")]]
}

synth<-df[,
   lapply(.SD, sum, na.rm=TRUE),
   .SDcols=paste0(outcomes,"_w"),.(date)][,flag:="Synthetic Nebraska"]

nebraska<-df[state=="Nebraska",
   lapply(.SD, sum, na.rm=TRUE),
   .SDcols=paste0(outcomes),.(date)][,flag:="Nebraska"]

names(synth)<-names(nebraska)

dfr<-rbind(synth,nebraska)
fwrite(dfr,here("trunk","analysis","Synthetic Control","synth_df.txt"))
#create figure----
p1<-dfr%>%
  pivot_longer(cols = outcomes,names_to = "Variable")%>%
  filter(year(date)%in%2020:2021)%>%
  ggplot()+
  geom_line(aes(x=date,y=value,col=flag))+
  facet_wrap(~Variable,scales = "free_y")+
  #plot the raw rates for outcomes
  geom_rect(xmin=lubridate::ymd("2020-08-01"),xmax=lubridate::ymd("2020-11-01"),ymin=-Inf,ymax=Inf,alpha=.05)+
  #shade first gap in coverage (Aug 2020 to Nov 2020)
  geom_rect(xmax=Inf,xmin=lubridate::ymd("2021-08-01"),ymin=-Inf,ymax=Inf,alpha=.05)
  #shade final cancellation (Aug 2021 onwards)

ggsave(p1,file=here("trunk","analysis","Synthetic Control","plot.png"),
       height=7.54,width=9.06,units=c("in"))
knitr::plot_crop(here("trunk","analysis","Synthetic Control","plot.png"))

gdata::keep(df,outcomes,pred,sure=T)
#create df for table----
dft<-df[time_var%in%0:2][,
                         lapply(.SD,mean,na.rm=T),
                         .SDcols=c(outcomes,pred),
                         by=c("state",paste0(outcomes,"_weights"))]

dft%<>%
  pivot_longer(cols=c(outcomes,pred),names_to = "Variable",values_to = "Value")%>%
  as.data.table()

dft<-
  dft[,
      lapply(.SD*Value,sum,na.rm=T),
      .SDcols=paste0(outcomes,"_weights"),
      .(Variable)]

dfn<-df[state=="Nebraska"&time_var%in%0:2][,lapply(.SD,mean),.SDcols=c(outcomes,pred)][,state:="Nebraska"]%>%
  pivot_longer(cols=c(outcomes,pred),names_to="Variable",values_to = "Nebraska")%>%
  select(Variable,Nebraska)

dfn%<>%
  merge(dft,by="Variable")%>%as.data.table()

r_c <- function(x) {
  y<-as.character(round(x,2))
  return(y)
}
dfn<-dfn[,lapply(.SD,r_c),.SDcols=names(dfn)[-1],.(Variable)]

for (i in 1:length(outcomes)) {
  for (j in outcomes[-i]) {
    dfn[Variable==outcomes[i]][[paste0(j,"_weights")]]<-"-"
  }
}
# Make table ----
dfn$Variable<-factor(dfn$Variable,levels=c(outcomes,pred))

dfn[order(Variable)]%>%
  flextable::flextable()

