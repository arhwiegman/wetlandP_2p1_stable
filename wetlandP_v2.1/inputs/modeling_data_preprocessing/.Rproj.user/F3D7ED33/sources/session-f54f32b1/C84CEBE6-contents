library(tidyverse)
#inputs, 
# df = a two column data frame, containing:
#   t = time of observation from 1 to n, hours, days, requires equal spaced increments
#   Zw = elevation or height of water in wetland
# thresh = an elevation threshold above which flooding occurs
fn_hydro_freq_duration_depth <- function(df=NULL,thresh=NULL){
  require(dplyr)
  require(rstatix)
  if (is.null(df)){
    thresh=0
    df <- data.frame(t=1:100,
                     Zw = fn_negcosine(x=1:100,prd=33)+
                       rnorm(100,0,0.5)*fn_negcosine(x=1:100,prd=33))
    df %>% plot %>% lines
  }else{
    if(is.null(thresh)) stop ("`thresh` is null, threshold elevation value needed")
  }
  df <- df %>% mutate(
    dx=append(diff(Zw),0,0), # difference of Zw from predeeding time, append a column to the first row. 
    signdx=sign(dx), # sign of dx
    dsigndx=append(diff(signdx),0,0), # difference in sign of dx
    locmax=(dsigndx==-2), # local maximum is when sign of dx changes from 1 to -1
    flood = Zw>thresh, # binary flood when Zw is greater than the threshold
    pulse_start = as.integer(append(diff(flood),0,0)==1), # a pulse starts when difference in flood is above zero
    pulse_count=cumsum(pulse_start)) %>% # the cumulative sum of pulses
    filter(pulse_count>0)
  df.pulse.summary <- df %>% 
    group_by(pulse_count) %>% 
    summarize(hydroperiod = sum(flood), # duration of flooding per event
              max = max(Zw)) %>% # max depth per event
    ungroup
  
  df.stats <- df.pulse.summary %>% select(-pulse_count) %>% get_summary_stats()
  df.sum <- df.pulse.summary %>% summarise(depth_sum=sum(max),
                                           hydroperiod_sum=sum(hydroperiod))
  cat(" observation length (t):",nrow(df),"\n",
      "n events (n):",nrow(df.pulse.summary),"\n",
      "freqency (n/yr):",nrow(df.pulse.summary)/(nrow(df)/365),"\n")
  return(list(event_averages=df.stats,totals=df.sum))
}
require(lubridate)
df <- df.hobo.wetland_ma %>%
  select(site,datetime,Zw=WL_r2_ma) %>%
  mutate(date=date(datetime)) %>% 
  group_by(date,site) %>%
  summarize(Zw=max(Zw)) %>%
  mutate(wateryear=case_when(
    `date`>"2018-09-30"&`date`<"2019-09-30"~"18/19",
    `date`>"2019-09-30"&`date`<"2020-09-30"~"19/20",
    `date`>"2020-09-30"&`date`<"2021-09-30"~"20/21",
    T~"NA")) %>%
  na.omit

write_lines(paste("summary by site and water year\n\n"),"hydrosummary.txt",append = F)
for(s in c("LC","OCD","OCSP")){
  for (y in c("18/19","19/20","20/21")){
    write_lines(paste(s,y,"\n"),"hydrosummary.txt",append = T)
    df %>%
      filter(site==s) %>% 
      filter(wateryear==y) %>% 
      rowid_to_column("t") %>%
      ungroup %>%
      select(t,Zw) %>%
      fn_hydro_freq_duration_depth(df=.,thresh=0) %>%
      write_lines("hydrosummary.txt",append = T)
      #mutate(t=rownames(.)) %>%
      #filter(t!=nrow(.),t!=1)
  }
}
rownames(df.out)
nrow(df.out)
  
  group_by(site,date,wateryear) %>%
  mutate(stats=map(fn_hydro_freq_duration_depth(thresh=0)))
  fn_hydro_freq_duration_depth(df=.,thresh=0)
df.hobo.wetland_ma %>% filter(site=="OCD") %>% 
  select(datetime,Zw=WL_r2_ma) %>%
  mutate(date=date(datetime)) %>% 
  group_by(date) %>%
  summarize(Zw=max(Zw)) %>% 
  mutate(t=rownames(.)) %>%
  filter(t!=nrow(.),t!=1) %>%
  fn_hydro_freq_duration_depth(df=.,thresh=0)
df.hobo.wetland_ma %>% filter(site=="LC") %>% 
  select(datetime,Zw=WL_r2_ma) %>%
  mutate(date=date(datetime)) %>% 
  group_by(date) %>%
  summarize(Zw=max(Zw)) %>% 
  mutate(t=rownames(.)) %>%
  filter(t!=nrow(.),t!=1) %>%
  fn_hydro_freq_duration_depth(df=.,thresh=0)

fn_negcosine <- function(
  prd = 13*2, # 1/time period units of x (time),
  yadj =.583/2, # horizontal adjustment units of y,
  xadj = 0, # vertical adjustment units of x
  amp = .583/2, # amplitude
  x = 1:prd ){
  #y = vertical_adj. + amplitude * sin(2*pi/period + horz.)
  y = yadj+amp*-cos(x*2*pi/prd+xadj)
  return(y)
}
fn_negcosine_0tamp <- function(
  prd = 13, # 1/time period units of x (time),
  amp = .583, # amplitude
  x = 1:prd ){
  #y = vertical_adj. + amplitude * sin(2*pi/period + horz.)
  y = amp/2+amp/2*-cos(x*2*pi/prd)
  return(y)
}
x = 1:13
y = fn_negcosine_0tamp(x=x)
plot(x,y)
lines(x,y)

  