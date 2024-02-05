# fit logistic regression to LC IN N
fn_plot_logistic_regression <- function (
  width=1, # width of domain where y increases/decreases as function of x
  a=1, #ymax maximum y value at upper end of xrange
  b=0, #xmin minimum y value at lower end of xrange
  slope=0.1, #slope slope over the xrange if slope is 1:1 then 10/width is good starting value
  d=0 #xinflect x value at the inflection point (center of range)
){
  c=slope*10^-log10(width/10)
  X=seq(d-width/2,d+width/2,length.out=100)
  Y = (a-b)/(1 + exp(-c*(X-d)))+b
  print(ggplot()+geom_line(aes(X,Y)))
  return(Y)
}
# Stage flow relationships
library(tidyverse)
library(readxl)
# load data -----------------------
load("C:/Workspace/wetlandP/data/lcbp/data.Rdata")
load("C:\\Users\\Admin\\OneDrive - University of Vermont\\LCBP-wetlandP\\Data\\Water\\HOBO\\df.hobo.data_processed.Rdata")


df.LCOUTWL.stageflow <- read_xlsx("C:\\Users\\Admin\\OneDrive - University of Vermont\\LCBP-wetlandP\\Data\\Water\\WaterSamples\\CrosssectionalArea.xlsx","stage-area m")
plot(df.LCOUTWL.stageflow)
#View(df.LCOUTWL.stageflow)

fn_approx_area_wl <- approxfun(x=df.LCOUTWL.stageflow$`water height above thalweg (m)`,df.LCOUTWL.stageflow$`area  (m2)`)
fn_approx_area_wl(.93)

# functions --------------------------
# 2021-04-12
fn_circle_segment_area <- function(
  # https://www.mathopenref.com/segmentareaht.html
  r = 2,   # r is the radius of the circle of which the segment is a part.
  h = 0.75 # h is the height of the segment (e.g. height from bottom of pipe center to water level)
){
  r^2*acos((r-h)/r)-(r-h)*sqrt(2*r*h-h^2)
}
fn_circle_segment_area()


# process data ---------------------
d.hobo <- df.hobo.data_processed %>%
  filter(site=="LC") %>%
  select(datetime=datetime,tempC,plot,dH_baro_correct,RWL=WL_rsoil,`Deployment Number`)

d.hobo.low <- d.hobo %>% filter(plot=="low") %>% rename(RWL_LOW=RWL)
d.hobo.ine <- d.hobo %>% filter(plot=="IN E") %>% rename(RWL_INE = RWL)
d.hobo.outwl <- d.hobo %>% filter(plot=="OUT WL") %>% rename(RWL_OUT = RWL)

#d.hobo %>% View
library(data.table) # rolling joins
d.hobo %>% #filter(datetime>"2021-04-01") %>% 
  ggplot()+geom_line(aes(datetime,RWL,color=plot))
setDT(d.hobo)

d <- df.grab %>% 
  filter(site=="LC",
         !is.na(flow.vel.fts)) %>%
  mutate(flow.vel.fts=ifelse(flow.vel.fts==43.0,4.3,flow.vel.fts)) %>% 
  select(datetime,site.plot,site,plot,time,date,flow.vel.fts,HWRB.cm,notes) %>%
  mutate(HWRB.cm = as.numeric(HWRB.cm),
         year = year(date),
         before_aug20 = ifelse(date < "2020-08-01","before 2020-08-01","after 2020-08-01"))
#View(d)
#use rolling join from data table to match flow observations to nearest water levels record
require(data.table)
setDT(d)[, RWL_LOW := setDT(d.hobo.low)[d, RWL_LOW, on = "datetime", roll = "nearest"]]
setDT(d)[, RWL_INE := setDT(d.hobo.ine)[d, RWL_INE, on = "datetime", roll = "nearest"]]
setDT(d)[, RWL_OUT := setDT(d.hobo.outwl)[d, RWL_OUT, on = "datetime", roll = "nearest"]]

# 2020-05-04: there is an issue with matching water level data with water sample data 

# outflow -----------------------
df.ratingcurves.outflow <- d %>% 
  filter(plot=="OUT WL") %>% 
  mutate(flow.vel.fts=ifelse(
    flow.vel.fts>0,
    flow.vel.fts,
    -flow.vel.fts),
         ACm2 = fn_approx_area_wl(RWL_OUT),
         Ums = flow.vel.fts*30.48/100,
         Qcfs = ACm2*10.76391*flow.vel.fts,
         Qm3s = ACm2*Ums,
         Qm3d = Qm3s*60*60*24,
         test=case_when(
           str_detect(notes,"this one")~T,
           T~F)) %>%
  filter(!test) %>% select(-test)
#View(df.ratingcurves.outflow)

# inflow -----------------
df.ratingcurves.inflow <- d %>%
  filter(plot=="IN E" | plot == "IN N",
         !is.na(HWRB.cm)) %>%
  mutate(
  HWRB.cm = ifelse(as.character(datetime)=="2019-11-02 10:16:00",HWRB.cm+10,HWRB.cm),
  notes = ifelse(as.character(datetime)=="2019-11-02 10:16:00","originally reported as -66\nadjusted HWRB.cm by 10cm due to measuring on upstream side of culvert",notes)
  #HWRB.cm = ifelse(date>"2020-08-01"&
  #                          plot=="IN E",HWRB.cm+20,HWRB.cm)
  ) %>%
  slice(-1) %>% 
  
  # convert units and calculate crossectional area 
  mutate(
    r = case_when(
      plot=="IN N"~1*30.48/100, # 1ft radius * 30.48cm/ft * 1m/100cm 
      plot=="IN E"~2*30.48/100 # 2ft radius * 30.48cm/ft * 1m/100cm 
  ),
  # if height of water relative to benchmark (top of cuvlert) is greater than zero set height equal to the diameter of the culvert, else set height equal to the culvert
  h = ifelse(HWRB.cm>0,r*2,r*2+HWRB.cm/100)) %>%
  mutate(
  ACm2 = case_when(
    # IN N has 2 pipes of 1ft radius 
    plot=="IN N"~2*fn_circle_segment_area(r=r,h=h),
    # IN E has 1 pipe of 2ft radius
    plot=="IN E"~fn_circle_segment_area(r=r,h=h),
  ))%>%
  mutate(
  Ums = flow.vel.fts*30.48/100,
  Qcfs = ACm2*10.76391*flow.vel.fts,
  Qm3s = ACm2*Ums,
  Qm3d = Qm3s*60*60*24
)



# plot data ----------------------

## IN E ----
g <- ggplot(data=df.ratingcurves.inflow %>%  filter(plot=="IN E",flow.vel.fts>0.2,before_aug20=="before 2020-08-01"),aes(x=log10(RWL_INE),y=log10(flow.vel.fts)))+geom_smooth(method="lm")+facet_grid(rows=vars(site.plot))+geom_point()
g
g <- ggplot(data=df.ratingcurves.inflow %>% filter(plot=="IN E",flow.vel.fts>0.2,before_aug20=="before 2020-08-01"),aes(x=log10(RWL_INE),y=log10(Qcfs)))+geom_smooth(method="lm")+facet_grid(rows=vars(site.plot))+geom_point()
g

m.ine.cfs <- df.ratingcurves.inflow %>%
  filter(plot=="IN E",flow.vel.fts>0.2,before_aug20=="before 2020-08-01") %>% 
  lm(log10(Qcfs)~log10(RWL_INE),data=.)
m.ine.cfs %>% summary


## IN N -----
g <- ggplot(data=df.ratingcurves.inflow %>% filter(plot=="IN N",flow.vel.fts>0.2,RWL_OUT>0),aes(x=log10(RWL_OUT),y=log10(flow.vel.fts)))+geom_smooth(method="lm")+facet_grid(rows=vars(site.plot))+geom_point()
g

g <- ggplot(data=df.ratingcurves.inflow %>% filter(plot=="IN N",flow.vel.fts>0.2,RWL_OUT>0),aes(x=log10(RWL_OUT),y=log10(Qcfs)))+geom_smooth(method="lm")+facet_grid(rows=vars(site.plot))+geom_point()
g



m.inn.cfs <- df.ratingcurves.inflow %>%
  filter(plot=="IN N",flow.vel.fts>0.2,RWL_OUT>0) %>% 
  lm(log10(Qcfs)~log10(RWL_OUT),data=.)
m.inn.cfs %>% summary


## OUT WL ----

g <- ggplot(data=df.ratingcurves.outflow %>%
              filter(flow.vel.fts>0.2,RWL_OUT>0),aes(x=log10(RWL_OUT),y=log10(flow.vel.fts)))+geom_smooth(method="lm")+facet_grid(rows=vars(site.plot))+geom_point()
g
g <- ggplot(data=df.ratingcurves.outflow %>%
              filter(flow.vel.fts>0.2,RWL_OUT>0),aes(x=log10(RWL_OUT),y=log10(Qcfs)))+geom_smooth(method="lm")+facet_grid(rows=vars(site.plot))+geom_point()
g
m.outflow.cfs <- df.ratingcurves.outflow %>%
  filter(flow.vel.fts>0.2,RWL_OUT>0) %>%
  lm(log10(Qcfs)~log10(RWL_OUT),data=.)
m.outflow.cfs %>% summary


## CALC Discharge timeseries based on model fits
d.outwl.Qcfs <- d.hobo %>% 
  filter(plot=="OUT WL") %>%
  mutate(RWL=ifelse(tempC<2,NA,RWL),
         plot="OUT WL",
         Qcfs = ifelse(RWL>0,10^(m.outflow.cfs$coefficients[1]+m.outflow.cfs$coefficients[2]*log10(RWL)),0))
#View(d.outwl.Qcfs)

d.ine.Qcfs <- d.hobo %>% 
  filter(plot=="IN E",datetime<"2021-08-01") %>%
  mutate(RWL=ifelse(tempC<2,NA,RWL),
         plot="IN E",
         Qcfs = ifelse(RWL>0,10^(m.ine.cfs$coefficients[1]+m.ine.cfs$coefficients[2]*log10(RWL)),0)) 
#View(d.ine.Qcfs)

d.inn.Qcfs <- d.hobo %>% 
  filter(plot=="OUT WL") %>%
  mutate(RWL=ifelse(tempC<2,NA,RWL),
         plot="IN N",
         Qcfs = ifelse(RWL>0,10^(m.inn.cfs$coefficients[1]+m.inn.cfs$coefficients[2]*log10(RWL)),0))
#View(d.inn.Qcfs)

RWL <- seq(0,1,length.out=10)
10^(m.outflow.cfs$coefficients[1]+m.outflow.cfs$coefficients[2]*log10(RWL))
10^(m.ine.cfs$coefficients[1]+m.ine.cfs$coefficients[2]*log10(RWL))
10^(m.inn.cfs$coefficients[1]+m.inn.cfs$coefficients[2]*log10(RWL))


## Rolling join data frames ------
d.inn.Qcfs <- d.inn.Qcfs %>% 
  select(datetime,Qcfs_inn=Qcfs) %>% 
  setDT
d.ine.Qcfs <- d.ine.Qcfs %>%
  select(datetime,Qcfs_ine=Qcfs) %>%
  setDT
d.outwl.Qcfs <- d.outwl.Qcfs %>%
  select(datetime,Qcfs_outwl=Qcfs) %>%
  setDT
d.low.RWL <- d.hobo %>%
  filter(plot=="low") %>%
  mutate(RWL=ifelse(tempC<2,NA,
                    ifelse(RWL>0,RWL,0))) %>% 
  select(datetime,RWL) %>%
  setDT



# Create time column by which to do a rolling join
setkey(d.inn.Qcfs, datetime)
setkey(d.ine.Qcfs, datetime)
setkey(d.outwl.Qcfs, datetime)
setkey(d.low.RWL, datetime)

# Rolling join by nearest time
df.Qcfs <- d.inn.Qcfs[d.ine.Qcfs, roll = "nearest"]
df.Qcfs <- df.Qcfs[d.outwl.Qcfs, roll = "nearest"]
df.RWL.Qcfs <- df.Qcfs[d.low.RWL, roll = "nearest"]
df.RWL.Qcfs <- df.RWL.Qcfs %>% 
  rename(Hw_meters = RWL) %>%
  mutate(Hw_feet=Hw_meters/0.3048,
         cf=Qcfs_inn+Qcfs_ine-Qcfs_outwl)
df.RWL.Qcfs %>% filter(datetime>"2019-10-01"&datetime<"2019-11-10") %>% ggplot() + geom_line(aes(x=datetime,y=Hw_feet)) 
df.RWL.Qcfs %>% filter(datetime>"2019-10-01"&datetime<"2019-11-10") %>% ggplot() + geom_line(aes(x=datetime,y=Qcfs_inn))
df.RWL.Qcfs %>% filter(datetime>"2019-10-01"&datetime<"2019-11-10") %>% ggplot() + geom_line(aes(x=datetime,y=Qcfs_ine))
df.RWL.Qcfs %>% filter(datetime>"2019-10-01"&datetime<"2019-11-10") %>% ggplot() + geom_line(aes(x=datetime,y=cf))
df.RWL.Qcfs %>% filter(datetime>"2019-10-01"&datetime<"2019-11-10") %>% ggplot() + geom_line(aes(x=Hw_feet,y=Qcfs_outwl,color=datetime)) 


# outputs from arcgis storage capacity analysis
df.storagecapacity <- read_xlsx("DEMHF_0p7m_PrindleRd_StorageCapacity_TableToExcel.xlsx")
df.storagecapacity$Hw_meters <- df.storagecapacity$ELEVATION - 112.5 # height of hobo

# approx function for relationship between Hw_meters and storage volume
fn_approx_vol_Hw_meters <- approxfun(x=df.storagecapacity$Hw_meters,y=df.storagecapacity$VOLUME)
fn_approx_area_Hw_meters <- approxfun(x=df.storagecapacity$Hw_meters,y=df.storagecapacity$AREA)

df <- df.RWL.Qcfs %>% filter(Qcfs_outwl>0) %>% 
  mutate(period = "0",
         period=case_when(
    datetime >="2019-10-01" & 
      datetime < "2020-03-01"~"2019-10-01 to 2020-03-01",
    datetime >="2020-03-01" & 
      datetime < "2020-03-19 15:00"~"2020-03-01 to 2020-03-19 15:00",
    datetime >="2020-03-19 15:00" & 
      datetime < "2020-11-01"~"2020-03-19 15:00 to 2020-11-01",
    datetime >="2020-06-01" & 
      datetime < "2020-11-01"~"2020-06-01 to 2020-11-01",
    datetime >="2020-11-01"~"after 2020-11-01",
    T~period
  )) %>% 
  mutate(
    Volume_m3 = fn_approx_vol_Hw_meters(Hw_meters),
    Area_m2 = fn_approx_area_Hw_meters(Hw_meters),
    Qm3s_outwl = Qcfs_outwl*0.028316847,
    HRT_days = (Volume_m3/Qm3s_outwl)/60/60/24,
    HLR_m_d = Qm3s_outwl/Area_m2*60*60*24,
    HRT_days_m2 = HRT_days/Area_m2) %>%
  group_by(period) 
df %>%
  select(period,HRT_days,Hw_meters) %>%
  summarize_all(list(~mean(.,na.rm=T),~min(.,na.rm=T),~max(.,na.rm=T))) %>%
  write_csv(file="df.HRT_days_PrindleRd_summary.csv")

df %>% ggplot() + geom_point(aes(x=Hw_meters,y=Qcfs_outwl,color=datetime,shape=period))+
  facet_wrap(vars(period))
df %>% ggplot() + geom_point(aes(x=Hw_meters,y=Qm3s_outwl,color=datetime,shape=period))+
  facet_wrap(vars(period))
df %>% ggplot() + geom_point(aes(x=Hw_meters,y=log10(HRT_days),color=datetime,shape=period))+
  facet_wrap(vars(period))
df %>% ggplot() + geom_point(aes(x=Hw_meters,y=log10(HRT_days),color=datetime,shape=period))
df %>% ggplot() + geom_point(aes(x=Hw_meters,y=HLR_m_d,color=datetime,shape=period))
df %>% ggplot() + geom_point(aes(x=Hw_meters,y=log10(HRT_days_m2),color=datetime,shape=period))

df.RWL.Qcfs %>% ggplot() + geom_line(aes(x=Hw_feet,y=Qcfs_outwl,color=datetime))
write_csv(df.RWL.Qcfs,"df.hydroCAD.Qcfs.Hw.hour.csv")

d.hobo %>% 
  filter(datetime>"2019-10-31"&datetime<"2019-11-06") %>%
  ggplot() +
  geom_line(aes(x=datetime,y=dH_baro_correct,color=plot))
  
load("df.hydroclimate.day.LC.Rdata")

df.hydroclimate.day <- df.hydroclimate.day %>%
  mutate(precip_inches=ip/2.54) %>% 
  select(date,precip_inches,amc,Tair)
write_csv(df.hydroclimate.day,"df.hydroCAD.climate.day.csv")

df.hydroclimate.day %>% 
  filter(date>"2019-10-06"&date<"2019-11-06") %>%
  ggplot() +
  geom_col(aes(x=date,y=precip_inches))



g <- ggplot(data=df.ratingcurves.outflow,aes(x=log(RWL_OUT),y=log(Qcfs)))+geom_smooth(method="lm")+geom_point(aes(color=year))+labs(title="Ratings Curve of Flow Velocity and Water Stage")
g

m.outflow.cfs <- df.ratingcurves.outflow %>% lm(log10(Qcfs)~log10(RWL_OUT),data=.)
m.outflow.cfs %>% summary

###### END OF USEFUL CODE









# fit logistic regression to LC IN N
a=ymax=50
b=xmin=0
c=slope=1.5e-1
d=xinflect=log(5)
x = -100:100
# logistic regression
y = (a-b)*1/(1 + exp(-(c*x)+d))+b
ggplot()+geom_line(aes(x,y))+geom_point(data=df.ratingcurves.inflow %>% filter(site.plot=="LC.IN N"),aes(x=RWL_OUT*100,y=Qcfs,color=paste(before_aug20,site.plot)))

# logistic regression on LC IN E
width=100 # width of domain where y increases/decreases as function of x
a=115#ymax maximum y value at upper end of xrange
b=0 #xmin minimum y value at lower end of xrange
c=1.2*10^-log10(width/10)#slope slope over the xrange if slope is 1:1 then 10/width is good starting value
d=-40 #xinflect x value at the inflection point (center of range)
X=seq(d-width/2,d+width/2,length.out=100)
Y = (a-b)/(1 + exp(-c*(X-d)))+b
y_beforeaug20 = Y 
ggplot()+geom_line(aes(X,Y))+geom_point(data=df.ratingcurves.inflow %>% filter(site.plot=="LC.IN E"),aes(x=RWL_INE*100,y=Qcfs,color=paste(before_aug20,site.plot)))
width=70 # width of domain where y increases/decreases as function of x
a=80#ymax maximum y value at upper end of xrange
b=0 #xmin minimum y value at lower end of xrange
c=.4*10^-log10(width/10)#slope slope over the xrange if slope is 1:1 then 10/width is good starting value
d=-40 #xinflect x value at the inflection point (center of range)
X =seq(-width+d,width+d,length.out=100)
Y = (a-b)/(1 + exp(-c*(X-d)))+b
#ggplot()+geom_line(aes(X,Y))
ggplot()+geom_line(aes(X,Y))+geom_point(data=df.ratingcurves.inflow %>% filter(site.plot=="LC.IN E"),aes(x=HWRB.cm,y=Qcfs,color=paste(before_aug20,site.plot)))


