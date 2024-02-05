# **************************************************************
# filename: climate_preprocessing.R
# description: preprocesses climate data from NOAA NCDC weather stations
# author: adrian wiegman 
# revision date:  2021-02-11
# project: LCBP wetland P 
# repository: https://github.com/arhwiegman/___    
# notes:
# - R is prone to crashing when nesting and unnesting data
# - ___
# **************************************************************


# setup --------------------------
# 2021-02-11

rm(list=ls()) # clear environment
library(tidyverse)
library(lubridate)
library(readxl)
library(zoo)
library(forecast)
parent <- "C:\\Users\\Admin\\OneDrive - University of Vermont\\LCBP-wetlandP\\Data"
sub <- "Climate"
folder <- "NOAA_NCDC"
file <- "2429535.csv"
path <- paste(parent,sub,folder,file,sep="\\")



# wrangle data --------------------------
# 2021-02-11

df.climate.raw <- read_csv(path) 

df.climate.full <- df.climate.raw %>%
  # merge report type columns
  mutate(REPORT_TYPE=case_when(
    str_detect(REPORT_TYPE_1,"^FM.*") ~ REPORT_TYPE_1,
    str_detect(REPORT_TYPE_1,"^SOD.*")~ REPORT_TYPE_1,
    str_detect(REPORT_TYPE_1,"^SOM.*")~ REPORT_TYPE_1,
    TRUE ~ REPORT_TYPE)) %>%
  mutate(report_dt=case_when(
    str_detect(REPORT_TYPE_1,"^SOD.*")~"day",
    str_detect(REPORT_TYPE_1,"^SOM.*")~"month",
    str_detect(REPORT_TYPE_1,"^FM.*")~"hr",
  )) %>%
  
  # format dates and times
  mutate(datetime = DATE %>%
           str_replace_all("T"," ") %>%
           strptime("%Y-%m-%d %H:%M:%S") %>%
           as.POSIXct()) %>% 
  rename(stringDATE = DATE) %>%
  mutate(date=date(datetime)) %>%
  mutate(time = as.numeric(format(datetime, "%H")) +
           as.numeric(format(datetime, "%M"))/60) %>%
  
  # classify stations by city
  # data formats
  mutate(STATION=as.factor(STATION)) %>%
  mutate(city=case_when(
    STATION=="72617014742"~"Burlington",
    STATION=="99999900425"~"Middlebury",
    TRUE~"Rutland")) %>%
  
  # Use okta values as proxy for albedo
  # note this is a poor approximation of cloud albedo
  # reclassify cloud cover to Okta values https://en.wikipedia.org/wiki/Okta
  mutate(HourlyCloudAlbedo=case_when( 
    str_detect(HourlySkyConditions,"CLR")~0,
    str_detect(HourlySkyConditions,"FEW")~.2,
    str_detect(HourlySkyConditions,"SCT")~.4,
    str_detect(HourlySkyConditions,"BKN")~.7,
    str_detect(HourlySkyConditions,"OVC")~.8,
    T~1
  )) %>%
  
  # convert sunrise and sunset to decimal hour
  separate(Sunrise,c("hr.rise","min.rise"),sep=2) %>%
  separate(Sunset,c("hr.set","min.set"),sep=2) %>%
  mutate(Sunrise = as.numeric(hr.rise)+as.numeric(min.rise)/60,
         Sunset = as.numeric(hr.set)+as.numeric(min.set)/60) %>%
  
  # convert hourly values to numeric
  mutate_at(vars(starts_with(c("Hourly","Daily","Monthly"))),as.numeric)


# put sunrise and sunset at hourly data 
df.climate.sub <- df.climate.full %>%
  select(city,report_dt,datetime,date,time,contains(c("DryBulb","Humid","WindSpeed","Precip","Pressure","Snow","Sun","Albedo")),-starts_with("ShortDura"))


# interpolate missing values --------------------------
# 2021-02-11

# NEST DATA AND FILL MISSNING VALUES WITH LINEAR INTERPOLATION
df.climate.approx.nested <- df.climate.sub %>% 
  group_by(city,report_dt) %>% 
  arrange(datetime) %>%
  nest() %>%
  
  # hourly data
  mutate(data.hr.approx = map(data,
                           function(x){
                             x %>% 
                               select(-starts_with("Daily"),
                                      -starts_with("Month"),
                                      -starts_with("Sun")) %>%
                               select_if(is.numeric) %>%
                               zoo %>%
                               na.approx(na.rm=FALSE) %>%
                               fortify.zoo %>%
                               select(-Index,-time)
                             } 
                           )
         ) %>%
  
  # daily data
  mutate(data.day.approx = map(data,
                              function(x){
                                x %>% 
                                  select(-starts_with("Hourly"),
                                         -starts_with("Month")) %>%
                                  select_if(is.numeric) %>%
                                  zoo %>%
                                  na.approx(na.rm=FALSE) %>%
                                  fortify.zoo %>%
                                  select(-Index,-time)
                                }
                             )
         ) %>%
  
  # monthly data
  mutate(data.month.approx = map(data,
                               function(x){
                                 x %>% 
                                   select(-starts_with("Hourly"),
                                          -starts_with("Daily"),
                                          -starts_with("Sun")) %>%
                                   select_if(is.numeric) %>%
                                   zoo %>%
                                   na.approx(na.rm=FALSE) %>%
                                   fortify.zoo %>% 
                                   select(-Index,-time)
                                 } 
                               )
         )


# UNNEST DATA
df.climate.approx <- df.climate.approx.nested %>%
  mutate(data2 = map(data,
                       function(x){
                         x %>% 
                           select(-contains("Sun"),
                                  -starts_with("Hourly"),
                                  -starts_with("Daily"),
                                  -starts_with("Monthly"))
                       } 
                       )
    ) %>%
  select(-data) %>%
  unnest(c(data2,data.month.approx,data.day.approx,data.hr.approx)) %>%
  ungroup





# sunshine --------------------------
# 2021-02-11
middlebur_kWh <- "
Month, Day, Solar kWh/m2
Jan, 1, 1.4
Feb, 1,  2.2
Mar, 1, 3.1
Apr, 1, 4.5
May, 3, 5.7
Jun, 30, 6.3
Jun, 30, 6.8
Aug, 21, 5.7
Sep, 1, 5.4
Sep, 30, 3.9
Oct, 30, 2.5
Dec, 1, 1.5
"
#https://weatherspark.com/m/24991/12/Average-Weather-in-December-in-Middlebury-(village)-Vermont-United-States#Sections-SolarEnergy



# calculate sunshine hours from cloud cover observations
df.sun.day <- df.climate.approx %>% 
  # trim off decimal
  mutate(Sunrise=str_sub(Sunrise,1,4),
         Sunset=str_sub(Sunset,1,4)) %>%
  # convert sunrise and sunset to decimal hour
  separate(Sunrise,c("hr.rise","min.rise"),sep=2,remove=FALSE) %>%
  separate(Sunset,c("hr.set","min.set"),sep=2,) %>%
  mutate(sunrise = as.numeric(hr.rise)+as.numeric(min.rise)/60,
         sunset = as.numeric(hr.set)+as.numeric(min.set)/60) %>%
  #select(city,date,Sunrise,Sunset) %>%
  filter(!is.na(sunrise)) %>%
  select(date,city,sunrise,sunset)

df.climate.sun <- df.climate.approx %>%
  left_join(df.sun.day) %>%
  # calculate day length and sunshine hours from cloud cover
  mutate(daytime = ifelse(sunrise<time & time>sunset,1,0)) %>%
  mutate(sunshine = daytime * (1 - HourlyCloudAlbedo))

df.sunshine.summary <- df.climate.sun %>% group_by(city,date) %>%
  summarise(DailySunshineHours=sum(sunshine))

df.climate.sunshine <- df.climate.sun %>%
  left_join(df.sunshine.summary) 


# antecedent moisture --------------------------
# 2021-02-16

# Ward, A. D., Trimble, S. W., Burckhard, S. R., & Lyon, J. G. (2016). Environmental Hydrology (Third Edit). CRC Press Taylor and Francis Group.

# antecedent moisture conditions
# 1. dry: Dormant season total 5-day antecedent rainfall less than 0.5 in. Growing season total 5-day antecedent rain- fall less than 1.4 in. 
# 2. normal: Dormant season total 5-day antecedent rainfall between 0.5 and 1.1 in. Growing season total 5-day anteced- ent rainfall between 1.4 and 2.1 in. 
# 3. wet: Dormant season total 5-day antecedent rainfall greater than 1.1 in. Growing season total 5-day antecedent rainfall greater than 2.1 in.
# 4. frozen or saturated: 14day average temp is less than 32F, or 14day average temp is less than 40F and 5day antecedent rainfall is greater than  
df.climate.amc <- df.climate.sunshine %>% 
  filter(report_dt=="day") %>%
  group_by(city,report_dt) %>%
  mutate(month = month(date)) %>%
  # classify dormant season by month
  mutate(season = ifelse(
    month > 4 & month < 11,
    "growing",
    "dormant")) %>%
  mutate(Daily5DayAntPrecipitation=rollsum(
    DailyPrecipitation,5,align="right",na.pad=T)) %>%
  mutate(Daily5DayAntPrecipitation=rollsum(
    DailyPrecipitation,14,align="right",na.pad=T)) %>%
  mutate(Daily14DayMeanTemp=rollmean(
    DailyAverageDryBulbTemperature,14,align="right",na.pad=T)) %>%
  # classify amc by 5 day antecedent precip and season
  mutate(AMC = 2,
         AMC = case_when(
    season == "growing" & 
      Daily5DayAntPrecipitation < 1.4 ~ 1,
    season == "growing" & 
      Daily5DayAntPrecipitation >= 1.4 & 
      Daily5DayAntPrecipitation <= 2.1 ~ 2,
    season == "growing" & 
      Daily5DayAntPrecipitation > 2.1 ~ 3,
    season == "dormant" & 
      Daily5DayAntPrecipitation < 0.5 ~ 1,
    season == "dormant" & 
      Daily5DayAntPrecipitation >= 0.5 & 
      Daily5DayAntPrecipitation <= 1.1 ~ 2,
    season == "dormant" & 
      Daily5DayAntPrecipitation > 1.1 ~ 3,
    Daily14DayMeanTemp < 32 ~ 4,
    Daily14DayMeanTemp < 40 &
      Daily5DayAntPrecipitation <= 2.1 ~ 4,
  )) %>%
  ungroup %>%
  select(city,date,month,season,
         Daily5DayAntPrecipitation,
         Daily14DayMeanTemp,
         AMC) 

df.climate <- df.climate.sunshine %>% left_join(df.climate.amc)
  

# subset & save --------------------------
# 2021-02-11

# save climate data
save(df.climate,file="df.climate.Rdata")

# hourly observations
df.climate.hour <- df.climate %>% 
  filter(str_detect(report_dt,"hr")) %>%
  select(-starts_with("Month"),
         -starts_with("Daily"))
save(df.climate.hour,file="df.climate.hour.Rdata")

# daily summary
df.climate.day <- df.climate %>%
  filter(str_detect(report_dt,"day")) %>%
  select(-starts_with("Monthly"),
         -starts_with("Hourly"))
save(df.climate.day,file="df.climate.day.Rdata")

# monthly summary
df.climate.month <- df.climate %>%
  filter(str_detect(report_dt,"day")) %>%
  select(-starts_with("Daily"),
         -starts_with("Hourly"))
save(df.climate.month,file="df.climate.month.Rdata")





# evapotranspiration --------------------------
# 2021-02-18' # issue with unit conversions? 
library(Evapotranspiration)
CITY = "Rutland"
df.climate.ET <- df.climate.sunshine %>% 
  filter(city==CITY) %>%
  # format dates and times
  separate(date,c("Year","Month","Day"),sep = "-") %>%
  mutate(Hour=round(time)) %>%
  # conversions and calculations
  mutate(
    Temp = (HourlyDryBulbTemperature - 32)/1.8, # F to C
    RH = HourlyRelativeHumidity, # %
    uz = HourlyWindSpeed*0.44704,# mi/hr to m/s
    Precip = HourlyPrecipitation*2.54*100, # in to cm to mm
    n = DailySunshineHours # sunshine hrs, estimated from cloud cover oktas
  ) %>%
  select(Year,Month,Day,Hour,Temp,RH,uz,Precip,n) %>%
  na.omit



data(constants)

# lattitude of sites in degrees
lat_sites <- c("LC"=44.29408545776703,
               "OCSP"=43.91904104479901,
               "OCD"=43.78071914762275)
Elev_sites <- c("LC"=113,
               "OCSP"=104,
               "OCD"=106)
site = "OCD"
constants$Elev <- Elev_sites[site] #ground elevation above mean sea level in m,
constants$lat_rad <- lat_sites[site] * pi/180 #latitude in radians,
constants$alphaPT <- 1.26 #1.26 (default), Priestley-Taylor coefficient = 1.26 for Priestley-Taylor model (Priestley and Taylor, 1972)
constants$as <- 0.23 #0.23 (default), fraction of extraterrestrial radiation reaching earth on sunless days,
constants$bs <- 0.5 #0.5 (default), difference between fracion of extraterrestrial radiation reaching full-sun days and that on sunless days.
constants$z <- 1.5 #0.5 (default), difference between fracion of extraterrestrial radiation reaching full-sun days and that on sunless days.

vars <- names(df.climate.ET)[5:ncol(df.climate.ET)]

df.climate.ET.processed <- ReadInputs(varnames=vars,
           climatedata=df.climate.ET,
           timestep="subdaily",
           constants=constants,
           stopmissing = c(10,20,3),
#           interp_abnormal = T,
#           interp_missing_days = T,
#           interp_missing_entries = T,
#           missing_method = "neighboring average",
#           abnormal_method = "neighboring average",
           message=T)

df.ET.PM <- ET.PenmanMonteith(
  data=df.climate.ET.processed,
  constants=constants,
  wind="yes",
  crop="tall", # tall crop 0.5m allen 2005
  ts="daily",
  solar="sunshine hours")

save(df.ET.PM,file=paste0("df.ET.PM.",site,".Rdata"))

df.ET.PT <- ET.PriestleyTaylor(
  data=df.climate.ET.processed,
  constants=constants,
  alpha=0.23, # albedo of reference crop
  ts="daily",
  solar="sunshine hours")
               
save(df.ET.PT,file=paste0("df.ET.PT.",site,".Rdata")) 

rm(list=ls()) # clear environment
