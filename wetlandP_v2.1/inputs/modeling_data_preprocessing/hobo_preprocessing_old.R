

#load data
load("C:\\Workspace\\wetlandP\\data\\lcbp\\HOBO.Rdata")

pascal_dP_to_dH <- function(dP,rho=1000,g=9.806){
  # Pascal's Law:
  # dP = dH * rho * g
  # dP/(rho*g) = dH
  # where
  # dH - vertical distance between points
  # dP - difference in pressure between two vertical points 
  # rho - density of fluid (kg/m^3) = 1000 for freshwater
  # g - gravitational acceleration (m/s^2) = 9.806 at sea level
  dH = dP/(rho*g)
  return(dH)
}
pascal_dP_to_dH(1000)


d <- df.hobo.joined_corrected %>% 
  select(
    datetime=`Date Time, GMT -0400`,
    tempF = `Temp, degF`,
    abs.psi =`Abs Press, psi`,
    baro.psi =`Baro Press, psi`,
    site,
    plot,
    HOBO_name,
    Filename
    ) %>%
  # convert to SI units and calculate water depth from pressure difference
  mutate(
    abs.Pa = abs.psi * 6894.76,
    baro.Pa = baro.psi * 6894.76,
    dP = (abs.Pa - baro.Pa), # 6894.76 Pa is 1 psi 
    dH = pascal_dP_to_dH(dP)
    ) %>% 
  distinct %>%
  arrange(datetime)
# 
# %>% 
#   pivot_wider(
#     names_from = HOBO_name,
#     values_from = c(abs.Pa,baro.Pa),
#     names_glue = "{HOBO_name} {.value}"
#     )

d %>% ggplot(aes(x=datetime,y=baro.Pa,color=HOBO_name))+facet_grid(vars(site))+geom_line()


# get record of barometric pressure at each site

# create an approximation function for ocd high baro
df.ocd_hi <- d %>% 
  filter(site=="OCD",
         plot=="HI") %>% 
x=df.ocd_hi$datetime
y=df.ocd_hi$baro.Pa
f_ocd_hi_baro <- approxfun(x,y)

# create an approximation function for lc low baro
df.lc_low <- d %>% 
  filter(site=="LC",plot=="low")
x=df.lc_low$datetime
y=df.lc_low$baro.Pa
f_lc_low_baro <- approxfun(x,y)


# Create new dataframe with OCD hi barometric pressure 
df <- d %>% 
  #filter(site=="OCD"|site=="OCSP") %>%
  mutate(baro.Pa.ocd_hi = f_ocd_hi_baro(datetime),
         dP.ocd_hi = abs.Pa - baro.Pa.ocd_hi,
         dH.ocd_hi = pascal_dP_to_dH(dP.ocd_hi),
         diff_baro.Pa.ocd_hi=baro.Pa-baro.Pa.ocd_hi) %>% 
  mutate(baro.Pa.lc_low = f_lc_low_baro(datetime),
       dP.lc_low = abs.Pa - baro.Pa.lc_low,
       dH.lc_low = pascal_dP_to_dH(dP.lc_low),
       diff_baro.Pa.lc_low=baro.Pa-baro.Pa.lc_low) %>%
  mutate(dlcocd=dH.ocd_hi-dH.lc_low) %>%
  mutate(dlcocd=dH-dH.ocd_hi)

# examine fit between barometric pressure among sites
df.hobo.info$HOBO_name %>% print

df %>% 
  filter(datetime<"2019-10-15") %>%
  #filter(HOBO_name=="lc 4"|HOBO_name=="oscp low") %>%
  ggplot(aes(baro.Pa.ocd_hi,baro.Pa,color=datetime))+
  facet_wrap(vars(HOBO_name))+
  geom_line()+
  geom_smooth(method="lm")+
  geom_abline(aes(slope=1,intercept=0))

df %>% 
  filter(datetime<"2019-10-15") %>%
  #filter(HOBO_name=="lc 4"|HOBO_name=="oscp low") %>%
  ggplot(aes(baro.Pa.lc_low,baro.Pa,color=datetime))+
  facet_wrap(vars(HOBO_name))+
  geom_line()+
  geom_smooth(method="lm")+
  geom_abline(aes(slope=1,intercept=0))

df %>% 
  filter(datetime<"2019-10-15") %>%
  #filter(HOBO_name=="lc 4"|HOBO_name=="oscp low") %>%
  ggplot(aes(datetime,diff_baro.Pa.ocd_hi,color=datetime))+
  facet_wrap(vars(HOBO_name))+
  geom_line()+
  geom_smooth(method="lm")+
  geom_abline(aes(slope=0,intercept=0))

df %>% 
  filter(datetime<"2019-10-15") %>%
  #filter(HOBO_name=="lc 4"|HOBO_name=="oscp low") %>%
  ggplot(aes(datetime,diff_baro.Pa.lc_low,color=datetime))+
  facet_wrap(vars(HOBO_name))+
  geom_line()+
  geom_smooth(method="lm")+
  geom_abline(aes(slope=0,intercept=0))

df %>% ggplot(aes(x=datetime,y=baro.Pa,color=HOBO_name))+facet_grid(vars(site))+geom_line()

df %>% ggplot(aes(x=datetime,y=dlcocd,color=HOBO_name))+facet_grid(vars(site))+geom_line()
df %>% ggplot(aes(x=datetime,y=dH,color=HOBO_name))+facet_grid(vars(site))+geom_line()
df %>% ggplot(aes(x=datetime,y=dH,color=HOBO_name))+facet_grid(vars(site))+geom_line()
df %>% ggplot(aes(x=datetime,y=dH.ocd_hi,color=HOBO_name))+facet_grid(vars(site))+geom_line()


df %>% 
  filter(datetime>"2019-10-15"&datetime<"2019-11-15") %>%
  ggplot()+
  facet_grid(vars(site))+
  geom_line(aes(x=datetime,y=dH,color=HOBO_name))+
  geom_line(aes(x=datetime,y=dH.ocd_hi,color=HOBO_name),linetype="dotdash")+
  geom_line(aes(x=datetime,y=dH.lc_low,color=HOBO_name),linetype="dashed")

df %>% 
  filter(datetime>"2021-03-15"&datetime<"2021-04-15") %>%
  ggplot()+
  facet_grid(vars(site))+
  geom_line(aes(x=datetime,y=dH,color=HOBO_name))+
  geom_line(aes(x=datetime,y=dH.ocd_hi,color=HOBO_name),linetype="dotdash")+
  geom_line(aes(x=datetime,y=dH.lc_low,color=HOBO_name),linetype="dashed")


########



