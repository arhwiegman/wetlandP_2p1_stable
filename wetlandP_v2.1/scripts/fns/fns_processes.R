# **************************************************************
# filename: process_fns.R
# description: declares functions for process rates and intermediate steps
# author: Adrian R.H. Wiegman 
# revision date: 2020-04-01
# project: wetlandP_v03
# repository: https://github.com/arhwiegman/wetlandP
# notes:
# - 
# - still need to add function for bioturbation (Marois & Mitsch 2016)
# **************************************************************

fn_assimilation <- function(
  # revised: 2020-09-22
  # returns: growth rate of biomass (day^-1) 
  # source: temperature (none), nutrient (none), water (Morris 2002)
  # notes: growth rate limited by temperature, biomass, water level, and nutrient availability
  # inputs:
  # name = value # units; description; assumptions
  BM = 1000, # g d.w. m-2: total biomass stock shoots + roots
  ABM = 500, # g d.w. m-2; above ground biomass stock in grams dry weight per meter squared
  k_ADNPP = 4000/365, # g d.w. m-2; annual aboveground primary productivity
  theta = 1.07, # g d.w. m-2; annual aboveground primary productivity
  T_STD = 10, # T
  H_w=.2, # m; height of water relative to sediment surface
  TW=20, # degC; temperature of water
  X=15, # g/m^3 ; mass of limiting nutrient in compartment ; see fn_Xlimitation 
  k_Ueff=0.50, # -; uptake efficiency (0-1) for nutrient X; see fn_Xlimitation 
  k_B2X=0.3/100, # X/B; X content in B ; see fn_Xlimitation 
  r_G_max=2, # 1/day; maximum growth rate rates in Schmidt reported as high as 2 1/day with an average of 0.6-0.8
  r_G_min=1e-3,# 1/day; minimum growth rate
  ks_G_TW = 17, # g/m^3 half saturation concentration for assimilation
  ks_G_B=2000, # g/m^2 half saturation biomass for assimilation
  T_G_min=4,# degC; minimum temperature for plant growth
  k_H_w = -5e-3, # -; slope of polynomial for water depth affects on assimilation rate
  k_H_w_max = 0 # m; optimal water height for growth
){
  # temperature effect
  r_G=(k_ADNPP)*theta**(TW-T_STD)/BM
  # shading effect
  #r_G=r_G-(r_G-r_G_min)*ABM/(ks_G_B+ABM)
  # water effects on net primary production rate; from Morris et al. 2002
  #r_G = r_G*(k_H_w*100*(H_w-k_H_w_max)^2+1) # convert m to cm multiply by 100
  if(r_G < r_G_min){r_G=r_G_min}
  
  # nutrient limitation
  X_avail = X*k_Ueff
  X_demand = r_G*BM*k_B2X
  r_G = ifelse(X_avail<X_demand,
               {(X_avail/X_demand)*r_G},
               {r_G})
  return(r_G)
}

fn_adsorption_k_E <- function (
  # revised: 2020-04-14
  # returns: rate adsorption of DIP onto PIP (1/d)
  # source: Wang et al. 2003 (applied by Marois & Mitsch 2016)
  # notes: added organic matter interaction with Ex_max using oxalate Al and Fe
  # inputs:
  # name = value # units; description; assumptions
  M = 1, # kg; mass of soil  
  V_w = 1, # (m^-3); volume of water
  PIP= 1.5, # (g); mass of particulate inorganic P in compartment
  DIP=0.01, # (g); mass of dissolved inorganic P in compartment
  phi=1, # (l/l); soil porosity, enter 1 if the volume of water is entered instead of compartment volume
  k_ad = 1.75 , # (d^-1); first order sorption rate constant 
  E = 0.1, # (m^3 kg^-1)(L/mg); langmuir constant of sorption; table 2, Reddy, K. R., O Connor, G. a., & Gale, P. M. (1998). Phosphorus Sorption Capacities of Wetland Soils and Stream Sediments Impacted by Dairy Effluent. Journal of Environment Quality, 27(2), 438. https://doi.org/10.2134/jeq1998.00472425002700020027x
  k_PIP2Ex = 0.8, # (g/g); ratio of exchangeable P to particulate inorganic P
  Ex_max = fn_Ex_max_1() # g/kg; maximum exchangeable P in sediments; increases as a function of LOI
){
  Ex = k_PIP2Ex*PIP/M  # (g kg-1) exchangeable P in sediments
  #Ex = Ex_max * (PIP/M)/(Ex_max + PIP/M)  # (g kg-1) exchangeable P in sediments
  #print(paste("Ex:",Ex, ",   Ex_max:", Ex_max)
  #print(paste("DIP_E:",DIP_E,"(g/m^3)"))
  
  r_sorp = k_ad*(DIP/V_w-DIP_E) # (g d^-1 m^-3) rate of DIP adsorption
  return(r_sorp)
}

fn_DIP_E_langmuir <- function(Ex=1,# (g kg-1) exchangeable P in sediments
                              Ex_max=3, # g/kg; maximum exchangeable P in sediments; 
                              E=1 # (m^3 kg^-1)(L/mg); langmuir constant of sorption;
                              ){
  DIP_E = Ex/(E*(Ex_max-Ex)) # (g m^-3) equilibrium DIP concentration in porewater
  }

fn_DIP_E_power_law <- function(
  # Statistical Fit to intact core data
  # DIP calculated from power model of DIP = a(Smax - S)(1-PSR)^(-b)
  # Defined operationally as SRP = a((OxAl/27+OxFe/56) - OxP/31)/(1-OxPSR)*^(-b)
  anoxic = T, # is the water column anaerobic
  Ex_max = 3, # (g kg-1) exchangeable P in sediments
  Ex = 0.1*3 # g/kg; maximum exchangeable P in sediments; 
){
  if(anoxic){
    # under completely anoxic soil conditions
    a = 3.71e+00
    b = 0.898 # must be positive
  }else{
    # under dirurnal 02 cycling conditions
    a = 4.0105
    b = 1.21E+00
  }
  DIP_E = a*((Ex_max - Ex)/(Ex/Ex_max))**(-b)
}

fn_DIP_E_power_law_PSR <- function(
  # Statistical fit to intact core data
  # DIP calculated from power model of DIP = a(Smax - S)(1-PSR)^(-b)
  # Defined operationally as SRP = a((OxAl/27+OxFe/56) - OxP/31)/(1-OxPSR)*^(-b)
  anearobic = T, # is the water column anearobic
  Ex_max = 3,
  PSR = 1
){
  if(anearobic){
    # under completely anearobic soil conditions
    a = 3.71e+00
    b = 0.898 # must be positive
  }else{
    # under dirurnal 02 cycling conditions
    a = 4.0105
    b = 1.21E+00
  }
  DIP_E = a*((Ex_max - Ex_max*PSR)/(PSR))**(-b)
}

fn_adsorption <- function (
  # revised: 2020-10-19
  # returns: rate adsorption of DIP onto PIP (1/d)
  # source: Wang et al. 2003 (applied by Marois & Mitsch 2016)
  # notes: added organic matter interaction with Ex_max using oxalate Al and Fe
  # inputs:
  # name = value # units; description; assumptions
  V_w = 1, # (m^-3); volume of water in compartment
  DIP_E= 1.5, #(g/m3=mg/L); equilibrium DIP concentration
  DIP=0.01, # (g); mass of dissolved inorganic P in compartment
  k_ad = 1.75 # (d^-1); first order sorption rate constant 
){
  r_sorp = k_ad*(DIP/V_w-DIP_E) # (g d^-1 m^-3) rate of DIP adsorption
  return(r_sorp)
}



fn_decay <- function (
  # revised: 2020-04-01
  # returns: rate of decay (1/d) limited by temperature
  # source: Wang and Mitsch 2000 (applied by Marois & Mitsch 2016)
  # notes:
  #  - only valid for values below optimal temperature for microbial activity
  # inputs:
  # name = value # units; description; assumptions
  k_STD = 1, # 1/d; decay rate at STP
  TW=10, # degC; water tempurature 
  theta=1.1, # exponential temperature factor 
  T_STD = 20 # tempurature at standard test conditions
){
  r_D = k_STD*theta**(TW-T_STD) # (d*-1) decay rate
  return(r_D)
}
# fn_decay(TW=(-4:30))


fn_sedimentation <- function(
  # returns: sedimentation rate (1/d)
  # source: Reddy and Delaune 2008 (adapted by Marois & Mitch 2016)
  # notes: 
  # inputs:
  # name = value; units; description; assumptions
  H_w=NA, # m; height of water above sediment surface; input
  u=fn_settlingvelocity() # m/d; particle settling velocity; laminar slow flow (Reddy & Delaune 2008)
){
  if(u>H_w){return(1)}
  r_sed = u/H_w # 1/day; settling rate for given water height
  return(r_sed)
}
#fn_sedimentation(.5)

fn_settlingvelocity <- function(
  
  # revised: 2020-04-01
  # returns: 
  # source: 
  # notes:
  # inputs:
  # name = value # units; description; assumptions
  
  k_rp=4.5e-7, # m; average radius of particles; input
  k_dp=2.65e6, # g/m^3; density of particles; 2.65e6 for minerals, 1.14e6 for organic matter (Delaune 1983)
  k_dw=1e6, # g/m^3; density of water; 1e6 for 0 salinity, 1.025e6 for 34 ppm salinity water
  mew=86.4e3, # g/m/d ; viscocity of water; viscocity at 20 degC
  g = 7.32e10 # m/d^2; acceleration due to gravity; constant
)
{
  u = (2/9*(g*k_rp^2)*(k_dp-k_dw))/mew # m/day; particle settling velocity
  return(u)
}
#fn_settlingvelocity()

fn_diffusion <- function(
  
  # returns: diffusion rate (1/day) from compartment 2 to compartment 1 
  # source: Wang et al. 2003 (adapted by Marois & Mitsch 2016)
  # notes:
  # - negative values = net loss by compartment 1 
  # - positive values = net gain by compartment 1
  # inputs:
  # name = value # units; description; assumptions
  
  C1 = 4, # g/m^3, concentration in compartment a; use mass of compartment divided by water volume
  C2 = 1, # g/m^3, concentration in compartment b; use mass of compartment divided by water volume
  phi_1 = 1, # -, porosity or density of compartment 1; use phi=1 if concentration is based on water volume in compartment
  kDiff= fn_kDiff(), # m^2/d; effective diffusion coefficient; standard tempurature and pressure
  area = 1, # m^2, the surface area of the compartments
  dz = 0.1 # m, the distance between compartment a and b OR the length of compartment b
){
  r_Diff = (2*phi_1*kDiff*area*(C2 - C1))/dz
  return(r_Diff)
}


fn_kDiff <- function(
  
  #  2020-03-31
  # returns: effective diffusion coefficient (day-1)
  # source: Wang et al. 2003 (adapted by Marois & Mitsch 2016)
  # notes:
  # inputs:
  # name = value # units; description; assumptions
  
  TW=10, # deg C ; temperature of water
  kDiff_STD = 2e-5, # m^2/d ; effective diffusion coefficient; calibrated value from Marois & Mitsch 2016
  mew = 86.4e3, # g/m/d; viscosity of water
  mew_STD = 86.4e3, # g/m/d; viscocity of water at standard temperature (20 degC)
  tempSTD = 20 # deg C; standard temperature
)
{
  kDiff = (mew_STD*kDiff_STD*(TW + 273))/((tempSTD+273)*mew)
  return(kDiff)
}
#fn_kDiff()

