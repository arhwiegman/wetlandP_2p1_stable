# **************************************************************
# filename: parameters.R
# description: declare parameter values and assemble in vector
# author: Adrian R.H. Wiegman
# revision date:  2024-06-18
# project: wetlandP
# repository: https://github.com/arhwiegman/wetlandP    
# notes:
# - currently using annual averages for VT for forcing parameters
# - added "path2userparameterfile" and "sub_compile_parameters_user"
# - together these allow the user to modify the parameters with just one line within xecute
# **************************************************************
sub_set_parameters_default <- expression({
  # SIMULATION SPECIFICATIONS -------------
  # name = value # units | description | assumptions
  version = "wetlandPv02" # chr | name of the model version 
  simname =  "demo" #"default"
  simtype = "static" # chr | charater string indicating the objective of the model run used to select model verions; "static" for steady sate, "forecast" for projections and scenario analysis, and "calibration" for training/calibration
  startday = 0 # d | julian day (0-365) of simulation start; based period of forcing data
  simyears = 14/365 # y | number of years in simulation; ""
  increment = 1 # d | number of days in each time step of model; if not equal to 1 then accuracy of simulation needs to be verified
  extended_outputs = T # logical | True/False indicating if the purpose of the run is to debug; if so writes extended outputs, this significantly slows the run
  inputdir = NULL # name of the path to the folder containing input data
  forcingfile = NULL # name of the input data file
  path2userparameterfile = NULL # name of file to used edit default parameters with user derived values
  solver = "lsoda" # chr | name of ode solver method "lsoda","euler","iter","rk4"
  
  
  # LOGICAL PARAMETERS ---------------------
  # SELECT PROCESSES TO BE SIMULATED ------------------
  # Parameters with "IO_" prefix turn process flow rates on and off (toggle)
  # must be logical T/F TRUE/FALSE
  # name = expression # units | description | assumptions
  # water 
  IO_Q_in = T # logical T/F or 1/0 | toggles surface inflow | 
  IO_Q_precip = T # logical T/F or 1/0 | toggle precipitation | 
  IO_Q_ground = T # logical T/F or 1/0 | toggles net groundwater flow (percolation  - infiltration)| 
  IO_Q_ET = T # logical T/F or 1/0 | toggles evapotranspiration | 
  IO_Q_out = T # logical T/F or 1/0 | toggles surface outflow | 
 
  # plant, sediments and phosphorus
  IO_assim_shootP = T # logical T/F or 1/0 | toggles assimilation of shoot P | 
  IO_assim_rootP = T  # logical T/F or 1/0 |  toggles growth of root P |
  IO_mort_shootP2litterP = T # logical T/F or 1/0 | toggles mortality of shoots | 
  IO_mort_rootP2LOP = T # logical T/F or 1/0 | toggles mortality of root P to LOP |
  IO_mort_rootP2ROP = T # logical T/F or 1/0 | toggles mortatlity of root P |
  IO_sed_IM = T # logical T/F or 1/0 | toggles sedimentation of inorganic matter | 
  IO_sed_OM = T # logical T/F or 1/0 | toggles sedimentation of refractory organic P
  IO_decay_litter = T # logical T/F or 1/0 | toggles decomposition of litter P to refractory organic P | 
  IO_decay_LOP = T # logical T/F or 1/0 | toggles decomposition of labile OP | 
  IO_decay_ROP = T # logical T/F or 1/0 | toggles decomposition of refractory OP | 
  IO_diffus = T # logical T/F or 1/0 | toggles diffusion of DIP from b to a | 
  IO_adsorp = T  # logical T/F or 1/0 | toggles adsorption of DIP onto PIP | 
  
  IO_in_IM = T # g d.w./d | inflow of inorganic matter as ISS | 
  IO_in_PIP = T # g P/d | inflow of PIP | 
  IO_in_LOP = T # g P/d | inflow of labile organic P | 
  IO_in_ROP = T # g P/d | inflow of recalcitrant organic P | 
  IO_in_DIP = T # g P/d | inflow of dissolved inorganic P | 
  IO_out_IM = T # g P/d | outflow of IM | 
  IO_out_PIP = T # g P/d | outflow of PIP | 
  IO_out_LOP = T # g P/d | outflow of LOP | 
  IO_out_ROP = T # g P/d | outflow of ROP | 
  IO_out_DIP = T # g P/d | outflow of DIP |
  
  # process functions 
  IO_variable_k_E = T # turns on variable calculation of k_E
  IO_variable_k_Ex_max = F # turns on variable calculation of k_Ex_max
  IO_anoxic = F # turns on/off anaerobic conditions for DIP_E concentration 
  IO_DIP_E_langmuir = F # turns on the use of langmuir model for 
  IO_variable_DIP_E = F # turns off calculation of DIP_E
  IO_Q_net = T # turn this off if inflows and outflows are known
  IO_HRT_power_model = F
  IO_Hx = F # amplifies water level by x by selecting different hydroclimate file
  # SELECT FORCINGS TO USE ------------------------- 
  # toggle whether to use parameter values for forcings or time series inputs
  use_k_TW = F 
  
  
  # LOCAL -------------------
  # hydroclimate 
  # geometric
  area = 1 # m^2| wetland surface area| uniform flat surface 
  H_b = 0.1 # m| height of belowground compartment (sediment column)| NA
  H_w = 0 # m| height of water relative to sediment surface| NA
  H_a = H_w # m| height of aboveground compartment | NA
  Z_a = 0 # m| elevation NAVD88 of sediment surface (bottom of aboveground compartment) |NA
  Z_c = Z_a + H_w # m: water elevation NAVD88 of adjacent channel | NA
  Z_b = Z_a - H_b  # m| elevation NAVD88 of the belowground compartment bottom |NA
  Z_w = Z_a + H_w  # m| elevation NAVD88 of water surface | NA
  # climate
  TW_max = 23 # degC| max daily mean water tempurature| fit to site data or assume equal to mean air temp on Aug 1
  TW_min = 4 # degC| min daily mean water tempurature| fit to site data or assume equal to 4 degC on Jan 31
  TA_h_max = 28 # degC| max daily high air tempurature| fit to data from local city/town (e.g. https://weatherspark.com/)
  TA_h_min = -2 # degC| min daily high air tempurature| ""
  TA_l_max = 17 # degC| max daily low air tempurature| ""
  TA_l_min = -11 # degC| min daily low air tempurature| ""
  T_SD = 3 # degC| standard deviation of random effect on temperature |  ""
  T_clim = 0 # degC| slope of climate change effect on temperature | ""
  t_T = 365 # d | period of temperature oscilation| standard 
  t_T_l = 31 # d | day of year with minimum tempurature| assume Jan 31st coldest day
  TA_h = 13 # degC| daily high air tempurature| ""
  TA_l = 0 # degC| daily low air tempurature| "" 
  RHmin = 60 # %, minimum relative humidity | ""
  RHmax = 80 # %, maximum relative humidity | ""
  Precip_monthly = 44/12 # inches| monthly rainfall total | ""
  ns = 2295/12 # hours| daily time of sunshine | ""
  # hydrologic
  k_TW = (TA_h+TA_l)/2 # degC| temperature of water| average value (static) input data measured with HOBO (calibration) or function of time with seasonal, random and climate change terms (forecast)  
  Q_in = 0 # m^3/d| inflow volume| used in static or forecast simulations
  Q_out = 0 # m^3/d | outflow volume |
  Q_precip = Precip_monthly * 12 / 365 * 2.54 / 100 * area # m^3| inflow volume| used in static or forecast simulations
  Q_ET = Q_precip # m/d| precipitation |assume equal to precip
  Q_ground = 0 # m^3/d| groundwater flow (percolation - infiltration)| assume zero for soils with low sand
  k_Q_out = 0.01 # 1/d| outflow coefficient| change based on friction of channel interface
  k_HRT = 1e3 # 1/d| residence time of wetland surface water| "" 
  
  # concentrations
  k_TSS = 15 # g/m^3| total suspended solids of inflow | based on field data, median of observations, 3.5 at prindle, 23.8 at union st, 12.25 at swamp rd
  # soil
  k_TP = 0.05 # g P/m^3 | TP concentration (mg P /L) in inflow | 0.071 at prindle, 0.059 at union, 0.056 at swamp
  k_LOI = 0.20 # g/g| initial fraction of organic matter in total mass of below ground compartmentmeasured as soil loss-on-ignition | .15 - .24, .30 - .16 union, .136 - 0.299
  k_PSR = 0.20 # mol/mol | P Saturation Ratio olar ratio P/(Al + Fe) Nair et al. 2004 | fit to field data, prindle  0.09 - 0.15, union  0.08 - 0.13, swamp rd 0.11 - 0.26
  k_Ex_max = 4 # g/kg | maximum sorption capacity measured as oxlate 31*(OxAl/27 + OxFe/56) | ranging from 3.3 - 5.5 prindle, 5.0 - 6.4 union, 3.44 - 5.1 swamp   
  k_clay = 0.1 # g/g | clay content of inorganic matter | from soil textural analysis OR from NRCS soil survey units texture class | .11 to 0.35, .0875 to 0.15 union, 0.075 - .15 swamp
  k_f_fines = 0.90 # g/g| silt + clay, fine sediment fraction of incoming total suspended solids| fit to field data | 0.627 - .84 prindle, 0.84 - 0.97 union, 0.75 - 0.985 swamp rd. 
  if(k_f_fines < k_clay ){stop("silt + clay must not be greater than 1")}
  k_f_OSS = 0.5 # g/g| organic matter fraction of incoming total suspended solids| fit to field data, %65 at prindle rd, 23% at union st, 54% swamp rd. 
  k_f_SRP = 0.3 # g SRP /g TP| fraction of TP as SRP in influent water | based on field data 0.404 at prindle, 0.25 at union, 0.27 at swamp rd. 
  k_DIP_E = 0.05 # equilibrium DIP_E
  
  # UNIVERSAL CONSTANTS ----------------------------
  k_dp_i = 2.65e6 # g/m^3| particle density of inorganic matter| Delaune et al. 1983 g/cm^3 * 10^6 cm^3/m^3
  k_dp_o = 1.14e6 # g/m^3| particle density of inorganic matter| Delaune et al. 1983 g/cm^3 * 10^6 cm^3/m^3
  k_db_i = 1.99e6 # g/m^3| bulk density of inorganic matter| Morris et al. 2016 
  k_db_o = 0.085e6 # g/m^3| bulk density of organic matter| Morris et al. 2016
  k_pi = 3.141593 # | arc length of a circle | 
  k_g = 7.32e10  # m/d^2| acceleration due to gravity| constant
  k_mew = 86.4e3  # g/m/d| viscosity of water | standard value
  k_mew_STD = 86.4e3  # g/m/d| viscocity of water at standard temperature (20 degC)
  k_dw=1e6  # g/m^3| density of water| 1e6 for 0 salinity, 1.025e6 for 34 ppm salinity water
  k_diff= 1.931741e-05 # m^2/d| effective diffusion coefficient| standard tempurature and pressure see fn_kDiff
  k_theta	= 1.07	#factor	exponential temperature factor	Wang & Mitsch 2000; Hantush et al. 2011
  k_T_STD =	13.75	#deg C|	standard temperature for metabolic processes |	calibrated to make actual NPP match ANPPmax, since experiments were conducted under field conditions this parameter is equal to the  (maximum daily average temp - minimum daily average temp)/2 + minimum daily average temp ~ 15 - 17 degrees
  
  
  # STOCHASTIC --------------------------
  # inflow concentrations
  k_SRP2PIP = 0.98 # g P/d.w. | ratio of LOP to SRP | 8.9e-1 for prindle, 1.42 for swamp rd, 6.2e-1 for union st
  k_ISS2P = 0.0013 # g P/d.w. | P content of inorganic suspended sediments |   site data  0.002 for prindle rd, 0.0009 for union st, 0.00094 for swamp rd
  
  # Biomass Growth & Mortality
  k_shootM = 0 # g dw/m2| shoot live biomass | need to set up a way to get this to vary based on start time
  k_rootM = 1000 # g dw/m2| shoot live biomass | need to set up a way to get this to vary based on start time
  k_BM2P = 0.001 # g/g P/d.w.| P content of biomass| McJannet et al. 1996 .001 - 0.003; Morris & Bowden 1986 0.002; Ch 2 data 0.001 to 0.003
  k_f_G_shoot =	0.5	# fraction | fraction of NPP allocated to shoot growth (shoot_NPP/total_NPP) |	Morris et al 1984 0.2 - 0.5
  k_NPP = 1500 # g m-2 y-1| combined annual rate of NPP for above and below ground biomass | Morris et al. 1984 1000 to 4000 
  k_ADNPP =	k_NPP/365	# g m-2 d-1| average daily rate of NPP | divide k_NPP by 365
  k_M = 0.001 # 1/day |	rate of baseline biomass mortality calibrated to root mass ~1000 - 2000 g m-2 and peak shootM ~300-800 g m-2 at use 0.003 for k_ANPPmax = 3000, with guidance from	Morris et al 1984 0.003 to 0.007; Marois & Mitsch 2016 0.0005 - 0.007
  #Morris, J. T., Houghton, R. A., & Botkin, D. B. (1984). Theoretical limits of belowground production by Spartina alterniflora: An analysis through modelling. Ecological Modelling. https://doi.org/10.1016/0304-3800(84)90068-1
  k_M_root = 	k_M	# 1/day |	rate of baseline biomass mortality | calibrated to root mass ~1200 g m-2 with guidance from	Morris et al 1984; Marois & Mitsch 2016 0.0005 - 0.007
  k_M_shoot = k_M		# 1/day |	rate of baseline biomass mortality | calibrated to peak shootM ~600 g m-2  with guidance from	Morris et al 1984 0.0005 - 0.007
  k_M_shoot_T_mult =	50 #	factor|	multiplier for shoot mortality after temp drops below threshold |	calibrated to field observations
  k_T_thresh_M_shoot =	6 #	deg C |	temperature at which shoot mortality increases	| calibrated to field observations
  

  # diffusion 
  k_whc = 1e-3 # | a small volume of water to prevent errors associated with empty compartments| best guess based on fit of oven dry verses air dry moisture content
  k_diff_STD = 1e-1  # m^2/d | effective diffusion coefficient| calibrated to intact core data; Marois & Mitsch 2016 calibrated value was 2e−5 m2 d−1
  
  # adsorption
  k_ad = 1.75 # 1/d | adsorption first order rate coefficient | Wang et al. 2003 1.75, Marois & Mitsch 2016 used 
  k_E = .56 # m^3/g | langmuir constant of adsorption (bond energy) | Calibrated to intact core data this value depends on what metric is used to define Ex_max,  Wang et al. 2003 2.75 m3 kg-1
  k_PIP2Ex = 1  # g/g| ratio of exchangeable P to particulate inorganic P | Wang et al. 2003 0.8
  
  # decay
  k_f_labile = 0.8 # g/g| labile fraction organic matter | Morris & Bowden 1986 refractory fraction of 0.2
  k_f_LOM_OSS = k_f_labile  # g/g| labile fraction incoming organic suspended solids| fit to field data
  k_f_LOM_BM = k_f_labile  # g/g| labile organic matter fraction of falling biomass| Morris & Bowden 1986
  k_f_labile_litter = k_f_labile # g/g | labile fraction of litter | Based on IP2TP ratio of biomass
  k_f_labile_root = k_f_labile  # g/g | labile fraction of roots at STD temp |   Based on IP2TP ratio of biomass
  k_f_labile_OSS = k_f_labile  # g/g | labile fraction of OSS | Based on IP2TP ratio of biomass
  k_OM2P = k_BM2P # g/g  P/d.w.| P content of OM| best guess as of now we will be informed by field data
  k_LOM2P = k_BM2P # g P/d.w. | P content of labile organic matter |   site data
  k_ROM2P = k_BM2P # g P/d.w. | P content of refractory organic matter |   site data
  k_decay_litter = 0.01 # 1/d | litter decomposition rate coefficient at STD temp| Morris & Bowden, Wiegman Ch 3, # Longhi et al. 2008 k = ranged from 0.01 1/d to 0.0027 1/d
  k_decay_LOP = 0.01  # 1/d | LOP decomposition rate coefficient at STD temp|   Marois & Mitsch 2016 DOP rate is 0.01, while LPOP rate is 0.003, since we do not model DOP LOP decay should be between 0.001 - 0.01
  k_decay_ROP = 1e-5 # 1/d | ROP decomposition rate coefficient at STD temp when soils are unsaturated and aerobic(H_w < 0) | Morris & Bowden 1986 assume refractory OM does not decompose, however this is assuming saturated soils, so we assume that when H_w < 0 that ROP decomposes at between 1e-5 and 5e-5 based on value from Marois & Mitsch 2016 of 2.5e-5
  
  # sedimentation
  k_rp_o = 4.5e-7  # m| average radius of organic particles| Marois & Mitsch 2016
  k_rp_i = fn_particle_radius(sand=1-k_f_fines,silt=k_f_fines-k_clay,clay=k_clay)  # m| average radius of inorganic particles|
  k_u_o = fn_settlingvelocity(k_rp = k_rp_o,k_dp = k_dp_o) # m/d| organic particle settling velocity| assumed constant laminar/slow flow (Reddy & Delaune 2008); fn_settlingvelocity(k_dp=k_dp_o)
  k_u_i = fn_settlingvelocity(k_rp = k_rp_i,k_dp = k_dp_i) # m/d| inorganic particle settling velocity| assumed laminar/slow flow (Reddy & Delaune 2008) ; see fn_settlingvelocity(k_dp=k_dp_i)
})

sub_check_process_IOs <- expression({
  # Check that IOs are of type logical
  . <- ls() %>% str_extract_all("^IO_.*",simplify=T)
  actual_names <- .[.!=""]
  needed_names <- c(
    'IO_Q_in',
    'IO_Q_precip',
    'IO_Q_ground',
    'IO_Q_ET',
    'IO_Q_out',
    'IO_assim_shootP',
    'IO_assim_rootP',
    'IO_mort_shootP2litterP',
    'IO_mort_rootP2LOP',
    'IO_mort_rootP2ROP',
    'IO_sed_IM',
    'IO_sed_OM',
    'IO_decay_litter',
    'IO_decay_LOP',
    "IO_decay_ROP",
    "IO_diffus",
    "IO_adsorp",
    "IO_in_IM",
    "IO_in_PIP",
    "IO_in_LOP",
    "IO_in_ROP",
    "IO_in_DIP",
    "IO_out_IM",
    "IO_out_PIP",
    "IO_out_LOP",
    "IO_out_ROP", 
    "IO_out_DIP")
  
  for (i in needed_names){
    if(!i %in% actual_names){
      stop(paste("Error:",i,"needs to be specified, enter it as a parameter value"))
    }
  }
  for (i in actual_names){
    IO_check = eval(parse(text=paste0("is.logical(",i,")")))
    if(!IO_check){
      stop(paste("Error:",i,"is not logical, please enter TRUE or FALSE to turn process on or off"))
    }
  }
})

sub_mget_parameters_from_globenv_vectors <- expression({
  # this subroutine compiles the vector objects from the global environment into one list called parameters
  . = names(Filter(is.vector, mget(ls(all=T))))
  . <- .[!str_detect(.,"^sub_")]
  . <- .[!str_detect(.,"^parameters$")]
  parameters <- mget(.)
})

sub_compile_parameters_default <- expression({
  eval(sub_set_parameters__default)
  eval(sub_check_process_IOs)
  eval(sub_mget_parameters_from_globenv_vectors)
})

sub_set_user_parameters <- expression({
  # if a script to modify input parameters has been declared then this subroutine will run that script
  if(!is.null(path2userparameterscript))
  try(source(path2userparameterscript))
})

sub_compile_parameters_user <- expression({
  # initialize default parameter values
  eval(sub_set_parameters_default)
  
  # check that processes IOs are logical
  eval(sub_check_process_IOs)
  
  # creates a list containing all parameter vectors
  eval(sub_mget_parameters_from_globenv_vectors)
  
  # provide user input now that defaults have been set
  eval(sub_set_user_parameters)
  
  # update the parameters object with user inputs
  eval(sub_mget_parameters_from_globenv_vectors)
})


# run the subroutines to compile parameters into a list
eval(sub_compile_parameters_user)




