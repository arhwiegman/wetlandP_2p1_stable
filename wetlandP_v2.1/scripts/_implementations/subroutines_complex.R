# **************************************************************
# filename: subroutines
# description: subroutines of the model
# author: Adrian R.H. Wiegman 
# revision date: 2020-05-15
# project: wetlandP_v04
# repository: https://github.com/arhwiegman/wetlandP
# notes:
# - the subroutines are declared as expressions, 
#   which draw from and manipulate variables in the local environment 

# **************************************************************

# INITIALIZE ------------
sub_init_states <- expression({ 
  
  # ABOVE GROUND STOCKS
  shootM = k_shootM*area # g d.w. ; shoot dry mass
  shootP = k_shootM*k_BM2P
  litterM = 0.5*k_shootM*area
  litterP = litterM*k_BM2P
  LOP_a = ROM_a*k_BM2P*(k_f_labile)
  ROM_a = (H_w*area)*OSS # g d.w.; recalcitrant organic sediment mass aboveground;
  IM_a = (H_w*area)*ISS
  DIP_a = (H_w*area)*SRP
  LOM_a = LOP_a/k_BM2P # g d.w.;labile organic sediment mass aboveground; 
  ROP_a = ROM_a*k_BM2P*(1-k_f_labile)
  PIP_a = IM_a*k_ISS2P + (H_w*area)*SRP*k_SRP2PIP
  
  # BELOW GROUND STOCKS
  rootM = k_rootM*area # g d.w.; root dry mass
  TM_b = 1/(k_LOI/k_db_o+(1-k_LOI)/k_db_i)*(H_b*area) # g d.w.; bulk density * volume; Morris et al. 2016
  LOM_b = TM_b*k_LOI*(0.001) # This is based on a POXC (active C) of 1g/kg
  ROM_b = TM_b*k_LOI*(1-0.001) # This is based on a POXC (active C) of 1g/kg
  IM_b = TM_b*(1-k_LOI)
  rootP = k_rootM*k_BM2P 
  LOP_b = LOM_b*k_BM2P
  ROP_b = ROM_b*k_BM2P
  
  if(IO_variable_k_Ex_max){
    Ex_max = 31*fn_AlFe_ox_fines(
      LOI = k_LOI,
      k_f_fines = k_f_fines)/1000
  }else{Ex_max = k_Ex_max}
  if(IO_variable_k_E){
    E = fn_k_E_statmodel(
      k_LOI = k_LOI,
      k_PSR = k_PSR,
      k_Ex_max = Ex_max)
  }else{E = k_E}  
  PIP_b = (Ex_max/k_PIP2Ex)*k_PSR*(TM_b*10^-3)
  DIP_b = 0 # calculated again below after water volume is determined
  
  # INTERMEDIATE STATE VARIABLES
  Z_w = forcings$Z_w(t)
  eval(sub_intermediates)
  
  
  # HYDROLOGIC VARIABLES
  # assemble list of inputs (X) to hydrology subroutine
  # Calculate water volumes of each compartment
  # Volume
  # calculate W (water volumes, m^3) above and below ground
  V_w = H_w*area*phi_a + H_b*area*phi_b
  d_V_w = 0
  vars_hydro = eval(sub_hydro)
  vars_hydro
  
  
  # STATE VARIABLES DEPENDANT ON WATER VOLUME IN SEDIMENTS
  DIP_b = DIP_E*V_w_b
  
  states <-c(
    # Water
    V_w = V_w,# aboveground
    
    # Solids
    IM_a=IM_a,
    IM_b=IM_b,
    
    # Phosphorus 
    shootP=shootP,    # aboveground
    litterP=litterP,
    ROP_a=ROP_a,
    LOP_a=LOP_a,
    PIP_a=PIP_a,
    DIP_a=DIP_a,
    rootP=rootP,   # belowground
    ROP_b=ROP_b,
    LOP_b=LOP_b,
    PIP_b=PIP_b,
    DIP_b=DIP_b)
})

sub_init_extended <- expression({
  # Water 
  d_V_w = NA# aboveground
  
  # Solids
  d_IM_a=NA
  d_IM_b=NA
  # Phosphorus 
  # aboveground
  d_shootP=NA
  d_litterP=NA
  d_ROP_a=NA
  d_LOP_a=NA
  d_PIP_a=NA
  d_DIP_a=NA
  # belowground
  d_rootP=NA
  d_ROP_b=NA
  d_LOP_b=NA
  d_PIP_b=NA
  d_DIP_b=NA
  eval(sub_compile_derivs)
  
  r_sed_IM=NA
  r_sed_OM=NA
  r_diffus=NA
  r_decay_litter=NA
  r_decay_LOP=NA
  r_decay_ROP=NA
  r_mort_shoot=NA
  r_mort_root=NA
  r_adsorp=NA
  r_assim=NA
  eval(sub_compile_extended)
  outgroups <- list(states=states,derivs=derivs,rates=rates,vars_mass=vars_mass,vars_elev=vars_elev,vars_pore=vars_pore,vars_hydro=vars_hydro)
  write(names(outs),outfile_ext,append=FALSE,ncolumns=length(outs),sep=",")
})

## FORCINGS ---------------------
sub_forcings <- expression({
  # atmospheric
  #TA_h = forcings$TA_h(t) # degC; daily high air tempurature; ""
  #TA_l = forcings$TA_l(t) # degC; daily low air tempurature; ""
  #RHmin = forcings$RHmax(t) # %, minimum relative humidity
  #RHmax = forcings$RHmin(t) # %, maximum relative humidity
  #Precip_monthly = forcings$Precip_monthly(t) # inches; monthly rainfall total
  #ns = forcings$ns(t) # hours; daily time of sunshine 
  # hydrologic
  TW = forcings$TW(t) # degC; temperature of water; average value (static) input data measured with HOBO (calibration) or function of time with seasonal, random and climate change terms (forecast)
  #V_w = forcings$V_w(t)
  #d_V_w = forcings$d_V_w(t)
  #Z_w = forcings$Z_w(t) # m; elevation of water in wetland; data measured with HOBO used in calibration
  #Z_c = forcings$Z_c(t) # m; elevation of water in channel adjacent to wetland; sometimes used in forecast simulations
  Q_net = forcings$Q_net(t) # m^3; net inflow - outflow 
  Q_out = forcings$Q_out(t) # m^3; inflow volume; used in static or forecast simulations
  Q_in = forcings$Q_in(t) # m^3; inflow volume; used in static or forecast simulations
  Q_precip =  forcings$Q_precip(t) # m^3; inflow volume; used in static or forecast simulations
  Q_ET = forcings$Q_ET(t) # m^3; evapotranspiration flux; Preistly-Taylor method requires Evapotranspiration package
  Q_ground = forcings$Q_ground(t) # m^3; groundwater flux; NA
  TSS = forcings$TSS(t) # g/m^3; total suspended solids of inflow ; model fit to field data from each site based on Q.in
  OSS = forcings$OSS(t)
  ISS = forcings$ISS(t)
  SRP = forcings$SRP(t)
  HRT = forcings$HRT(t) # 1/d; residence time of wetland surface water; "" 
})

## INTERMEDIATES -----------------
sub_intermediates <- expression({
  # Aggregate mass of different compartments
  shootM = shootP/k_BM2P # g d.w. | shoot dry mass | 
  rootM = rootP/k_BM2P # g d.w. | root dry mass | 
  litterM = litterP/k_BM2P # g d.w. | litter dry mass | 
  LOM_a = LOP_a/k_LOM2P # g d.w. | labile OM aboveground | 
  ROM_a = ROP_a/k_ROM2P # g d.w. | refractory OM aboveground | 
  LOM_b = LOP_b/k_LOM2P # g d.w. | labile OM belowground | 
  ROM_b = ROP_b/k_ROM2P # g d.w. | refractory OM belowground | 
  OM_a = sum(shootM,litterM,LOM_a,ROM_a) # g d.w. | OM aboveground | 
  OM_b = sum(rootM,LOM_b,ROM_b) # g d.w. | OM belowground | 
  BM = sum(shootM,rootM) # g d.w. | total biomass | 
  TM_a = IM_a+OM_a # g d.w. | total mass aboveground | 
  TM_b = IM_b+OM_b # g d.w. | total mass belowground | 
  TM = TM_a + TM_b # g d.w. | total mass | 
  TP = sum(shootP,rootP,litterP,LOP_a,LOP_b,ROP_a,ROP_b,PIP_a,PIP_b,DIP_a,DIP_b) # g P | total P | 
  
  # shoot canopy dimensions
  #shootV = shootM/k_dp_o # m^3; shoot biomass volume; Delaune et al. 1983 need to verify particle density of live biomass
  #shootA = shootdensity*pi*(shootdiameter/2)^2 # m^2/m^2; basal area of shoots; NA
  #shootH = shootV/shootA # m; average shoot height
  
  # Organic matter content 
  LOI_a = OM_a/(TM_a) # g/g; organic matter content belowground
  LOI_b = OM_b/(TM_b) # g/g; bulk density belowground; Morris et al. 2016 
  
  # Bulk density
  BD_b = fn_bulkdensity(LOI_b,
                        k_db_i=k_db_i,
                        k_db_o=k_db_o) # g/g; bulk density belowground; Morris et al. 2016 
  
  # Volume, Height & Elevation
  H_b = TM_b/BD_b/area # m; height of the below compartment
  Z_a = H_b + Z_b # m; elevation of the aboveground compartment (sediment surface elevation)
  H_w = Z_w-Z_a # m; height of water relative to sediment surface elevation
  H_a = H_w # m; height of the above ground compartment; the greater value of the height of water and the height of shoots
  
  #if (shootH > H_w){
  #  k_f_submerged = 1
  #} else {
  #  k_f_submerged = H_w/shootH
  #}
  
  # Porosity
  PD_a = fn_particledensity(LOI_b,
                            k_dp_i=k_dp_i,
                            k_dp_o=k_dp_o)
  PD_b = fn_particledensity(LOI_b,
                            k_dp_i=k_dp_i,
                            k_dp_o=k_dp_o)
  phi_a = 1 #- ((shootM*k_f_submerged+litterM+LOM_a+ROM_a)/k_dp_o+IM_a/k_dp_i)/(H_a*area) # m^3/m^3; porosity (void fraction) of compartment a; 
  phi_b = 1 - (TM_b/PD_b)/(H_b*area) # m^3/m^3; porosity (void fraction) of compartment b; 
  
  # Pore volume 
  V_pore_a = H_a*area*phi_a # m^3; volume of pore space aboveground
  V_pore_b = H_b*area*phi_b # m^3; volume of pore space belowground
  
  # temperature factor
  k_TW = fn_decay(TW=TW,theta=k_theta)
  
  # P Saturation Ratio
  Ex_a = PIP_a/(TM_a*1e-3)
  PSR_a = Ex_a/Ex_max_a
  Ex_b = PIP_b/(TM_b*1e-3)
  PSR_b = Ex_b/Ex_max_b
  # adsorption rate of DIP to PIP
  if(IO_variable_k_Ex_max){
    AlFe_ox_a = fn_AlFe_ox_fines(LOI = LOI_a,
                               k_f_fines = k_f_fines)
    # Al + Fe (mmol/kg) increases as a function of fine sediment and LOI 
    # Updated AlFe ~ LOI*(1-k_f_fines) relationship based on data from Ch1 and perillo
    Ex_max_a = 31*AlFe_ox/1000
    AlFe_ox_b = fn_AlFe_ox_fines(LOI = LOI_b,
                                 k_f_fines = k_f_fines)
    # Al + Fe (mmol/kg) increases as a function of fine sediment and LOI 
    # Updated AlFe ~ LOI*(1-k_f_fines) relationship based on data from Ch1 and perillo
    Ex_max_b = 31*AlFe_ox/1000
    }
  
  if(IO_DIP_E_langmuir){
    if(IO_variable_k_E){
      # DIP calculated from Langmuir model (Wang et al. 2003)
      # Langmuir bond energy modeled based on statistical fit of soil data to k_E estimates of intact cores across 20 sample sites
      E_a = fn_k_E_statmodel(k_LOI=LOI_a,
                           k_PSR=PSR_a,
                           k_Ex_max = Ex_max_a)
      E_b = fn_k_E_statmodel(k_LOI=LOI_b,
                             k_PSR=PSR_b,
                             k_Ex_max = Ex_max_b)
    }
    # DIP calculated from power model of DIP = a(Smax - S)(1-PSR)^(-b)
    # Defined operationally as SRP = a((OxAl/27+OxFe/56) - OxP/31)/(1-OxPSR)*^(-b)
    DIP_E_a = fn_DIP_E_langmuir(Ex=Ex_a,
                              Ex_max=Ex_max_a,
                              E=E_a)
    DIP_E_b = fn_DIP_E_langmuir(Ex=Ex_b,
                                Ex_max=Ex_max_b,
                                E=E_b)
  }else{
    DIP_E_a = fn_DIP_E_power_law(anoxic=IO_anoxic_a,
                               Ex_max = Ex_max_a,
                               Ex = Ex_a)
    DIP_E_b = fn_DIP_E_power_law(anoxic=IO_anoxic_b,
                                 Ex_max = Ex_max_a,
                                 Ex = Ex_a)
  }
  DIP_E_a = ifelse(IO_variable_k_E,k_DIP_E,DIP_E_a)
  DIP_E_b = ifelse(IO_variable_k_E,k_DIP_E,DIP_E_b)
})

## HYDROLOGY ---------------------
sub_hydro_outs <- expression({
  vars_hydro <- c(
    Z_w = Z_w,
    H_w = H_w,
    V_w_b = V_w_b,
    V_w_a = V_w_a,
    V_w = V_w,
    d_V_w = d_V_w,
    Q_in = Q_in,
    Q_out = Q_out,
    Q_ET = Q_ET,
    Q_precip = Q_precip,
    Q_ground = Q_ground)
})

sub_hydro <- expression({
  # revised: 2021-09-23
  # returns:"outputs" a list of hydrologic parameters 
  # for static simulation with Z_w as hydrologic forcing
  # (see "outputs" in function body)
  # source: Adrian R. H. Wiegman
  # notes:
  # d_V_w is solved based Qs that are forced
  # inputs: are pulled from the global environment
  # name = value, # units; description; assumptions
  # Calculations
  # partition water volume into compartments
  eval(sub_hydro_partition_V_w)
  # control water flow 
  # and toggle process rates
  # to avoid errors
  eval(sub_hydro_control)
})

sub_hydro_control <- expression({
  # Adjust Q_in and Q_out with through flow
  if(IO_Q_net){
    if(IO_HRT_power_model){
      Z_w = H_w + Z_a
      HRT = (k_HRT_a)*Z_W^(-k_HRT_b)
    }
    Q_HRT = (V_w>0)*V_w/HRT
    Q_out = (V_w>0)*ifelse(Q_net>=0,Q_HRT,Q_HRT-Q_net)
    Q_in = (V_w>0)*ifelse(Q_net>=0,Q_HRT+Q_net,Q_HRT)
  }
  
  
  if(V_w_a < k_whc*V_pore_a){
    V_w_a = k_whc*V_pore_a # keep some adhered water
    V_w = V_w_a
    if(V_w_b < k_whc*V_pore_b) {
      V_w_b = k_whc*V_pore_b
    }
    # Turn off water related processes
    IO_sed_IM = F # sedimentation
    IO_sed_OM = F 
    IO_diffus = F # diffusion
    IO_adsorp = F # adsorption
  } else {
    # Restore inital related processes
    IO_sed_IM = parameters$IO_sed_IM # sedimentation
    IO_sed_OM = parameters$IO_sed_OM 
    IO_diffus = parameters$IO_diffus # diffusion
    IO_adsorp = parameters$IO_adsorp # adsorption
  }
  if(V_w < 0 & d_V_w < - V_w){
    cat(paste("t=",t,"Warning negative V_w:",V_w,"\nadding Q_ground to balance"))
    Q_ET = 0
    Q_ground = -V_w + V_w_b
  }
})



sub_hydro_partition_V_w <- expression({
  # calculate water volumes in each compartment
  # revised: 2021-09-23
  # calculate new water height and elevation
  if (V_w < V_pore_b){ 
    # if water volume less than pore volume of B
    Z_w = V_w/(area*phi_b) + Z_b
    V_w_b = phi_b*area*(Z_w-Z_b)
    V_w_a = V_w - V_w_b
  }else{
    # if water volume greater than pore volume of B
    Z_w = (V_w-V_pore_b)/(area*phi_a) + Z_a
    V_w_b = V_pore_b
    V_w_a = V_w - V_w_b
  }
  H_w = Z_w - Z_a # calculate water height from different in between soil and water elevation
})

sub_hydroforc_Z_w <- expression({
  # revised: 2020-04-02
  # returns:"outputs" a list of hydrologic parameters 
  # for static simulation with Z_w as hydrologic forcing
  # (see "outputs" in function body)
  # source: Adrian R. H. Wiegman
  # notes:
  # Z_w is the forcing function, Q.in and Q.out are solved
  # inputs: are pulled from the global environment
  # name = value, # units; description; assumptions
  
  # Volume
  # calculate V (water volumes, m^3) above and below ground
  eval(sub_hydro_V_w_from_Z_w)
  # change in water volume
  d_V_w = V_w_a + V_w_b - V_w
  Q_net = area*(porosity*dHw - (ip - ET)/100)
  Q_out = ifelse(Qnet<0-Vw/HRT,Qnet,-V_w/tau)
  Q_in = ifelse(Qnet>=0+Vw/HRT,Qnet,V_w/tau)
  # new water volume
  V_w = d_V_w
  if(Vw < 0){
    Qout = Qout + dVw
    dVw = 0
    Vw = 0
  }
  if (Q_in<0){warning("ERROR Q_in < 0!!!")}
})

sub_hydroforc_calibration <- expression({
  # revised: 2020-04-02
  # returns:"outputs" a list of hydrologic parameters 
  # for static simulation with Z_w as hydrologic forcing
  # (see "outputs" in function body)
  # source: Adrian R. H. Wiegman
  # notes:
  # Z_w is the forcing function, Q.in and Q.out are solved
  # inputs: are pulled from the global environment
  # name = value, # units; description; assumptions
  
  # Volume
  # calculate V (water volumes, m^3) above and below ground
  # calculate water volumes from water level 
  # revised: 2020-05-07
  adhered_a = k_whc
  adhered_b = k_whc
  if (Z_w > Z_a){ 
    # if water depth greater than zero___ 
    V_w_a = V_w
    V_w_b = phi_b*area*(H_b)
  } else if (Z_w <= Z_a & Z_w >= Z_b) { 
    V_w_b = phi_b*area*(Z_w-Z_b)
  }
  V_w_a = ifelse(V_w_a < adhered_a,adhered_a,V_w_a)
  V_w_b = ifelse(V_w_b < adhered_b,adhered_b,V_w_b)
  # change in water volume
  eval(sub_hydro_outs)
})


sub_hydro_Z_w_from_V_w <- expression({
  # calculate new water height and elevation
  if (V_w < V_pore_b){ 
    # if water volume less than pore volume of B
    Z_w = V_w/(area*phi_b) + Z_b
  }else{
    # if water volume greater than pore volume of B
    Z_w = (V_w-V_pore_b)/(area*phi_a) + Z_a
  }
  H_w = Z_w - Z_a
})

sub_hydro_V_w_from_Z_w <- expression({
  # calculate water volumes in each compartment from water level 
  # revised: 2020-05-07
  adhered_a = k_whc
  adhered_b = k_whc
  if (Z_w > Z_a){ 
    # if water depth greater than zero___ 
    V_w_a = phi_a*area*(Z_w-Z_a)
    V_w_b = phi_b*area*(H_b)
  } else if (Z_w <= Z_a & Z_w >= Z_b) { 
    V_w_b = phi_b*area*(Z_w-Z_b)
  }
  V_w_a = ifelse(V_w_a < adhered_a,adhered_a,V_w_a)
  V_w_b = ifelse(V_w_b < adhered_b,adhered_b,V_w_b)
})

sub_hydroforc_V_w <- expression({
  # Volume
  # calculate W (water volumes, m^3) above and below ground
  adhered_a = k_whc
  adhered_b = k_whc
  if (V_w > V_pore_b){ 
    # if water depth greater than zero___ 
    V_w_a = V_w - V_pore_b + adhered_a
    V_w_b = V_pore_b
    Z_w = Z_a + (V_w - V_pore_b)/(area*phi_a)
  } else  {
    V_w_a = adhered_a
    V_w_b = V_w + adhered_b
    Z_w = Z_b + (V_w)/(area*phi_b) 
  }
  
  H_w = Z_w - Z_a
  d_V_w = 0
  V_w = d_V_w + V_w
  Q_out = d_V_w
})

sub_hydroforc_Z_c <- expression({
  # Volume
  # calculate W (water volumes, m^3) above and below ground
  eval(sub_hydro_V_w_from_Z_w)
  # change in water volume from previous timestep
  # d_V_w = Q_channel + Q_precip + Q_ground - Q_ET # m^3; water volume in all wetland compartments; (forecast)
  d_V_w = V_w_a + V_w_b - V_w # m^3; water volume in all wetland compartments; (calibration) V_w
  V_w = V_w_a + V_w_b
  Q_channel = d_V_w - Q_precip + Q_ET + Q_ground
  if (Q_channel >= 0){
    Q_in = Q_channel + V_w/tau
    Q_out = V_w/tau
  }else{
    Q_in = V_w/tau
    Q_out = Q_channel + V_w/tau
  }
  eval(sub_hydro_outs)
})

## PROCESSES ------------------
# process rates
sub_process_rates <- expression({
  r_assim = fn_assimilation(
    BM = BM, # biomass stock in grams dry weight per meter squared
    k_ADNPP = k_ADNPP, # average daily NPP rate
    TW=TW, # temperature of water
    theta = k_theta, # g d.w. m-2; annual aboveground primary productivity
    T_STD = k_T_STD, # T
    X=k_PIP2Ex*PIP_b, # nutrient mass
    k_B2X=k_BM2P, # X/B; X content in B ; see fn_Xlimitation 
  )
  
  # decay rate of refractory organic matter
  r_decay_litter = k_decay_litter*k_TW
  
  # decay rate of refractory organic matter
  r_decay_ROP = k_decay_ROP*k_TW*ifelse(H_w<0,1,0) 
  # assume zero when soils are saturated

  
  # decay rate of labile organic matter
  r_decay_LOP = k_decay_LOP*k_TW
  
  # mortality rate of shoots
  r_mort_shoot = k_M*k_TW*ifelse(TW<k_T_thresh_M_shoot,k_M_shoot_T_mult,1) 
  # increases when temperature drops below threshold
  
  # miortality rate of roots
  r_mort_root = k_M*k_TW
  
  # Smax aka Ex_max is a function of Al and Fe 
  r_adsorp = fn_adsorption(
    # name = value # units; description; assumptions
    V_w = V_w_b, # (m^-3); volume of water in compartment
    DIP_E=DIP_E, #(g/m3=mg/L); equilibrium DIP concentration
    DIP=DIP_b, # (g); mass of dissolved inorganic P in compartment
    k_ad = k_ad # (d^-1); first order sorption rate constant 
  )
  
    
  # diffusion rate from of DIP from belowground to aboveground
  k_diff = fn_kDiff(TW=TW,kDiff_STD=k_diff_STD,mew=k_mew,mew_STD=k_mew_STD)
  r_diffus = fn_diffusion(
    C1=DIP_a/V_w_a, # concentration aboveground
    C2=DIP_b/V_w_b, # concentration below ground
    kDiff=k_diff, # effective diffusion coefficient
    area=area, # surface area between compartments
    dz = (H_b) # distance between compartments
  )
  # sedimentation rate of organic matter
  u_o = fn_settlingvelocity(k_dp=k_dp_o) #m/d; organic particle settling velocity
  r_sed_OM = fn_sedimentation(
    H_w=H_w, # m; hieght of water above sediment surface; input
    u=u_o # m/d; inorganic particle settling velocity
  )
  
  # sedimentation rate of inorganic matter
  u_i = fn_settlingvelocity(k_dp=k_dp_i) #m/d; organic particle settling velocity
  r_sed_IM = fn_sedimentation(
    H_w=H_w, # m; hieght of water above sediment surface; input
    u=u_i #m/d; organic particle settling velocity
    
  )
})

# process flows 
sub_process_flows <- expression({

  
  # name = expression # units | description | assumptions
  Q_in = Q_in*IO_Q_in # m^3/d | surface water lateral inflow | note all hydrologic should be positive magnitude values, they are then multiplied by 1, 0, -1 in differential equaitions 
  Q_out = Q_out*IO_Q_out # m^3/d | surface water lateral outflow |
  Q_ground = Q_ground*IO_Q_ground # m^3/d | net vertical flow from groundwater (percolation - infiltration) |
  Q_precip = Q_precip*IO_Q_precip # m^3/d | direct precipitation |
  Q_ET = Q_ET*IO_Q_ET # m^3/d | evapotranspiration precipitation |
  
  # name = expression # units | description | assumptions
  assim_shootP = r_assim*(k_f_sh_G)*IO_assim_shootP # g P/d | assimilation of shoot P
  assim_rootP = r_assim*(1-k_f_sh_G)*IO_assim_rootP # g P/d | growth of root P 
  mort_shootP2litterP = r_mort_shoot*shootP*IO_mort_shootP2litterP # g P/d | growth of root P 
  mort_rootP2LOP = r_mort_root*rootP*(k_f_labile_root)*IO_mort_rootP2LOP # g P/d | mortality of shoot P to LOP
  mort_rootP2ROP = r_mort_root*rootP*(1-k_f_labile_root)*IO_mort_rootP2ROP # g P/d | mortatlity of root P
  sed_IM = r_sed_IM*IM_a*IO_sed_IM # g d.w./d | sedimentation of inorganic matter
  sed_PIP = sed_IM*k_ISS2P*IO_sed_PIP # g P/d | sedimentation of inorganic P
  sed_LOP = r_sed_OM*LOP_a*IO_sed_LOP # g P/d | sedimentation of labile organic P
  sed_ROP = r_sed_OM*ROP_a*IO_sed_ROP # g P/d | sedimentation of refractory organic P
  dec_litter2LOP_a = r_decay_litter*litterP*(k_f_labile_litter)*IO_dec_litter2LOP_a # g P/d | decomposition of litter P to labile organic P
  dec_litter2ROP_a = r_decay_litter*litterP*(1-k_f_labile_litter)*IO_dec_litter2ROP_a # g P/d | decomposition of litter P to refractory organic P
  dec_LOP_a = r_decay_LOP*LOP_a*IO_dec_LOP_a # g P/d | decomposition of labile OP to DIP
  dec_ROP_a = r_decay_ROP*ROP_a*IO_dec_ROP_a # g P/d | decomposition of refractory OP to labile OP
  dec_LOP_b = r_decay_LOP*LOP_b*IO_dec_LOP_b # g P/d | decomposition of labile OP to DIP
  dec_ROP_b = r_decay_ROP*ROP_b*IO_dec_ROP_b # g P/d | decomposition of refractory OP to labile OP
  diff_DIP_b2a = r_diffus*DIP_b*IO_diff_DIP_b2a # g P/d | diffusion of DIP from b to a
  bioturb_IM = r_bioturb*IM_b*IO_bioturb_IM # g d.w./d | bioturbation diffusion of IM from b to a
  bioturb_PIP = r_bioturb*PIP_b*IO_bioturb_PIP # g P/d | bioturbation diffusion of PIP from b to a
  bioturb_LOP = r_bioturb*LOP_b*IO_bioturb_LOP # g P/d | bioturbation diffusion of LOP from b to a
  bioturb_ROP = r_bioturb*ROP_b*IO_bioturb_ROP # g P/d | bioturbation diffusion of ROP from b to a
  sorp_DIP2PIP_b = r_adsorp_b*V_w_b*IO_sorp_DIP2PIP_b # g P/d | adsorption of DIP onto PIP aboveground
  sorp_DIP2PIP_a = r_adsorp_a*V_w_a*IO_sorp_DIP2PIP_a # g d.w./d | adsorption of DIP onto PIP belowground
  in_IM = Q_in*ISS*IO_in_IM # g d.w./d | inflow of IM from surface waters
  in_PIP = in_IM*k_ISS2P*IO_in_PIP # g P/d | ""
  in_LOP = Q_in*OSS*k_f_labile_OSS*k_LOM2P*IO_in_LOP # g P/d | ""
  in_ROP = Q_in*OSS*(1-k_f_labile_OSS)*k_ROM2P*IO_in_ROP # g P/d | ""
  in_DIP = Q_in*C_DIP_in*IO_in_DIP # g P/d | ""
  out_IM = Q_out*IM_a/V_w_a*IO_out_IM # g P/d | outflow of IM
  out_PIP = Q_out*PIP_a/V_w_a*IO_out_PIP # g P/d | ""
  out_LOP = Q_out*LOP_a/V_w_a*IO_out_LOP # g P/d | ""
  out_ROP = Q_out*ROP_a/V_w_a*IO_out_ROP # g P/d | ""
  out_DIP = Q_out*DIP_a/V_w_a*IO_out_DIP # g P/d | ""
  
})

## DIFFERENTIAL EQUATIONS ----------
sub_diffeqs <- expression({
  # name = expression
  # water
  d_V_w = Q_in + Q_precip + Q_ground + -1*Q_ET + -1*Q_out
  
  # solids
  d_IM_a = -1*sed_IM + 1*in_IM + -1*out_IM
  d_IM_b = 1*sed_IM
  
  # inorganic matter 
  d_IM_a = -1*sed_IM + 1*bioturb_IM + 1*in_IM + -1*out_IM
  d_IM_b = 1*sed_IM + -1*bioturb_IM
  
  # phosphorus 
  d_shootP = 1*assim_shootP + -1*mort_shootP2litterP
  d_rootP = 1*assim_rootP + -1*mort_rootP2LOP + -1*mort_rootP2ROP
  d_litterP = 1*mort_shootP2litterP + -1*dec_litter2LOP_a + -1*dec_litter2ROP_a
  d_ROP_a = -1*sed_ROP + 1*dec_litter2ROP_a + -1*dec_ROP_a + 1*bioturb_ROP + 1*in_ROP + -1*out_ROP
  d_LOP_a = 1*assim_periphyte + -1*sed_LOP + 1*dec_litter2LOP_a + -1*dec_LOP_a + 1*dec_ROP_a + 1*bioturb_LOP + 1*in_LOP + -1*out_LOP
  d_PIP_a = -1*sed_PIP + 1*bioturb_PIP + 1*sorp_DIP2PIP_a + 1*in_PIP + -1*out_PIP
  d_DIP_a = -1*assim_periphyte + 1*dec_LOP_a + 1*diff_DIP_b2a + -1*sorp_DIP2PIP_a + 1*in_DIP + -1*out_DIP
  d_ROP_b = 1*mort_rootP2ROP + 1*sed_ROP + -1*dec_ROP_b + -1*bioturb_ROP
  d_LOP_b = 1*mort_rootP2LOP + 1*sed_LOP + -1*dec_LOP_b + 1*dec_ROP_b + -1*bioturb_LOP
  d_PIP_b = -1*assim_shootP + -1*assim_rootP + 1*sed_PIP + -1*bioturb_PIP + 1*sorp_DIP2PIP_b
  d_DIP_b = 1*dec_LOP_b + -1*diff_DIP_b2a + -1*sorp_DIP2PIP_b
  
})



## COMPILE OUTPUTS --------------------
sub_compile_derivs <- expression({
  # derivatives
  derivs <- c(
    # Water
    d_V_w = d_V_w,
    # Solids
    d_IM_a=d_IM_a,
    d_IM_b=d_IM_b,
    # Phosphorus 
    # aboveground
    d_shootP=d_shootP,
    d_litterP=d_litterP,
    d_ROP_a=d_ROP_a,
    d_LOP_a=d_LOP_a,
    d_PIP_a=d_PIP_a,
    d_DIP_a=d_DIP_a,
    # belowground
    d_rootP=d_rootP,
    d_ROP_b=d_ROP_b,
    d_LOP_b=d_LOP_b,
    d_PIP_b=d_PIP_b,
    d_DIP_b=d_DIP_b)
})

sub_compile_newstates <- expression({
  # new values for state variables
  newstates <- c(
    V_w = V_w + d_V_w,
    IM_a=IM_a+d_IM_a,
    IM_b=IM_b+d_IM_b,
    # Phosphorus 
    # aboveground
    shootP=shootP+d_shootP,
    litterP=litterP+d_litterP,
    ROP_a=ROP_a+d_ROP_a,
    LOP_a=LOP_a+d_LOP_a,
    PIP_a=PIP_a+d_PIP_a,
    DIP_a=DIP_a+d_DIP_a,
    # belowground
    rootP=rootP+d_rootP,
    ROP_b=ROP_b+d_ROP_b,
    LOP_b=LOP_b+d_LOP_b,
    PIP_b=PIP_b+d_PIP_b,
    DIP_b=DIP_b+d_DIP_b)
})

sub_compile_extended <- expression({
  
  rates <- c(
    r_sed_IM=r_sed_IM,
    r_sed_OM=r_sed_OM,
    r_diffus=r_diffus,
    r_bioturb=r_bioturb,
    r_decay_litter=r_decay_litter,
    r_decay_labile=r_decay_LOP,
    r_decay_refrac=r_decay_ROP,
    r_mort_shoot=r_mort_shoot,
    r_mort_root=r_mort_root,
    r_adsorp_a=r_adsorp_a,
    r_adsorp_b=r_adsorp_b,
    r_assim=r_assim)
  vars_mass <- c(shootM=shootM,rootM=rootM,BM=BM,litterM=litterM,OM_a=OM_a,OM_b=OM_b,TM_a=TM_a,TM_b=TM_b,TP=TP)
  vars_hydro <- c(
    V_w_b = V_w_b,
    V_w_a = V_w_a,
    V_w = V_w,
    d_V_w = d_V_w,
    TW = TW,
    Q_ET = Q_ET,
    Q_precip = Q_precip,
    Q_ground = Q_ground,
    Q_in = Q_in,
    Q_out = Q_out)
  vars_pore <- c(LOI_a=LOI_a,LOI_b=LOI_b,BD_b=BD_b,
                 PD_a=PD_a,PD_b=PD_b,phi_a=phi_a,phi_b=phi_b,
                 V_pore_a=V_pore_a,V_pore_b=V_pore_b)
  vars_elev <- c(H_b=H_b,Z_a=Z_a,Z_w=Z_w,H_w=H_w,H_a=H_a)
  vars_adsorp <- c(Ex_max=Ex_max_a,Ex_max_b=Ex_max_b,PSR_a=PSR_a,DIP_E_a=DIP_E_a)
  outs <- c(t=t,yr=yr,states,derivs,rates,vars_mass,vars_elev,vars_pore,vars_hydro)
})


