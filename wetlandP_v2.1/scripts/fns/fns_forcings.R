fn_TW <- function(){
  if(simtype=="static"){return(rep(mean(TW_max,TX_min),simdays))}
  if(simtype=="calibration"){return()}# read in data
  fn_cosine(simtimes,TW_max,TW_min,t_T,t_T_l) + rnorm(simtimes,T_SD)/2 + simtimes*T_clim/2
}
fn_TA_h <- function(){
  if(simtype=="static"){return(rep(mean(TA_h_max,TA_h_min),simdays))}
  if(simtype=="calibration"){return()}# read in data
  fn_cosine(simtimes,TA_h_max,TA_h_min,t_T,t_T_l) + rnorm(simtimes,T_SD) + simtimes*T_clim
}
fn_TA_l <- function(){
  if(simtype=="static"){return(rep(mean(TA_l_max,TA_l_min),simdays))}
  if(simtype=="calibration"){return()}# read in data
  fn_cosine(simtimes,TA_l_max,TA_l_min,t_T,t_T_l) + rnorm(simtimes,T_SD) + simtimes*T_clim
}
fn_Z_w <- function(){
  if(simtype=="static"){return(rep(Z_w,simdays))}
  if(simtype=="calibration"){return()}# read in data
  fn_cosine(t,2,0,365,30)
}

fn_Z_w <- function(){
  if(simtype=="static"){return(rep(Z_c,simdays))}
  if(simtype=="calibration"){return()}# read in data
  fn_cosine(simtimes,2,0,365,30)
}

fn_forcings_old <- function(forcingfile=NULL){
  if(simtype=="static"){return(fn_forcings_static())}
  if(simtype=="calibration"){return(fn_forcings_calibration())}
  if(simtype=="forecast"){return(fn_forcings_forecast())}
}

fn_approxfun <- function(X=simtimes,Y){
  approxfun(X,Y,rule=2)
}

fn_forcings_static <- function(){
  # atmospheric
  TA_h = fn_approxfun(rep(TA_h,length(simtimes))) # degC; daily high air tempurature; ""
  TA_l = fn_approxfun(rep(TA_l,length(simtimes))) # degC; daily low air tempurature; ""
  RHmin = fn_approxfun(rep(RHmin,length(simtimes))) # %, minimum relative humidity
  RHmax = fn_approxfun(rep(RHmax,length(simtimes))) # %, maximum relative humidity
  Precip_monthly = fn_approxfun(rep(Precip_monthly,length(simtimes))) # inches; monthly rainfall total
  ns = fn_approxfun(rep(ns,length(simtimes))) # hours; daily time of sunshine 
  # hydrologic
  TW = fn_approxfun(rep(TW,length(simtimes))) # degC; temperature of water; average value (static) input data measured with HOBO (calibration) or function of time with seasonal, random and climate change terms (forecast)  
  Z_w = fn_approxfun(rep(Z_w,length(simtimes))) # m; elevation of water in wetland; data measured with HOBO used in calibration
  Z_c = fn_approxfun(rep(Z_c,length(simtimes))) # m; elevation of water in channel adjacent to wetland; sometimes used in forecast simulations
  Q_in = fn_approxfun(rep(Q_in,length(simtimes))) # m^3; inflow volume; used in static or forecast simulations
  Q_precip = fn_approxfun(rep(Q_precip,length(simtimes))) # m^3; inflow volume; used in static or forecast simulations
  Q_ET = fn_approxfun(rep(Q_ET,length(simtimes)))
  Q_ground = fn_approxfun(rep(Q_ground,length(simtimes)))
  TSS = fn_approxfun(rep(TSS,length(simtimes))) # g/m^3; total suspended solids of inflow ; model fit to field data from each site based on Q.in
  tau = fn_approxfun(rep(tau,length(simtimes))) # 1/d; residence time of wetland surface water; "" 
  
  forcings <- list(
    TA_h = TA_h,
    TA_l = TA_l,
    RHmax = RHmax, 
    RHmin = RHmax,
    Precip_monthly = Precip_monthly,
    ns = ns,
    TW = TW,
    Z_w = Z_w,
    Z_c = Z_c,
    Q_in = Q_in,
    Q_precip = Q_precip,
    Q_ground = Q_ground,
    Q_ET = Q_ET,
    TSS = TSS,
    tau = tau
  )
  return(forcings)
}

sub_preprocess_hydro_Q_net <- expression({
  # Adjust Q_in and Q_out with through flow
  df.hydroclimate <- df.hydroclimate %>%
    mutate(
      HRT = ifelse(IO_HRT_power_model,k_HRT_a*(H_w + k_Z_a)^k_HRT_b,k_HRT),
      QHRT = Vw/k_HRT,
      QHRT = (Hw!=0)*Vw/HRT,
      Qout = (Hw!=0)*ifelse(Qnet>=0,QHRT,QHRT-Qnet),
      Qin = (Hw!=0)*ifelse(Qnet>=0,QHRT+Qnet,QHRT))
})

fn_forcings <- function(forcingfile=NULL,inputdir=NULL){
  if(is.null(inputdir)){inputdir=file.path(wrkdir,"inputs")}
  if(is.null(forcingfile)){forcingfile="df.hydroclimate_static.csv"}
  df.hydroclimate <- read_csv(file.path(inputdir,forcingfile),show_col_types=F)
  if(IO_Q_net){eval(sub_preprocess_hydro_Q_net)}
  simdates=df.hydroclimate$date
  simtimes=df.hydroclimate$t
  # create linear approximation functions for each forcing variable
  Z_w = fn_approxfun(simtimes,df.hydroclimate$Hw) # m; elevation of water in wetland; data measured with HOBO used in calibration
  V_w = fn_approxfun(simtimes,df.hydroclimate$Vw) # m; elevation of water in wetland; data measured with HOBO used in calibration
  d_V_w = fn_approxfun(simtimes,df.hydroclimate$dVw) # change in water volume
  Z_c = fn_approxfun(simtimes,df.hydroclimate$Hw) # m; elevation of water in channel adjacent to wetland; sometimes used in forecast simulations
  Q_precip = fn_approxfun(simtimes,df.hydroclimate$Qip) # m^3; inflow volume; used in static or forecast simulations
  Q_ET = fn_approxfun(simtimes,df.hydroclimate$QET)
  Q_ground = fn_approxfun(simtimes,df.hydroclimate$Qg)
  Q_net = fn_approxfun(simtimes,df.hydroclimate$Qnet)
  Q_in = fn_approxfun(simtimes,df.hydroclimate$Qin) # m^3; inflow volume; used in static or forecast simulations
  Q_out = fn_approxfun(simtimes,df.hydroclimate$Qout) # m^3; outflow volume; used in static or forecast simulations
  
  if(use_k_TW){
    TW = fn_approxfun(simtimes,rep(k_TW,length(simtimes))) 
  }else{
    TW = fn_approxfun(simtimes,df.hydroclimate$Twater) # degC; temperature of water; average value (static) input data measured with HOBO (calibration) or function of time with seasonal, random and climate change terms (forecast)  
  }
  TSS = fn_approxfun(simtimes,rep(k_TSS,length(simtimes))) # g/m^3; total suspended solids of inflow ; model fit to field data from each site based on Q.in
  HRT = fn_approxfun(simtimes,rep(k_HRT,length(simtimes))) # 1/d; hydraulic residence time of wetland surface water; "" 
  SRP = fn_approxfun(simtimes,rep(k_TP*k_f_SRP,length(simtimes))) # g/m^3; total suspended solids of inflow ; model fit to field data from each site based on Q.in
  OSS = fn_approxfun(simtimes,rep(k_TSS*k_f_OSS,length(simtimes)))
  ISS = fn_approxfun(simtimes,rep(k_TSS*(1-k_f_OSS),length(simtimes)))
  forcings <- list(
    simdates=simdates,
    simtimes=simtimes,
    Zs = df.hydroclimate$Zs[1],
    TW = TW,
    Z_w = Z_w,
    V_w = V_w,
    H_w = H_w,
    Z_c = Z_c,
    d_V_w = d_V_w,
    Q_in = Q_in,
    Q_out= Q_out,
    Q_net= Q_net,
    Q_precip = Q_precip,
    Q_ground = Q_ground,
    Q_ET = Q_ET,
    TSS = TSS,
    OSS = OSS,
    ISS = ISS,
    HRT = HRT,
    SRP = SRP
  )
  return(forcings)
}




fn_forcings_sensitivity <- function(forcingfile=NULL,inputdir=NULL){
  if(is.null(inputdir)){inputdir=file.path(wrkdir,"inputs")}
  if(is.null(forcingfile)){forcingfile="df.hydroclimate_static.csv"}
  df.hydroclimate <- read_csv(file.path(inputdir,forcingfile),show_col_types = F)
  simdates=df.hydroclimate$date
  simtimes=df.hydroclimate$t
  # create linear approximation functions for each forcing variable
  Z_w = rep(Hw,length(simtimes)) # m; elevation of water in wetland; data measured with HOBO used in calibration
  V_w = rep(Hw,length(simtimes)) # m; elevation of water in wetland; data measured with HOBO used in calibration
  d_V_w = rep(0,length(simtimes)) # change in water volume
  Z_c = rep(Hw,length(simtimes)) # m; elevation of water in channel adjacent to wetland; sometimes used in forecast simulations
  Q_precip = fn_approxfun(simtimes,df.hydroclimate$Qip) # m^3; inflow volume; used in static or forecast simulations
  Q_ET = fn_approxfun(simtimes,df.hydroclimate$QET)
  Q_ground = fn_approxfun(simtimes,df.hydroclimate$Qg)
  HRT = fn_approxfun(simtimes,rep(k_HRT,length(simtimes))) # 1/d; residence time of wetland surface water; "" 
  if(IO_Q_net){
    Q_in = fn_approxfun(simtimes,Hw/k_HRT) # m^3; inflow volume; used in static or forecast simulations
    Q_out = fn_approxfun(simtimes,Hw/k_HRT) # m^3; outflow volume; used in static or forecast simulations
  }
  TW = fn_approxfun(simtimes,df.hydroclimate$Tw) 
  TSS = fn_approxfun(simtimes,rep(k_TSS,length(simtimes))) # g/m^3; total suspended solids of inflow ; model fit to field data from each site based on Q.in
  SRP = fn_approxfun(simtimes,rep(k_TP*k_f_SRP,length(simtimes))) # g/m^3; total suspended solids of inflow ; model fit to field data from each site based on Q.in
  OSS = fn_approxfun(simtimes,rep(k_TSS*k_f_OSS,length(simtimes)))
  ISS = fn_approxfun(simtimes,rep(k_TSS*(1-k_f_OSS),length(simtimes)))
  forcings <- list(
    simdates=simdates,
    simtimes=simtimes,
    Zs = df.hydroclimate$Zs[1],
    TW = TW,
    Z_w = Z_w,
    V_w = V_w,
    H_w = H_w,
    Z_c = Z_c,
    d_V_w = d_V_w,
    Q_in = Q_in,
    Q_out= Q_out,
    Q_precip = Q_precip,
    Q_ground = Q_ground,
    Q_ET = Q_ET,
    TSS = TSS,
    OSS = OSS,
    ISS = ISS,
    tau = HRT,
    SRP = SRP
  )
  return(forcings)
}
#fn_forcings_calibration()




fn_write_forcings_to_disk <- function(){
  . <- names(forcings)
  h <- cat("#",version,"forcing inputs",", run date:",format(Sys.time(), "%Y-%m-%d"),", simulation type:",simtype,", debug=",debug)
  write(h,"inputs/forcings.csv",append=F)
  write(.,"inputs/forcings.csv",append=T,sep=",",ncolumns=length(.))
  f_t <- vector(mode="double",length(.))
  for (t in seq_along(simtimes)){
    print(t)
    for (i in seq_along(.)){
      f_t[i] <- forcings[[i]](t)
    } # end names loop
    write(f_t,"inputs/forcings.csv",append=T,sep=",",ncolumns=length(.))
  } # end times loop
}

