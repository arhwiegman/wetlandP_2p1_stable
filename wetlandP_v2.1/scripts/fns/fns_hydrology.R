# **************************************************************
# filename: hydrology_fns.R
# description: declares hydrology functions
# author: Adrian Wiegman 
# revision date:  2020-05-15
# project: wetlandP
# repository: https://github.com/arhwiegman/wetlandP   
# notes:
# - ___
# - ___
# **************************************************************


# NO LONGER USEFUL 2020-09-24
fn_hydrology_subroutine<- function(){
  if(simtype=="calibration")return(sub_hydroforc_control)
  if(hydroforc=="Z_w")return(sub_hydroforc_Z_w)
  if(hydroforc=="Q_in")return(sub_hydroforc_Q_in)
  if(hydroforc=="Z_c")return(sub_hydroforc_Z_c)
  if(hydroforc=="V_w")return(sub_hydroforc_V_w)
}

fn_Q_ET <- function(
  
  # revised: 2020-04-02
  # returns: evapotranspiration flux (m/day)
  # source: requires(Evapotranspiration)
  # references:
  # Mohamed, Y. A., Bastiaanssen, W. G. M., Savenije, H. H. G., Van den Hurk, B. J. J. M., & Finlayson, C. M. (2012). Wetland versus open water evaporation: An analysis and literature review. Physics and Chemistry of the Earth, 47–48, 114–121. https://doi.org/10.1016/j.pce.2011.08.005
  # notes: see detials @ https://rdrr.io/cran/Evapotranspiration/man/ET.PriestleyTaylor.html
  # inputs:
  # name = value # units; description; assumptions
  
  ET_data, # data passed to Priest taylor equation
  ET_constants # parameters passed to Priest Taylor Equation
){
  require(Evapotranspiration)
  if (simtype=="static"){return(Q = 0.02*area)}
  ET = ET.PriestleyTaylor(data,constants,ts="daily",solar="monthly precipitation",message="no")
  Q = ET*area 
  return(Q)
}

# function for groundwater flux
fn_Q_ground <- function(...){
  if (simtype=="static"){return(Q = 0)}
  Q = 0 # need to develop ground water assumptions
  return(Q)
}

fn_Q_precip <- function(
  
  # revised: 2020-04-02
  # returns: 
  # source: 
  # notes:
  # inputs:
  # name = value # units; description; assumptions
  
  precip_data = runif(simdays,0,0.1) # m/d; daily rainfall totals; 
){
  if (simtype=="static"){return(Q = 0.02*area)}
  if (simtype=="calibration"){return(Q = precip_data(t)*area)}
  if (simtype=="forecast"){return}
  
  return(Q)
}

# wetland and channel surface flux
fn_Q_channel <- function(z.w.c, # m, elevation of water in the channel (relative to benchmark)
                         z.d, # m, elevation of dike
                         z.w.w, # m, elevation of water surface
                         z.a, # m, elevation of sediment surface
                         z.b, # m, elevation of sediment bottom
                         k.flow, # d^-1, 0-1 fraction of head conveyed per day in free surface water
                         psi.a = 0.01, # -, 0-1 effective porosity of aboveground compartment
                         psi.b = 0.8, # -, 0-1 effective porosity of below ground compartment
                         area = 1)
{
  r <-  max(c(z.d, z.a)) # calculate height of restrictive layer, soil or dike
  if (z.w.c > z.w.w. & z.w.c > r){ # if channel water is higher than dike & wetland...
    Q = (z.w.c - z.d)*k.flow
  } else if(z.w.w > z.w.c & z.w.w > r){ # if wetland water is higher than dike & channel...
    Q = (z.d - z.w.w)*k.flow
  } else { # there is no water head, or the water is below the dike
    Q = 0
  } # end if
  return(Q)
} # end fn.Q.channel