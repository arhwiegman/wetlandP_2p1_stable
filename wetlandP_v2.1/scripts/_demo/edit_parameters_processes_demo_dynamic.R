# **************************************************************
# filename: edit_parameters_processes_demo_dynamic.R
# description: 
#   use this as a template to modifies parameter values for wetlandP runs
#   declare new values for any variable found in the `parameters` list
# author: Adrian R.H. Wiegman 
# revision date: 2020-06-18
# repository: https://github.com/arhwiegman/wetlandP_2p1_stable
# notes:
#    see `EXAMPLES ---` below
# **************************************************************

# EXAMPLES -----------------------------------------------------------------

# values can be assigned directly to the object within the `parameters` list
parameters$simname = "demo_dynam"

# this can also be done in the global environment...
# HOWEVER, in order to change existing parameter values
# the name of the variable declared must match the object name in parameters
simname = "demo_dynam"
# you can check if that is the case using this line
"simname" %in% names(parameters)
# in fact the any variable you declare in this file will be saved in the `parameters` list
# this may be valuable to you in more complex implementations later on
# for example, below I add a 
simnotes = "this is a demonstration of how provide user inputs and a run a dynamic simulation with the wetlandP model"
# if you do this make sure the variable name is not already contained in `parameters`
"simnotes" %in% names(parameters)


# EDIT PARAMETER VALUES --------------------------------------------------
# SIMULATION NAME (for naming output files)
simname = "demo_dynam"

# INPUT FILE NAME AND PATH
inputdir = "_demo/"
forcingfile = "df.hydroclimate_demo1.csv"

# CHANGE PARAMETER VALUES
k_ad = 1.75 # 1/d | adsorption first order rate coefficient | Wang et al. 2003 1.75, Marois & Mitsch 2016 used

k_E = 0.5 # m^3/g | langmuir constant of adsorption (bond energy) | Calibrated to intact core data this value depends on what metric is used to define Ex_max,  Wang et al. 2003 2.75 m3 kg-1

k_Ex_max = 1 # g/kg | maximum exchangeable P | Modeled as a function of LOI and soil texture, OR taken directly as the Smax value, OR oxalate Al + Fe; Wang et al. 2003 used 0.25 g/kg
parameters$k_Ex_max=k_Ex_max

k_PIP2Ex = 0.8  # g/g| ratio of exchangeable P to particulate inorganic P | Wang et al. 2003 0.8
parameters$k_PIP2Ex=k_PIP2Ex

k_diff_STD = 1e-1  # m^2/d | effective diffusion coefficient| calibrated to intact core data; Marois & Mitsch 2016 calibrated value was 2e−5 m2 d−1
parameters$k_diff_STD=k_diff_STD

parameters$k_diff_STD = k_diff_STD
parameters$k_E = k_E
parameters$k_decay_LOP = 1e-6
parameters$k_decay_ROP = 1e-9
parameters$k_M_max_root = 0


# TOGGLE PROCESSES TO BE SIMULATED ---------------
# Parameters with "IO_" prefix turn process flow rates on and off (toggle)
# must be logical T/F TRUE/FALSE
# name = expression # units | description | assumptions
# water 
parameters$IO_Q_in = T # logical T/F or 1/0 | toggles surface inflow | 
parameters$IO_Q_precip = T # logical T/F or 1/0 | toggle precipitation | 
parameters$IO_Q_ground = T # logical T/F or 1/0 | toggles net groundwater flow (percolation  - infiltration)| 
parameters$IO_Q_ET = T # logical T/F or 1/0 | toggles evapotranspiration | 
parameters$IO_Q_out = T # logical T/F or 1/0 | toggles surface outflow | 


# plant, sediments and phosphorus
parameters$IO_assim_shootP = T # logical T/F or 1/0 | toggles assimilation of shoot P | 
parameters$IO_assim_rootP = T  # logical T/F or 1/0 |  toggles growth of root P |
parameters$IO_mort_shootP2litterP = T # logical T/F or 1/0 | toggles mortality of shoots | 
parameters$IO_mort_rootP2LOP = T # logical T/F or 1/0 | toggles mortality of root P to LOP |
parameters$IO_mort_rootP2ROP = T # logical T/F or 1/0 | toggles mortatlity of root P |
parameters$IO_decay_litter = T # logical T/F or 1/0 | toggles decomposition of litter P to refractory organic P | 
parameters$IO_decay_LOP = T # logical T/F or 1/0 | toggles decomposition of labile OP | 
parameters$IO_decay_ROP = T # logical T/F or 1/0 | toggles decomposition of refractory OP |

parameters$IO_sed_IM = T # logical T/F or 1/0 | toggles sedimentation of inorganic matter | 
parameters$IO_sed_OM = T # logical T/F or 1/0 | toggles sedimentation of refractory organic P
parameters$IO_diffus = T # logical T/F or 1/0 | toggles diffusion of DIP from b to a | 
parameters$IO_adsorp = T  # logical T/F or 1/0 | toggles adsorption of DIP onto PIP | 


parameters$IO_DIP_E_langmuir = F
parameters$IO_anoxic = F
parameters$IO_variable_k_Ex_max = F
parameters$IO_variable_k_E = F
parameters$IO_use_k_DIP_E = F