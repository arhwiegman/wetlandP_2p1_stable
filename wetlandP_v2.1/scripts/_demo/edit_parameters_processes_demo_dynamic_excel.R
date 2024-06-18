# **************************************************************
# filename: edit_parameters_processes_demo_dynamic_excel.R
# description: 
#   use this as a template to modifies parameter values for wetlandP runs
#   declare new values for any variable found in the `parameters` list
#   this provides an example of assigning parameters from an excel sheet
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

# load the dataframe `df.sim.pars` from an excel sheet
# the excel sheet contains parameters for a simulation on each row
# the spreadsheet can contain multiple rows 
# the spreadsheet can also supplemental variables not used by the model
# such as those are useful for metadata or post-processing 
df.sim.pars <- read_excel(file.path(wrkdir,"scripts/_demo/demo_parameters.xlsx"),
                              skip=1)
j = 1 # select row number

# this code block automatically assigns objects in the list into variables in the global environment
sub_load_row_variables_into_global_env <- expression({
  # select a row, then convert the data to a list format
  par.list <- as.list(df.sim.pars[j,colSums(is.na(df.sim.pars[j,]))!=1])
  # this loops through the variables then assigns them to global env.
  for(i in names(par.list)){
    assign(i,unname(unlist(par.list[i])))
  }
})

# run the subroutine
eval(sub_load_row_variables_into_global_env)

# alternatively values from the user_parameters dataframe can be assigned manually
extended_outputs = F
parameters$extended_outputs = extended_outputs

IO_use_k_DIP_E = df.sim.pars$IO_use_k_DIP_E[j]
IO_Q_net = df.sim.pars$IO_Q_net[j]
IO_HRT_power_model = df.sim.pars$IO_HRT_power_model[j]
k_Z_a = df.sim.pars$Z_a[j]
parameters$k_Z_a = df.sim.pars$Z_a[j]
parameters$IO_DIP_E_langmuir = IO_DIP_E_langmuir 
parameters$IO_anoxic = IO_anoxic
parameters$IO_variable_k_Ex_max = IO_variable_k_Ex_max
parameters$IO_variable_k_E = IO_variable_k_E
parameters$IO_use_k_DIP_E = IO_use_k_DIP_E
parameters$IO_Q_net = IO_Q_net
parameters$IO_HRT_power_model = IO_HRT_power_model
parameters$k_BM2P=k_BM2P
parameters$k_TP=k_TP
parameters$k_f_SRP=k_f_SRP
parameters$k_TSS=k_TSS
parameters$k_shootM=k_shootM
parameters$k_rootM=k_rootM

simname = paste0(simname,"_",df.sim.pars$site.plot[j])
parameters$simname = simname
k_PSR =  df.sim.pars$k_PSR[j]
parameters$k_PSR = k_PSR
k_LOI =  df.sim.pars$k_LOI[j]
parameters$k_LOI = k_LOI
k_Ex_max =  df.sim.pars$k_Ex_max[j]
parameters$k_Ex_max = k_Ex_max
k_f_fines =  df.sim.pars$k_f_fines[j]
parameters$k_f_fines = k_f_fines
simyears = df.sim.pars$simyears[j]
parameters$simyears = simyears


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