# **************************************************************
# filename: xecute.R
# description: 
#   loads source code, executes simulation and manages outputs
# author: Adrian R.H. Wiegman 
# revision date: 2020-09-29
# repository: https://github.com/arhwiegman/wetlandP
# notes:
# **************************************************************

# CLEAR GLOBAL ENVIRONMENT ---------------------------------
rm(list = ls(all.names = TRUE)) # will clear all objects 
gc() #free up memory and report the memory usage.
wrkdir <- getwd() # set up working directory in ~/wetlandP_v2.1 folder
#wrkdir <- "C:/Workspace/wetlandP/model_versions/wetlandP_v2/wetlandP_v2.1"

# LOAD DEPENDANCIES and SOURCE CODE ------------------------
try(setwd(file.path(wrkdir,"scripts"))) # navigate to scripts folder 

try({
  # dependancies
  cat("sourcing 'dependancies.R'\n")
  source("dependancies.R")
  
  # functions
  cat("sourcing 'functions.R'\n")
  source("functions.R")
  
  # parameters
  cat("sourcing 'parameters.R'\n")
  source("parameters.R")
  eval(sub_compile_parameters)
  #fn_edit_parameter_values() # manually edit parameter values before execution
  
  # model and subroutines
  cat("sourcing 'subroutines.R'\n")
  source("subroutines.R")
  
  cat("sourcing 'model.R'\n")
  source("model.R")
  
  # INITIALIZE VARIABLES -------------------------------------
  cat("sourcing 'initialize.R'\n")
  source("initialize.R")
})
setwd(wrkdir) # set wd back to working directory.


# CHANGE PARAMETER VALUES BY HAND -------------------------------

parameters$simname = "demo"
parameters$k_DIP_E = 0.5


# EXECUTE RUN -------------------------------------------------------
system.time({
  try({
    sim <- fn_sim_ode(y = states, 
                      times = simtimes, 
                      func = wetlandP, 
                      parms = parameters,
                      method="lsoda")
  })
})

# MANIPULATE DATA & PLOT OUTPUTS ---------------------------------------
eval(sub_plot_outputs)

# SAVE IMAGE OF SIMULATION --------------------------------------
save.image(file=simfile)


