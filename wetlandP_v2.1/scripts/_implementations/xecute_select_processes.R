# **************************************************************
# filename: xecute.R
# description: 
#   loads source code, executes simulation and manages outputs
# author: Adrian R.H. Wiegman 
# revision date: 2020-05-15
# repository: https://github.com/arhwiegman/wetlandP
# notes:
# **************************************************************

# CLEAR GLOBAL ENVIRONMENT ---------------------------------
rm(list = ls(all.names = TRUE)) # will clear all objects 
gc() #free up memory and report the memory usage.

# LOAD DEPENDANCIES and SOURCE CODE ------------------------
wrkdir <- "C:/Workspace/wetlandP/model_versions/wetlandP_v2/wetlandP_v2.1"

try(setwd(file.path(wrkdir,"scripts"))) # navigate to scripts folder 

try({
  # dependancies
  cat("sourcing 'dependancies.R'\n")
  source("dependancies.R")
  
  # functions
  cat("sourcing 'functions.R'\n")
  source("functions.R")
  
  # LOAD DEFAULT PARAMETERS AND MODEL SCRIPTS----------------
  # parameters
  cat("sourcing 'parameters.R'\n")
  source("parameters.R")
  eval(sub_compile_parameters)
  # model and subroutines
  cat("sourcing 'subroutines.R'\n")
  source("subroutines.R")
  
  cat("sourcing 'model.R'\n")
  source("model.R")
})  
setwd(wrkdir) # set wd back to working directory.


# MANAGE INPUT/OUTFILES & INITIALIZE STATE VARIABLES -------------------
try(setwd(file.path(wrkdir,"scripts"))) # navigate to scripts folder 
try({
  #fn_edit_parameter_values() # manually edit parameter values before execution
  source("edit_parameters_processes.R")

  cat("sourcing 'initialize.R'\n")
  source("initialize.R")
})
setwd(wrkdir) # set wd back to working directory. 


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


