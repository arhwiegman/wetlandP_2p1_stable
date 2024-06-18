# **************************************************************
# filename: xecute_demo_dynamic.R
# description: 
#   loads source code, executes simulation and manages outputs
#   using a separate file to change user inputs 
# author: Adrian R.H. Wiegman 
# revision date: 2024-06-18
# repository: https://github.com/arhwiegman/wetlandP
# notes:
# **************************************************************

# CLEAR GLOBAL ENVIRONMENT ---------------------------------
rm(list = ls(all.names = TRUE)) # will clear all objects 
gc() #free up memory and report the memory usage.


# USER INPUTS -------------------------------
# set up working directory in ~/wetlandP_v2.1 folder
# wrkdir <- getwd() 
# save the Rproj home folder to wrkdir variable
wrkdir <- "C:/Users/Adrian.Wiegman/Documents/GitHub/wetlandP_2p1_stable/wetlandP_v2.1/"
# it is best to hard code the .Rproj directory in this script

# path to user defined parameter values
# path2userparameterscript = NULL # use NULL for default
path2userparameterscript = file.path(wrkdir,"scripts/_demo/edit_parameters_processes_demo_dynamic.R")


# LOAD DEPENDANCIES and SOURCE CODE ------------------------
setwd(wrkdir) # make sure to start in the home folder
try({
  setwd(file.path(wrkdir,"scripts"))  # navigate to scripts folder 
  # dependancies
  cat("sourcing 'dependancies.R'\n")
  source("dependancies.R")
  
  # functions
  cat("sourcing 'functions.R'\n")
  source("functions.R")
  
  # subroutines
  cat("sourcing 'subroutines.R'\n")
  source("subroutines.R")
  
  # model
  cat("sourcing 'model.R'\n")
  source("model.R")
  
  # parameters
  cat("sourcing 'parameters.R'\n")
  source("parameters.R")
  
  cat("sourcing 'initialize.R'\n")
  source("initialize.R")
})
setwd(wrkdir) # set wd back to project home directory.

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

