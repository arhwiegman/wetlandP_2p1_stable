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
wrkdir <- getwd() # set up working directory in ~/wetlandP_v2.1 folder
print(wrkdir)
#wrkdir <- "~/GitHub/wetlandP_v2/wetlandP_v2.1"

# USER INPUTS -------------------------------
# path2userparameterscript = NULL
path2userparameterscript = file.path(wrkdir,"scripts/_demo/edit_parameters_processes_demo_dynamic_excel.R")

# LOAD DEPENDANCIES and SOURCE CODE ------------------------
try(setwd(file.path(wrkdir,"scripts"))) # navigate to scripts folder 

try({
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
  
  # SET PARAMETER VALUES -------------------------------------
  cat("sourcing 'parameters.R'\n")
  source("parameters.R")
  
  # INITIALIZE VARIABLES -------------------------------------
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

