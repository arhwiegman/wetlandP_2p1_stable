# **************************************************************
# filename: functions.R
# description: declares functions for model implementation
# author: Adrian R.H. Wiegman 
# revision date: 2020-04-01
# project: wetlandP_v03
# repository: https://github.com/arhwiegman/wetlandP
# notes:
# - all functions checked as of 4/1/20
# - still need to add function for bioturbation (Marois & Mitsch 2016)
# **************************************************************

# PARAMETER FUNCTIONS --------------------------
source(file.path(wrkdir,"scripts/fns/fns_parameters.R"))

# FORCING FUNCTIONS ----------------------------
source(file.path(wrkdir,"scripts/fns/fns_forcings.R"))

# INTERMEDIATE FUNCTIONS ----------------------------
source(file.path(wrkdir,"scripts/fns/fns_intermediates.R"))

# PROCESS FUNCTIONS ----------------------------
source(file.path(wrkdir,"scripts/fns/fns_processes.R"))

# HYDROLOGY FUNCTIONS ----------------------
source(file.path(wrkdir,"scripts/fns/fns_hydrology.R"))

# ALL OTHER FUNCTIONS ------------------
source(file.path(wrkdir,"scripts/fns/fns_other.R"))
