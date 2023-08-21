# **************************************************************
# filename: initialize.R
# description: initializes state variables and select parameters
# author: Adrian R.H. Wiegman 
# revision date: 2020-03-31
# repository: https://github.com/arhwiegman/wetlandP
# notes: wetlandP_v0p3
# **************************************************************


# compile extended outputs
if (!extended_outputs) {
  sub_compile_outputs <- sub_compile_derivs
} else {
  sub_compile_outputs <- expression({
    eval(sub_compile_derivs)
    eval(sub_compile_extended)
    write(outs,outfile_ext,append=T,ncolumns=length(outs),sep=",")
  })
}

simdays = ceiling(365*simyears) # d; total number of days to be simulated;
simtimes = seq(startday, startday + simdays, by = increment) # NA; a vector of days used to step the model through time; NA

# NAME OUTPUT FILES -----------------------------------------------
outfile_ext <- paste0(wrkdir,"/outputs/outputs_extended_",simname,"_",format(Sys.time(), "%Y-%m-%d"),".csv")
outfile <- paste0(wrkdir,"/outputs/outputs_",simname,"_",format(Sys.time(), "%Y-%m-%d"),".csv")
simfile <- paste0(wrkdir,"/outputs/sim_",simname,"_",format(Sys.time(), "%Y-%m-%d_%Hh%Mm%Ss"),".Rdata")
figprefix <- paste0(wrkdir,"/outputs/fig_",simname,"_",format(Sys.time(), "%Y-%m-%d"))


# INITIALIZE SIMULATION TIMES --------------------
t = startday
yr = t/365

# FORCING VARIABLES -----------------------------
if(simtype=="sensitivity"){fn_forcings=fn_forcings_sensitivity}
forcings <- fn_forcings(inputdir=inputdir,forcingfile=forcingfile)
eval(sub_forcings)

# CALCULATE INITIAL STATES -----------------------------------
eval(sub_init_states)

if(extended_outputs) eval(sub_init_extended)


