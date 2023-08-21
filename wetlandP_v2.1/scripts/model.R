# **************************************************************
# filename: model.R
# description: 
# model equations wrapped inside function
# author: Adrian R.H. Wiegman 
# repository: https://github.com/arhwiegman/wetlanP
# revision date: 2020-05-15
# notes:
# - subroutines are in the subroutines.R file
# **************************************************************
wetlandP <- function(t, states, parameters) {
  with(as.list(c(states, parameters)),{
    yr = t/365.25
    if(floor((t %% 0.01*simdays)*10)==0){
      print(paste0(simname,", t=",floor(t)," (%",signif(100*t/simdays,3),")"))
      }
    # FORCING VARIABLES -----------------------------
    eval(sub_forcings)
    
    # INTERMEDIATE STATE VARIABLES------------------
    eval(sub_intermediates)
    
    # HYDROLOGIC VARIABLES ------------------
    eval(sub_hydro)
    # PROCESSES ---------------------------------
    # rates 
    eval(sub_process_rates)
    # flows 
    eval(sub_process_flows)

    # DIFFERENTIAL EQUATIONS ---------------------------------
    eval(sub_diffeqs)

    # COMPILE OUTPUTS ----------------------
    eval(sub_compile_outputs) # 
    # derivatives must be returned as list in same order as states
    return(list(derivs)) 
    } # end model code block
  ) # end with
} # end function 

wetlandP_inout <- function(t, states, parameters) {
  with(as.list(c(states, parameters)),{
    yr = t/365.25
    if(floor((t %% 0.01*simdays)*10)==0){
      print(paste0(simname,", t=",floor(t)," (%",signif(100*t/simdays,3),")"))
    }
    # FORCING VARIABLES -----------------------------
    eval(sub_forcings)
    
    # INTERMEDIATE STATE VARIABLES------------------
    eval(sub_intermediates)
    
    # HYDROLOGIC VARIABLES ------------------
    eval(sub_hydro)
    # PROCESSES ---------------------------------
    # rates 
    eval(sub_process_rates)
    # flows 
    eval(sub_process_flows)
    
    # DIFFERENTIAL EQUATIONS ---------------------------------
    eval(sub_diffeqs)
    
    # COMPILE OUTPUTS ----------------------
    eval(sub_compile_outputs) # 
    # derivatives must be returned as list in same order as states
    inout <- c(in_IM = in_IM,in_PIP=in_PIP,in_LOP = in_LOP,in_ROP=in_ROP,in_DIP=in_DIP,
                    out_IM = out_IM,out_PIP = out_PIP,out_LOP = out_LOP,out_ROP = out_ROP,out_DIP = out_DIP)
    return(list(derivs,inout)) 
  } # end model code block
  ) # end with
} # end function 

wetlandP_iteration <- function(t, states, parameters) {
  with(as.list(c(states, parameters)),{
    yr = t/365.25 
    t_old = t
    dt = t - t_old
    # FORCING VARIABLES -----------------------------
    eval(sub_forcings)
    
    # INTERMEDIATE STATE VARIABLES------------------
    eval(sub_intermediates)
    
    # HYDROLOGIC VARIABLES ------------------
    eval(sub_hydro)
    # PROCESSES ---------------------------------
    # rates 
    eval(sub_process_rates)
    # flows 
    eval(sub_process_flows)
    
    # DIFFERENTIAL EQUATIONS ---------------------------------
    eval(sub_diffeqs)
    
    # COMPILE OUTPUTS ----------------------
    eval(sub_compile_outputs) # 
    if(t %% 1 ==0){print(paste("dt:",dt,"t:",t," LOI: ",OM_b/TM_b,
                               ", BM:",BM,"r_assim:",r_assim))} # counting iterations...
    # NEW VALUES ---------------------------------
    eval(sub_compile_newstates)
    
    # derivatives must be returned as list in same order as states
    return(list(newstates)) 
  } # end model code block
  ) # end with
} # end function 
