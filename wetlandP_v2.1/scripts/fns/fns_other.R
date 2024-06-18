# **************************************************************
# filename: other_fns.R
# description: declares other functions
# author: Adrian Wiegman 
# revision date:  2020-05-15
# project: wetlandP
# repository: https://github.com/arhwiegman/wetlandP    
# notes:
# - all non numerical modeling functions are held in this script
# **************************************************************

# SIMULATION FUNCTIONS -------------
fn_sim_ode <- function(y = states, 
                       times = simtimes, 
                       func = wetlandP, 
                       parms = parameters,
                       method=solver
){
  require(deSolve)
  out <- ode(y = y, 
             times = times, 
             func = func, 
             parms = parms,
             method= method)
  write.csv(out,file=outfile)
  list(out=out,
       states=y,
       times=times,
       model=func,
       parameters=parms,
       solver=method
  )
}

# DATA MANIPULATION AND PLOTTING FUNCTIONS -----------------
require(tidyverse); theme_set(theme_bw(base_size = 14))

sub_plot_DIP_concentration_simple <- expression({
  out_mod <- df_out_ext %>% 
    mutate(`DIP_a (mg/L)` = DIP_a/V_w_a,
           `DIP_b (mg/L)` = DIP_b/V_w_b) %>%
    select(yr,`DIP_a (mg/L)`,`DIP_b (mg/L)`,V_w_a,V_w_b,) %>%
    pivot_longer(cols=-yr,names_to="compartment",values_to="value")
  
  ggplot(data=out_mod) + 
    geom_line(aes(x=yr,y=value))+
    facet_wrap(vars(compartment),scales="free_y")
})

sub_plot_DIP_concentration_t <- expression({
  out_mod <- df_out_ext %>% 
    mutate(`DIP_a (mg/L)` = DIP_a/V_w_a,
           `DIP_b (mg/L)` = DIP_b/V_w_b,
           `LOI_b (g/g)` = LOI_b,
           `P Saturation Ratio` = PSR,
           #`Langmuir E (m3/g)` = E,
           `Ex_max g/kg`= Ex_max,
           `DIP_E (mg/L)` = DIP_E) %>%
    select(t,`DIP_a (mg/L)`,`DIP_b (mg/L)`,`DIP_E (mg/L)`,`P Saturation Ratio`,`Ex_max g/kg`,`LOI_b (g/g)`) %>%
    pivot_longer(cols=-t,names_to="compartment",values_to="value")
  
  g <- ggplot(data=out_mod) + 
    geom_line(aes(x=t,y=value))+
    facet_wrap(vars(compartment),
               scales = "free",
               strip.position = "left") +
    labs(title=stringr::str_extract(simfile,"sim_[^/]*"),
         subtitle=paste(paste(names(parameters[c("k_LOI","k_f_fines","k_PSR","k_Ex_max")]), signif(unlist(parameters[c("k_LOI","k_f_fines","k_PSR","k_Ex_max")]),3),sep="="),collapse=", "))+
    theme(strip.background = element_blank(), # remove the background
          strip.placement = "outside",
          axis.title.y = element_blank())
  fname <- paste0(figprefix,"_","DIP",".png")
  ggsave(fname,width=12,height=6)
  print(g)
})

sub_plot_DIP_concentration <- expression({
  out_mod <- df_out_ext %>%
    mutate(`DIP_a (mg/L)` = ifelse(V_w_a < 10e-3,NA,DIP_a/V_w_a),
            #`IM_a (mg/L)` = ifelse(V_w_a < 1e-3,NA,(IM_a)/V_w_a),
           `PIP_a (mg/L)` = ifelse(V_w_a < 10e-3,NA,(PIP_a)/V_w_a)) %>%
    select(yr,`DIP_a (mg/L)`,`PIP_a (mg/L)`,H_w) %>%
    pivot_longer(cols=-yr,names_to="compartment",values_to="value")
  
  g <- ggplot(data=out_mod) + 
    geom_line(aes(x=yr,y=value))+
    facet_wrap(vars(compartment),ncol=1,
             scales = "free",
             strip.position = "left") +
    ylab(NULL)+ # remove the word "values"
    theme(strip.background = element_blank(), # remove the background
          strip.placement = "outside")+
    labs(title=stringr::str_extract(simfile,"sim_[^/]*"))
  fname <- paste0(figprefix,"_","DIP_mgl",".png")
  ggsave(fname,width=12,height=6)
  print(g)
})

sub_plot_water_column_stocks <- expression({
  out_mod <- df_out_ext %>%
    mutate(`DIP_a (g/m^2)` = DIP_a,
           `IM_a (g/m^2)` =IM_a,
           `PIP_a (g/m^2)` = PIP_a) %>%
    select(yr,`DIP_a (g/m^2)`,`PIP_a (g/m^2)`) %>%
    pivot_longer(cols=-yr,names_to="Stock",values_to="value")
    
    g <- ggplot(data=out_mod) + 
      geom_line(aes(x=yr,y=value,color=Stock,linetype=Stock))+
      scale_color_manual(values=c("red","blue"))+
      ylab(NULL)+ # remove the word "values"
      theme(strip.background = element_blank(), # remove the background
            strip.placement = "outside",
            legend.position = c(0.15, 0.85),
            legend.background = element_rect(fill = "white", color = "black"))+
      labs(title=stringr::str_extract(simfile,"sim_[^/]*"))
    fname <- paste0(figprefix,"_","DIP_gm2",".png")
    ggsave(fname,width=12,height=6)
    print(g)
})

sub_plot_outputs <- expression({
  if(extended_outputs){
    df_out_ext <- read.csv(file=outfile_ext)
    vargroups <- outgroups[c("states","rates","vars_mass","vars_elev","vars_pore","vars_hydro")]
    trimmed <- na.omit(df_out_ext) # trim off last few rows incase of error
    figs <- fn_plot_debug(trimmed,vargroups)
    print(figs)
    if(simyears<15/365){
      eval(sub_plot_DIP_concentration_t)
    }else{
      #plot DIP concentrations in each compartment
      eval(sub_plot_DIP_concentration)
      eval(sub_plot_water_column_stocks)
    }
    
  } else {
    par(mfrow=c(3,4))
    plot(sim$out)
    }
})



fn_plot_debug <- function(data,varlist){
  require(tidyverse); theme_set(theme_bw(base_size = 14))
  block <- expression({
    # convert outputs to long format
    col_vec <- names(varlist[[i]])
    fig[[i]] <- data[,c("yr",col_vec)] %>% 
      distinct(yr, .keep_all= TRUE) %>%
      gather(-yr, key = "var", value = "value") %>%
      ggplot(aes(x = yr, y = value)) +
      geom_line() +
      facet_wrap(~ var,
                 scales = "free",
                 strip.position = "left") +
      ylab(NULL)+ # remove the word "values"
      theme(strip.background = element_blank(), # remove the background
            strip.placement = "outside")+
      labs(title=stringr::str_extract(simfile,"sim_[^/]*"))
    fname <- paste0(figprefix,"_",i,".png")
    ggsave(fname,width=12,height=6)
  })
  
  loop <- function(){
    fig <- vector("list",length(varlist))
    names(fig) <- names(varlist)
    for (i in names(varlist)){
      print(paste("plotting",i))
      eval(block)
    }
    return(fig)
  }
  return(loop())
}

fn_sim_ode_intact_core <- function(make_plots=T,save_data=T){
  try(system.time({
    # UPDATE PARAMETERS ---------------- 
    setwd(file.path(wrkdir,"scripts")) # navigate to scripts folder 
    cat("sourcing 'edit_parameters_processes_intact_core.R'\n")
    source("edit_parameters_processes_intact_core.R")
    cat("sourcing 'initialize.R'\n")
    source("initialize.R")
    setwd(wrkdir) # set wd back to working directory.
    
    # RUN SIMULATION ---------------
    sim <- fn_sim_ode(y = states, 
                      times = simtimes, 
                      func = wetlandP, 
                      parms = parameters,
                      method="lsoda")
    
    # MANIPULATE DATA & PLOT OUTPUTS ---------------------------------------
    if(make_plots){eval(sub_plot_outputs)} 
    
    # SAVE IMAGE OF SIMULATION --------------------------------------
    if(save_data){save.image(file=simfile)}
  }))
  
  # Calculate calibration parameter as SRP concentration on day ~11
  cal_metric = fn_cal_metric_intact_core("DIP_a_mgl")
  return(cal_metric)
}

fn_sim_ode_accretion <- function(make_plots=F,save_data=F){
  try(system.time({
    # RUN SIMULATION ---------------
    sim <- fn_sim_ode(y = states, 
                      times = simtimes, 
                      func = wetlandP, 
                      parms = parameters,
                      method="lsoda")
    
    # MANIPULATE DATA & PLOT OUTPUTS ---------------------------------------
    if(make_plots){eval(sub_plot_outputs)} 
    
    # SAVE IMAGE OF SIMULATION --------------------------------------
    if(save_data){save.image(file=simfile)}
    
    return(fn_cal_metric_accretion())
  }))
}


fn_cal_metric_accretion <- function(metric_name="IM_a"){
  df.sim.outs <- read.csv(outfile_ext)
  metric_value <- approx(x=df.sim.outs$t,y=df.sim.outs[,metric_name],xout=365*simyears)$y
  return(metric_value)
}

fn_cal_metric_intact_core <- function(metric_name="DIP_a_mgl"){
  df.sim.outs <- read.csv(outfile_ext) %>% 
    mutate(DIP_a_mgl=DIP_a/V_w_a)
  metric_value <- approx(x=df.sim.outs$t,y=df.sim.outs[,metric_name],xout=365*simyears)$y
  return(metric_value)
}

