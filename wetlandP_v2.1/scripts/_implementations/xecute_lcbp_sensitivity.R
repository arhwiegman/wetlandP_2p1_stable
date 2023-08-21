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

fn_parse_r_script <- function(script_path){
  cat(paste(script_path,"..."))
  script <- readLines(script_path)
  cat("parsed\n")
  return(parse(text=paste(script,collapse="\n")))
}

sub_parse_source_code <- expression({
  try({
    # PARSE DEPENDANCIES and SOURCE CODE ------------------------
    wrkdir <- "C:/Workspace/wetlandP/model_versions/wetlandP_v2/wetlandP_v2.1"
    setwd(file.path(wrkdir,"scripts")) # navigate to scripts folder 
    cat("parsing model scripts\n")
    # dependancies
    sub_dependancies <- fn_parse_r_script("dependancies.R")
    # functions
    sub_functions <- fn_parse_r_script("functions.R")
    # subroutines
    sub_subroutines <- fn_parse_r_script("subroutines.R")
    # model
    sub_model <- fn_parse_r_script("model.R")
    # parameters
    sub_parameters<- fn_parse_r_script("parameters.R")
    # initialize
    sub_initialize <- fn_parse_r_script("initialize.R")
    setwd(wrkdir) # set wd back to working directory.
  })
})
  
sub_evaluate_source_code <- expression({
  try({
    # EVALUATE SOURCE CODE -----------------
    cat("evaluating source code...\n")
    cat("'dependancies.R'...")
    eval(sub_dependancies)
    cat("evaluated!\n")
    cat("'functions.R'...")
    eval(sub_functions)
    cat("evaluated!\n")
    cat("'subroutines.R'...")
    eval(sub_subroutines)
    cat("evaluated!\n")
    cat("'model.R'...")
    eval(sub_model)
    cat("evaluated!\n")
    cat("'parameters.R'...")
    eval(sub_parameters)
    cat("evaluated!\n")
    cat("'initialize.R'...")
    eval(sub_initialize)
    cat("evaluated!\n")
  })
})

sub_set_forcings_and_parameters_for_sim <- expression({
  forcingfile = paste0("df.hydroclimate.1m.",s,".csv")
  inputdir=file.path(wrkdir,"inputs/")
  simname = paste0(parsheet,"_",s)
  # select row j of df sim pars, and remove any columns containing NA
  par.list <- as.list(df.sim.pars[j,colSums(is.na(df.sim.pars[j,]))!=1])
  for(i in names(par.list)){
    assign(i,unname(unlist(par.list[i])))
  }
  IO_DIP_E_langmuir = T
  IO_anoxic = F
  IO_variable_k_Ex_max = F
  IO_variable_k_E = T
  IO_use_k_DIP_E = F
  IO_Q_net = T
  IO_HRT_power_model = F
  # compile global env vectors into parameters vector
  eval(sub_mget_parameters_from_globenv_vectors)
})

sub_loop_parsheets_and_loop_sims <- expression({
  # parsheet must be declared before runing this loop
  try({
    df.sim.pars <- readxl::read_xlsx("inputs/lcbp_sites/lcbp_input_concentrations.xlsx",
                                     sheet=parsheet,skip=1) %>%
      mutate(across(starts_with("IO_"),~as.logical(.))) %>% 
      mutate(
        street=case_when(
          site=="LC"~"Prindle Rd",
          site=="OCD"~"Union St",
          site=="OCSP"~"Swamp Rd",
        ),
        street_abbrev1=case_when(
          site=="LC"~"PrRd",
          site=="OCD"~"UnSt",
          site=="OCSP"~"SwRd",
        ),
        street_abbrev2=case_when(
          site=="LC"~"Pr",
          site=="OCD"~"Un",
          site=="OCSP"~"Sw",
        ),
        id=paste0(street_abbrev2,plot))
    
    sites <- df.sim.pars$site.plot
    rowindexes = which(df.sim.pars$site.plot %in% sites)
    # LOOP THROUGH INPUT FILES FOR EACH SITE
    for (s in sites){
      rm(list=vars)
      j <- which(df.sim.pars$site.plot == s)
      print(s)
      eval(sub_evaluate_source_code)
      #sim_metric[s] = fn_sim_ode_accretion()
      try(system.time({
        # UPDATE PARAMETERS & FORCINGS ---------------- 
        eval(sub_update_forcings_and_parameters)
        eval(sub_initialize)
        # RUN SIMULATION ---------------
        sim <- fn_sim_ode(y = states, 
                          times = simtimes, 
                          func = wetlandP_inout, 
                          parms = parameters,
                          method="lsoda")
        
        # MANIPULATE DATA & PLOT OUTPUTS ---------------------------------------
        #eval(sub_plot_outputs) 
        
        # SAVE IMAGE OF SIMULATION --------------------------------------
        save.image(file=simfile)
      }))
    } # end for
  }) # end try
}) # end sub


sub_post_process_lcbp_sensitivty <- expression({
  df.sim.pars <- readxl::read_xlsx("inputs/lcbp_sites/lcbp_input_concentrations.xlsx",
                                   sheet=parsheet,skip=1) %>%
    mutate(across(starts_with("IO_"),~as.logical(.))) %>% 
    mutate(
      street=case_when(
        site=="LC"~"Prindle Rd",
        site=="OCD"~"Union St",
        site=="OCSP"~"Swamp Rd",
      ),
      street_abbrev1=case_when(
        site=="LC"~"PrRd",
        site=="OCD"~"UnSt",
        site=="OCSP"~"SwRd",
      ),
      street_abbrev2=case_when(
        site=="LC"~"Pr",
        site=="OCD"~"Un",
        site=="OCSP"~"Sw",
      ),
      id=paste0(street_abbrev2,plot))
  
  sites <- df.sim.pars$site.plot
  rowindexes = which(df.sim.pars$site.plot %in% sites)
  sim_metric <- vector(mode="numeric",length=length(sites))
  names(sim_metric) <- sites
  rowindexes = which(df.sim.pars$site.plot %in% sites)
  sim_IM_deposition = sim_metric
  sim_litterP_t365 = sim_metric
  sim_shootM_t49 = sim_metric
  sim_shootM_t92 = sim_metric
  sim_shootM_t458 = sim_metric
  sim_init_TP = sim_metric
  sim_final_TP = sim_metric
  sim_init_TP_b = sim_metric
  sim_final_TP_b = sim_metric
  sim_in_IM = sim_metric
  sim_out_IM = sim_metric
  sim_in_DIP = sim_metric
  sim_out_DIP = sim_metric
  sim_in_TP = sim_metric
  sim_out_TP = sim_metric
  
  for (s in sites){
    j <- which(df.sim.pars$site.plot == s)
    print(s)
    eval(sub_update_forcings_and_parameters)
    outfile <- paste0(wrkdir,"/outputs/outputs_",simname,"_",format(Sys.time(),"%Y-%m-%d"),".csv")
    df.hydroclimate <- read.csv(file.path(inputdir,forcingfile))
    df.sim.outs <- read.csv(outfile) %>% 
      rename(t=time) %>% 
      left_join(df.hydroclimate %>% 
                  rename(site.plot=site_plot)) %>% 
      left_join(df.sim.pars %>% filter(site.plot==s)) %>% mutate(
        forcingfile = forcingfile,
        outfile = outfile,
        TP=DIP_a+DIP_b+LOP_a+LOP_b+ROP_a+ROP_b+shootP+litterP+rootP+PIP_a+PIP_b,
        in_TP = in_DIP+in_ROP+in_LOP+in_PIP,
        out_TP = out_DIP+out_ROP+out_LOP+out_PIP)
    print(outfile)
    postfilename = paste0(wrkdir,"/outputs/postprocessed_outputs_",simname,"_",format(Sys.time(),"%Y-%m-%d"),".csv")
    write_csv(df.sim.outs,
              file=postfilename)
    
    sim_IM_deposition[s] = df.sim.outs %>% filter(t==365) %>% .[,"IM_b"] - 
      df.sim.outs %>% filter(t==0) %>% .[,"IM_b"]
    sim_litterP_t365[s] = df.sim.outs %>% filter(t==365) %>% .[,"litterP"]
    sim_shootM_t49[s] = df.sim.outs %>% filter(t==49) %>% .[,"shootP"]/k_BM2P
    sim_shootM_t92[s] = df.sim.outs %>% filter(t==92) %>% .[,"shootP"]/k_BM2P
    sim_shootM_t458[s] = df.sim.outs %>% filter(t==458) %>% .[,"shootP"]/k_BM2P
    sim_init_TP[s] = df.sim.outs %>% filter(t==0) %>% .[,"TP"]
    sim_final_TP[s] = df.sim.outs %>% filter(t==nrow(df.sim.outs)-1) %>% .[,"TP"]
    sim_in_DIP[s] = sum(df.sim.outs$in_DIP,na.rm=T)
    sim_out_DIP[s] = sum(df.sim.outs$out_DIP,na.rm=T)
    sim_in_TP[s] = sum(df.sim.outs$in_TP,na.rm=T)
    sim_out_TP[s] = sum(df.sim.outs$out_TP,na.rm=T)
    sim_in_IM[s] = sum(df.sim.outs$in_TP,na.rm=T)
    sim_out_IM[s] = sum(df.sim.outs$out_TP,na.rm=T)
  }
  if(which(parsheet==parsheets)==1){
    print(parsheet)
    df.sim.outs_summary <- df.sim.pars
    df.sim.outs_summary$sim_IM_deposition = sim_IM_deposition
    df.sim.outs_summary$sim_litterP_t365 = sim_litterP_t365
    df.sim.outs_summary$sim_shootM_t49 = sim_shootM_t49
    df.sim.outs_summary$sim_shootM_t92 = sim_shootM_t92
    df.sim.outs_summary$sim_shootM_t458 = sim_shootM_t458
    df.sim.outs_summary$sim_init_TP = sim_init_TP
    df.sim.outs_summary$sim_final_TP = sim_final_TP
    df.sim.outs_summary$sim_in_TP = sim_in_TP
    df.sim.outs_summary$sim_out_TP = sim_out_TP
    df.sim.outs_summary$sim_in_DIP = sim_in_DIP
    df.sim.outs_summary$sim_out_DIP = sim_out_DIP
    df.sim.outs_summary <- df.sim.outs_summary %>% mutate(
      sim_delta_TP = sim_final_TP - sim_init_TP,
      sim_delta_TP_yr = sim_delta_TP/2,
      sim_TP_retention = (sim_in_TP - sim_out_TP),
      sim_TP_retention_efficiency = 100*(sim_in_TP - sim_out_TP)/sim_in_TP,
      sim_TP_retention_eff2 = 100*(1-sim_out_TP/sim_in_TP),
      sim_TP_check = sim_delta_TP - sim_TP_retention)
    # make dataframe for observed vs simulated 
    df.sim.outs_summary$parsheet <- parsheet
  }else{
    print(parsheet)
  dos <- df.sim.pars
  dos <- dos %>% mutate(
    sim_IM_deposition = sim_IM_deposition,
    sim_litterP_t365 = sim_litterP_t365,
    sim_shootM_t49 = sim_shootM_t49,
    sim_shootM_t92 = sim_shootM_t92,
    sim_shootM_t458 = sim_shootM_t458,
    sim_init_TP = sim_init_TP,
    sim_final_TP = sim_final_TP,
    sim_in_IM = sim_in_IM,
    sim_out_IM = sim_out_IM,
    sim_in_DIP = sim_in_DIP,
    sim_out_DIP = sim_out_DIP,
    sim_in_TP = sim_in_TP,
    sim_out_TP = sim_out_TP,
    sim_delta_TP = sim_final_TP - sim_init_TP,
    sim_delta_TP_yr = sim_delta_TP/2,
    sim_TP_retention = (sim_in_TP - sim_out_TP),
    sim_TP_retention_efficiency = 100*(sim_in_TP - sim_out_TP)/sim_in_TP,
    sim_TP_retention_eff2 = 100*(1-sim_out_TP/sim_in_TP),
    sim_TP_check = sim_delta_TP - sim_TP_retention,
    parsheet = parsheet)
  df.sim.outs_summary <- df.sim.outs_summary %>% bind_rows(dos)
  }
})

# MANAGE INPUT/OUTFILES & SET PARAMETERS & INITIALIZE STATE VARIABLES -------------------
eval(sub_parse_source_code)
eval(sub_evaluate_source_code)
sub_update_forcings_and_parameters <- fn_parse_r_script("scripts/_implementations/edit_parameters_processes_lcbp_sensitivity.R")
vars <- grep("^fn_|^sub_|^wrkdir",ls(),invert=T,value=T) # list of variables to clear before each iteration
sheets = readxl::excel_sheets("inputs/lcbp_sites/lcbp_input_concentrations.xlsx")
#parsheets = sheets[sheets %>% str_detect("pars_lcbp") %>% na.omit] %>% .[(length(.)-2):length(.)]
parsheets = sheets[sheets %>% str_detect("pars_lcbp") %>% na.omit]
#parsheets = c("pars_lcbp_stream_pow_Hx","pars_lcbp_stream_power")

# RUN THE MODEL -----
for (parsheet in parsheets){
   eval(sub_loop_parsheets_and_loop_sims)
}

# POST PROCESS THE DATA ----
for (parsheet in parsheets){
  try({
  eval(sub_post_process_lcbp_sensitivty)
  })
}



df.sim.outs_summary <- df.sim.outs_summary %>% 
  mutate(scenario=str_sub(parsheet,start=11)) %>% 
  mutate(obs_IM_a_minus_ash=ifelse((obs_IM_a-(obs_OM_a*0.1))<0,0,obs_IM_a-(obs_OM_a*0.1))) %>%
  mutate(Csource=ifelse(str_detect(scenario,"stream"),"stream","siphon"),
         Cx ="Cx1",
         Cx = case_when(
           str_detect(scenario,"0p5x")~"Cx0.5",
           str_detect(scenario,"2x")~"Cx2",
           T~Cx),
         Hx= ifelse(str_detect(scenario,"Hx"),"Hx1.2","Hx1"),
         CxHx = paste(Hx,Cx))
View(df.sim.outs_summary)
write_csv(df.sim.outs_summary,paste0("outputs/outputs_summary_lcbp_sensitivity_sedsretained",format(Sys.time(),"%Y-%m-%d_%Hm%Mm"),".csv"))


# observed vs simulated plots
ggplot(df.sim.outs_summary) + geom_point(aes(obs_shootM_t458,sim_shootM_t458))+geom_point(aes(obs_shootM_t92,sim_shootM_t92),color="red")+geom_point(aes(obs_shootM_t49,sim_shootM_t49),color="red")+geom_abline(slope=1,intercept=0)

ggplot(df.sim.outs_summary) + 
  geom_point(aes(obs_shootM_t458,sim_shootM_t458),color="black")+
  geom_point(aes(obs_shootM_t92,sim_shootM_t92),color="red",pch=2)+
  geom_point(aes(obs_shootM_t49,sim_shootM_t49),color="red",pch=3)+
  geom_abline(slope=1,intercept=0)+
  xlab("observed biomass (g dw/m^2)")+
  ylab("simulated biomass (g dw/m2)")
ggplot(df.sim.outs_summary) + 
  geom_point(aes(est_Litter_tp_gm2_mean,sim_litterP_t365),color="black")+
  geom_abline(slope=1,intercept=0)+xlab("osberved litter P (g P/m^2) 7/15/20") + ylab("simulated litter P (g P/m^2)  7/15/20")

ggplot(df.sim.outs_summary %>% mutate(scenario=str_sub(parsheet,start=11)) %>% 
         mutate(obs_IM_a_minus_ash=ifelse((obs_IM_a-(obs_OM_a*0.1))<0,0,obs_IM_a-(obs_OM_a*0.1)))) + 
  geom_point(aes(x=obs_IM_a_minus_ash,y=sim_IM_deposition),size=2,stroke=1)+scale_shape_manual(values=c(4,5,6,7,8,11,10))+facet_wrap(vars(scenario),scale="free_y")+
  geom_abline(slope=1,intercept=0)+ylab("simulated inorganic deposition (g dw/m2/yr) 7/15/20")+xlab("observed inorganic accretion - ash (g dw/m^2/yr) 7/15/20")

ggplot(df.sim.outs_summary)+
  geom_col(aes(x=scenario,y=sim_delta_TP_yr,fill=sim_delta_TP_yr),color="black")+labs(x="Site and Sampling Plot",title="Net TP Balance (g P/m^2/yr)")+theme(axis.title.y=element_blank(),axis.text.x=element_text(angle=90))+facet_wrap(vars(id),scales="free_y")+scale_fill_gradient2()

ggplot(df.sim.outs_summary %>% filter(plot==2,k_HRT!=1e6,str_detect(scenario,"xC")))+
  geom_col(aes(x=scenario,y=sim_delta_TP_yr,fill=sim_delta_TP_yr),color="black")+labs(x="Site and Sampling Plot",title="Net TP Balance (g P/m^2/yr)")+theme(axis.title.y=element_blank(),axis.text.x=element_text(angle=90))+facet_wrap(vars(id),scales="free_y")+scale_fill_gradient2()

ggplot(df.sim.outs_summary %>% filter(plot==2,k_HRT!=1e6,!str_detect(scenario,"xC")))+
  geom_col(aes(x=scenario,y=sim_delta_TP_yr,fill=sim_delta_TP_yr),color="black")+labs(x="Site and Sampling Plot",title="Net TP Balance (g P/m^2/yr)")+theme(axis.title.y=element_blank(),axis.text.x=element_text(angle=90))+facet_wrap(vars(id),scales="free_y")+scale_fill_gradient2()

ggplot(df.sim.outs_summary %>% filter(str_detect(scenario,"stream_power")))+
  geom_col(aes(x=id,y=sim_delta_TP_yr,fill=sim_delta_TP_yr),color="black")+labs(x="Site and Sampling Plot",title="Net TP Balance (g P/m^2/yr)")+theme(axis.title.y=element_blank(),axis.text.x=element_text(angle=90))+scale_fill_gradient2()

ggplot(df.sim.outs_summary %>% 
         filter(k_HRT==100,str_detect(scenario,"stream.*100d")))+
  geom_col(aes(x=id,y=sim_delta_TP_yr,fill=sim_delta_TP_yr),color="black")+
    labs(x="Site and Sampling Plot",title="Net TP Balance (g P/m^2/yr)")+
    facet_wrap(vars(scenario))+
    theme(axis.title.y=element_blank(),axis.text.x=element_text(angle=90))+
    scale_fill_gradient2()

ggplot(df.sim.outs_summary %>% filter(plot==2,str_detect(parsheet,"stream.*100d")))+
  geom_col(aes(x=CxHx,y=sim_delta_TP,fill=sim_delta_TP),color="black")+
  geom_point(aes(x=CxHx,y=sim_in_TP,fill=sim_in_TP),color="blue")+
  geom_point(aes(x=CxHx,y=-sim_out_TP,fill=sim_out_DIP),color="red")+
  labs(x="Concentration (C) and Water Height (H) Factor",title="Net TP Balance (g P/m^2/yr)")+
  facet_wrap(vars(assumptions,id),scales='free_y')+
  theme(axis.title.y=element_blank(),axis.text.x=element_text(angle=90))+
  scale_fill_gradient2()

# ------------ 
save.image(paste0(wrkdir,"/outputs/env_","xecute_lcbp_sensitivity_sedsretained","_",format(Sys.time(), "%Y-%m-%d_%h%m%s"),".Rdata"))
# # =========END OF FILE===================


