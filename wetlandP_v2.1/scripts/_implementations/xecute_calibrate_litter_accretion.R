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

sub_loop_sites_calc_cost <- expression({
  # LOOP THROUGH INPUT FILES FOR EACH SITE
  for (s in site.sample){
    rm(list=vars)
    j <- which(df.site.pars$site.plot == s)
    print(s)
    # UPDATE PARAMETERS ---------------- 
    setwd(file.path(wrkdir,"scripts")) # navigate to scripts folder 
    cat("sourcing 'edit_parameters_processes_intact_core.R'\n")
    source("edit_parameters_processes_accretion.R")
    cat("sourcing 'initialize.R'\n")
    source("initialize.R")
    setwd(wrkdir) # set wd back to working directory.
    
    eval(sub_load_dependancies_and_source_code)
    sim_metric[s] = fn_sim_ode_accretion(make_plots=T ,save_data=T)
  }

  # make dataframe for observed vs simulated 
  df.obs_metric <- df.site.pars %>% 
    mutate(id,obs_metric = obs_IM_a,obs_error=1) %>% 
    select(id,obs_metric,obs_error)
  df.sim_metric <- data.frame(id=names(sim_metric),sim_metric=sim_metric%>%unlist)
  df.cost <- left_join(df.obs_metric,df.sim_metric) %>% 
    mutate(sqresid=(obs_metric - sim_metric)**2,
           cost=sqresid/obs_error)
  cost_sum = sum(df.cost$cost)
})

try({
# MANAGE INPUT/OUTFILES & SET PARAMETERS & INITIALIZE STATE VARIABLES -------------------
eval(sub_parse_source_code)
eval(sub_evaluate_source_code)
vars <- grep("^fn_|^sub_|^wrkdir",ls(),invert=T,value=T) # list of variables to clear before each iteration

df.site.pars <- readxl::read_xlsx("inputs/lcbp_sites/lcbp_input_concentrations.xlsx",
                                  sheet="parameters_siteplot",skip=1) %>%
  mutate(street=case_when(
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

sites <- df.site.pars$site.plot
sim_metric <- vector(mode="numeric",length=length(sites))
names(sim_metric) <- sites
rowindexes = which(df.site.pars$site.plot %in% sites)
#eval(sub_loop_sites_calc_cost)
# LOOP THROUGH INPUT FILES FOR EACH SITE
for (s in sites){
  rm(list=vars)
  j <- which(df.site.pars$site.plot == s)
  print(s)
  eval(sub_evaluate_source_code)
  #sim_metric[s] = fn_sim_ode_accretion()
  try(system.time({
    # UPDATE PARAMETERS ---------------- 
    setwd(file.path(wrkdir,"scripts")) # navigate to scripts folder 
    cat("sourcing 'edit_parameters_processes_intact_core.R'\n")
    source("edit_parameters_processes_accretion.R")
    cat("sourcing 'initialize.R'\n")
    source("initialize.R")
    setwd(wrkdir) # set wd back to working directory.
    sim_metric[s] = fn_sim_ode_accretion(make_plots=F,save_data=F)
  }))
}
df.obs_metric <- df.site.pars %>% 
  mutate(obs_metric = obs_IM_a,obs_error=1) %>% 
  select(id,site.plot,obs_metric,obs_error)
df.sim_metric <- data.frame(site.plot=names(sim_metric),sim_metric=sim_metric%>%unlist) %>%
  mutate(sim_metric=as.numeric(sim_metric))
df.cost <- left_join(df.obs_metric,df.sim_metric) %>% 
  mutate(sqresid=(obs_metric - sim_metric)**2,
         cost=sqresid/obs_error)
cost_sum = sum(df.cost$cost)

ggplot(data=df.cost,aes(y=cost,x=id))+
  geom_col()
ggplot(data=df.cost,aes(y=sim_metric,x=obs_metric))+
  geom_smooth(method='lm')+
  geom_point()+
  ggrepel::geom_text_repel(aes(label=id),size=2)+
  geom_abline(intercept=0,slope=1)


sim_metric <- vector(mode="numeric",length=length(sites))
names(sim_metric) <- sites
rowindexes = which(df.site.pars$site.plot %in% sites)
sim_PIP_a = sim_metric
sim_DIP_a = sim_metric
sim_IP_a = sim_metric
sim_OP_a = sim_metric
sim_shootP = sim_metric
sim_litterP <- sim_metric
sim_ROP_a <- sim_metric
sim_LOP_a <- sim_metric

for (s in sites){
  j <- which(df.site.pars$site.plot == s)
  print(s)
  setwd(file.path(wrkdir,"scripts")) # navigate to scripts folder 
  cat("sourcing 'edit_parameters_processes_intact_core.R'\n")
  source("edit_parameters_processes_accretion.R")
  # NAME OUTPUT FILES -----------------------------------------------
  setwd(wrkdir) # set wd back to working directory.
  outfile_ext <- paste0(wrkdir,"/outputs/outputs_extended_",simname,"_",format(Sys.time(), "%Y-%m-%d"),".csv")
  df.sim.outs <- read.csv(outfile_ext)
  print(outfile_ext)
  sim_shootP[s] <- approx(x=df.sim.outs$t,y=df.sim.outs[,"shootP"],xout=365*simyears)$y %>% as.numeric
  sim_litterP[s] <- approx(x=df.sim.outs$t,y=df.sim.outs[,"litterP"],xout=365*simyears)$y %>% as.numeric
  sim_ROP_a[s] <- approx(x=df.sim.outs$t,y=df.sim.outs[,"ROP_a"],xout=365*simyears)$y %>% as.numeric
  sim_LOP_a[s] <- approx(x=df.sim.outs$t,y=df.sim.outs[,"LOP_a"],xout=365*simyears)$y %>% as.numeric
  sim_PIP_a[s] <- approx(x=df.sim.outs$t,y=df.sim.outs[,"PIP_a"],xout=365*simyears)$y %>% as.numeric
  sim_DIP_a[s] <- approx(x=df.sim.outs$t,y=df.sim.outs[,"DIP_a"],xout=365*simyears)$y %>% as.numeric
  sim_IP_a[s] <- sim_DIP_a[s]+sim_PIP_a[s]
  sim_OP_a[s] <- sim_litterP[s]+sim_ROP_a[s]+sim_LOP_a[s]
}

df.obs_metric <- df.site.pars %>% 
  mutate(obs_metric = obs_OP_a,obs_error=1) %>% 
  select(id,site.plot,starts_with("obs"),obs_metric,obs_error)
# make dataframe for observed vs simulated 
df.sim_metric <- data.frame(site.plot=names(sim_metric),
                            sim_OP_a = sim_OP_a %>% unlist,
                            sim_shootP = sim_shootP %>% unlist,
                            sim_litterP = sim_litterP %>% unlist,
                            sim_ROP_a = sim_ROP_a %>% unlist,
                            sim_LOP_a = sim_LOP_a %>% unlist,
                            sim_IP_a = sim_IP_a %>% unlist
                            ) %>%
  mutate(sim_metric=as.numeric(sim_metric),
         sim_metric=sim_OP_a)
df.cost_org <- left_join(df.obs_metric,df.sim_metric) %>% 
  mutate(sqresid=(obs_metric - sim_metric)**2,
         cost=sqresid/obs_error) %>%
  separate(site.plot,into=c("site","plot"),remove=F)
cost_sum = sum(df.cost$cost)
# param_names <- c("k_ad","k_E","k_PIP2Ex","k_diff_STD")
# param_values <- c(0.2,0.3,3,.4)
# names(param_values) <- param_names
# #p = c(parameters[which(parameters %in% param_names)],param_values)
# #p = "k_ad"

# sub_assign_new_parameter_values <- expression({
#   for(p in param_names){
#     assign(p,pars[p])
#     pars[p] = param_values[p]
#   }
# })
ggplot(data=df.cost_org,aes(y=cost,x=id))+
  geom_col()
ggplot(data=df.cost_org,aes(y=sim_OP_a/0.001,x=obs_OM_a,color=site))+
  geom_smooth(method='lm',se=F)+
  geom_point()+
  ggrepel::geom_text_repel(aes(label=id),size=2)+
    geom_abline(intercept=0,slope=1)
ggplot(data=df.cost_org,aes(y=cost,x=id))+
  geom_col()
ggplot(data=df.cost_org,aes(y=sim_OP_a,x=obs_OP_a,color=site))+
  geom_smooth(method='lm',se=F)+
  geom_point()+
  ggrepel::geom_text_repel(aes(label=id),size=2)+
  geom_abline(intercept=0,slope=1)
ggplot(data=df.cost_org,aes(y=sim_litterP_a+sim_OP_a,x=obs_litterP_a,color=site))+
  geom_smooth(method='lm',se=F)+
  geom_point()+
  ggrepel::geom_text_repel(aes(label=id),size=2)+
  geom_abline(intercept=0,slope=1)
ggplot(data=df.cost_org,aes(y=sim_IP_a+sim_OP_a,x=obs_TP_a,color=site))+
  geom_smooth(method='lm',se=F)+
  geom_point()+
  ggrepel::geom_text_repel(aes(label=id),size=2)+
  geom_abline(intercept=0,slope=1)
ggplot(data=df.cost_org,aes(y=sim_IP_a,x=obs_IP_a,color=site))+
  geom_smooth(method='lm',se=F)+
  geom_point()+
  ggrepel::geom_text_repel(aes(label=id),size=2)+
  geom_abline(intercept=0,slope=1)
  

}) # END TRY

save.image(paste0(wrkdir,"/outputs/env_","xecute_calibrate_litter_accretion","_",format(Sys.time(), "%Y-%m-%d_%h%m%s"),".Rdata"))

  