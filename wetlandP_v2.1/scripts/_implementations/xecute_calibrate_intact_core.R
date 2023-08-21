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

sub_load_dependancies_and_source_code <- expression({
  try({
  # LOAD DEPENDANCIES and SOURCE CODE ------------------------
  wrkdir <- "C:/Workspace/wetlandP/model_versions/wetlandP_v2/wetlandP_v2.1"
  setwd(file.path(wrkdir,"scripts")) # navigate to scripts folder 
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
  
  cat("sourcing 'initialize.R'\n")
  source("initialize.R")
  
  setwd(wrkdir) # set wd back to working directory.
  })
})


sub_loop_sites_calc_cost <- expression({
  # LOOP THROUGH INPUT FILES FOR EACH SITE
  for (s in site.sample){
    rm(list=vars)
    j <- which(df.site.pars$site.plot == s)
    print(s)
    eval(sub_load_dependancies_and_source_code)
    sim_SRP_mgl[s] = fn_sim_ode_intact_core()
  }
  # make dataframe for observed vs simulated 
  df.obs_metric <- df.site.pars %>% 
    mutate(id,obs_metric = test_SRP_mgl_mean,obs_error=test_SRP_mgl_sd) %>% 
    select(id,obs_metric,obs_error)
  df.sim_metric <- data.frame(id=names(sim_SRP_mgl),sim_metric=sim_SRP_mgl%>%unlist)
  df.cost <- left_join(df.obs_metric,df.sim_metric) %>% 
    mutate(sqresid=(obs_metric - sim_metric)**2,
           cost=sqresid/obs_error)
  cost_sum = sum(df.cost$cost)
})

# MANAGE INPUT/OUTFILES & SET PARAMETERS & INITIALIZE STATE VARIABLES -------------------
eval(sub_load_dependancies_and_source_code)
vars <- grep("^fn_|^sub_",ls(),invert=T,value=T) # list of variables to clear before each iteration

df.site.pars <- readxl::read_excel("inputs/coreflux/df.IC.finalSRP.soil.0t10.xlsx",sheet="parameters") %>% filter(treatment=="O2")
sites <- df.site.pars$site.plot
df.site.pars$id <- sites
sim_SRP_mgl <- vector(mode="numeric",length=length(sites))
names(sim_SRP_mgl) <- sites
site.sample <- sites %>% sample(round(length(sites)*1,0),replace=F)
rowindexes = which(df.site.pars$site.plot %in% site.sample)

eval(sub_loop_sites_calc_cost)

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
  
ggplot(data=df.cost %>% filter(id %in% site.sample),aes(y=sim_metric,x=obs_metric))+
  geom_smooth(method='lm')+
  geom_point()+
  ggrepel::geom_text_repel(aes(label=id),size=2)+
    geom_abline(intercept=0,slope=1)
ggplot(data=df.cost %>% filter(id %in% site.sample),aes(y=cost,x=id))+
  geom_col()

  