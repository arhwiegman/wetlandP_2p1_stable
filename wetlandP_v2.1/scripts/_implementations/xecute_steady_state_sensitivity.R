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
#rm(list = ls(all.names = TRUE)) # will clear all objects 
#gc() #free up memory and report the memory usage.

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
    j <- which(df.sim.pars$site.plot == s)
    print(s)
    # UPDATE PARAMETERS ---------------- 
    setwd(file.path(wrkdir,"scripts")) # navigate to scripts folder 
    cat("sourcing 'edit_parameters_processes_intact_core.R'\n")
    source("edit_parameters_processes_accretion.R")
    cat("sourcing 'initialize.R'\n")
    source("initialize.R")
    setwd(wrkdir) # set wd back to working directory.
    
    eval(sub_load_dependancies_and_source_code)
    sim_metric[s] = fn_sim_ode_accretion(make_plots=T,save_data=T)
  }

  # make dataframe for observed vs simulated 
  df.obs_metric <- df.sim.pars %>% 
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
# WARINING ANY VARIABLES DECLARED ABOVE THIS LINE WILL BE DELETED EACH FOR LOOP
vars <- grep("^fn_|^sub_|^wrkdir",ls(),invert=T,value=T) # list of variables to clear before each iteration
# VARIABLES DECLARED BELOW THIS LINE WILL NOT BE DELETED

n = 10000
set.seed(1)
df.sim.pars_soil <- data.frame(
  k_LOI = runif(n,0.05,0.5),
  k_PSR = runif(n,0.05,0.5),
  k_Ex_max = runif(n,1,10),
  k_TSS = runif(n,1,100),
  k_TP = runif(n,0.005,0.5),
  k_f_SRP = runif(n,0,1),
  k_f_OSS = runif(n,0,1),
  # hydrology pars are constant
  k_HRT = rep(1e1,n), # 10 days
  k_Hw = rep(0.5,n), # 0.5 m
  simyears = rep(10/365,n),
  randoms = rep("local_conc",n),
  n = 1:n
)

df.sim.pars_hydro <- data.frame(
  k_LOI = rep(0.10,n),
  k_PSR = rep(0.2,n),
  k_Ex_max = rep(4,n),
  k_TSS = rep(15,n),
  k_TP = rep(0.05,n),
  k_f_SRP = rep(0.3,n),
  k_f_OSS = rep(0.5,n), 
  k_Hw = runif(n,0.1,1),
  k_HRT = runif(n,1e-3,1e3), # from 0.001 days to 1000 days
  k_Tw = runif(n,0,25),
  simyears = runif(n,1,100)/365, 
  randoms = rep("local_hydro",n),
  n = 1:n
)

df.sim.pars_soil_hydro <- data.frame(
  k_LOI = runif(n,0.05,0.5),
  k_PSR = runif(n,0.05,0.5),
  k_Ex_max = runif(n,0,1),
  k_TSS = runif(n,5,50),
  k_TP = runif(n,0.005,0.5),
  k_f_SRP = runif(n,0,1),
  k_f_OSS = runif(n,0,1), 
  k_Hw = runif(n,0.1,1),
  k_HRT = runif(n,1e-3,1e3),
  simyears = runif(n,1,100)/365,
  randoms = rep("local_all",n),
  n = 1:n
)

df.sim.pars_stoch <- data.frame(
  k_SRP2PIP = rlnorm(n,log(k_SRP2PIP)), # g P/d.w. | ratio of LOP to SRP | 8.9e-1 for prindle, 1.42 for swamp rd, 6.2e-1 for union st
  k_ISS2P = rlnorm(n,log(k_ISS2P)), # g P/d.w. | P content of inorganic suspended sediments |   site data  0.002 for prindle rd, 0.0009 for union st, 0.00094 for swamp rd
  # Biomass Growth & Mortality
  k_BM2P = rlnorm(n,log(k_BM2P)), # g/g P/d.w.| P content of biomass| McJannet et al. 1996 .001 - 0.003; Morris & Bowden 1986 0.002; Ch 2 data 0.001 to 0.003
  k_f_G_shoot =	 runif(n,0,1),	# fraction | fraction of NPP allocated to shoot growth (shoot_NPP/total_NPP) |	Morris et al 1984 0.2 - 0.5
  k_NPP = rlnorm(n,log(k_NPP)), # g m-2 y-1| combined annual rate of NPP for above and below ground biomass | Morris et al. 1984 1000 to 4000 
  k_M = rlnorm(n,log(k_M)),
  k_whc = rlnorm(n,log(k_whc)), # | a small volume of water to prevent errors associated with empty compartments| best guess based on fit of oven dry verses air dry moisture content
  k_diff_STD = rlnorm(n,log(k_diff_STD)), # m^2/d | effective diffusion coefficient| calibrated to intact core data; Marois & Mitsch 2016 calibrated value was 2e−5 m2 d−1
  k_ad = rlnorm(n,log(k_ad)), # 1/d | adsorption first order rate coefficient | Wang et al. 2003 1.75, Marois & Mitsch 2016 used 
  k_E = rlnorm(n,log(k_E)), # m^3/g | langmuir constant of adsorption (bond energy) | Calibrated to intact core data this value depends on what metric is used to define Ex_max,  Wang et al. 2003 2.75 m3 kg-1
  # decay
  k_f_labile = runif(n,0,1),# g/g| labile fraction organic matter | Morris & Bowden 1986 refractory fraction of 0.2
  k_decay_litter = rlnorm(n,log(k_decay_litter)), # 1/d | litter decomposition rate coefficient at STD temp| Morris & Bowden, Wiegman Ch 3, # Longhi et al. 2008 k = ranged from 0.01 1/d to 0.0027 1/d
  k_decay_LOP = rlnorm(n,log(k_decay_LOP)),  # 1/d | LOP decomposition rate coefficient at STD temp|   Marois & Mitsch 2016 DOP rate is 0.01, while LPOP rate is 0.003, since we do not model DOP LOP decay should be between 0.001 - 0.01
  k_decay_ROP = rlnorm(n,log(k_decay_ROP)), # 1/d | ROP decomposition rate coefficient at STD temp when soils are unsaturated and aerobic(H_w < 0) | Morris & Bowden 1986 assume refractory OM does not decompose, however this is assuming saturated soils, so we assume that when H_w < 0 that ROP decomposes at between 1e-5 and 5e-5 based on value from Marois & Mitsch 2016 of 2.5e-5
  k_rp_o = rlnorm(n,log(k_rp_o)),  # m| average radius of organic particles| input
  
  simyears = rep(1,n),
  randoms = rep("stoch",n),
  n = 1:n
)

df.sim.pars <- full_join(df.sim.pars_soil,df.sim.pars_hydro) %>% 
  full_join(df.sim.pars_soil_hydro) %>%
  full_join(df.sim.pars_stoch) %>% 
  mutate(id=paste("steady","rand",randoms,n,sep="_"))
n_sims = 10000
. <- df.sim.pars %>% filter(n<=n_sims)
sims <- .$id

# prepare columns for output metrics
init_vals <- rep(NA,length(states)+1)
names(init_vals) <-c(names(states),"TP")
final_vals <- init_vals
dif <- init_vals
names(dif) <- paste0(names(init_vals),"_dif")
absdif <- init_vals
names(absdif) <- paste0(names(init_vals),"_absdif")
pctdif <- init_vals # percent difference
names(pctdif) <- paste0(names(init_vals),"_pctdif")
abspctdif <- init_vals
names(abspctdif) <- paste0(names(init_vals),"_abspctdif")

cnames <- names(c(init_vals,dif,absdif,pctdif,abspctdif))
rowindexes <- df.sim.pars$id

M.sim.outs <- matrix(ncol=length(cnames),nrow=length(rowindexes)) 
row.names(M.sim.outs) <- rowindexes
colnames(M.sim.outs) <- cnames
#eval(sub_loop_sites_calc_cost)
# LOOP THROUGH INPUT FILES FOR EACH SITE
system.time({
for (s in sims){
  rm(list=vars)
  j <- which(df.sim.pars$id == s)
  print(s)
  eval(sub_evaluate_source_code)
  #sim_metric[s] = fn_sim_ode_accretion()
  try(system.time({
    # UPDATE PARAMETERS ---------------- 
    setwd(file.path(wrkdir,"scripts")) # navigate to scripts folder 
    source("edit_parameters_processes_steady_state_sensitivity.R")
    cat("sourcing 'initialize.R'\n")
    source("initialize.R")
    setwd(wrkdir) # set wd back to working directory.
    out <- ode(y = states, 
               times = simtimes, 
               func = wetlandP, 
               parms = parameters,
               method = solver)
    init_vals <- unname(c(out[1,2:ncol(out)],TP=sum(out[1,5:ncol(out)])))
    final_vals <- unname(c(out[nrow(out),2:ncol(out)],TP=sum(out[nrow(out),5:ncol(out)])))
    dif <- final_vals - init_vals # difference
    absdif <- abs(dif)
    pctdif <- 100*(final_vals - init_vals)/init_vals # percent difference
    abspctdif <- abs(pctdif)
    M.sim.outs[j,] <- c(init_vals,dif,absdif,pctdif,abspctdif)
  }))
}# FOR LOOP
}) # END SYSTEM TIME
}) # END TRY

df.sim.outs <- cbind(df.sim.pars,M.sim.outs) %>% filter(!is.na(TP)) %>% 
  mutate(TP_b_dif = shootP_dif+litterP_dif+rootP_dif+ROP_b_dif+LOP_b_dif+PIP_b_dif+DIP_b_dif,
         TP_b_pctdif = TP_b_dif/(shootP+litterP+rootP+ROP_b+LOP_b+PIP_b+DIP_b))

write_csv(df.sim.outs,paste0(wrkdir,"/outputs/df.sim.outs_steady_state_sensitivity_nsims",n_sims,"_",format(Sys.time(), "%Y-%m-%d"),".csv"))

. <- df.sim.outs %>% 
  filter(randoms=="local_hydro") %>% 
  select(names(df.sim.pars),-n,"TP_b_pctdif") %>% 
  .[,colSums(is.na(.[,]))==0] %>% 
  select_if(is.numeric) %>% 
  cor %>%
  .[,colSums(!is.na(.[,]))>1] %>% 
  .[rowSums(!is.na(.[,]))>1,] %>% 
  .[1:(nrow(.)-1),"TP_b_pctdif"]
ggplot(data=NULL,aes(x=names(.),y=.))+geom_col(fill="lightgrey",color="black")+labs(y="pearson")+theme(axis.title.x=element_blank(),axis.text.x = element_text(angle = 90,hjust=1,vjust=0.5))+ylim(-1,1)#+scale_fill_gradient2(limits=c(-1,1))

. <- df.sim.outs %>% 
  filter(randoms=="local_conc") %>% 
  select(names(df.sim.pars),-n,"TP_b_pctdif") %>% 
  .[,colSums(is.na(.[,]))==0] %>% 
  select_if(is.numeric) %>% 
  cor %>%
  .[,colSums(!is.na(.[,]))>1] %>% 
  .[rowSums(!is.na(.[,]))>1,] %>% 
  .[1:(nrow(.)-1),"TP_b_pctdif"]
ggplot(data=NULL,aes(x=names(.),y=.))+geom_col(fill="lightgrey",color="black")+labs(y="pearson")+theme(axis.title.x=element_blank(),axis.text.x = element_text(angle = 90,hjust=1,vjust=0.5))+ylim(-1,1)#+scale_fill_gradient2(limits=c(-1,1))
. <- df.sim.outs %>% 
  filter(randoms=="local_all") %>% 
  select(names(df.sim.pars),-n,"TP_b_pctdif") %>% 
  .[,colSums(is.na(.[,]))==0] %>% 
  select_if(is.numeric) %>% 
  cor %>%
  .[,colSums(!is.na(.[,]))>1] %>% 
  .[rowSums(!is.na(.[,]))>1,] %>% 
  .[1:(nrow(.)-1),"TP_b_pctdif"]
ggplot(data=NULL,aes(x=names(.),y=.))+geom_col(fill="lightgrey",color="black")+labs(y="pearson")+theme(axis.title.x=element_blank(),axis.text.x = element_text(angle = 90,hjust=1,vjust=0.5))+ylim(-1,1)#+scale_fill_gradient2(limits=c(-1,1))
. <- df.sim.outs %>% 
  filter(randoms=="stoch") %>% 
  select(names(df.sim.pars),-n,"TP_b_pctdif") %>% 
  .[,colSums(is.na(.[,]))==0] %>% 
  select_if(is.numeric) %>% 
  cor %>%
  .[,colSums(!is.na(.[,]))>1] %>% 
  .[rowSums(!is.na(.[,]))>1,] %>% 
  .[1:(nrow(.)-1),"TP_b_pctdif"]
ggplot(data=NULL,aes(x=names(.),y=.))+geom_col(fill="lightgrey",color="black")+labs(y="pearson")+theme(axis.title.x=element_blank(),axis.text.x = element_text(angle = 90,hjust=1,vjust=0.5))+ylim(-1,1)#+scale_fill_gradient2(limits=c(-1,1))
. <- df.sim.outs %>% 
  filter(randoms=="local_hydro") %>% 
  select(names(df.sim.pars),-n,"TP_b_pctdif") %>% 
  .[,colSums(is.na(.[,]))==0] %>% 
  select_if(is.numeric) %>% 
  cor(method="spearman") %>%
  .[,colSums(!is.na(.[,]))>1] %>% 
  .[rowSums(!is.na(.[,]))>1,] %>% 
  .[1:(nrow(.)-1),"TP_b_pctdif"]
ggplot(data=NULL,aes(x=names(.),y=.))+geom_col(fill="grey",color="black")+labs(y="spearman")+theme(axis.title.x=element_blank(),axis.text.x = element_text(angle = 90,hjust=1,vjust=0.5))+ylim(-1,1)#+scale_fill_gradient2(limits=c(-1,1))
. <- df.sim.outs %>% 
  filter(randoms=="local_conc") %>% 
  select(names(df.sim.pars),-n,"TP_b_pctdif") %>% 
  .[,colSums(is.na(.[,]))==0] %>% 
  select_if(is.numeric) %>% 
  cor(method="spearman") %>%
  .[,colSums(!is.na(.[,]))>1] %>% 
  .[rowSums(!is.na(.[,]))>1,] %>% 
  .[1:(nrow(.)-1),"TP_b_pctdif"]
ggplot(data=NULL,aes(x=names(.),y=.))+geom_col(fill="grey",color="black")+labs(y="spearman")+theme(axis.title.x=element_blank(),axis.text.x = element_text(angle = 90,hjust=1,vjust=0.5))+ylim(-1,1)#+scale_fill_gradient2(limits=c(-1,1))

. <- df.sim.outs %>% 
  filter(randoms=="local_all") %>% 
  select(names(df.sim.pars),-n,"TP_b_pctdif") %>% 
  .[,colSums(is.na(.[,]))==0] %>% 
  select_if(is.numeric) %>% 
  cor(method="spearman") %>%
  .[,colSums(!is.na(.[,]))>1] %>% 
  .[rowSums(!is.na(.[,]))>1,] %>% 
  .[1:(nrow(.)-1),"TP_b_pctdif"]
ggplot(data=NULL,aes(x=names(.),y=.))+geom_col(fill="grey",color="black")+labs(y="spearman")+theme(axis.title.x=element_blank(),axis.text.x = element_text(angle = 90,hjust=1,vjust=0.5))+ylim(-1,1)#+scale_fill_gradient2(limits=c(-1,1))

. <- df.sim.outs %>% 
  filter(randoms=="stoch") %>% 
  select(names(df.sim.pars),-n,"TP_b_pctdif") %>% 
  .[,colSums(is.na(.[,]))==0] %>% 
  select_if(is.numeric) %>% 
  cor(method="spearman") %>%
  .[,colSums(!is.na(.[,]))>1] %>% 
  .[rowSums(!is.na(.[,]))>1,] %>% 
  .[1:(nrow(.)-1),"TP_b_pctdif"]
ggplot(data=NULL,aes(x=names(.),y=.))+geom_col(fill="lightgrey",color="black")+labs(y="spearman")+theme(axis.title.x=element_blank(),axis.text.x = element_text(angle = 90,hjust=1,vjust=0.5))+ylim(-1,1)#+scale_fill_gradient2(limits=c(-1,1))


ggplot(data=df.sim.outs %>% filter(abs(TP_b_dif)<1e3),aes(x=randoms,y=TP_b_dif))+
  geom_boxplot(fill="grey",color='black')+geom_hline(yintercept=0,color="red")+
  labs(y="net P balance (g P)",x="Sensitivity Test")

save.image(paste0(wrkdir,"/outputs/env_","xecute_steady_state_sensitivity_nsims",n_sims,"_",format(Sys.time(), "%Y-%m-%d_%h%m%s"),".Rdata"))

  