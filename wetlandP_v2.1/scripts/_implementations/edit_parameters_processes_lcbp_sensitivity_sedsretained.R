forcingfile = paste0("df.hydroclimate.1m.",s,".csv")
inputdir=file.path(wrkdir,"inputs/")
simname = paste0(parsheet,"_",s)
# select row j of df sim pars, and remove any columns containing NA
par.list <- as.list(df.sim.pars[j,colSums(is.na(df.sim.pars[j,]))!=1])
for(i in names(par.list)){
  assign(i,unname(unlist(par.list[i])))
}
# height of water increased by a factor of 1.2 
try({
  if(as.logical(IO_Hx))forcingfile = paste0("df.hydroclimate.1m.",s,"x1p2.csv")
})
extended_outputs = F
parameters$extended_outputs = extended_outputs

IO_use_k_DIP_E = df.sim.pars$IO_use_k_DIP_E[j]
IO_Q_net = df.sim.pars$IO_Q_net[j]
IO_HRT_power_model = df.sim.pars$IO_HRT_power_model[j]
k_Z_a = df.sim.pars$Z_a[j]
parameters$k_Z_a = df.sim.pars$Z_a[j]
parameters$IO_DIP_E_langmuir = IO_DIP_E_langmuir 
parameters$IO_anoxic = IO_anoxic
parameters$IO_variable_k_Ex_max = IO_variable_k_Ex_max
parameters$IO_variable_k_E = IO_variable_k_E
parameters$IO_use_k_DIP_E = IO_use_k_DIP_E
parameters$IO_Q_net = IO_Q_net
parameters$IO_HRT_power_model = IO_HRT_power_model
parameters$k_BM2P=k_BM2P
parameters$k_TP=k_TP
parameters$k_f_SRP=k_f_SRP
parameters$k_TSS=k_TSS
parameters$k_shootM=k_shootM
parameters$k_rootM=k_rootM

simname = paste0(parsheet,"_",df.sim.pars$site.plot[j])
parameters$simname = simname
k_PSR =  df.sim.pars$k_PSR[j]
parameters$k_PSR = k_PSR
k_LOI =  df.sim.pars$k_LOI[j]
parameters$k_LOI = k_LOI
k_Ex_max =  df.sim.pars$k_Ex_max[j]
parameters$k_Ex_max = k_Ex_max
k_f_fines =  df.sim.pars$k_f_fines[j]
parameters$k_f_fines = k_f_fines
simyears = df.sim.pars$simyears[j]
parameters$simyears = simyears

IO_Q_in = T # logical T/F or 1/0 | toggles surface inflow | 
parameters$IO_Q_in = IO_Q_in
IO_Q_precip = T # logical T/F or 1/0 | toggle precipitation | 
parameters$IO_Q_precip = IO_Q_precip
IO_Q_ground = T # logical T/F or 1/0 | toggles net groundwater flow (percolation  - infiltration)| 
parameters$IO_Q_ground = IO_Q_ground
IO_Q_ET = T # logical T/F or 1/0 | toggles evapotranspiration | 
parameters$IO_Q_ET = IO_Q_ET
IO_Q_out = T # logical T/F or 1/0 | toggles surface outflow | 
parameters$IO_Q_out = IO_Q_out

# plant, sediments and phosphorus
IO_assim_shootP = T # logical T/F or 1/0 | toggles assimilation of shoot P | 
IO_assim_rootP = T  # logical T/F or 1/0 |  toggles growth of root P |
IO_mort_shootP2litterP = T # logical T/F or 1/0 | toggles mortality of shoots | 
IO_mort_rootP2LOP = T # logical T/F or 1/0 | toggles mortality of root P to LOP |
IO_mort_rootP2ROP = T # logical T/F or 1/0 | toggles mortatlity of root P |

IO_sed_IM = T # logical T/F or 1/0 | toggles sedimentation of inorganic matter | 
IO_sed_OM = T # logical T/F or 1/0 | toggles sedimentation of refractory organic P

IO_decay_litter = T # logical T/F or 1/0 | toggles decomposition of litter P to refractory organic P | 
IO_decay_LOP = T # logical T/F or 1/0 | toggles decomposition of labile OP | 
IO_decay_ROP = T # logical T/F or 1/0 | toggles decomposition of refractory OP | 
IO_diffus = T # logical T/F or 1/0 | toggles diffusion of DIP from b to a | 
IO_adsorp = T  # logical T/F or 1/0 | toggles adsorption of DIP onto PIP | 
# plant, sediments and phosphorus
parameters$IO_assim_shootP = T # logical T/F or 1/0 | toggles assimilation of shoot P | 
parameters$IO_assim_rootP = T  # logical T/F or 1/0 |  toggles growth of root P |
parameters$IO_mort_shootP2litterP = T # logical T/F or 1/0 | toggles mortality of shoots | 
parameters$IO_mort_rootP2LOP = T # logical T/F or 1/0 | toggles mortality of root P to LOP |
parameters$IO_mort_rootP2ROP = T # logical T/F or 1/0 | toggles mortatlity of root P |

parameters$IO_sed_IM = IO_sed_IM  # logical T/F or 1/0 | toggles sedimentation of inorganic matter | 
parameters$IO_sed_OM = IO_sed_OM # logical T/F or 1/0 | toggles sedimentation of refractory organic P

parameters$IO_decay_litter = T # logical T/F or 1/0 | toggles decomposition of litter P to refractory organic P | 
parameters$IO_decay_LOP = T # logical T/F or 1/0 | toggles decomposition of labile OP | 
parameters$IO_decay_ROP = T # logical T/F or 1/0 | toggles decomposition of refractory OP | 
parameters$IO_diffus = T # logical T/F or 1/0 | toggles diffusion of DIP from b to a | 
parameters$IO_adsorp = T  # logical T/F or 1/0 | toggles adsorption of DIP onto PIP | 



IO_in_IM = T # g d.w./d | inflow of inorganic matter as ISS | 
IO_in_PIP = T # g P/d | inflow of PIP | 
IO_in_LOP = T # g P/d | inflow of labile organic P | 
IO_in_ROP = T # g P/d | inflow of recalcitrant organic P | 
IO_in_DIP = T # g P/d | inflow of dissolved inorganic P | 

# Toggle IO_out_... IM, PIP, LOP, ROP, to F in order to retain all incoming sediment
IO_out_IM = F # g P/d | outflow of IM | 
IO_out_PIP = F # g P/d | outflow of PIP | 
IO_out_LOP = F # g P/d | outflow of LOP | 
IO_out_ROP = F # g P/d | outflow of ROP | 
IO_out_DIP = T # g P/d | outflow of DIP |


parameters$IO_in_IM = IO_in_IM # g d.w./d | inflow of inorganic matter as ISS | 
parameters$IO_in_PIP = IO_in_PIP # g P/d | inflow of PIP | 
parameters$IO_in_LOP = IO_in_LOP # g P/d | inflow of labile organic P | 
parameters$IO_in_ROP = IO_in_ROP # g P/d | inflow of recalcitrant organic P | 
parameters$IO_in_DIP = IO_in_DIP # g P/d | inflow of dissolved inorganic P | 
parameters$IO_out_IM = IO_out_IM # g P/d | outflow of IM | 
parameters$IO_out_PIP = IO_out_PIP # g P/d | outflow of PIP | 
parameters$IO_out_LOP = IO_out_LOP # g P/d | outflow of LOP | 
parameters$IO_out_ROP = IO_out_ROP # g P/d | outflow of ROP | 
parameters$IO_out_DIP = IO_out_DIP # g P/d | outflow of DIP |
