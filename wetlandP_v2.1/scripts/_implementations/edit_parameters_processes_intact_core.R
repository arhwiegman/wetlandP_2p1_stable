#forcingfile = "df.hydroclimate.1m.OCD.0.csv"
forcingfile = "df.hydroclimate_static.csv"
parameters$forcingfile=forcingfile

forcingfile = "df.hydroclimate_static.csv"
parameters$forcingfile=forcingfile
H_w = 0.2
parameters$H_w = H_w
k_TP = df.site.pars$k_TP[j]
parameters$k_TP=k_TP
k_f_SRP = df.site.pars$k_f_SRP[j]
parameters$k_f_SRP=k_f_SRP
k_TSS = df.site.pars$k_TSS[j]
parameters$k_TSS=k_TSS
k_shootM = df.site.pars$k_shootM[j]
parameters$k_shootM=k_shootM
k_rootM = df.site.pars$k_rootM[j]
parameters$k_rootM=k_rootM

simname = paste0("intact_core_",df.site.pars$site[j],df.site.pars$plot[j])
parameters$simname = simname
k_PSR =  df.site.pars$k_PSR[j]
parameters$k_PSR = k_PSR
k_LOI =  df.site.pars$k_LOI[j]
parameters$k_LOI = k_LOI
k_Ex_max =  df.site.pars$k_Ex_max[j]
parameters$k_Ex_max = k_Ex_max
k_WEP_gm2 =  df.site.pars$k_WEP_gm2[j]
parameters$k_WEP_gm2 = k_WEP_gm2
k_f_fines =  df.site.pars$k_f_fines[j]
parameters$k_f_fines = k_f_fines
simyears = df.site.pars$test_t_yrs[j]
parameters$simyears = simyears

k_ad = 1.75 # 1/d | adsorption first order rate coefficient | Wang et al. 2003 1.75, Marois & Mitsch 2016 used 
parameters$k_ad=k_ad

k_E = 0.55 # m^3/g | langmuir constant of adsorption (bond energy) | Calibrated to intact core data this value depends on what metric is used to define Ex_max,  Wang et al. 2003 2.75 m3 kg-1
parameters$k_E=k_E

k_PIP2Ex = 1  # g/g| ratio of exchangeable P to particulate inorganic P | Wang et al. 2003 0.8
parameters$k_PIP2Ex=k_PIP2Ex

k_diff_STD = 1e-1  # m^2/d | effective diffusion coefficient| calibrated to intact core data; Marois & Mitsch 2016 calibrated value was 2e−5 m2 d−1
parameters$k_diff_STD=k_diff_STD


IO_Q_in = F # logical T/F or 1/0 | toggles surface inflow | 
parameters$IO_Q_in = F
IO_Q_precip = F # logical T/F or 1/0 | toggle precipitation | 
parameters$IO_Q_precip = F
IO_Q_ground = F # logical T/F or 1/0 | toggles net groundwater flow (percolation  - infiltration)| 
parameters$IO_Q_ground = F
IO_Q_ET = F # logical T/F or 1/0 | toggles evapotranspiration | 
parameters$IO_Q_ET = F
IO_Q_out = F # logical T/F or 1/0 | toggles surface outflow | 
parameters$IO_Q_out = F

# plant, sediments and phosphorus
IO_assim_shootP = F # logical T/F or 1/0 | toggles assimilation of shoot P | 
IO_assim_rootP = F  # logical T/F or 1/0 |  toggles growth of root P |
IO_mort_shootP2litterP = F # logical T/F or 1/0 | toggles mortality of shoots | 
IO_mort_rootP2LOP = F # logical T/F or 1/0 | toggles mortality of root P to LOP |
IO_mort_rootP2ROP = F # logical T/F or 1/0 | toggles mortatlity of root P |
IO_sed_IM = F # logical T/F or 1/0 | toggles sedimentation of inorganic matter | 
IO_sed_OM = F # logical T/F or 1/0 | toggles sedimentation of refractory organic P
IO_decay_litter = F # logical T/F or 1/0 | toggles decomposition of litter P to refractory organic P | 
IO_decay_LOP = F # logical T/F or 1/0 | toggles decomposition of labile OP | 
IO_decay_ROP = F # logical T/F or 1/0 | toggles decomposition of refractory OP | 
IO_diffus = T # logical T/F or 1/0 | toggles diffusion of DIP from b to a | 
IO_adsorp = T  # logical T/F or 1/0 | toggles adsorption of DIP onto PIP | 
# plant, sediments and phosphorus
parameters$IO_assim_shootP = F # logical T/F or 1/0 | toggles assimilation of shoot P | 
parameters$IO_assim_rootP = F  # logical T/F or 1/0 |  toggles growth of root P |
parameters$IO_mort_shootP2litterP = F # logical T/F or 1/0 | toggles mortality of shoots | 
parameters$IO_mort_rootP2LOP = F # logical T/F or 1/0 | toggles mortality of root P to LOP |
parameters$IO_mort_rootP2ROP = F # logical T/F or 1/0 | toggles mortatlity of root P |
parameters$IO_sed_IM = F # logical T/F or 1/0 | toggles sedimentation of inorganic matter | 
parameters$IO_sed_OM = F # logical T/F or 1/0 | toggles sedimentation of refractory organic P
parameters$IO_decay_litter = F # logical T/F or 1/0 | toggles decomposition of litter P to refractory organic P | 
parameters$IO_decay_LOP = F # logical T/F or 1/0 | toggles decomposition of labile OP | 
parameters$IO_decay_ROP = F # logical T/F or 1/0 | toggles decomposition of refractory OP | 
parameters$IO_diffus = T # logical T/F or 1/0 | toggles diffusion of DIP from b to a | 
parameters$IO_adsorp = T  # logical T/F or 1/0 | toggles adsorption of DIP onto PIP | 

