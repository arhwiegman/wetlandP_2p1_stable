cat("sourcing 'edit_parameters_processes_steady_state_sensitivity.R'\n")

# select row j of df sim pars, and remove any columns containing NA
par.list <- as.list(df.sim.pars[j,colSums(is.na(df.sim.pars[j,]))!=1])
for(i in names(par.list)){
  assign(i,unname(unlist(par.list[i])))
}

forcingfile = "df.hydroclimate_steady_state_sensitivity.csv"
simname = df.sim.pars$id[j]

extended_outputs = F

# hydrology
IO_Q_in = T # logical T/F or 1/0 | toggles surface inflow | 
IO_Q_precip = F # logical T/F or 1/0 | toggle precipitation | 
IO_Q_ground = F # logical T/F or 1/0 | toggles net groundwater flow (percolation  - infiltration)| 
IO_Q_ET = F # logical T/F or 1/0 | toggles evapotranspiration | 
IO_Q_out = F # logical T/F or 1/0 | toggles surface outflow | 
IO_Q_out = T

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

# sedimentation
k_ADNPP =	k_NPP/365	# g m-2 d-1| average daily rate of NPP | divide k_NPP by 365
k_M_root = 	k_M	# 1/day |	rate of baseline biomass mortality | calibrated to root mass ~1200 g m-2 with guidance from	Morris et al 1984; Marois & Mitsch 2016 0.0005 - 0.007
k_M_shoot = k_M		# 1/day |	rate of baseline biomass mortality | calibrated to peak shootM ~600 g m-2  with guidance from	Morris et al 1984 0.0005 - 0.007
k_rp_i = fn_particle_radius(sand=1-k_f_fines,silt=k_f_fines-k_clay,clay=k_clay)  # m| average radius of inorganic particles|
k_u_o = fn_settlingvelocity(k_rp = k_rp_o,k_dp = k_dp_o) # m/d| organic particle settling velocity| assumed constant laminar/slow flow (Reddy & Delaune 2008); fn_settlingvelocity(k_dp=k_dp_o)
k_u_i = fn_settlingvelocity(k_rp = k_rp_i,k_dp = k_dp_i) # m/d| inorganic particle settling velocity| assumed laminar/slow flow (Reddy & Delaune 2008) ; see fn_settlingvelocity(k_dp=k_dp_i)
k_f_LOM_OSS = k_f_labile  # g/g| labile fraction incoming organic suspended solids| fit to field data
k_f_LOM_BM = k_f_labile  # g/g| labile organic matter fraction of falling biomass| Morris & Bowden 1986
k_OM2P = k_BM2P # g/g  P/d.w.| P content of OM| best guess as of now we will be informed by field data
k_f_labile_litter = k_f_labile # g/g | labile fraction of litter | Based on IP2TP ratio of biomass
k_f_labile_root = k_f_labile  # g/g | labile fraction of roots at STD temp |   Based on IP2TP ratio of biomass
k_f_labile_OSS = k_f_labile  # g/g | labile fraction of OSS | Based on IP2TP ratio of biomass
k_LOM2P = k_BM2P # g P/d.w. | P content of labile organic matter |   site data
k_ROM2P = k_BM2P # g P/d.w. | P content of refractory organic matter |   site data


# compile global env vectors into parameters vector
eval(sub_mget_parameters_from_globenv_vectors)

