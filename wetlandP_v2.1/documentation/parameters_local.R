
  # LOCAL -------------------
  # name = expression # units | description | assumptions
  # hydroclimate 
  # geometric
  area = 1 # m^2| wetland surface area| uniform flat surface 
  H_b = 0.1 # m| height of belowground compartment (sediment column)| NA

  # concentrations
  k_HRT = 1e3 # d | hydraulic residence time of wetland surface water | calculated by dividing total system water volume (m3) by outlfow rate (m3/d), often changes as function of system water volume
  k_TSS = 15 # g/m^3| total suspended solids of inflow | based on field data, median of observations, 3.5 at prindle, 23.8 at union st, 12.25 at swamp rd
  # soil
  k_TP = 0.05 # g P/m^3 | TP concentration (mg P /L) in inflow | 0.071 at prindle, 0.059 at union, 0.056 at swamp
  k_LOI = 0.20 # g/g| initial fraction of organic matter in total mass of below ground compartment | measured as soil loss-on-ignition | .15 - .24, .30 - .16 union, .136 - 0.299
  k_PSR = 0.20 # mol/mol | P Saturation Ratio | molar ratio of oxalate extractable P/(Al + Fe) (Nair et al. 2004), fit to field data, prindle  0.09 - 0.15, union  0.08 - 0.13, swamp rd 0.11 - 0.26
  k_Ex_max = 4 # g/kg | maximum P storage capacity | 31*(Al/27 + Fe/56), where Al and Fe are determined by acid ammonium oxalate extraction, fit to field data, ranging from 3.3 - 5.5 prindle, 5.0 - 6.4 union, 3.44 - 5.1 swamp   
  k_clay = 0.1 # g/g | clay content of inorganic matter, used for particle settling velocity | from soil textural analysis OR from NRCS soil survey units texture class,  .11 to 0.35, .0875 to 0.15 union, 0.075 - .15 swamp
  k_f_fines = 0.90 # g/g| silt + clay, fine sediment fraction of incoming total suspended solids used for particle setting velocity | fit to field data and 0.627 - .84 prindle, 0.84 - 0.97 union, 0.75 - 0.985 swamp rd. 
  k_f_OSS = 0.5 # g/g| organic matter fraction of incoming total suspended solids| fit to field data, %65 at prindle rd, 23% at union st, 54% swamp rd. 
  k_f_SRP = 0.3 # g SRP /g TP| fraction of TP as SRP in influent water | based on field data 0.404 at prindle, 0.25 at union, 0.27 at swamp rd. 
  k_DIP_E = 0.05 # # mg P/L | equilibrium DIP concentration | used if IO_variable_DIP_E = F, set equal to final intact SRP for aerobic treatments
  k_rp_i = fn_particle_radius(sand=1-k_f_fines,silt=k_f_fines-k_clay,clay=k_clay)  # m| average radius of inorganic particles| calculated based on soil texture see `fn_particle_radius` 