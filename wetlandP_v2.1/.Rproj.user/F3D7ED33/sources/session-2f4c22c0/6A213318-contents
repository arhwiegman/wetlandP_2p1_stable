#Input files
##"1_runparameters.csv"
variable name  | value | units | description
---------------|-------|-------|---------------
TF_Zw_Qin      |T      |T/F    | Does a time series of Zw force hydrology? If true then Qin is solved for from Zw, dZw, dVw, and Qout, if false, then Zw is solved...
TF_Qout_tHR    |T      |T/F    | Does Qout force hydrology? if true, Qin forces hydrology then tHR is solved (tHR = Vw/Qin), if false, then Qin is solved (Qin = Vw/tHR)
TF_dynamic     |F      |T/F    | Is the dynamic simulation dynamic, do forcing variables change with time? If False, the simulation the variables in the file hydroclimate will remain at the value at t = 0  
TF_hyd_preproc |T      |T/F    | Calculate water volumes prior to simulation? If true, then Vw and Hw will be solved for before the model run, this will cause errors that grow with simulation length due to changes in the soil column elevation |
TF_ET_meas     |F      |T/F    | Is evapotranspiration measured at site? If False, it will be calculated from Us, Tair, ip
TF_bayesian    |F      |T/F    | If a bayesian monte-carlo simulation? If true, then hydroclimate will be forced with table 2b_hydroclimate_averages.csv
n_steps        |100    |#      | number of time steps, if TF_dynamic==T, then n_steps equals nrows of 2_hydroclimate.csv
dt             |1      |d      | number of days per time step

##"2a_hydroclimate_obs.csv"
first row with variable names, followed by atleast one row of data columns
put averages for the site at time zero for static simulation 
the values at time = 0 will be used for static simulations, the values at time 
variable name  | value | units | description
---------------|-------|-------|---------------
t              |0      |d      | days since start of simulation
Zw             |0.1    |m      | elevation of water relative to a benchmark (e.g. HOBO or NAD83), may be left blank if forcing with Qin, kTF_Qin == T
Qin            |0      |m3/d   | volumetric inflow rate, may be left blank if forcing with Zw (TF_ZW_Qin == T)
Qg             |0      |m3/d   | ground water flow rate (positive for upwelling, negative for infiltration)
Us             |0      |m/s    | wind speed at Xm above surface 
Tair           |10     |degC   | air temperature degrees celcius  
Qip            |0      |m3/m2/d| intercepted precipitation measured at local weather station or estimated from annual rates
Qet            |0      |m3/m2/d| evapotranspiration measured locally or calculated from Us, Tair and ip. 

formated data with minimum required information in "2_hydroclimate.csv" file: 
t,Zw,Qin,Qg,Us,Tair,Qpcp,Qet
0,0.1,0,0,0,10,0,0

##"2a_hydroclimate_parameters.csv"
first row with variable names, followed by atleast one row of data columns
put averages for the site at time zero for static simulation 
the values at time = 0 will be used for static simulations, the values at time 
*center and range are parameters for the probability distribution
**PDF is the probability density function shape: U - uniform, N - normal, L - lognormal
if PDF = U, center is the mean, and range is 1/2*(min - max)
for PDF = N, center is the mean, and range is the standard deviation 
for PDF = L, center is the mean of ln(x), range is the standard deviation of ln(x)
variable name  | center*| range*| PDF** | units | description
---------------|--------|-------|-------|-------|---------------
t              |0       |       |       |d      | days since start of simulation
Zw_max         |0.1     |0      |U      |m      | the average peak water level for a flood pulse
Zw_min         |0       |0      |U      |       | 
Zw_max_trend   |-1e-4   |0      |U      |m/d    | the trend over time in Zw_max 
Zw_min_trend   |1e-4    |0	|U      |m/d    | the trend over time in Zw_min 
hydroperiod    |0.5     |       |       |       | fraction of time per year Zw exceeds Zs 
n_pulses       |3       |       |       |       | number of pulses per year that are large enough to inundate the site
Qin            |0       |       |       |m3/d   | volumetric inflow rate, may be left blank if forcing with Zw (TF_ZW_Qin == T)
Qg             |0       |       |       |m3/d   | ground water flow rate (positive for upwelling, negative for infiltration)
Us             |0       |       |       |m/s    | wind speed at Xm above surface 
Tair           |10      |       |       |degC   | air temperature degrees celcius  
Qpcp           |0       |       |       |m3/m2/d| intercepted precipitation measured at local weather station or estimated from annual rates

formated data with minimum required information in "2_hydroclimate.csv" file: 
t,Zw,Qin,Qg,Us,Tair,Qpcp,Qet
0,0.1,0,0,0,10,0,0

##"3_bathymetry.csv"
relationships bewteen Zw and A and Qout
variable name  | default | units | description
---------------|---------|-------|---------------
Zw             | 0.1     |m      | elevation of water column
area           | 1       |m2     | wetted surface area of wetland
Qout           | 0       |m3/d   | volumetric outflow rate, estimated or solved (Qout = Vw/tHR)

Zw,A,Qout
0,0,0
0.01,1,0
0.1,1,0
1,1,0
2,1,0
5,1,0

##initialstates.csv
initial values for state variables of the wetland system
variable name  | center*| range*| PDF** | units | description
---------------|--------|-------|-------|-------|---------------
Zw             |        |       |       |m      | elevation of water column
Zs             |        |       |       |m      | elevation of soil surface from which initial water height relative to soil surface will be calculated |
Hs (H_b)       |        |       |       |m      | height of the biochemically active soil column 0.1 to 1m | 
LOI            |        |       |       |g/g    | mass ratio (0 - 1) of organic soil to total soil as determined by loss-on-ignition |
PSR            |        |       |       |mol/mol| molar ratio (0 - 1) of oxalate P to Al and Fe |
shootM         |        |       |       |       |
rootM          |        |       |       |gdw/m2 | belowground root/rhyzome biomass
detrM          |        |       |       |gdw/m2 | mass of litter after leaves fall before winter.

# local parameters are constant throughout a simulation period
variable | units | description
---------|-------|---------------
k_SRP    |mg/L   | soluble reactive P in during flood stage in inflow/river |
k_TSS    |mg/L   | total suspended sediments during flood stage in inflow/river |
k_Fines  |g/g    | mass ratio (0 - 1) of silt and clay to total inorganic soil and suspended sediments |
k_ISS    |g/g    | fraction of suspended sediments as inorganic in river
k_ISS2PIP|mg/mg  | P content of ISS in river water
k_shoot2P|gP/g   | P content of biomass
k_root2P |gP/g   | P content of roots
k_OM2P   |gP/g   | P content of soil organic matter


calculated variables (intermediates)
Zw
Zs
Qin
Qout
tHR
Vw
Hw
IM_a
OM_a
...



# global parameters 






# If LOI, Fines, PSR, are unknown, enter values for the following 
LULC | character | current landcover type on first day of simulation (rowcrop, hay, herbaceous, shrub, mixed, mature forest) 
YSF | character | years since farming: 0 - 2, 3 - 10, 11 - 20, >20
LFT | character | last farming type: hay or rowcrop
soil series | character | NRCS soil series name, 


