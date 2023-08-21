# **************************************************************
# filename: dependancies
# description: manages R packages used by the model
# author: Adrian R.H. Wiegman 
# revision date: 2021-09-15
# project: wetlandP_v2.1
# repository: https://github.com/arhwiegman/wetlandP
# notes:

# **************************************************************
dependancies <- 
  c(
    'pacman',
    'tidyverse', # for data manipulation read/write and plotting
    'deSolve', # for ordinary differential equations
    'diffeqr',
    'readxl',
    'zoo', # for managing time series data
    'soiltexture', # for converting soil classes into mean particle size estimates
    'Evapotranspiration', # for estimating ET from weather station data
    'rlang', # for managing environments
    'ecolMod', # for examples in calibration
    'ggrepel' # for repelling text labels
    )
installed_packages <- installed.packages()
if(!"pacman"  %in% installed_packages) install.packages("pacman")
try(pacman::p_unload(pacman::p_loaded(),character.only = T))
try(pacman::p_load(dependancies,character.only = T))
cat("successfully loaded dependant R packages:\n")
dependancies <- pacman::p_loaded() 
print(dependancies)
dependancy_citations <- c("R",dependancies)
names(dependancy_citations) <- dependancy_citations
dependancy_citations <- lapply(dependancy_citations,pacman::p_cite,copy2clip=F,tex=F)

