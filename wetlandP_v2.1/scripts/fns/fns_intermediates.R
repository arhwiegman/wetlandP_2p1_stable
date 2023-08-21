# **************************************************************
# filename: intermediate_fns.R
# description: declares functions for intermediate calculations
# author: Adrian Wiegman
# revision date:  2020-05-15
# project: wetlandP
# repository: https://github.com/arhwiegman/wetlandP    
# notes:
# - ___
# - ___
# **************************************************************

# NONLINEAR MATHEMATICAL MODELS -------------------

# logistic
fn_logistic <- function(x,
                        y_min,
                        y_max,
                        a,
                        b){
  y = y_min + (y_max-y_min)*(exp(a+b*x)/(1+exp(a+b*x)))
}

# cosine
fn_cosine <- function(x,
                      ymax=1,
                      ymin=0,
                      period=365,
                      x_ymin=0){
  amp = (ymax - ymin)/2
  yadj = ymin + amp
  y = yadj + amp*(-cos((2*pi*(x+x_ymin)/period)))
  return(y)
}

# michaelis menten kinetics model
fn_mich_men <- function(S, #substate concentration
                        Jmax, # max rate 
                        ks # subraate concentration at half of Jmax
){Jmax*S/(ks+S)}


# OTHER FUNCTIONS -------------------------

fn_bulkdensity <- function (LOI,
                            k_db_i=1.99,
                            k_db_o=0.085){
  # bulk density of wetland soil consisting of discrete packetts of mineral and organic matter
  # source: ideal mixing model fit by Morris et al. 2016 earth's future doi:10.1002/2015EF000334
  BD = 1/(LOI/k_db_o + (1-LOI)/k_db_i)
  return(BD)
}

fn_LOI <- function (inorganic,total){
  # organic content of materials loss on ignition method
  (total-inorganic)/total
}

fn_particledensity <- function(
  # particle density of discrete packetts of mineral and organic matter
  # source: delaune baumann and gosselink 1983
  LOI, # g/g; soil mass loss on ignition
  k_dp_i=2.65, # g/cm^3; organic partical density
  k_dp_o=1.14 # g/cm^3; inorganic partical density
)
{
  PD = 1/(LOI/k_dp_o + (1-LOI)/k_dp_i)
  return(PD)
}

fn_porosity <- function(bulkdensity,
                        particledensity){
  
  porosity = 1 - bulkdensity/particledensity
  return(porosity)
}

fn_sandsiltclay_from_textureclass <- function(texture.class=NULL,
                                              texture.system=NULL){
  require(dplyr)
  require(soiltexture)
  if(is.null(texture.system)){
    message <- 
      'Text code of the texture classification system.
    "USDA.TT" (USDA texture triangle),
    "HYPRES.TT" (texture triangle of the European Soil Map),
    "FR.AISNE.TT" (French texture triangle of the Aisne region soil survey),
    "FR.GEPPA.TT" (French GEPPA texture triangle),
    "DE.BK94.TT" (German texture triangle),
    "UK.SSEW.TT" (Soil Survey of England and Wales),
    "AU.TT" (Australian texture triangle),
    "BE.TT" (Belgium texture triangle),
    "CA.EN.TT" (Canadian texture triangle, with English class abbreviations)
    "CA.FR.TT" (Canadian texture triangle, with French class abbreviations) 
    (see the `soiltexture`` package vignette for a complete list).'
    cat(message)
    texture.system <- readline("enter a value for `texture.system`\n from the options above (without quotes):")}
  set.seed(1)
  n=10000
  sand <- runif(n,0,100)
  silt <- (100 - sand)*runif(n,0,1)
  clay <- 100 - sand - silt
  df <- data.frame(SAND=sand,SILT=silt,CLAY=clay)
  df$class <- TT.points.in.classes(tri.data=df,class.sys=texture.system,PiC.type="t",collapse    = " ")
  df.class.mean<- df %>% 
    group_by(class) %>%
    summarize_all(mean) %>%
    mutate(statistic = "mean")
  df.class.sd<- df %>% 
    group_by(class) %>%
    summarize_all(sd) %>%
    mutate(statistic = "sd")
  df.class.summary <- full_join(df.class.mean,df.class.sd)
  #TT.plot(class.sys = texture.system, tri.data=df.class.mean)
  df.class.averages <- df %>% 
    group_by(class) %>%
    summarize_all(list(~mean(.),~sd(.))) %>% as.data.frame
  print(df.class.averages)
  return(df.class.averages[df.class.mean$class==texture.class,])
}

fn_textureclass_abbreviations <- function(texture.system=NULL){
  require(dplyr)
  require(soiltexture)
  if(is.null(texture.system)){
    message <- 
      'Text code of the texture classification system.
    "USDA.TT" (USDA texture triangle),
    "HYPRES.TT" (texture triangle of the European Soil Map),
    "FR.AISNE.TT" (French texture triangle of the Aisne region soil survey),
    "FR.GEPPA.TT" (French GEPPA texture triangle),
    "DE.BK94.TT" (German texture triangle),
    "UK.SSEW.TT" (Soil Survey of England and Wales),
    "AU.TT" (Australian texture triangle),
    "BE.TT" (Belgium texture triangle),
    "CA.EN.TT" (Canadian texture triangle, with English class abbreviations)
    "CA.FR.TT" (Canadian texture triangle, with French class abbreviations) 
    (see the `soiltexture`` package vignette for a complete list).'
    cat(message)
    texture.system <- readline("enter a value for `texture.system`\n from the options above (without quotes):")}
  x <- TT.classes.tbl(class.sys = texture.system, collapse = NULL)
  return(x[,1:2])
}
#fn_textureclass_abbreviations("USDA.TT")
#fn_sandsiltclay_from_textureclass("Cl","USDA.TT")

fn_particle_radius_from_textureclass <- function (texture.class,texture.system){
  fn_sandsiltclay_from_textureclass("Cl","USDA.TT")
  #sand = 1e-4m, silt = 1e-5m, clay=1e-6m, sum weighted average of sand silt and clay
  k_rp = 2e-4*sand/100+1e-5*silt/100+1e-6*clay/100 #https://simple.wikipedia.org/wiki/Particle_size_(grain_size)
  return(k_rp) # radius of particles m
}

fn_particle_radius <- function (sand=NULL,silt=NULL,clay=NULL){
  
  #sand = 1e-4m, silt = 1e-5m, clay=1e-6m, sum weighted average of sand silt and clay
  k_rp = 2e-4*sand/100+1e-5*silt/100+1e-6*clay/100 #https://simple.wikipedia.org/wiki/Particle_size_(grain_size)
  return(k_rp) # radius of particles m
}

# ADSORPTION FUNCTIONS ----------

fn_AlFe_ox <- function(
  # revised: 2020-04-14
  # returns: moles of Al and Fe in sediments mol/kg
  # source: fit to data on oxalate Al + Fe verses LOI
  # notes:
  # inputs:
  # name = value # units; description; assumptions
  b_0 = 7.09401, # (Intercept)  7.09401    0.18465  38.420  < 2e-16 ***
  b_fines = 0.44693, #   Clay_Silt    0.44693    0.03350  13.340  < 2e-16 ***
  b_LOI =  0.17820, #   LOI          0.17820    0.02222   8.018 1.72e-14 ***
  LOI=0.05, # g/g; loss on ignition 
  fines=0.2 # g/g; fraction of fines in inorganic sediments
){
  # Call:
  #   lm(formula = log(PSC) ~ log(Clay_Silt) + log(LOI), data = Xy)
  # 
  # Residuals:
  #   Min       1Q   Median       3Q      Max 
  # -1.69075 -0.19413  0.00974  0.21194  1.86160 
  # 
  # Coefficients:
  #   Estimate Std. Error t value Pr(>|t|)    
  # (Intercept)  7.09401    0.18465  38.420  < 2e-16 ***
  #   Clay_Silt    0.44693    0.03350  13.340  < 2e-16 ***
  #   LOI          0.17820    0.02222   8.018 1.72e-14 ***
  #   ---
  #   Signif. codes:  
  #   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  # 
  # Residual standard error: 0.4168 on 342 degrees of freedom
  # Multiple R-squared:  0.5056,	Adjusted R-squared:  0.5027 
  # F-statistic: 174.9 on 2 and 342 DF,  p-value: < 2.2e-16
  AlFe_ox <- 7.09401 + 0.44693*log(100*fines) + 0.17820*log(LOI)
  AlFe_ox <- exp(AlFe_ox)/31 # raise e to Ex_max and divide by 1000 to get g/kgf
  return(AlFe_ox)
}
fn_AlFe_ox()

fn_Smax_1 <- function(
  # revised: 2020-04-15
  # returns: Smax (g/kg) maximum sorption capacity 
  # source: Reddy, K. R., O Connor, G. a., & Gale, P. M. (1998). Phosphorus Sorption Capacities of Wetland Soils and Stream Sediments Impacted by Dairy Effluent. Journal of Environment Quality, 27(2), 438. https://doi.org/10.2134/jeq1998.00472425002700020027x
  # notes: Table 3 in Reddy et al. 1998
  # inputs:
  # name = value # units; description; assumptions
  AlFe_ox =100 # mmol/kg; oxalate extractable Al and Fe
){
  Smax <- 1.74 + 0.172*AlFe_ox #mmol/kg
  return(Smax*31/1000) #g/kg
}
fn_Smax_1()
fn_Smax_2 <- function(
  # revised: 2020-04-15
  # returns: Smax g/kg maximum sorption capacity 
  # source: Reddy, K. R., O Connor, G. a., & Gale, P. M. (1998). Phosphorus Sorption Capacities of Wetland Soils and Stream Sediments Impacted by Dairy Effluent. Journal of Environment Quality, 27(2), 438. https://doi.org/10.2134/jeq1998.00472425002700020027x
  # notes: Table 3 in Reddy et al. 1998
  # inputs:
  # name = value # units; description; assumptions
  AlFe_ox=10, # mmol/kg; oxalate extractable Al and Fe
  TOC = 0.1*1000*0.45 # g/kg; total organic carbon; use LOI * 1000g/kg * 0.45 g OM/g OC
){
  
  Smax <- -0.02 + 5.31*AlFe_ox + 2.64*TOC #mg/kg
  return(Smax/1000) #g/kg
}
fn_Smax_2()
fn_Smax_1()

fn_AlFe_from_Smax_TOC <- function(
  S_max=1/31*1000, # mmol/kg
  TOC=0.03*0.45*1000 #g/kg
){
  print(S_max)
  # see fn_smax_2
  AlFe_ox <- (S_max + 0.02 - 2.64*TOC)/5.31 # mmol/kg
  return(AlFe_ox)
}

fn_SPSC <- function(
  AlFe=100, # mmol/kg; mmol of extractable Al + Fe (oxalate, mehlich3)
  P = 20, # mmol/k; mmol of extractable P (oxalate, mehlich3)
  k_PSR=0.1 # ; threshold PSR
){
  PSR <- P/AlFe
  SPSC <- 31*AlFe*(k_PSR - PSR)
  return(SPSC)
}
fn_SPSC()

fn_k_L_from_SPSC <- function(
  SPSC = 300 # mg/kg; soil P sorption capacity
){
  if (SPSC > 0){
    k_L <- 0.02 + 0.299*SPSC
  }else{
    k_L <- 0.02
  } # if
  return(k_L)
}
fn_k_L_from_SPSC()
fn_k_L_from_PSR <- function(
  PSR = 0.1, # mg/kg; soil P sorption capacity
  b1 = -1.9,
  b0 = -6.5
){
  k_L = exp(b1*log(PSR)+b0)
  return(k_L)
}
fn_PSR <- function(
  AlFe=100, # mmol/kg; mmol of extractable Al + Fe (oxalate, mehlich3)
  P = 20 # mmol/k; mmol of extractable P (oxalate, mehlich3)
){
  PSR <- P/AlFe
  return(PSR)
}

fn_AlFe_from_Smax <- function(
  S_max=1/31*1000 # mmol/kg
){
  # see fn_smax_2
  AlFe_ox <- (S_max + 1.72)/0.172 # mmol/kg
  return(AlFe_ox)
}
fn_AlFe_from_Smax()

fn_k_E_statmodel <- function(k_PSR=0.1,
                   k_LOI=0.1,
                   k_Ex_max=4,
                   treatmentO2=T){
  # 2021-10-16
  # fit to intact core data 
  # Call:
  #   lm(formula = log(k_E_opt) ~ log(k_PSR) + log(k_Ex_max) + log(k_LOI) + 
  #        treatment + log(k_PSR):log(k_Ex_max) + log(k_PSR):log(k_LOI) + 
  #        log(k_Ex_max):log(k_LOI) + log(k_PSR):log(k_Ex_max):log(k_LOI), 
  #      data = df)
  # 
  # Residuals:
  #   Min       1Q   Median       3Q      Max 
  # -1.34374 -0.62329 -0.00399  0.48061  1.39091 
  # 
  # Coefficients:
  #   Estimate Std. Error t value Pr(>|t|)  
  # (Intercept)                         -11.8453    15.1749  -0.781   0.4410  
  # log(k_PSR)                           -1.6164     7.6807  -0.210   0.8347  
  # log(k_Ex_max)                        20.9258    11.6037   1.803   0.0811 .
  # log(k_LOI)                           -7.1447     5.1306  -1.393   0.1737  
  # treatmentO2                           0.6853     0.2704   2.534   0.0165 *
  #   log(k_PSR):log(k_Ex_max)              7.1366     5.9963   1.190   0.2430  
  # log(k_PSR):log(k_LOI)                -2.0402     2.9919  -0.682   0.5004  
  # log(k_Ex_max):log(k_LOI)             11.5669     5.1252   2.257   0.0312 *
  #   log(k_PSR):log(k_Ex_max):log(k_LOI)   4.5110     2.9104   1.550   0.1313  
  # ---
  #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  # 
  # Residual standard error: 0.8551 on 31 degrees of freedom
  # Multiple R-squared:  0.4716,	Adjusted R-squared:  0.3352 
  # F-statistic: 3.458 on 8 and 31 DF,  p-value: 0.005845
  log_k_E = +
  +-11.8453+  #15.1749  -0.781   0.4410
  log(k_PSR)*-1.6164+  #7.6807  -0.210   0.8347
  log(k_Ex_max)*20.9258+ #11.6037   1.803   0.0811 .
  log(k_LOI)*-7.1447+ #5.1306  -1.393   0.1737
  treatmentO2*0.6853+ #0.2704   2.534   0.0165 *
  log(k_PSR)*log(k_Ex_max)*7.1366+ #5.9963   1.190   0.2430
  log(k_PSR)*log(k_LOI)*-2.0402+ #2.9919  -0.682   0.5004
  log(k_Ex_max)*log(k_LOI)*11.5669+ #5.1252   2.257   0.0312 *
  log(k_PSR)*log(k_Ex_max)*log(k_LOI)*4.5110 #2.9104   1.550   0.1313
  return(exp(log_k_E))
}
fn_k_E_statmodel()

fn_DIP_E <- function (Ex=2,E=2.75,Ex_max=10){
  DIP_E = Ex/(E*(Ex_max-Ex))
  return(DIP_E)
}

#fn_DIP_E(Ex=0.1*4,E=fn_k_E_statmodel(),Ex_max=4)
#fn_DIP_E(Ex=10,E=0.5,Ex_max=2)
#fn_DIP_E(Ex=52.84/91000e-3*0.8/0.2,E=seq(1,10,length.out=30),Ex_max=5)
# plot(seq(1,10,length.out=30),fn_DIP_E(Ex=52.84/91000e-3*0.8/0.2,E=seq(1,10,length.out=30),Ex_max=5))
fn_AlFe_ox_sand <- function(
  # revised: 2020-09-29
  # returns: soil oxalate extractable Al + Fe mol/kg
  # source: Wiegman Dissertation, NRCS Pedons Vermont, Perillo et al. 2019
  # notes: Best Multiple Regression Model fit on 2020-09-30 using 0.75 of data and random seet set.seed(2)
  # inputs:
  # name = value # units; description; assumptions
  LOI = 0.1, # g/g; Loss-on-ignition organic content of soil 
  sand = 0.99 # g/g; sand content in soil (inorganic soil) results from soil textural analysis for sand/silt/clay 
){
  # Call:
  #   lm(formula = paste(yname, frm, sep = "~"), data = df.train)
  # 
  # Residuals:
  #   Min       1Q   Median       3Q      Max 
  # -0.72220 -0.21784  0.00537  0.21975  0.72162 
  # 
  # Coefficients:
  #   Estimate Std. Error t value Pr(>|t|)    
  # (Intercept)  5.83552    0.15284  38.179  < 2e-16 ***
  #   sand        -0.57661    0.18093  -3.187  0.00194 ** 
  #   log(LOI)     0.25858    0.06068   4.261 4.75e-05 ***
  #   ---
  #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  # 
  # Residual standard error: 0.3194 on 96 degrees of freedom
  # Multiple R-squared:  0.4344,	Adjusted R-squared:  0.4226 
  # F-statistic: 36.87 on 2 and 96 DF,  p-value: 1.317e-12
  AlFe_ox <- exp(
    +5.83552 
    -0.57661*sand
    +0.25858*log(LOI)
    
  )
  return(AlFe_ox) # mmol/kg; 
}

fn_AlFe_ox_fines <- function(
  # revised: 2021-10-06
  # returns: soil oxalate extractable Al + Fe mol/kg
  # source: Wiegman Dissertation, NRCS Pedons Vermont, Perillo et al. 2019
  # notes: Best Multiple Regression Model fit on 2021-10-06
  # this will return the highest AlFe values at high values for k_f_fines and medium values for LOI
  # inputs:
  # name = value # units; description; assumptions
  LOI = 0.1, # g/g; Loss-on-ignition organic content of soil 
  k_f_fines = 0.99 # g/g; content of silt + clay in inorganic soil (inorganic soil) results from soil textural analysis for sand/silt/clay
){
  # Call:
  #   lm(formula = paste(yname, frm, sep = "~"), data = df.train)
  # 
  # Residuals:
  #   Min      1Q  Median      3Q     Max 
  # -0.7228 -0.2043 -0.0017  0.2150  0.7491 
  # 
  # Coefficients:
  #   Estimate Std. Error t value Pr(>|t|)    
  # (Intercept)  5.81244    0.14835  39.182  < 2e-16 ***
  #   log(LOI)     0.25773    0.05494   4.691 9.00e-06 ***
  #   log(fines)   0.32107    0.07900   4.064 9.87e-05 ***
  #   ---
  #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  # 
  # Residual standard error: 0.3102 on 96 degrees of freedom
  # Multiple R-squared:  0.4664,	Adjusted R-squared:  0.4553 
  # F-statistic: 41.95 on 2 and 96 DF,  p-value: 8.067e-14
  fines = k_f_fines*(1-LOI)
  AlFe_ox <- exp(+
                   5.81244 + 
                   0.25773*log(LOI) + 
                   0.32107*log(fines)
  )
  return(AlFe_ox) # mmol/kg; 
}

fn_AlFe_ox_2_3 <- function(
  LOI = 0.1,
  k_f_fines = 0.99 # g/g; fraction of silt and clay in inorganic sediment 
){
  fines = (1-LOI)*k_f_fines
  AlFe_ox <- exp(+
                   5.54370 + 
                   0.85705*fines + 
                   0.36780*log(LOI) +
                   -0*fines*log(LOI)
  )
  return(AlFe_ox) # mmol/kg; 
}
fn_AlFe_ox_2_3()


fn_Ex_max_1 <- function(
  # revised: 2020-04-14
  # returns: Ex_max g/kg maximum exchangeable P in sediments
  # source: fit to data on oxalate Al + Fe verses LOI
  # notes:
  # inputs:
  # name = value # units; description; assumptions
  LOI = 0.25, # g/g; loss on ignition, organic matter content; increases Ex_max
  a_Ex_max = 9983.5/1000, # (g/kg/g/g); slope of exchangeable P in sediments with LOI; fit to data of oxalate Al + Fe verses LOI
  b_Ex_max = 2161.4/1000 # (g/kg); minimum exchangeable P in sediments; fit to data of oxalate Al + Fe verses LOI
){
  Ex_max = a_Ex_max*LOI + b_Ex_max
  return(Ex_max)
}

