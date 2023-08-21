library(tidyverse)
df.obs_vs_sim <- read_csv("outputs/outputs_obs_vs_sim_lcbp_sensitivity_2021-10-24_4h58m.csv") %>% 
  mutate(scenario=str_sub(parsheet,start=11)) %>% 
  mutate(obs_IM_a_minus_ash=ifelse((obs_IM_a-(obs_OM_a*0.1))<0,0,obs_IM_a-(obs_OM_a*0.1))) %>%
# observed vs simulated plots
ggplot(df.obs_vs_sim) + geom_point(aes(obs_shootM_t458,sim_shootM_t458))+geom_point(aes(obs_shootM_t92,sim_shootM_t92),color="red")+geom_point(aes(obs_shootM_t49,sim_shootM_t49,label=id),color="red")+geom_abline(slope=1,intercept=0)

ggplot(df.obs_vs_sim) + 
  geom_point(aes(obs_shootM_t458,sim_shootM_t458,shape=scenario),color="black")+
  geom_point(aes(obs_shootM_t92,sim_shootM_t92,shape=scenario),color="red")+
  geom_point(aes(obs_shootM_t49,sim_shootM_t49,shape=scenario),color="green")+
  geom_abline(slope=1,intercept=0)+
  myshapes+
  xlab("observed biomass (g dw/m^2)")+
  ylab("simulated biomass (g dw/m2)")
ggplot(df.obs_vs_sim) + 
  geom_point(aes(est_Litter_tp_gm2_mean,sim_litterP_t365),color="black")+
  geom_abline(slope=1,intercept=0)+xlab("osberved litter P (g P/m^2) 7/15/20") + ylab("simulated litter P (g P/m^2)  7/15/20")


myshapes <- scale_shape_manual(values=c(4,5,6,7,8,11,10))
ggplot(df.obs_vs_sim ) + 
  geom_point(aes(x=obs_IM_a_minus_ash,y=sim_IM_deposition),size=2,stroke=1)+facet_wrap(vars(scenario))+
  geom_abline(slope=1,intercept=0)+ylab("simulated inorganic deposition (g dw/m2/yr) 7/15/20")+xlab("observed inorganic accretion - ash (g dw/m^2/yr) 7/15/20")
ggplot(df.obs_vs_sim %>% mutate(scenario=str_sub(parsheet,start=11)))+
  geom_col(aes(x=id,fill=scenario,y=sim_delta_TP_yr),color="black")+scale_fill_grey()+labs(x="Site and Sampling Plot",y="Net TP Balance (g/m^2/yr)")+theme(legend.position="top")
ggplot(df.obs_vs_sim %>% mutate(scenario=str_sub(parsheet,start=11)))+
  geom_col(aes(x=id,y=sim_delta_TP_yr),color="black")+labs(x="Site and Sampling Plot",title="Net TP Balance (g P/m^2/yr)")+theme(axis.title.y=element_blank())+facet_wrap(vars(scenario))

ggplot(df.obs_vs_sim %>% filter(str_detect(parsheet,"stream")) %>% mutate(scenario=str_sub(parsheet,start=11)))+
  geom_point(aes(x=id,fill=scenario,y=sim_delta_TP_yr),pch=21,color="black")+scale_fill_grey()+labs(x="Site and Sampling Plot",title="Net TP Balance (g P/m^2/yr)")+theme(axis.title.y=element_blank())
save.image(paste0(wrkdir,"/outputs/env_","xecute_calibrate_litter_accretion","_",format(Sys.time(), "%Y-%m-%d_%h%m%s"),".Rdata"))
# 