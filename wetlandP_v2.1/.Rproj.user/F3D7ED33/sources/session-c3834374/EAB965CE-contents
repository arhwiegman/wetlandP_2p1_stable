library("tidyverse")
df1 <- read_csv(file.path(getwd(),"outputs/outputs_summary_lcbp_sensitivity_sedsretained2021-10-26_13m48m.csv")) %>% mutate(assumptions="100%trapping")
df2 <- read_csv(file.path(getwd(),"outputs/outputs_summary_lcbp_sensitivity_Litter2021-10-26_02m32m.csv")) %>% mutate(assumptions="settlingVelocity") 
df.sim.outs_summary <- bind_rows(df1,df2) %>%
  mutate(Csource=ifelse(str_detect(scenario,"stream"),"stream","siphon"),
         HRTx = case_when(
           str_detect(scenario,"100d")~"100d",
           str_detect(scenario,"10d")~"10d",
           str_detect(scenario,"1e6")~"1Md",
           str_detect(scenario,"pow")~"pow"),
           
         Cx ="Cx1",
         Cx = case_when(
           str_detect(scenario,"0p5x")~"Cx0.5",
           str_detect(scenario,"2x")~"Cx2",
           T~Cx),
         Hx= ifelse(str_detect(scenario,"Hx"),"Hx1.2","Hx1"),
         CxHx = paste(Hx,Cx))

View(df.sim.outs_summary %>% filter(HRTx=="pow"))

# observed vs simulated plots
ggplot(df.sim.outs_summary) + geom_point(aes(obs_shootM_t458,sim_shootM_t458))+geom_point(aes(obs_shootM_t92,sim_shootM_t92),color="red")+geom_point(aes(obs_shootM_t49,sim_shootM_t49),color="red")+geom_abline(slope=1,intercept=0)

ggplot(df.sim.outs_summary) + 
  geom_point(aes(obs_shootM_t458,sim_shootM_t458),color="black")+
  geom_point(aes(obs_shootM_t92,sim_shootM_t92),color="red",pch=2)+
  geom_point(aes(obs_shootM_t49,sim_shootM_t49),color="red",pch=3)+
  geom_abline(slope=1,intercept=0)+
  xlab("observed biomass (g dw/m^2)")+
  ylab("simulated biomass (g dw/m2)")
ggplot(df.sim.outs_summary) + 
  geom_point(aes(est_Litter_tp_gm2_mean,sim_litterP_t365))+
  geom_abline(slope=1,intercept=0)+xlab("osberved litter P (g P/m^2) 7/15/20") + ylab("simulated litter P (g P/m^2)  7/15/20")

ggplot(df.sim.outs_summary %>% mutate(scenario=str_sub(parsheet,start=11)) %>% 
         mutate(obs_IM_a_minus_ash=ifelse((obs_IM_a-(obs_OM_a*0.1))<0,0,obs_IM_a-(obs_OM_a*0.1)))) + 
  geom_point(aes(x=obs_IM_a_minus_ash,y=sim_IM_deposition,color=assumptions),size=2,stroke=1)+scale_shape_manual(values=c(4,5,6,7,8,11,10))+facet_wrap(vars(scenario),scale="free_y")+
  geom_abline(slope=1,intercept=0)+ylab("simulated inorganic deposition (g dw/m2/yr) 7/15/20")+xlab("observed inorganic accretion - ash (g dw/m^2/yr) 7/15/20")

# rectangle
ggplot(df.sim.outs_summary)+
  geom_col(aes(x=scenario,y=sim_delta_TP_yr,fill=sim_delta_TP_yr),color="black")+labs(x="Site and Sampling Plot",title="Net TP Balance (g P/m^2/yr)")+theme(axis.title.y=element_blank(),axis.text.x=element_text(angle=90))+facet_wrap(vars(id),scales="free_y")+scale_fill_gradient2()

ggplot(df.sim.outs_summary %>% filter(plot==2,k_HRT!=1e6,str_detect(scenario,"xC")))+
  geom_col(aes(x=scenario,y=sim_delta_TP_yr,fill=sim_delta_TP_yr),color="black")+labs(x="Site and Sampling Plot",title="Net TP Balance (g P/m^2/yr)")+theme(axis.title.y=element_blank(),axis.text.x=element_text(angle=90))+facet_wrap(vars(id,assumptions),scales="free_y")+scale_fill_gradient2()


# 
ggplot(df.sim.outs_summary %>% filter(plot==2,str_detect(scenario,"stream"),!str_detect(scenario,"xC")))+
  geom_col(aes(x=paste(scenario),y=sim_delta_TP_yr,fill=sim_delta_TP_yr),color="black")+
  labs(
    x="Hydraulic Residence Time Assumptions",
    y="Net TP Balance (g P/m^2/yr)")+
  theme(axis.text.x=element_text(angle=90))+
  facet_grid(rows=vars(id),cols=vars(assumptions),scales="free_y")+scale_fill_gradient2()

ggplot(df.sim.outs_summary %>% filter(str_detect(scenario,"stream_power")))+
  geom_col(aes(x=paste(id),y=sim_delta_TP_yr,fill=sim_delta_TP_yr),color="black")+
  labs(x="Site and Sampling Plot",y="Net TP Balance (g P/m^2/yr)")+
  theme(axis.text.x=element_text(angle=90))+scale_fill_gradient2()+facet_wrap(vars(assumptions))

ggplot(df.sim.outs_summary %>% 
         filter(k_HRT==100,str_detect(scenario,"stream.*100d")))+
  geom_col(aes(x=id,y=sim_delta_TP_yr,fill=sim_delta_TP_yr),color="black")+
  labs(x="Site and Sampling Plot",title="Net TP Balance (g P/m^2/yr)")+
  facet_wrap(vars(assumptions,scenario))+
  theme(axis.title.y=element_blank(),axis.text.x=element_text(angle=90))+
  scale_fill_gradient2()


ggplot(df.sim.outs_summary %>% 
         filter(k_HRT==100,str_detect(scenario,"stream.*100d")))+
  geom_col(aes(x=id,y=sim_delta_TP_yr,fill=sim_delta_TP_yr),color="black")+
  labs(x="Site and Sampling Plot",y="Net TP Balance (g P/m^2/yr)")+
  geom_point(aes(x=id,y=sim_in_TP,fill=sim_in_TP),color="blue")+
  geom_point(aes(x=id,y=sim_in_DIP,fill=sim_in_DIP),pch=4,color="blue")+
  geom_point(aes(x=id,y=-sim_out_TP,fill=-sim_out_TP),color="red")+
  geom_point(aes(x=id,y=-sim_out_DIP,fill=sim_out_DIP),pch=4,color="red")+
  facet_wrap(vars(assumptions,scenario),scales="free_y")+
  theme(axis.text.x=element_text(angle=90))+
  scale_fill_gradient2()


ggplot(df.sim.outs_summary %>% 
         filter(k_HRT==100,plot==2,str_detect(scenario,"stream.*100d")))+
  geom_col(aes(x=scenario,y=sim_delta_TP_yr,fill=sim_delta_TP_yr),color="black")+
  labs(x="Concentration Scenario",y="Net TP Balance (g P/m^2/yr)")+
  facet_wrap(vars(assumptions,id),scales="free_y")+
  geom_point(aes(x=scenario,y=sim_in_TP,fill=sim_in_TP),color="blue")+
  geom_point(aes(x=scenario,y=sim_in_DIP,fill=sim_in_DIP),pch=4,color="blue")+
  geom_point(aes(x=scenario,y=-sim_out_TP,fill=-sim_out_TP),color="red")+
  geom_point(aes(x=scenario,y=-sim_out_DIP,fill=sim_out_DIP),pch=4,color="red")+
  theme(axis.text.x=element_text(angle=90))+
  scale_fill_gradient2()

ggplot(df.sim.outs_summary %>% filter(plot==2,str_detect(parsheet,"stream.*100d")))+
  geom_col(aes(x=CxHx,y=sim_delta_TP,fill=sim_delta_TP),color="black")+
  geom_point(aes(x=CxHx,y=sim_in_TP,fill=sim_in_TP),color="blue")+
  geom_point(aes(x=CxHx,y=sim_in_DIP,fill=sim_in_DIP),pch=4,color="blue")+
  geom_point(aes(x=CxHx,y=-sim_out_TP,fill=-sim_out_TP),color="red")+
  geom_point(aes(x=CxHx,y=-sim_out_DIP,fill=sim_out_DIP),pch=4,color="red")+
  labs(x="Concentration (C) and Water Height (H) Factor",title="Net TP Balance (g P/m^2/yr)")+
  facet_wrap(vars(assumptions,id),scales='free_y')+
  theme(axis.title.y=element_blank(),axis.text.x=element_text(angle=90))+
  scale_fill_gradient2()

ggplot(df.sim.outs_summary %>% filter(plot==2,
                                      str_detect(parsheet,"stream.*pow"),
                                      assumptions=="settlingVelocity",
                                      Cx!="Cx1"))+
  geom_col(aes(x=CxHx,y=sim_delta_TP,fill=sim_delta_TP),color="black")+
  geom_point(aes(x=CxHx,y=sim_in_TP,fill=sim_in_TP),color="blue")+
  geom_point(aes(x=CxHx,y=sim_in_DIP,fill=sim_in_DIP),pch=4,color="blue")+
  geom_point(aes(x=CxHx,y=-sim_out_TP,fill=-sim_out_TP),color="red")+
  geom_point(aes(x=CxHx,y=-sim_out_DIP,fill=sim_out_DIP),pch=4,color="red")+
  labs(x="Concentration (C) and Water Height (H) Factor",y="Net TP Balance (g P/m^2/yr)")+
  facet_wrap(vars(assumptions,id),scales='free_y')+
  theme(axis.text.x=element_text(angle=90))+
  scale_fill_gradient2()


ggplot(df.sim.outs_summary %>% filter(str_detect(scenario,"stream")))+
  geom_boxplot(aes(x=site,y=sim_TP_retention_efficiency,fill=assumptions),color="black",size=1)+
  scale_fill_manual(values=c("white","grey"))+
  labs(y="TP Retention Efficiency\n(100*(in - out)/in)")+theme_bw()

df.sim.outs_summary <- df.sim.outs_summary %>% mutate(DIP_change = 100*(sim_in_DIP - sim_out_DIP)/sim_in_DIP)
df.sim.outs_summary %>% filter(str_detect(scenario,"stream")) %>% .$sim_TP_retention_efficiency %>% summary
df.sim.outs_summary %>% filter(str_detect(scenario,"stream"))%>% filter(!str_detect(assumptions,"100")) %>% .$sim_TP_retention_efficiency %>% summary
df.sim.outs_summary %>% filter(str_detect(scenario,"stream")) %>% filter(str_detect(assumptions,"100")) %>% .$sim_TP_retention_efficiency %>% summary
df.sim.outs_summary %>% filter(str_detect(scenario,"stream"))%>% filter(!str_detect(assumptions,"100")) %>% .$sim_TP_retention_efficiency %>% summary
df.sim.outs_summary %>% filter(str_detect(id,"Pr")) %>% .$DIP_change %>% summary
df.sim.outs_summary %>% filter(str_detect(scenario,"stream"))%>% filter(!str_detect(id,"Un")) %>% .$DIP_change %>% summary
df.sim.outs_summary %>% filter(str_detect(scenario,"stream")) %>% filter(!str_detect(id,"Sw")) %>% .$DIP_change %>% summary

ggplot(df.sim.outs_summary %>% filter(str_detect(scenario,"stream")))+
  geom_boxplot(aes(x=Cx,y=DIP_change,fill=DIP_change),color="black",size=1)+
  facet_wrap(vars(site))+
  scale_fill_gradient2()+
  labs(y="DIP retention (%)")+theme_bw()

write_csv(df.sim.outs_summary,file="outputs/outputs_net_TP_balance_lcbp_sensitivity.csv")
