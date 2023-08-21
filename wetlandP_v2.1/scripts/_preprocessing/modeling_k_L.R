require(readxl)
df <- read_xlsx("C:/Workspace/wetlandP/model_versions/wetlandP_v2/wetlandP_v2.1/inputs/coreflux/df.IC.finalSRP.soil.0t10.xlsx","parameters_calib",range=cell_cols("A:AH")) %>%
  mutate(k_SPSC = (0.21-k_PSR)*k_Ex_max) %>%
  mutate(
    k_power = (k_Ex_max - k_PSR*k_Ex_max)/(k_PSR))
    #k_power = (k_Ex_max*k_LOI - k_PSR*k_Ex_max*k_LOI)/(k_PSR))
    #k_power = k_PSR)


## POWER MODEL OF DIP E

m.power.O2 <- df %>% filter(treatment=="O2") %>% lm(log(test_SRP_mgl_mean)~log(k_power),data=.)
m.power.N2 <- df %>% filter(treatment=="N2") %>% lm(log(test_SRP_mgl_mean)~log(k_power),data=.)
m.power <- df %>% lm(log(test_SRP_mgl_mean)~log(k_power)*treatment,data=.)
summary(m.power.O2)
summary(m.power.N2)
summary(m.power)
m.power.diagplot <- m.power %>% olsrr::ols_plot_diagnostics(print_plot = F)
m.power %>% plot
m.power.O2 %>% plot
m.power.N2 %>% plot
df.pred <- df %>% cbind(exp(predict(m.power,interval="confidence")))
ggplot(data=df.pred,aes(y=DIP_E_pred,x=test_SRP_mgl_mean,fill=treatment))+
  scale_fill_manual(values=c("grey","red"))+
  geom_smooth(method="lm",color="black",alpha=0.5,linetype="dashed")+
  geom_abline(intercept=0,slope=1)+
  geom_point(aes(fill=treatment),pch=21,size=2,stroke=1)+
  xlim(0,2.5)+ylim(0,2.5)

ggplot(data=df.pred,aes(y=fit,x=test_SRP_mgl_mean,fill=treatment))+
  scale_fill_manual(values=c("grey","red"))+
  geom_smooth(method="lm",color="black",alpha=0.5,linetype="dashed")+
  geom_abline(intercept=0,slope=1)+
  geom_errorbar(aes(ymin=lwr,ymax=upr),alpha=0.5)+
  geom_errorbarh(aes(xmin=test_SRP_mgl_mean-test_SRP_mgl_sd,
                     xmax=test_SRP_mgl_mean+test_SRP_mgl_sd),alpha=0.5)+
  geom_point(aes(fill=treatment),pch=21,size=2,stroke=1)

require(scales)
lm(fit~test_SRP_mgl_mean,data=df.pred)  %>% summary
ggplot(data=df.pred,aes(y=fit,x=test_SRP_mgl_mean,fill=treatment))+
  scale_fill_manual(values=c("grey","red"))+
  geom_abline(intercept=0,slope=1,size=1)+
  #geom_smooth(method="lm",color="black",alpha=0.5,linetype="dashed")+
  geom_errorbar(aes(ymin=lwr,ymax=upr),alpha=0.5)+
  geom_errorbarh(aes(xmin=test_SRP_mgl_mean-test_SRP_mgl_sd,
                     xmax=test_SRP_mgl_mean+test_SRP_mgl_sd),alpha=0.5)+
  geom_point(aes(fill=treatment),pch=21,size=2,stroke=1)+
  scale_x_log10(breaks = 10^(-2:1))+#, labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = 10^(-2:1))#, labels = trans_format("log10", math_format(10^.x)))


## STATISTICAL MODELING OF K_L

df %>% select(k_E_opt2,k_f_fines,k_clay,k_PSR,k_LOI,k_Ex_max,k_SPSC) %>%
  plot

m <- lm(log(k_Ex_max)~log(k_clay),data=df %>% filter(treatment=="O2"))
summary(m)
best <- step(m,direction = "both",k=2)
#plot(best)
summary(best)
m <- lm(log(k_E_opt2)~log(k_PSR)*log(k_Ex_max)*log(k_LOI)*treatment,data=df)
mO2 <- lm(log(k_E_opt2)~log(k_PSR)*log(k_Ex_max)*log(k_LOI),data=df %>% filter(treatment=="O2"))
mN2 <- lm(log(k_E_opt2)~log(k_PSR)*log(k_Ex_max)*log(k_LOI),data=df %>% filter(treatment=="N2"))


m_m <- lm(log(k_E_opt2)~log(k_PSR)*log(k_Ex_max_modeled)*log(k_LOI)*treatment,data=df)
mO2_m <- lm(log(k_E_opt2)~log(k_PSR)*log(k_Ex_max_modeled)*log(k_LOI),data=df %>% filter(treatment=="O2"))
mN2_m <- lm(log(k_E_opt2)~log(k_PSR)*log(k_Ex_max_modeled)*log(k_LOI),data=df %>% filter(treatment=="N2"))
summary(m_m)
plot(mN2$fitted.values,mN2$model$`log(k_E_opt2)`)
abline(0,1)
plot(mO2$fitted.values,mO2$model$`log(k_E_opt2)`)
abline(0,1)

best_m <- step(m_m,direction = "both",k=2)
best <- step(m,direction = "both",k=2)
summary(best_m)
summary(best_m)
plot(best)

df.pred.m <- cbind(best$model[1:4],exp(predict(best,interval="confidence"))) %>% cbind(df)
df1 <- df.pred.m %>% mutate(
  DIP_E_pred = (k_Ex_max*k_PSR)/((1 - k_PSR)*k_Ex_max*fit),
  DIP_E_pred_upr = (k_Ex_max*k_PSR)/((1 - k_PSR)*k_Ex_max*(upr)),
  DIP_E_pred_lwr = (k_Ex_max*k_PSR)/((1 - k_PSR)*k_Ex_max*(lwr)),
  DIP_E_k_E1 = (k_Ex_max*k_PSR)/((1 - k_PSR)*k_Ex_max*1.04),
  DIP_E_k_E0p5 = (k_Ex_max*k_PSR)/((1 - k_PSR)*k_Ex_max*0.56)
)
lm(DIP_E_pred~test_SRP_mgl_mean,data=df1) %>% summary
lm(fit~test_SRP_mgl_mean,data=df.pred.m)  %>% summary

df$test
ggplot(data=df.pred.m,aes(y=log(fit),x=log(k_E_opt2),fill=treatment))+
  scale_fill_manual(values=c("grey","red"))+
  geom_smooth(method="lm",color="black",linetype="dashed")+
  geom_abline(intercept=0,slope=1)+
  geom_errorbar(aes(ymin=log(lwr),ymax=log(upr)),alpha=0.5)+
  geom_point(aes(fill=treatment),pch=21,size=2,stroke=1)

ggplot(data=df1,aes(y=log(DIP_E_pred),x=log(test_SRP_mgl_mean),fill=treatment))+
  scale_fill_manual(values=c("grey","red"))+
  geom_smooth(method="lm",color="black",alpha=0.5,linetype="dashed")+
  geom_abline(intercept=0,slope=1)+
  geom_errorbar(aes(ymin=log(DIP_E_pred_lwr),ymax=log(DIP_E_pred_upr)),alpha=0.5)+
  geom_errorbarh(aes(xmin=log(test_SRP_mgl_mean-test_SRP_mgl_sd),
                     xmax=log(test_SRP_mgl_mean+test_SRP_mgl_sd)),alpha=0.5)+
  geom_point(aes(fill=treatment),pch=21,size=2,stroke=1)
require(scales)
ggplot(data=df1,aes(y=DIP_E_pred,x=test_SRP_mgl_mean,fill=treatment))+
  scale_fill_manual(values=c("grey","red"))+
  geom_abline(intercept=0,slope=1,size=1)+
  #geom_smooth(method="lm",color="black",alpha=0.5,linetype="dashed")+
  geom_errorbar(aes(ymin=DIP_E_pred_lwr,ymax=DIP_E_pred_upr),alpha=0.5)+
  geom_errorbarh(aes(xmin=test_SRP_mgl_mean-test_SRP_mgl_sd,
                     xmax=test_SRP_mgl_mean+test_SRP_mgl_sd),alpha=0.5)+
  geom_point(aes(fill=treatment),pch=21,size=2,stroke=1)+
  scale_x_log10(breaks = 10^(-2:1))+#, labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = 10^(-2:1))#, labels = trans_format("log10", math_format(10^.x)))


ggplot(data=df1,aes(y=DIP_E_k_E0p5,x=test_SRP_mgl_mean,fill=treatment))+
  scale_fill_manual(values=c("grey","red"))+
  geom_smooth(method="lm",color="black",linetype="dashed")+
  geom_abline(intercept=0,slope=1)+
  geom_point(aes(fill=treatment),pch=21,size=2,stroke=1)+
  xlim(0,2)+ylim(0,2)
ggplot(data=df1,aes(y=DIP_E_k_E1,x=test_SRP_mgl_mean,fill=treatment))+
  scale_fill_manual(values=c("grey","red"))+
  geom_smooth(method="lm",color="black",linetype="dashed")+
  geom_abline(intercept=0,slope=1)+
  geom_point(aes(fill=treatment),pch=21,size=2,stroke=1)+
  xlim(0,2)+ylim(0,2)

ggplot(data=df,aes(y=k_Ex_max_modeled,x=k_Ex_max))+
  scale_fill_manual(values=c("white"))+
  geom_smooth(method="lm",color="black",linetype="dashed")+
  geom_abline(intercept=0,slope=1)+
  geom_point(pch=21,size=2,stroke=1)+
  xlim(0,8)+xlim(0,8)


