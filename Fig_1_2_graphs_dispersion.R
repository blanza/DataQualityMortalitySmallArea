### Packages 
library(tidyverse);library(readxl);library(lubridate); library(ggpubr)

## Datasets preparing
load('qx.intercensitaria_com_prorata.Rdata'); com.prorata = qx15_45_interc
load('qx.intercensitaria_sem_prorata.Rdata'); sem.prorata = qx15_45_interc

datpro = com.prorata[,c('period','microcode','sex','qx1545.fcmean','qx1545.fcggbseg')]
colnames(datpro) = c('period','micro','sex','prob_mean_pro','prob_ggbseg_pro')

datspro = sem.prorata[,c('period','microcode','sex', 'qx1545','qx1545.fcmean','qx1545.fcggbseg')]
colnames(datspro) = c('year','micro','sex','prob_obs','prob_mean','prob_ggbseg')

dat = cbind(datpro[,c('period','micro','sex','prob_mean_pro','prob_ggbseg_pro')],datspro[,c('prob_obs','prob_mean','prob_ggbseg')])

names(dat)

## Separating by sexes and geographic region
dat.f = subset(dat, sex == 'f'); dat.m = subset(dat, sex == 'm')
dat.f$uf = as.integer(dat.f$micro/1000);
dat.m$uf = as.integer(dat.m$micro/1000)

dat.f$Region = factor(as.integer(dat.f$uf/10), 
     labels = c('North','Northeast','Southeast','South','Midwest'));
dat.m$Region = factor(as.integer(dat.m$uf/10), 
     labels = c('North','Northeast','Southeast','South','Midwest'))

## Females - dispersion graphs
## figures raw data vs adjusted by under-registration of death counts
fig1f = ggplot(subset(dat.f, period=='1980/1991'), 
        aes(prob_obs, prob_ggbseg, colour = Region))+ 
  geom_point()+
  xlim(0,0.25)+
  ylim(0,0.50)+
  ggtitle('Period 1980-1991')+
  xlab('Observed data')+
  ylab('Ajusted data by DDM')+
  theme_bw()   

fig2f = ggplot(subset(dat.f, period=='1991/2000'), 
               aes(prob_obs, prob_ggbseg, colour = Region)) + 
  geom_point()+
  xlim(0,0.25)+
  ylim(0,0.50)+
  ggtitle('Period 1991-2000')+
  xlab('Observed data')+
  ylab('Ajusted data by DDM')+
  theme_bw()

fig3f = ggplot(subset(dat.f, period=='2000/2010'), 
        aes(prob_obs, prob_ggbseg, colour = Region)) + 
  geom_point()+
  xlim(0,0.25)+
  ylim(0,0.50)+
  ggtitle('Period 2000-2010')+
  xlab('Observed data')+
  ylab('Ajusted data by DDM')+
  theme_bw()

## figures raw data vs adjusted (pro rata + under-registration of death counts) 
fig1fp = ggplot(subset(dat.f, period=='1980/1991'), 
         aes(prob_obs, prob_ggbseg_pro, colour = Region))+ 
  geom_point()+
  xlim(0,0.25)+
  ylim(0,0.50)+
  ggtitle('Period 1980-1991')+
  xlab('Observed data')+
  ylab('Ajusted data by pro rata + DDM')+
  theme_bw()   

fig2fp = ggplot(subset(dat.f, period=='1991/2000'), 
         aes(prob_obs, prob_ggbseg_pro, colour = Region)) + 
  geom_point()+
  xlim(0,0.25)+
  ylim(0,0.50)+
  ggtitle('Period 1991-2000')+
  xlab('Observed data')+
  ylab('Ajusted data by pro rata + DDM')+
  theme_bw()

fig3fp = ggplot(subset(dat.f, period=='2000/2010'), 
        aes(prob_obs, prob_ggbseg_pro, colour = Region)) + 
  geom_point()+
  xlim(0,0.25)+
  ylim(0,0.50)+
  ggtitle('Period 2000-2010')+
  xlab('Observed data')+
  ylab('Ajusted data by pro rata + DDM')+
  theme_bw()

## Fig all females graphs. Figure 1
windows()
figfem = ggarrange(fig1f, fig2f, fig3f, fig1fp, fig2fp, fig3fp, ncol=3, nrow=2, common.legend = TRUE, legend="bottom")
annotate_figure(figfem, top = text_grob("Intercensal female adult mortality probabilities in small areas of Brazil, 1980-2010", color = "red", face = "bold", size = 14))

## Males - dispersion graphs
## figures raw data vs adjusted by under-registration of death counts
fig1m = ggplot(subset(dat.m, period=='1980/1991'), 
        aes(prob_obs, prob_ggbseg, colour = Region))+ 
  geom_point()+
  xlim(0,0.25)+
  ylim(0,0.50)+
  ggtitle('Period 1980-1991')+
  xlab('Observed data')+
  ylab('Ajusted data by DDM')+
  theme_bw()   

fig2m = ggplot(subset(dat.m, period=='1991/2000'), 
        aes(prob_obs, prob_ggbseg, colour = Region)) + 
  geom_point()+
  xlim(0,0.25)+
  ylim(0,0.50)+
  ggtitle('Period 1991-2000')+
  xlab('Observed data')+
  ylab('Ajusted data by DDM')+
  theme_bw()

fig3m = ggplot(subset(dat.m, period=='2000/2010'), 
        aes(prob_obs, prob_ggbseg, colour = Region)) + 
  geom_point()+
  xlim(0,0.25)+
  ylim(0,0.50)+
  ggtitle('Period 2000-2010')+
  xlab('Observed data')+
  ylab('Ajusted data by DDM')+
  theme_bw()

## figures brutos vs ggb-seg + pro rata
fig1mp = ggplot(subset(dat.m, period=='1980/1991'), 
         aes(prob_obs, prob_ggbseg_pro, colour = Region))+ 
  geom_point()+
  xlim(0,0.25)+
  ylim(0,0.50)+
  ggtitle('Period 1980-1991')+
  xlab('Observed data')+
  ylab('Ajusted data by pro rata + DDM')+
  theme_bw()   

fig2mp = ggplot(subset(dat.m, period=='1991/2000'), 
         aes(prob_obs, prob_ggbseg_pro, colour = Region)) + 
  geom_point()+
  xlim(0,0.25)+
  ylim(0,0.50)+
  ggtitle('Period 1991-2000')+
  xlab('Observed data')+
  ylab('Ajusted data by pro rata + DDM')+
  theme_bw()

fig3mp = ggplot(subset(dat.m, period=='2000/2010'), 
         aes(prob_obs, prob_ggbseg_pro, colour = Region)) + 
  geom_point()+
  xlim(0,0.25)+
  ylim(0,0.50)+
  ggtitle('Period 2000-2010')+
  xlab('Observed data')+
  ylab('Ajusted data by pro rata + DDM')+
  theme_bw()

## ## Fig all males graphs. Figure 2
windows()
figmas = ggarrange(fig1m, fig2m, fig3m, fig1mp, fig2mp, fig3mp, ncol=3, nrow=2, common.legend = TRUE, legend="bottom")
annotate_figure(figmas, top = text_grob("Intercensal male adult mortality probabilities in small areas of Brazil, 1980-2010", color = "red", face = "bold", size = 14))
