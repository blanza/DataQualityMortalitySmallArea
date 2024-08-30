### packages 
library(geobr);library(tidyverse);library(readxl);library(lubridate);library(classInt);library(ggspatial);library(spdep);library(spatialreg);library(biscale);library(cowplot);library(ggpubr);library(sf)

## datasets
load('qx.intercensitaria_com_prorata.Rdata'); com.prorata = qx15_45_interc
load('qx.intercensitaria_sem_prorata.Rdata'); sem.prorata = qx15_45_interc

datpro = com.prorata[,c('period','microcode','sex','qx1545.fcmean','qx1545.fcggbseg')]
colnames(datpro) = c('period','micro','sex','prob_mean_pro','prob_ggbseg_pro')

datspro = sem.prorata[,c('period','microcode','sex', 'qx1545','qx1545.fcmean','qx1545.fcggbseg')]
colnames(datspro) = c('year','micro','sex','prob_obs','prob_mean','prob_ggbseg')

dat = cbind(datpro[,c('period','micro','sex','prob_mean_pro','prob_ggbseg_pro')],datspro[,c('prob_obs','prob_mean','prob_ggbseg')])

names(dat)

## Separating by sexes and geographical areas
dat.f = subset(dat, sex == 'f')

dat.f$uf = as.integer(dat.f$micro/1000)

dat.f$Region = factor(as.integer(dat.f$uf/10), 
                      labels = c('North','Northeast','Southeast','South','Midwest'))

## Bivariate maps ####################################
## comparing with poverty line
renda = read.csv('pobr.csv')

## Female data
dat.f_91 = subset(dat.f,period=='1980/1991'); dat.f_00 = subset(dat.f,period=='1991/2000'); dat.f_10 = subset(dat.f,period=='2000/2010')

dat.f_91 = dat.f_91[,cbind("micro","prob_obs","prob_ggbseg","prob_ggbseg_pro")];colnames(dat.f_91) = cbind("micro","p_obs91","p_ggbseg91","p_ggbseg_pro91")

dat.f_00 = dat.f_00[,cbind("micro","prob_obs","prob_ggbseg","prob_ggbseg_pro")];colnames(dat.f_00) = cbind("micro","p_obs00","p_ggbseg00","p_ggbseg_pro00")

dat.f_10 = dat.f_10[,cbind("micro","prob_obs","prob_ggbseg","prob_ggbseg_pro")];colnames(dat.f_10) = cbind("micro","p_obs10","p_ggbseg10","p_ggbseg_pro10")

dat.f9110 = cbind(dat.f_91[,c("micro","p_obs91","p_ggbseg91","p_ggbseg_pro91")], dat.f_00[,c("p_obs00","p_ggbseg00","p_ggbseg_pro00")], dat.f_10[,c("p_obs10","p_ggbseg10","p_ggbseg_pro10")])

# merging data
dado.f = merge(renda, dat.f9110, by.x= 'Micro', by.y = 'micro')

## shapes
regiao <- geobr::read_region(year = 2010)
micro <- geobr::read_micro_region(year = 2013) ## ano com 558 micros

## merging all data
map.f = merge(micro, dado.f, by.x= 'code_micro', by.y = 'Micro')

## Bivariate maps -- adjusted data
# creating classes
map.f91 <- bi_class(map.f, x = p_pobr91, y = p_ggbseg91, style = "quantile", dim = 3)

map.f00 <- bi_class(map.f, x = p_pobr00, y = p_ggbseg00, style = "quantile", dim = 3)

map.f10 <- bi_class(map.f, x = p_pobr10, y = p_ggbseg10, style = "quantile", dim = 3)

# 91 male
map91f <- 
  ggplot() +
  geom_sf(data = map.f91, mapping = aes(fill = bi_class),color = NA,size = 0.01, show.legend = FALSE) +
  geom_sf(data = regiao, size=.45, fill = NA, color = "black", show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  labs(title = "Ajusted female intercensal adult probability of death\n(1980-1991) and poverty line in 1991") +
  bi_theme() +
  theme(plot.title=element_text(size = 12))+
  theme(plot.subtitle=element_text(size = 13, color = 'red'))

legend91f <- bi_legend(pal = "GrPink",
                       dim = 3,
                       xlab = "Higher poverty",
                       ylab = "Higher 45q15",
                       size = 6.5,
                       arrows=T)

Plot91f <- ggdraw() +
  draw_plot(map91f, 0, 0, 1, 1) +
  draw_plot(legend91f, 0, 0, 0.4, 0.4)

# 00 male
map00f <- 
  ggplot() +
  geom_sf(data = map.f00, mapping = aes(fill = bi_class),color = NA,size = 0.01, show.legend = FALSE) +
  geom_sf(data = regiao, size=.45, fill = NA, color = "black", show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  labs(title = "Adjusted female intercensal adult probability of death\n(1991-2000) and poverty line in 2000") +
  bi_theme() +
  theme(plot.title=element_text(size = 12))+
  theme(plot.subtitle=element_text(size = 13, color = 'red'))

legend00f <- bi_legend(pal = "GrPink",
                       dim = 3,
                       xlab = "Higher poverty",
                       ylab = "Higher 45q15",
                       size = 6.5,
                       arrows=T)

Plot00f <- ggdraw() +
  draw_plot(map00f, 0, 0, 1, 1) +
  draw_plot(legend00f, 0, 0, 0.4, 0.4)

# 10 male
map10f <- 
  ggplot() +
  geom_sf(data = map.f10, mapping = aes(fill = bi_class),color = NA,size = 0.01, show.legend = FALSE) +
  geom_sf(data = regiao, size=.45, fill = NA, color = "black", show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  labs(title = "Adjusted female intercensal adult probability of death\n(2000-2010) and poverty line in 2010") +
  bi_theme() +
  theme(plot.title=element_text(size = 12))+
  theme(plot.subtitle=element_text(size = 13, color = 'red'))

legend10f <- bi_legend(pal = "GrPink",
                       dim = 3,
                       xlab = "Higher poverty",
                       ylab = "Higher 45q15",
                       size = 6.5,
                       arrows=T)

Plot10f <- ggdraw() +
  draw_plot(map10f, 0, 0, 1, 1) +
  draw_plot(legend10f, 0, 0, 0.4, 0.4)

## Bivariate maps -- observed data
# creating classes
map.f91s <- bi_class(map.f, x = p_pobr91, y = p_obs91, style = "quantile", dim = 3)

map.f00s <- bi_class(map.f, x = p_pobr00, y = p_obs00, style = "quantile", dim = 3)

map.f10s <- bi_class(map.f, x = p_pobr10, y = p_obs10, style = "quantile", dim = 3)

# 91 male
map91fs <- 
  ggplot() +
  geom_sf(data = map.f91s, mapping = aes(fill = bi_class),color = NA,size = 0.01, show.legend = FALSE) +
  geom_sf(data = regiao, size=.45, fill = NA, color = "black", show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  labs(title = "Observed female intercensal adult probability of death\n(1980-1991) and poverty line in 1991") +
  bi_theme() +
  theme(plot.title=element_text(size = 12))+
  theme(plot.subtitle=element_text(size = 13, color = 'red'))

legend91fs <- bi_legend(pal = "GrPink",
                       dim = 3,
                       xlab = "Higher poverty",
                       ylab = "Higher 45q15",
                       size = 6.5,
                       arrows=T)

Plot91fs <- ggdraw() +
  draw_plot(map91fs, 0, 0, 1, 1) +
  draw_plot(legend91fs, 0, 0, 0.4, 0.4)

# 00 male
map00fs <- 
  ggplot() +
  geom_sf(data = map.f00s, mapping = aes(fill = bi_class),color = NA,size = 0.01, show.legend = FALSE) +
  geom_sf(data = regiao, size=.45, fill = NA, color = "black", show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  labs(title = "Observed female intercensal adult probability of death\n(1991-2000) and poverty line in 2000") +
  bi_theme() +
  theme(plot.title=element_text(size = 12))+
  theme(plot.subtitle=element_text(size = 13, color = 'red'))

legend00fs <- bi_legend(pal = "GrPink",
                       dim = 3,
                       xlab = "Higher poverty",
                       ylab = "Higher 45q15",
                       size = 6.5,
                       arrows=T)

Plot00fs <- ggdraw() +
  draw_plot(map00fs, 0, 0, 1, 1) +
  draw_plot(legend00fs, 0, 0, 0.4, 0.4)

# 10 male
map10fs <- 
  ggplot() +
  geom_sf(data = map.f10s, mapping = aes(fill = bi_class),color = NA,size = 0.01, show.legend = FALSE) +
  geom_sf(data = regiao, size=.45, fill = NA, color = "black", show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  labs(title = "Observed female intercensal adult probability of death\n(2000-2010) and poverty line in 2010") +
  bi_theme() +
  theme(plot.title=element_text(size = 12))+
  theme(plot.subtitle=element_text(size = 13, color = 'red'))

legend10fs <- bi_legend(pal = "GrPink",
                       dim = 3,
                       xlab = "Higher poverty",
                       ylab = "Higher 45q15",
                       size = 6.5,
                       arrows=T)

Plot10fs <- ggdraw() +
  draw_plot(map10fs, 0, 0, 1, 1) +
  draw_plot(legend10fs, 0, 0, 0.4, 0.4)

## Figure 3
windows()
ggarrange(Plot91f, Plot00f, Plot10f,Plot91fs, Plot00fs, Plot10fs, ncol = 3, nrow = 2, common.legend = T)
