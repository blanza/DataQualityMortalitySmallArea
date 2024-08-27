### pacotes 
library(geobr);library(tidyverse);library(readxl);library(lubridate);library(classInt);library(ggspatial);library(spdep);library(spatialreg);library(biscale);library(cowplot);library(ggpubr);library(sf)

#library(rgdal);library(maptools)

### dados pro rata
## 
datprata <- read.csv2("micro45q15_1980_2010_com_prorata.csv", sep=',', encoding = "UTF-8")

datsrata <- read.csv2("micro45q15_1980_2010_sem_prorata.csv", sep = ";", encoding = "UTF-8")

## mais selecoes
datprataf = datprata %>% filter (year ==1980 & sex == 'm') %>%
        select(c('microcode','qx1545.fcggbseg', 'sex')) %>%
        rename ('code' = 'microcode', 
                'prob_rata' ='qx1545.fcggbseg', 
                'sex' ='sex')

datsrataf = datsrata %>% filter (year ==1980 & sex == 'm') %>%
  select(c('microcode','qx1545.fcggbseg', 'sex')) %>%
  rename ('code' = 'microcode', 
          'prob_srata' ='qx1545.fcggbseg', 
          'sex' ='sex')

dat = merge (datprataf, datsrataf, by.x = 'code', by.y = 'code')

##numerico os dados
dat$prob_rata = as.numeric(paste(dat$prob_rata)); dat$prob_srata = as.numeric(paste(dat$prob_srata))

micro <- geobr::read_micro_region(year = 2013) ## ano com 558 micros

## merge 1o sf e depois data.frame
map = merge(micro, dat, by.x= 'code_micro', by.y = 'code')

## Mapa bivariado
# create classes
map1 <- bi_class(map, x = prob_rata, y = prob_srata, style = "quantile", dim = 3)
## esquece os warnings

## unificando fontes
#windowsFonts(A = windowsFont("Cambria"))

## Região
regiao <- geobr::read_region(year = 2010)

map80 <- 
  ggplot() +
  geom_sf(data = map1, mapping = aes(fill = bi_class),color = NA , size = 0.01, show.legend = FALSE) +
  geom_sf(data = regiao, size=.45, fill = NA, color = "black", show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  labs(title = "Comparison of male adult probability of death in 1980\nwith pro rata and without pro rata deaths' distributions.",
  ) +
  bi_theme() +
  theme(plot.title=element_text(size = 12))+
  theme(plot.subtitle=element_text(size = 13, color = 'red'))
  #+theme(text = element_text(family = "A"))

legend1 <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "Higher probs with pro rata",
                    ylab = "Higher probs without pro rata",
                    size = 7,
                    arrows=T)

Plot80 <- ggdraw() +
  draw_plot(map80, 0, 0, 1, 1) +
  draw_plot(legend1, 0.65, 0, 0.3, 0.3)

windows()
Plot80 



