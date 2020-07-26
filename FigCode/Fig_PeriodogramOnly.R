####
#### Kitayama et al. "Temperature is a dominant driver of distinct annual
#### seasonality of leaf litter production of equatorial tropical rain forests"
#### Figure code for periodogram
####

# Load libraries
library(ggplot2)
library(cowplot); theme_set(theme_cowplot())
library(gridExtra) 
library(reshape2)
library(scales)
library(ggsci)
library(tidyverse)
library(lubridate)

# Load helper functions for visualization
setwd('../'); source('config.R'); setwd('FigCode/')
source('0_FigFunctions/FigFunctionsPeriodogram.R')

# Load workspace
load("../03_out/03_ClimateFourierRes_NA6.RData")

# Read periodogram objects of litter data
# Exclude results of Branch, Epiphyte, Bamboo and Dust
litperi.1 <- read.csv("../02_out/fig_csv/data_litter_period_1.csv") %>% subset(ID == "leaf"|ID == "flower")
litperi.2 <- read.csv("../02_out/fig_csv/data_litter_period_2.csv") %>% subset(ID == "leaf"|ID == "flower")
litperi.3 <- read.csv("../02_out/fig_csv/data_litter_period_3.csv") %>% subset(ID == "leaf"|ID == "flower")
litperi.4 <- read.csv("../02_out/fig_csv/data_litter_period_4.csv") %>% subset(ID == "leaf"|ID == "flower")
litperi.5 <- read.csv("../02_out/fig_csv/data_litter_period_5.csv") %>% subset(ID == "leaf"|ID == "flower")
litperi.6 <- read.csv("../02_out/fig_csv/data_litter_period_6.csv") %>% subset(ID == "leaf"|ID == "flower")
litperi.7 <- read.csv("../02_out/fig_csv/data_litter_period_7.csv") %>% subset(ID == "leaf"|ID == "flower")
litperi.8 <- read.csv("../02_out/fig_csv/data_litter_period_8.csv") %>% subset(ID == "leaf"|ID == "flower")
litperi.9 <- read.csv("../02_out/fig_csv/data_litter_period_9.csv") %>% subset(ID == "leaf"|ID == "flower")

litcyc.1 <- read.csv("../02_out/fig_csv/data_litter_cycle_1.csv") %>% .[1:2,]
litcyc.2 <- read.csv("../02_out/fig_csv/data_litter_cycle_2.csv") %>% .[1:2,]
litcyc.3 <- read.csv("../02_out/fig_csv/data_litter_cycle_3.csv") %>% .[1:2,]
litcyc.4 <- read.csv("../02_out/fig_csv/data_litter_cycle_4.csv") %>% .[1:2,]
litcyc.5 <- read.csv("../02_out/fig_csv/data_litter_cycle_5.csv") %>% .[1:2,]
litcyc.6 <- read.csv("../02_out/fig_csv/data_litter_cycle_6.csv") %>% .[1:2,]
litcyc.7 <- read.csv("../02_out/fig_csv/data_litter_cycle_7.csv") %>% .[1:2,]
litcyc.8 <- read.csv("../02_out/fig_csv/data_litter_cycle_8.csv") %>% .[1:2,]
litcyc.9 <- read.csv("../02_out/fig_csv/data_litter_cycle_9.csv") %>% .[1:2,]

# Read periodogram objects of climate data
cliperi.1 <- read.csv("../03_out/fig_csv/data_climate_period_1.csv") %>% subset(ID != "relative_humidity")
cliperi.2 <- read.csv("../03_out/fig_csv/data_climate_period_2.csv") %>% subset(ID != "relative_humidity")
cliperi.3 <- read.csv("../03_out/fig_csv/data_climate_period_3.csv") %>% subset(ID != "relative_humidity")
cliperi.4 <- read.csv("../03_out/fig_csv/data_climate_period_4.csv") %>% subset(ID != "relative_humidity")

clicyc.1 <- read.csv("../03_out/fig_csv/data_climate_cycle_1.csv") %>% subset(ID != "relative_humidity")
clicyc.2 <- read.csv("../03_out/fig_csv/data_climate_cycle_2.csv") %>% subset(ID != "relative_humidity")
clicyc.3 <- read.csv("../03_out/fig_csv/data_climate_cycle_3.csv") %>% subset(ID != "relative_humidity")
clicyc.4 <- read.csv("../03_out/fig_csv/data_climate_cycle_4.csv") %>% subset(ID != "relative_humidity")


# Combine litter data
lit0700 <- rbind(litcyc.1, litcyc.5)
lit1700 <- rbind(litcyc.2, litcyc.9, litcyc.6)
lit2700 <- rbind(litcyc.3, litcyc.7) 
lit3100 <- rbind(litcyc.4, litcyc.8)

lit0700$site <- factor(c("Sedimentary", "Sedimentary", "Ultrabasic", "Ultrabasic"),
                       levels = c("Sedimentary", "Ultrabasic"))
lit1700$site <- factor(c("Sedimentary", "Sedimentary", "Quaternary",  "Quaternary", "Ultrabasic", "Ultrabasic"),
                       levels = c("Sedimentary", "Quaternary", "Ultrabasic"))
lit2700$site <- factor(c("Sedimentary", "Sedimentary", "Ultrabasic", "Ultrabasic"),
                       levels = c("Sedimentary", "Ultrabasic"))
lit3100$site <- factor(c("Sedimentray", "Sedimentray", "Ultrabasic", "Ultrabasic"),
                       levels = c("Sedimentray", "Ultrabasic"))

# Compile climate data
config$kSiteNamesLitter # site names

litperi.1$site <- litperi.2$site <- litperi.3$site <- "Sedimentary"
litperi.5$site <- litperi.6$site <- litperi.7$site <- litperi.8$site <- "Ultrabasic"
litperi.4$site <- "Sedimentray"
litperi.9$site <- "Quaternary"

litperi0700 <- rbind(litperi.1, litperi.5)
litperi1700 <- rbind(litperi.2, litperi.6, litperi.9)
litperi2700 <- rbind(litperi.3, litperi.7)
litperi3100 <- rbind(litperi.4, litperi.8)

litperi0700$site <- factor(litperi0700$site, levels = c(unique(litperi0700$site)))
litperi1700$site <- factor(litperi1700$site, levels = c(unique(litperi1700$site)))
litperi2700$site <- factor(litperi2700$site, levels = c(unique(litperi2700$site)))
litperi3100$site <- factor(litperi3100$site, levels = c(unique(litperi3100$site)))

# Compile climate data
config$kSiteNamesClimate # site names

cliperi.1$site <- "2,700 m"
cliperi.2$site <- "3,100 m"
cliperi.3$site <- "1,700 m"
cliperi.4$site <- "700 m"

# Linear interpolation to make smoothed figures
litperi0700 <- PeriSmoothLitter(litperi0700)
litperi1700 <- PeriSmoothLitter(litperi1700)
litperi2700 <- PeriSmoothLitter(litperi2700)
litperi3100 <- PeriSmoothLitter(litperi3100)
cliperi.1 <- PeriSmoothClimate(cliperi.1)
cliperi.2 <- PeriSmoothClimate(cliperi.2)
cliperi.3 <- PeriSmoothClimate(cliperi.3)
cliperi.4 <- PeriSmoothClimate(cliperi.4)

# Figure functions
# Litter figures
gr <- c(0,2,5,16)
pl1 <- PeriLitFig2(litperi0700, lit0700, "Litter at 700 m", r = gr, h = 0.5)
pl2 <- PeriLitFig2(litperi1700, lit1700, "Litter at 1,700 m", r = gr, h = 0.5, scolor = "red")
pl3 <- PeriLitFig2(litperi2700, lit2700, "Litter at 2,700 m", r = gr, h = 0.5)
pl4 <- PeriLitFig2(litperi3100, lit3100, "Litter at 3,100 m", r = gr, h = 0.5, scolor = "red")

# Climate figures
pc1 <- PeriCliFig2(cliperi.1, clicyc.1, "Climate at 2,700 m", r = gr, h = 0.6, scolor = "gray70")
pc2 <- PeriCliFig2(cliperi.2, clicyc.2, "Climate at 3,270 m", r = gr, h = 0.6)
pc3 <- PeriCliFig2(cliperi.3, clicyc.3, "Climate at 1,560 m", r = gr, h = 0.6)
pc4 <- PeriCliFig2(cliperi.4, clicyc.4, "Climate at 550 m", r = gr, h = 0.6)

# Combine figures
# Periodogram
p.legend <- get_legend(pl1)
p1 <- plot_grid(pl4 + theme(legend.position = "none", axis.title.x = element_blank()),
                pc2 + theme(legend.position = "none", axis.title.x = element_blank()),
                rel_heights = c(1, 0.6), labels = c("a", "e"))
p2 <- plot_grid(pl3 + theme(legend.position = "none", axis.title.x = element_blank()),
                pc1 + theme(legend.position = "none", axis.title.x = element_blank()),
                rel_heights = c(1, 0.6), labels = c("b", "f"))
p3 <- plot_grid(pl2 + theme(legend.position = "none", axis.title.x = element_blank()),
                pc3 + theme(legend.position = "none", axis.title.x = element_blank()),
                rel_heights = c(1, 0.6), labels = c("c", "g"))
p4 <- plot_grid(pl1 + theme(legend.position = "none"),
                pc4 + theme(legend.position = "none"),
                rel_heights = c(1, 0.6), labels = c("d", "h"))
peri.all <- plot_grid(p1, p2, p3, p4, ncol = 1, rel_heights = c(1,1,1,1.1))


# Figure output
# Periodogram
quartz(type = "pdf", file = "0_RawFigs/Fig_Periodogram.pdf", width = 12, height = 14)
plot_grid(peri.all, p.legend,
          rel_widths = c(1, 0.08),
          ncol = 2)
dev.off()

