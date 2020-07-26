####
#### Kitayama et al. "Temperature is a dominant driver of distinct annual
#### seasonality of leaf litter production of equatorial tropical rain forests"
#### Figure code for Supplementary Information: CI of the seasonality strength
####

# Load libraries
library(ggplot2)
library(cowplot)
library(gridExtra) 
library(reshape2)
library(scales)
library(ggsci)
library(lubridate)

# Load helper functions for visualization
setwd('../'); source('config.R'); setwd('FigCode/')
source('0_FigFunctions/FigFunctions.R')

# Load workspace
load("../03_out/03_ClimateFourierRes_NA6.RData")

# Read periodogram objects of litter data
litperi.1 <- read.csv("../02_out/fig_csv/data_litter_period_1.csv")
litperi.2 <- read.csv("../02_out/fig_csv/data_litter_period_2.csv")
litperi.3 <- read.csv("../02_out/fig_csv/data_litter_period_3.csv")
litperi.4 <- read.csv("../02_out/fig_csv/data_litter_period_4.csv")
litperi.5 <- read.csv("../02_out/fig_csv/data_litter_period_5.csv")
litperi.6 <- read.csv("../02_out/fig_csv/data_litter_period_6.csv")
litperi.7 <- read.csv("../02_out/fig_csv/data_litter_period_7.csv")
litperi.8 <- read.csv("../02_out/fig_csv/data_litter_period_8.csv")
litperi.9 <- read.csv("../02_out/fig_csv/data_litter_period_9.csv")

litcyc.1 <- read.csv("../02_out/fig_csv/data_litter_cycle_1.csv")
litcyc.2 <- read.csv("../02_out/fig_csv/data_litter_cycle_2.csv")
litcyc.3 <- read.csv("../02_out/fig_csv/data_litter_cycle_3.csv")
litcyc.4 <- read.csv("../02_out/fig_csv/data_litter_cycle_4.csv")
litcyc.5 <- read.csv("../02_out/fig_csv/data_litter_cycle_5.csv")
litcyc.6 <- read.csv("../02_out/fig_csv/data_litter_cycle_6.csv")
litcyc.7 <- read.csv("../02_out/fig_csv/data_litter_cycle_7.csv")
litcyc.8 <- read.csv("../02_out/fig_csv/data_litter_cycle_8.csv")
litcyc.9 <- read.csv("../02_out/fig_csv/data_litter_cycle_9.csv")

# Exclude results of Branch, Epiphyte, Bamboo and Dust
litperi.1 <- litperi.1[litperi.1$ID == "leaf" | litperi.1$ID == "flower",]
litperi.2 <- litperi.2[litperi.2$ID == "leaf" | litperi.2$ID == "flower",]
litperi.3 <- litperi.3[litperi.3$ID == "leaf" | litperi.3$ID == "flower",]
litperi.4 <- litperi.4[litperi.4$ID == "leaf" | litperi.4$ID == "flower",]
litperi.5 <- litperi.5[litperi.5$ID == "leaf" | litperi.5$ID == "flower",]
litperi.6 <- litperi.6[litperi.6$ID == "leaf" | litperi.6$ID == "flower",]
litperi.7 <- litperi.7[litperi.7$ID == "leaf" | litperi.7$ID == "flower",]
litperi.8 <- litperi.8[litperi.8$ID == "leaf" | litperi.8$ID == "flower",]
litperi.9 <- litperi.9[litperi.9$ID == "leaf" | litperi.9$ID == "flower",]

litcyc.1 <- litcyc.1[1:2,]; litcyc.2 <- litcyc.2[1:2,]; litcyc.3 <- litcyc.3[1:2,]
litcyc.4 <- litcyc.4[1:2,]; litcyc.5 <- litcyc.5[1:2,]; litcyc.6 <- litcyc.6[1:2,]
litcyc.7 <- litcyc.7[1:2,]; litcyc.8 <- litcyc.8[1:2,]; litcyc.9 <- litcyc.9[1:2,]

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

# Rename periodogram objects
config$kSiteNamesLitter # site names

litperi.1$site <- litperi.2$site <- litperi.3$site <- "Sedimentary"
litperi.5$site <- litperi.6$site <- litperi.7$site <- litperi.8$site <- "Ultrabasic"
litperi.4$site <- "Granite"
litperi.9$site <- "Quaternary"

litperi0700 <- rbind(litperi.1, litperi.5)
litperi1700 <- rbind(litperi.2, litperi.6, litperi.9)
litperi2700 <- rbind(litperi.3, litperi.7)
litperi3100 <- rbind(litperi.4, litperi.8)

litperi0700$site <- factor(litperi0700$site, levels = c(unique(litperi0700$site)))
litperi1700$site <- factor(litperi1700$site, levels = c(unique(litperi1700$site)))
litperi2700$site <- factor(litperi2700$site, levels = c(unique(litperi2700$site)))
litperi3100$site <- factor(litperi3100$site, levels = c(unique(litperi3100$site)))


# Read periodogram objects of climate data
cliperi.1 <- read.csv("../03_out/fig_csv/data_climate_period_1.csv")
cliperi.2 <- read.csv("../03_out/fig_csv/data_climate_period_2.csv")
cliperi.3 <- read.csv("../03_out/fig_csv/data_climate_period_3.csv")
cliperi.4 <- read.csv("../03_out/fig_csv/data_climate_period_4.csv")

clicyc.1 <- read.csv("../03_out/fig_csv/data_climate_cycle_1.csv")
clicyc.2 <- read.csv("../03_out/fig_csv/data_climate_cycle_2.csv")
clicyc.3 <- read.csv("../03_out/fig_csv/data_climate_cycle_3.csv")
clicyc.4 <- read.csv("../03_out/fig_csv/data_climate_cycle_4.csv")

# Exclude relative humidity
config$kSiteNamesClimate # site names

cliperi.1 <- cliperi.1[cliperi.1$ID != "relative_humidity",]
cliperi.2 <- cliperi.2[cliperi.2$ID != "relative_humidity",]
cliperi.3 <- cliperi.3[cliperi.3$ID != "relative_humidity",]
cliperi.4 <- cliperi.4[cliperi.4$ID != "relative_humidity",]
cliperi.1$site <- "2,700 m"
cliperi.2$site <- "3,100 m"
cliperi.3$site <- "1,700 m"
cliperi.4$site <- "700 m"

clicyc.1 <- clicyc.1[clicyc.1$ID != "relative_humidity",]
clicyc.2 <- clicyc.2[clicyc.2$ID != "relative_humidity",]
clicyc.3 <- clicyc.3[clicyc.3$ID != "relative_humidity",]
clicyc.4 <- clicyc.4[clicyc.4$ID != "relative_humidity",]


# Figure functions
cl1 <- ConfLitFig(lit0700, "Litter at 700 m"); yl1 <- CycLitFig(lit0700, "Litter at 700 m")
cl2 <- ConfLitFig(lit1700, "Litter at 1,700 m"); yl2 <- CycLitFig(lit1700, "Litter at 1,700 m")
cl3 <- ConfLitFig(lit2700, "Litter at 2,700 m"); yl3 <- CycLitFig(lit2700, "Litter at 2,700 m")
cl4 <- ConfLitFig(lit3100, "Litter at 3,100 m"); yl4 <- CycLitFig(lit3100, "Litter at 3,100 m")

# Climate figures
cc1 <- ConfCliFig(clicyc.1, "Climate at 2,700 m"); yc1 <- CycCliFig(clicyc.1, "Climate at 2,700 m")
cc2 <- ConfCliFig(clicyc.2, "Climate at 3,270 m"); yc2 <- CycCliFig(clicyc.2, "Climate at 3,270 m")
cc3 <- ConfCliFig(clicyc.3, "Climate at 1,560 m"); yc3 <- CycCliFig(clicyc.3, "Climate at 1,560 m")
cc4 <- ConfCliFig(clicyc.4, "Climate at 550 m"); yc4 <- CycCliFig(clicyc.4, "Climate at 550 m")

# Combine figures
# Confidence interval
conf.all <- plot_grid(cl4 + scale_y_log10(),
                      cc2 + scale_y_log10() + theme(axis.title.y = element_blank()),
                      cl3 + scale_y_log10(),
                      cc1 + scale_y_log10() + theme(axis.title.y = element_blank()),
                      cl2 + scale_y_log10(),
                      cc3 + scale_y_log10() + theme(axis.title.y = element_blank()),
                      cl1 + scale_y_log10(),
                      cc4 + scale_y_log10() + theme(axis.title.y = element_blank()),
                      ncol = 2,
                      rel_widths = c(1, 1),
                      labels = c("a", "e", "b", "f",
                                 "c", "g", "d", "h"))

# Cycle length and significance
cycle.all <- plot_grid(yl4,
                       yc2 + theme(axis.title.y = element_blank()),
                       yl3,
                       yc1 + theme(axis.title.y = element_blank()),
                       yl2,
                       yc3 + theme(axis.title.y = element_blank()),
                       yl1,
                       yc4 + theme(axis.title.y = element_blank()),
                       ncol = 2,
                       rel_widths = c(1, 1),
                       labels = c("a", "e", "b", "f",
                                  "c", "g", "d", "h"))

# Figure output
# Confidence intervals
quartz(type = "pdf", file = "0_RawFigs/FigS_ConfidenceInt.pdf", width = 8, height = 12)
conf.all
dev.off()

# Cycle length and significance
quartz(type = "pdf", file = "0_RawFigs/FigS_CycleLength.pdf", width = 8, height = 12)
cycle.all
dev.off()
