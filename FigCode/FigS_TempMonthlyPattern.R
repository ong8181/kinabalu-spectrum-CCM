####
#### Kitayama et al. "Temperature is a dominant driver of distinct annual
#### seasonality of leaf litter production of equatorial tropical rain forests"
#### Figure code for Supplementary Information: Monthly patterns of daily air temperature
####

# Load environments
load("../06_out/06_Fourier_CCMcompile_FFT.RData")

# Load libraries
library(ggplot2)
library(cowplot)
library(gridExtra)
library(reshape2)
library(ggsci)
library(lubridate)

# Load helper functions for visualization
source('0_FigFunctions/FigFunctions.R')

# Compile data
climate.correct2[[1]]$date_cat <- ymd(climate.correct2[[1]]$date_cat)
climate.correct2[[2]]$date_cat <- ymd(climate.correct2[[2]]$date_cat)
climate.correct2[[3]]$date_cat <- ymd(climate.correct2[[3]]$date_cat)
climate.correct2[[4]]$date_cat <- ymd(climate.correct2[[4]]$date_cat)

climate.correct2[[1]]$month <- month(climate.correct2[[1]]$date_cat, label = TRUE, abbr = TRUE)
climate.correct2[[2]]$month <- month(climate.correct2[[1]]$date_cat, label = TRUE, abbr = TRUE)
climate.correct2[[3]]$month <- month(climate.correct2[[1]]$date_cat, label = TRUE, abbr = TRUE)
climate.correct2[[4]]$month <- month(climate.correct2[[1]]$date_cat, label = TRUE, abbr = TRUE)

climate.ylab <- "Mean daily temperature (Normalized)"
climate.ylim <- c(-2.2, 2.2)

t1 <- MonthlyTs2(climate.correct2[[1]], "ATC", climate.ylim, climate.ylab) + theme(axis.title.y = element_blank()) + ggtitle("2,700 m")
t2 <- MonthlyTs2(climate.correct2[[2]], "ATC", climate.ylim, climate.ylab) + theme(axis.title.y = element_blank()) + ggtitle("3,270 m")
t3 <- MonthlyTs2(climate.correct2[[3]], "ATC", climate.ylim, climate.ylab) + theme(axis.title.y = element_blank()) + ggtitle("1,560 m")
t4 <- MonthlyTs2(climate.correct2[[4]], "ATC", climate.ylim, climate.ylab) + theme(axis.title.y = element_blank()) + ggtitle("550 m")

climate.ts.all <- plot_grid(t2, t1, t3, t4, ncol = 1, align = "v")

# Figure output
quartz(type = "pdf", file = "0_RawFigs/FigS_MonthlyPatternClimate.pdf", width = 4, height = 8)
grid.arrange(climate.ts.all, left = climate.ylab)
#climate.ts.all
dev.off()

