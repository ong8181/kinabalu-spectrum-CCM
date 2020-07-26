####
#### Kitayama et al. "Temperature is a dominant driver of distinct annual
#### seasonality of leaf litter production of equatorial tropical rain forests"
#### Figure code for Supplementary Information: Time series of flower litter data
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
measure.names <- colnames(lit.d2)[which(sapply(substr(colnames(lit.d2), 5, 15), "[") ==  "flower_mean")]
flower.melt <- as.data.frame.array(melt(lit.d2,
                                      id.vars = c("time_index", "date_cat", "census_lag"),
                                      measure.vars = measure.names))
flower.melt$date_cat <- ymd(flower.melt$date_cat)
flower.melt$site <- factor(substr(flower.melt$variable, 1, 3), levels = unique(substr(flower.melt$variable, 1, 3)))

# Generate plot
flower.1 <- flower.melt[flower.melt$site == unique(flower.melt$site)[1],]
flower.2 <- flower.melt[flower.melt$site == unique(flower.melt$site)[2],]
flower.3 <- flower.melt[flower.melt$site == unique(flower.melt$site)[3],]
flower.4 <- flower.melt[flower.melt$site == unique(flower.melt$site)[4],]
flower.5 <- flower.melt[flower.melt$site == unique(flower.melt$site)[5],]
flower.6 <- flower.melt[flower.melt$site == unique(flower.melt$site)[6],]
flower.7 <- flower.melt[flower.melt$site == unique(flower.melt$site)[7],]
flower.8 <- flower.melt[flower.melt$site == unique(flower.melt$site)[8],]
flower.9 <- flower.melt[flower.melt$site == unique(flower.melt$site)[9],]

flower.ylab <- "Flower litter (kg/ha)"
flower.ylim <- c(0, 180)

t1 <- KinabaluTs(flower.1, flower.ylim, flower.ylab) + theme(axis.title.y = element_blank()) + ggtitle("700 m, Sedimantary")
t2 <- KinabaluTs(flower.2, flower.ylim, flower.ylab) + theme(axis.title.y = element_blank()) + ggtitle("1,700 m, Sedimentary")
t3 <- KinabaluTs(flower.3, flower.ylim, flower.ylab) + theme(axis.title.y = element_blank()) + ggtitle("2,700 m, Sedimentary")
t4 <- KinabaluTs(flower.4, flower.ylim, flower.ylab) + theme(axis.title.y = element_blank()) + ggtitle("3,100 m, Sedimentray")
t5 <- KinabaluTs(flower.5, flower.ylim, flower.ylab) + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) + ggtitle("700 m, Ultrabasic")
t6 <- KinabaluTs(flower.6, flower.ylim, flower.ylab) + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) + ggtitle("1,700 m, Ultrabasic")
t7 <- KinabaluTs(flower.7, flower.ylim, flower.ylab) + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) + ggtitle("2,700 m, Ultrabasic")
t8 <- KinabaluTs(flower.8, flower.ylim, flower.ylab) + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) + ggtitle("3,100 m, Ultrabasic")
t9 <- KinabaluTs(flower.9, flower.ylim, flower.ylab) + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) + ggtitle("1,700 m, Quaternary")

# Overwrite climate variable
climate.correct2[[1]]$date_cat <- ymd(climate.correct2[[1]]$date_cat)
climate.correct2[[2]]$date_cat <- ymd(climate.correct2[[2]]$date_cat)
climate.correct2[[3]]$date_cat <- ymd(climate.correct2[[3]]$date_cat)
climate.correct2[[4]]$date_cat <- ymd(climate.correct2[[4]]$date_cat)

# Overwrite temperature
amp <- 30
add <- 100
t1 <- t1 + geom_line(data = climate.correct2[[4]], aes(x = date_cat, y = ATC*amp + add), colour = "red3", size = 0.3, alpha = 0.5)
t2 <- t2 + geom_line(data = climate.correct2[[3]], aes(x = date_cat, y = ATC*amp + add), colour = "red3", size = 0.3, alpha = 0.5)
t3 <- t3 + geom_line(data = climate.correct2[[1]], aes(x = date_cat, y = ATC*amp + add), colour = "red3", size = 0.3, alpha = 0.5)
t4 <- t4 + geom_line(data = climate.correct2[[2]], aes(x = date_cat, y = ATC*amp + add), colour = "red3", size = 0.3, alpha = 0.5)
t5 <- t5 + geom_line(data = climate.correct2[[4]], aes(x = date_cat, y = ATC*amp + add), colour = "red3", size = 0.3, alpha = 0.5)
t6 <- t6 + geom_line(data = climate.correct2[[3]], aes(x = date_cat, y = ATC*amp + add), colour = "red3", size = 0.3, alpha = 0.5)
t7 <- t7 + geom_line(data = climate.correct2[[1]], aes(x = date_cat, y = ATC*amp + add), colour = "red3", size = 0.3, alpha = 0.5)
t8 <- t8 + geom_line(data = climate.correct2[[2]], aes(x = date_cat, y = ATC*amp + add), colour = "red3", size = 0.3, alpha = 0.5)
t9 <- t9 + geom_line(data = climate.correct2[[3]], aes(x = date_cat, y = ATC*amp + add), colour = "red3", size = 0.3, alpha = 0.5)

flower.ts.all <- plot_grid(t4, t8, NULL,
                         t3, t7, NULL,
                         t2, t6, t9,
                         t1, t5, NULL,
                         ncol = 3, align = "hv")
flower.ts.col1 <- plot_grid(t4, t3, t2, t1, ncol = 1, align = "hv")
flower.ts.col2 <- plot_grid(t8, t7, t6, t5, ncol = 1, align = "hv")
flower.ts.col3 <- plot_grid(NULL, NULL, t9, NULL, ncol = 1, align = "hv")

# Figure output
quartz(type = "pdf", file = "0_RawFigs/FigS_FlowerLitterTS.pdf", width = 10, height = 7)
grid.arrange(flower.ts.col1, flower.ts.col2, flower.ts.col3,
             ncol = 3, widths = c(1.15,1,1),
             left = "Flower litter (kg/ha)")
dev.off()

