####
#### Kitayama et al. "Temperature is a dominant driver of distinct annual
#### seasonality of leaf litter production of equatorial tropical rain forests"
#### Figure code for Kinabalu litter time series
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
measure.names <- colnames(lit.d2)[which(sapply(substr(colnames(lit.d2), 5, 13), "[") ==  "leaf_mean")]
leaf.melt <- as.data.frame.array(melt(lit.d2,
                                      id.vars = c("time_index", "date_cat", "census_lag"),
                                      measure.vars = measure.names))
leaf.melt$date_cat <- ymd(leaf.melt$date_cat)
leaf.melt$site <- factor(substr(leaf.melt$variable, 1, 3), levels = unique(substr(leaf.melt$variable, 1, 3)))

# Generate plot
leaf.1 <- leaf.melt[leaf.melt$site == unique(leaf.melt$site)[1],]
leaf.2 <- leaf.melt[leaf.melt$site == unique(leaf.melt$site)[2],]
leaf.3 <- leaf.melt[leaf.melt$site == unique(leaf.melt$site)[3],]
leaf.4 <- leaf.melt[leaf.melt$site == unique(leaf.melt$site)[4],]
leaf.5 <- leaf.melt[leaf.melt$site == unique(leaf.melt$site)[5],]
leaf.6 <- leaf.melt[leaf.melt$site == unique(leaf.melt$site)[6],]
leaf.7 <- leaf.melt[leaf.melt$site == unique(leaf.melt$site)[7],]
leaf.8 <- leaf.melt[leaf.melt$site == unique(leaf.melt$site)[8],]
leaf.9 <- leaf.melt[leaf.melt$site == unique(leaf.melt$site)[9],]

leaf.ylab <- "Leaf litter (XX)"
leaf.ylim <- c(0, 2100)

t1 <- KinabaluTs(leaf.1, leaf.ylim, leaf.ylab) + theme(axis.title.y = element_blank()) + ggtitle("700 m, Sedimantary")
t2 <- KinabaluTs(leaf.2, leaf.ylim, leaf.ylab) + theme(axis.title.y = element_blank()) + ggtitle("1,700 m, Sedimentary")
t3 <- KinabaluTs(leaf.3, leaf.ylim, leaf.ylab) + theme(axis.title.y = element_blank()) + ggtitle("2,700 m, Sedimentary")
t4 <- KinabaluTs(leaf.4, leaf.ylim, leaf.ylab) + theme(axis.title.y = element_blank()) + ggtitle("3,100 m, Sedimentary")
t5 <- KinabaluTs(leaf.5, leaf.ylim, leaf.ylab) + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) + ggtitle("700 m, Ultrabasic")
t6 <- KinabaluTs(leaf.6, leaf.ylim, leaf.ylab) + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) + ggtitle("1,700 m, Ultrabasic")
t7 <- KinabaluTs(leaf.7, leaf.ylim, leaf.ylab) + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) + ggtitle("2,700 m, Ultrabasic")
t8 <- KinabaluTs(leaf.8, leaf.ylim, leaf.ylab) + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) + ggtitle("3,100 m, Ultrabasic")
t9 <- KinabaluTs(leaf.9, leaf.ylim, leaf.ylab) + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) + ggtitle("1,700 m, Quaternary")

# Overwrite climate variable
climate.correct2[[1]]$date_cat <- ymd(climate.correct2[[1]]$date_cat)
climate.correct2[[2]]$date_cat <- ymd(climate.correct2[[2]]$date_cat)
climate.correct2[[3]]$date_cat <- ymd(climate.correct2[[3]]$date_cat)
climate.correct2[[4]]$date_cat <- ymd(climate.correct2[[4]]$date_cat)

# Overwrite temperature
t1 <- t1 + geom_line(data = climate.correct2[[4]], aes(x = date_cat, y = ATC*300 + 1000), colour = "red3", size = 0.3, alpha = 0.5)
t2 <- t2 + geom_line(data = climate.correct2[[3]], aes(x = date_cat, y = ATC*300 + 1000), colour = "red3", size = 0.3, alpha = 0.5)
t3 <- t3 + geom_line(data = climate.correct2[[1]], aes(x = date_cat, y = ATC*300 + 1000), colour = "red3", size = 0.3, alpha = 0.5)
t4 <- t4 + geom_line(data = climate.correct2[[2]], aes(x = date_cat, y = ATC*300 + 1000), colour = "red3", size = 0.3, alpha = 0.5)
t5 <- t5 + geom_line(data = climate.correct2[[4]], aes(x = date_cat, y = ATC*300 + 1000), colour = "red3", size = 0.3, alpha = 0.5)
t6 <- t6 + geom_line(data = climate.correct2[[3]], aes(x = date_cat, y = ATC*300 + 1000), colour = "red3", size = 0.3, alpha = 0.5)
t7 <- t7 + geom_line(data = climate.correct2[[1]], aes(x = date_cat, y = ATC*300 + 1000), colour = "red3", size = 0.3, alpha = 0.5)
t8 <- t8 + geom_line(data = climate.correct2[[2]], aes(x = date_cat, y = ATC*300 + 1000), colour = "red3", size = 0.3, alpha = 0.5)
t9 <- t9 + geom_line(data = climate.correct2[[3]], aes(x = date_cat, y = ATC*300 + 1000), colour = "red3", size = 0.3, alpha = 0.5)

leaf.ts.all <- plot_grid(t4, t8, NULL,
                         t3, t7, NULL,
                         t2, t6, t9,
                         t1, t5, NULL,
                         ncol = 3, align = "hv")
leaf.ts.col1 <- plot_grid(t4, t3, t2, t1, ncol = 1, align = "hv")
leaf.ts.col2 <- plot_grid(t8, t7, t6, t5, ncol = 1, align = "hv")
leaf.ts.col3 <- plot_grid(NULL, NULL, t9, NULL, ncol = 1, align = "hv")

# Figure output
quartz(type = "pdf", file = "0_RawFigs/Fig_LeafLitterTS.pdf", width = 10, height = 7)
grid.arrange(leaf.ts.col1, leaf.ts.col2, leaf.ts.col3,
             ncol = 3, widths = c(1.15,1,1),
             left = "Leaf litter (kg/ha)")
dev.off()
