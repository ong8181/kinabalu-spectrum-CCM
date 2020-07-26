####
#### Kitayama et al. "Temperature is a dominant driver of distinct annual
#### seasonality of leaf litter production of equatorial tropical rain forests"
#### Figure code for Supplementary Information: Monthly patterns of flower litter
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
floewr.melt <- as.data.frame.array(melt(lit.d2,
                                      id.vars = c("time_index", "date_cat", "census_lag"),
                                      measure.vars = measure.names))
floewr.melt$date_cat <- ymd(floewr.melt$date_cat)
floewr.melt$site <- factor(substr(floewr.melt$variable, 1, 3), levels = unique(substr(floewr.melt$variable, 1, 3)))
floewr.melt$month <- month(floewr.melt$date_cat, label = TRUE, abbr = TRUE)

# Generate plot
floewr.1 <- floewr.melt[floewr.melt$site == unique(floewr.melt$site)[1],]
floewr.2 <- floewr.melt[floewr.melt$site == unique(floewr.melt$site)[2],]
floewr.3 <- floewr.melt[floewr.melt$site == unique(floewr.melt$site)[3],]
floewr.4 <- floewr.melt[floewr.melt$site == unique(floewr.melt$site)[4],]
floewr.5 <- floewr.melt[floewr.melt$site == unique(floewr.melt$site)[5],]
floewr.6 <- floewr.melt[floewr.melt$site == unique(floewr.melt$site)[6],]
floewr.7 <- floewr.melt[floewr.melt$site == unique(floewr.melt$site)[7],]
floewr.8 <- floewr.melt[floewr.melt$site == unique(floewr.melt$site)[8],]
floewr.9 <- floewr.melt[floewr.melt$site == unique(floewr.melt$site)[9],]

floewr.ylab <- "Flower litter (kg/ha)"
#floewr.ylim <- c(0, 2000)

t1 <- MonthlyTs(floewr.1, floewr.ylab) + theme(axis.title.y = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + ggtitle("700 m, Sedimantary")
t2 <- MonthlyTs(floewr.2, floewr.ylab) + theme(axis.title.y = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + ggtitle("1,700 m, Sedimentary")
t3 <- MonthlyTs(floewr.3, floewr.ylab) + theme(axis.title.y = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + ggtitle("2,700 m, Sedimentary")
t4 <- MonthlyTs(floewr.4, floewr.ylab) + theme(axis.title.y = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + ggtitle("3,100 m, Sedimentary")
t5 <- MonthlyTs(floewr.5, floewr.ylab) + theme(axis.title.y = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + ggtitle("700 m, Ultrabasic")
t6 <- MonthlyTs(floewr.6, floewr.ylab) + theme(axis.title.y = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + ggtitle("1,700 m, Ultrabasic")
t7 <- MonthlyTs(floewr.7, floewr.ylab) + theme(axis.title.y = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + ggtitle("2,700 m, Ultrabasic")
t8 <- MonthlyTs(floewr.8, floewr.ylab) + theme(axis.title.y = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + ggtitle("3,100 m, Ultrabasic")
t9 <- MonthlyTs(floewr.9, floewr.ylab) + theme(axis.title.y = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + ggtitle("1,700 m, Quaternary")

floewr.ts.all <- plot_grid(t4, t8, NULL,
                         t3, t7, NULL,
                         t2, t6, t9,
                         t1, t5, NULL,
                         ncol = 3, align = "hv")
floewr.ts.col1 <- plot_grid(t4, t3, t2, t1, ncol = 1, align = "hv")
floewr.ts.col2 <- plot_grid(t8, t7, t6, t5, ncol = 1, align = "hv")
floewr.ts.col3 <- plot_grid(NULL, NULL, t9, NULL, ncol = 1, align = "hv")

# Figure output
quartz(type = "pdf", file = "0_RawFigs/FigS_MonthlyPatternFloewr.pdf", width = 12, height = 9)
grid.arrange(floewr.ts.col1, floewr.ts.col2, floewr.ts.col3,
             ncol = 3, widths = c(1,1,1),
             left = "Flower litter (kg/ha)")
dev.off()

