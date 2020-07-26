####
#### Kitayama et al. "Temperature is a dominant driver of distinct annual
#### seasonality of leaf litter production of equatorial tropical rain forests"
#### Figure code for Supplementary Information: Monthly patterns of leaf litter
####

# Load environments
load("../06_out/06_Fourier_CCMcompile_FFT.RData")

# Load libraries
library(ggplot2)
library(cowplot); theme_set(theme_cowplot())
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
leaf.melt$month <- month(leaf.melt$date_cat, label = TRUE, abbr = TRUE)

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

leaf.ylab <- "Leaf litter (kg/ha)"

t1 <- MonthlyTs(leaf.1, leaf.ylab) + theme(axis.title.y = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + ggtitle("700 m, Sedimantary")
t2 <- MonthlyTs(leaf.2, leaf.ylab) + theme(axis.title.y = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + ggtitle("1,700 m, Sedimentary")
t3 <- MonthlyTs(leaf.3, leaf.ylab) + theme(axis.title.y = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + ggtitle("2,700 m, Sedimentary")
t4 <- MonthlyTs(leaf.4, leaf.ylab) + theme(axis.title.y = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + ggtitle("3,100 m, Sedimentary")
t5 <- MonthlyTs(leaf.5, leaf.ylab) + theme(axis.title.y = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + ggtitle("700 m, Ultrabasic")
t6 <- MonthlyTs(leaf.6, leaf.ylab) + theme(axis.title.y = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + ggtitle("1,700 m, Ultrabasic")
t7 <- MonthlyTs(leaf.7, leaf.ylab) + theme(axis.title.y = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + ggtitle("2,700 m, Ultrabasic")
t8 <- MonthlyTs(leaf.8, leaf.ylab) + theme(axis.title.y = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + ggtitle("3,100 m, Ultrabasic")
t9 <- MonthlyTs(leaf.9, leaf.ylab) + theme(axis.title.y = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + ggtitle("1,700 m, Quaternary")

leaf.ts.all <- plot_grid(t4, t8, NULL,
                         t3, t7, NULL,
                         t2, t6, t9,
                         t1, t5, NULL,
                         ncol = 3, align = "hv")
leaf.ts.col1 <- plot_grid(t4, t3, t2, t1, ncol = 1, align = "hv")
leaf.ts.col2 <- plot_grid(t8, t7, t6, t5, ncol = 1, align = "hv")
leaf.ts.col3 <- plot_grid(NULL, NULL, t9, NULL, ncol = 1, align = "hv")

# Figure output
quartz(type = "pdf", file = "0_RawFigs/FigS_MonthlyPatternLeaf.pdf", width = 12, height = 9)
grid.arrange(leaf.ts.col1, leaf.ts.col2, leaf.ts.col3,
             ncol = 3, widths = c(1,1,1),
             left = "Leaf litter (kg/ha)")
dev.off()

