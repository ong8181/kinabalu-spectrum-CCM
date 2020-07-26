####
#### Kitayama et al. "Temperature is a dominant driver of distinct annual
#### seasonality of leaf litter production of equatorial tropical rain forests"
#### Figure code for Supplementary Information: Performance test of normal CCM analysis using simulation data
####

# Load library
library(ggplot2)
library(cowplot)
library(gridExtra)
library(reshape2)
library(ggsci)

# Load helper functions for visualization
source('0_FigFunctions/FigFunctions.R')

# Load the simulation results
sr_all <- data.frame(NULL)
for(season_i in seq(0, 2, by = 0.2)){
  sim_file_name <- sprintf("../S03_out/S03_Fourier_SimResOriCCM_%s.csv", season_i)
  sim_file <- read.csv(sim_file_name)
  sr_all <- rbind(sr_all, sim_file)
}

# Data compilation
sr_all$causality <- 0
sr_all$causality[sr_all$oriccm_comb_p < 0.05] <- 1

sr1 <- subset(sr_all, force_var == "force1")
sr2 <- subset(sr_all, force_var == "force2")
sr3 <- subset(sr_all, force_var == "force3")

## Calculate mean p-value
sr1tp <- aggregate(x=list(sr1$oriccm_comb_p), by=list(sr1$error_rate, sr1$force_seasonality), FUN=mean)
sr2tp <- aggregate(x=list(sr2$oriccm_comb_p), by=list(sr2$error_rate, sr2$force_seasonality), FUN=mean)
sr3tp <- aggregate(x=list(sr3$oriccm_comb_p), by=list(sr3$error_rate, sr3$force_seasonality), FUN=mean)
sr1tp_sd <- aggregate(x=list(sr1$oriccm_comb_p), by=list(sr1$error_rate, sr1$force_seasonality), FUN=sd)
sr2tp_sd <- aggregate(x=list(sr2$oriccm_comb_p), by=list(sr2$error_rate, sr2$force_seasonality), FUN=sd)
sr3tp_sd <- aggregate(x=list(sr3$oriccm_comb_p), by=list(sr3$error_rate, sr3$force_seasonality), FUN=sd)
colnames(sr1tp) <- colnames(sr2tp) <- colnames(sr3tp) <- c("error", "seasonality", "mean_joint_p")

# Detection probability
sr1tpP <- aggregate(x=list(sr1$oriccm_comb_p), by=list(sr1$error_rate, sr1$force_seasonality), FUN=calc_ratio)
sr2tpP <- aggregate(x=list(sr2$oriccm_comb_p), by=list(sr2$error_rate, sr2$force_seasonality), FUN=calc_ratio)
sr3tpP <- aggregate(x=list(sr3$oriccm_comb_p), by=list(sr3$error_rate, sr3$force_seasonality), FUN=calc_ratio)
colnames(sr1tpP) <- colnames(sr2tpP) <- colnames(sr3tpP) <- c("error", "seasonality", "sig_ratio")

# Line plot
# Figure a
g1 <- ggplot(sr1tp, aes(error, mean_joint_p, color=as.factor(seasonality)))
g1 <- g1 + geom_point(size=2) + geom_line() + ylim(0, 0.5) + scale_color_igv(name="Seasonality\nstrength")
g1 <- g1 + geom_hline(yintercept = c(0, 0.05), linetype = c(1, 2))
g1 <- style_plot(g1) + xlab("Observation error") + ylab("Mean joint P-value") + ggtitle("Driver of seasonality")
# Figure b
g2 <- ggplot(sr2tp, aes(error, mean_joint_p, color=as.factor(seasonality)))
g2 <- g2 + geom_point(size=2) + geom_line() + ylim(0, 0.5) + scale_color_igv(name="Seasonality\nstrength")
g2 <- g2 + geom_hline(yintercept = c(0, 0.05), linetype = c(1, 2))
g2 <- style_plot(g2) + xlab("Observation error") + ylab("Mean joint P-value") + ggtitle("Periodic influence")
# Figure c
g3 <- ggplot(sr3tp, aes(error, mean_joint_p, color=as.factor(seasonality)))
g3 <- g3 + geom_point(size=2) + geom_line() + ylim(0, 0.5) + scale_color_igv(name="Seasonality\nstrength")
g3 <- g3 + geom_hline(yintercept = c(0, 0.05), linetype = c(1, 2))
g3 <- style_plot(g3) + xlab("Observation error") + ylab("Mean joint P-value") + ggtitle("Non-driver")
g_legend <- get_legend(g1)

# Another visualization
# Figure a
h1 <- ggplot(sr1tpP, aes(error, sig_ratio, color=as.factor(seasonality)))
h1 <- h1 + geom_point(size=2) + geom_line() + ylim(0,1) + scale_color_igv(name="Seasonality\nstrength")
h1 <- style_plot(h1) + xlab("Observation error") + ylab("Detection probability") + ggtitle("Driver of seasonality")
# Figure b
h2 <- ggplot(sr2tpP, aes(error, sig_ratio, color=as.factor(seasonality)))
h2 <- h2 + geom_point(size=2) + geom_line() + ylim(0,1) + scale_color_igv(name="Seasonality\nstrength")
h2 <- style_plot(h2) + xlab("Observation error") + ylab("Detection probability") + ggtitle("Periodic influence")
# Figure c
h3 <- ggplot(sr3tpP, aes(error, sig_ratio, color=as.factor(seasonality)))
h3 <- h3 + geom_point(size=2) + geom_line() + ylim(0,1) + scale_color_igv(name="Seasonality\nstrength")
h3 <- style_plot(h3) + xlab("Observation error") + ylab("Detection probability") + ggtitle("Non-driver")
h_legend <- get_legend(h1)

## contour plot
# Figure a
k1 <- ggplot(sr1tp, aes(error, seasonality, mean_joint_p))
k1 <- ggplot_sim(k1) + geom_tile(aes(fill = mean_joint_p))
# Figure b
k2 <- ggplot(sr2tp, aes(error, seasonality, mean_joint_p))
k2 <- ggplot_sim(k2) + geom_tile(aes(fill = mean_joint_p))
# Figure c
k3 <- ggplot(sr3tp, aes(error, seasonality, mean_joint_p))
k3 <- ggplot_sim(k3) + geom_tile(aes(fill = mean_joint_p))
k_legend <- get_legend(k1)

# Figure output (separate version)
comb.g1 <- cowplot::plot_grid(g1 + theme(legend.position = "none"),
                              g2 + theme(legend.position = "none"),
                              g3 + theme(legend.position = "none"),
                              g_legend, ncol = 4, rel_widths = c(1,1,1,0.5), labels = c("a", "b", "c", NULL))
comb.g2 <- cowplot::plot_grid(h1 + theme(legend.position = "none"),
                              h2 + theme(legend.position = "none"),
                              h3 + theme(legend.position = "none"),
                              ncol = 4, rel_widths = c(1,1,1,0.5), labels = c("d", "e", "f", NULL))
comb.g3 <- cowplot::plot_grid(k1 + theme(legend.position = "none"),
                              k2 + theme(legend.position = "none"),
                              k3 + theme(legend.position = "none"),
                              k_legend, ncol = 4, rel_widths = c(1,1,1,0.5), labels = c("g", "h", "i", NULL))

quartz(type = "pdf", file = "0_RawFigs/0_archives/FigS_nCCMtest_Pval.pdf", width = 10, height = 3)
comb.g1
dev.off()

quartz(type = "pdf", file = "0_RawFigs/0_archives/FigS_nCCMtest_FDR.pdf", width = 10, height = 3)
comb.g2
dev.off()

quartz(type = "pdf", file = "0_RawFigs/0_archives/FigS_nCCMtest_Contour.pdf", width = 11, height = 3)
comb.g3
dev.off()


# Combined figure output
quartz(type = "pdf", file = "0_RawFigs/FigS_nCCMtest_All.pdf", width = 10, height = 9)
plot_grid(comb.g1, comb.g2, comb.g3, ncol = 1)
dev.off()

