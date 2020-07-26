####
#### Kitayama et al. "Temperature is a dominant driver of distinct annual
#### seasonality of leaf litter production of equatorial tropical rain forests"
#### Figure code for Supplementary Information: Illustrations of spectrum-CCM
####

# Load workspace
load('../S02_out/S02_Fourier_SimulationMainTest.RData')

# Load library
library(ggplot2)
library(cowplot); theme_set(theme_cowplot())
library(gridExtra)
library(reshape2)
library(ggsci)
library(imputeTS)
library(rEDM)
library(timeSeries)
library(zoo)
library(pforeach)
library(psych)

# Load helper functions for visualization
# Preparation of simulation time series
#source('../config_sim.R')
source('../functions/seasonality_ts.R')
source('../functions/SimDataCompiler_Window_v1.R')
source('../functions/Parallel_TwinSur_95CI_v3.R')
source('../functions/CheckTS_rEDM_20151117.R')
source('../functions/PowerCIFunc_v1.R')
source('../functions/ggStyle_ushio_v1.R')
source('../functions/ClimateDataCompiler_v1.R')
source('../functions/LitterDataCompiler_v1.R')
source('../functions/LitterDataCompiler_Window_v1.R')
source('../functions/Funcs_for_blake.R')
source('../functions/SimParameterSets.R')

source('0_FigFunctions/FigFunctions.R')

# Make seasonal time series
set.seed(8181) # For reproduction
d.1 <- MakeSeasonalTS(t = 24 * 12, year = 12, sea.parms = c(0, 0, 0), show.fig = T)
d.2 <- MakeSeasonalTS(t = 24 * 12, year = 12, sea.parms = c(.5, .5, .5), show.fig = T)
d.3 <- MakeSeasonalTS(t = 24 * 12, year = 12, sea.parms = c(1, 1, 1), show.fig = T)
d.4 <- MakeSeasonalTS(t = 24 * 12, year = 12, sea.parms = c(2, 2, 2), show.fig = T)

ed <- data.frame(time = d.1$time,
                 season_0 = d.1$xs + 24, # Adjust value for visualizaton
                 season_05 = d.2$xs + 12, # Adjust value for visualizaton
                 season_1 = d.3$xs,
                 season_2 = d.4$xs - 12, # Adjust value for visualizaton
                 y1 = d.3$ys1 - 7, # Adjust value for visualizaton
                 y2 = d.3$ys2 - 14, # Adjust value for visualizaton
                 y3 = d.3$ys3 - 21, # Adjust value for visualizaton
                 seasonality = d.3$season)

ed1 <- melt(ed, id.vars = c("time"),
            measure.vars = c("season_0", "season_05", "season_1", "season_2"))
ed2 <- melt(ed, id.vars = c("time"),
            measure.vars = c("season_1", "y1", "y2", "y3"))

# Perform Fourier-CCM for an example
parms.list <- list(yl = yl,
                   num.iter = num.iter,
                   Erange = Erange,
                   ccm.tp = ccm.tp,
                   error.rate.seq = error.rate.i,
                   force.seasonality.seq = force.seasonality.i,
                   cores = config$cores,
                   seed = 8181)
set.seed(8181) # For reproduction

# Fourier analysis for "window" time series (3-year window)
target <- ts(d.3$xs, start = 1, frequency = 24)
force1 <- ts(d.3$ys1, start = 1, frequency = 24)
force2 <- ts(d.3$ys2, start = 1, frequency = 24)
force3 <- ts(d.3$ys3, start = 1, frequency = 24)
all.ts <- list(target, force1, force2, force3)
ts.names <- c("target", "force1", "force2", "force3")
sim.mw.exm <- SimMwPower(all.ts, trim.length = 3 * 24)
# Fourier analysis (for explanation)
trim.length <- 3 * 24
annual.power.all <- data.frame()
max.trim.no <- min(unlist(lapply(all.ts, length))) - trim.length
data.ls.trim <- lapply(all.ts, function(x) as.ts(zoo::as.zoo(x)[1:trim.length]))
fourier.df <- PerformFourier(data.ls.trim, data.variables =  c("target", "force1", "force2", "force3"))
fourier.df1 <- subset(fourier.df, ID == "target")

# Preparation for CCM for power spectrum time series
# generate first-order difference time series
all.ts.0 <- list(sim.mw.exm[[1]]$spec_norm,
                 sim.mw.exm[[2]]$spec_norm,
                 sim.mw.exm[[3]]$spec_norm,
                 sim.mw.exm[[4]]$spec_norm)
all.ts.embed <- lapply(all.ts.0, function(x) embed(x, dimension = 2))
all.ts.dif <- lapply(all.ts.embed, function(x) as.numeric(scale(x[, 1] - x[, 2])))
spec.w.na <- c(rep(NaN, 3*12), sim.mw.exm[[1]]$spec_norm, rep(NaN, 3*12))
spec.dif.w.na <- c(rep(NaN, 3*12 + 1), all.ts.dif[[1]], rep(NaN, 3*12))

expl.df <- data.frame(time = d.3$time,
                      ts = d.3$xs,
                      spec.ts = spec.w.na,
                      spec.dif = spec.dif.w.na)

# Generate ggplot objects
g1 <- ggplot(ed1, aes(x = time, y = value, group = variable, color = variable))
g1 <- g1 + geom_line(size = 0.5) + scale_color_igv(name = "Seasonality\nstrength", labels = c("None (0.0)", "Weak (0.5)", "Modest (1.0)", "Strong (2.0)"))
g1 <- g1 + xlab("Time index") + theme(legend.title = element_text(size = 12), legend.text = element_text(size = 10))
g1 <- style_plot(g1) + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) + ylab("Value")
g1 <- g1 + ggtitle("Examples of \"Seasonality strength\"")

g2 <- ggplot(ed2, aes(x = time, y = value, group = variable, color = variable))
g2 <- g2 + geom_line(size = 0.5) + scale_color_igv(name = "Variables", labels = c("Response variable", "Continuous influence\n(driver of seasonality)", "Periodic influence", "No influence"))
g2 <- style_plot(g2) + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) + ylab("Value")
g2 <- g2 + xlab("Time index") + ggtitle("Model time series examples")

g3 <- ggplot(expl.df, aes(x = time, y = ts))
g3 <- g3 + geom_line(size = 0.5, color = "gray") + geom_point(size = 1)
g3 <- style_plot(g3) + xlab("Time index") + ylab("Value")

g4 <- ggplot(fourier.df1, aes(x = freq, y = spec_norm))
g4 <- g4 + geom_line(size = 0.5, color = "gray") + geom_point(size = 1)
g4 <- style_plot(g4) + xlab("Frequency") + ylab("Power\n(standardized spectrum)")
g4 <- g4 + geom_point(aes(x = fourier.df1$freq[3], y = fourier.df1$spec_norm[3]), colour = "red3")

g5 <- ggplot(expl.df, aes(x = time, y = spec.w.na))
g5 <- g5 + geom_line(size = 0.5, color = "gray") + geom_point(size = 1)
g5 <- style_plot(g5) + xlab("Time index") + ylab("Time series of \npower at 1-year cycle")

g6 <- ggplot(expl.df, aes(x = time, y = spec.dif.w.na))
g6 <- g6 + geom_line(size = 0.5, color = "gray") + geom_point(size = 1)
g6 <- style_plot(g6) + xlab("Time index") + ylab("First difference \nof power at 1-year cycle")

g1.legend <- get_legend(g1)
g2.legend <- get_legend(g2)

left <- plot_grid(g3 + theme(plot.margin = unit(c(.2,.5,.2, 1), "cm")),
                  g4 + theme(plot.margin = unit(c(.2,.5,.2, 1), "cm")),
                  g5 + theme(plot.margin = unit(c(.2,.5,.2, 1), "cm")),
                  g6 + theme(plot.margin = unit(c(.2,.5,.2, 1), "cm")),
                  ncol = 1,
                  labels = c("a", "b", "c", "d"), greedy = T,
                  align = "hv", label_x = 0.02, label_y = 1)
right <- plot_grid(g1 + theme(legend.position = "none"), g1.legend,
                   g2 + theme(legend.position = "none"), g2.legend,
                   rel_widths = c(1, 0.5),
                   ncol = 2, labels = c("e", NA, "f", NA))

quartz(type = "pdf", file = "0_RawFigs/FigS_fCCM_Explanation.pdf", width = 12, height = 9)
plot_grid(left, right, ncol = 2, rel_widths = c(1, 1.2))
dev.off()
