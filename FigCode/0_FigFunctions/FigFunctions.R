####
#### Kitayama et al. "Temperature is a dominant driver of distinct annual
#### seasonality of leaf litter production of equatorial tropical rain forests"
#### Figure functions
####

# Define functions
ggplot_sim <- function(f0, limits_vals = c(0,0.6)){
  f0 <- f0 + scale_fill_gradient2(low="red3", mid="white", high="royalblue", midpoint=0.1, limits=limits_vals, name = "Mean\njoint P-value")
  f0 <- f0 + ylab("Seasonality of the force") 
  f0 <- f0 + xlab("Observation error") + theme_bw()
  return(f0)
}

style_plot <-  function(ggobject){
  return(ggobject + theme_bw() + theme(axis.text.x = element_text(angle=0),
                                       panel.grid.major = element_blank(),
                                       panel.grid.minor = element_blank(),
                                       axis.text = element_text(size=12),
                                       axis.title = element_text(size=12),
                                       panel.background=element_rect(colour="black", fill=NA, size=0.8)))
}

# Functions for time series plot
KinabaluTs <-  function(ggdata, ylim.vec, ylab.char){
  t1 <- ggplot(ggdata, aes(x = date_cat, y = value))
  t1 <- t1 + geom_line(size = 0.5) + ylab(ylab.char)
  t1 <- t1 + theme_bw() + theme(axis.text.x = element_text(angle=0),
                                    axis.text = element_text(size=12),
                                    axis.title = element_text(size=12),
                                    panel.grid.minor = element_blank(),
                                    axis.title.x = element_blank(),
                                    panel.background=element_rect(colour="black", fill=NA, size=0.8))
  t1 <- t1 + ylim(ylim.vec[1], ylim.vec[2])
  return(t1)
}

MonthlyTs <-  function(ggdata, ylab.char){
  t1 <- ggplot(ggdata, aes(x = month, y = value))
  t1 <- t1 + geom_boxplot(outlier.shape = NA) + ylab(ylab.char)
  t1 <- t1 + geom_jitter(size = 1, width = 0.1, alpha = 0.5)
  t1 <- t1 + theme_bw() + theme(axis.text.x = element_text(angle=90),
                                axis.text = element_text(size=12),
                                axis.title = element_text(size=12),
                                panel.grid.minor = element_blank(),
                                panel.grid.major = element_blank(),
                                axis.title.x = element_blank(),
                                panel.background=element_rect(colour="black", fill=NA, size=0.8))
  t1 <- t1 + scale_y_log10()
  return(t1)
}

MonthlyTs2 <-  function(ggdata, c.var, ylim.vec, ylab.char){
  ggdata$value <- as.numeric(ggdata[,c.var])
  t1 <- ggplot(ggdata, aes(x = month, y = value))
  t1 <- t1 + geom_boxplot(outlier.shape = NA) + ylab(ylab.char)
  t1 <- t1 + geom_jitter(size = 1, width = 0.1, alpha = 0.5)
  t1 <- t1 + theme_bw() + theme(axis.text.x = element_text(angle=90),
                                axis.text = element_text(size=12),
                                axis.title = element_text(size=12),
                                panel.grid.minor = element_blank(),
                                panel.grid.major = element_blank(),
                                axis.title.x = element_blank(),
                                panel.background=element_rect(colour="black", fill=NA, size=0.8))
  t1 <- t1 + ylim(ylim.vec[1], ylim.vec[2])
  return(t1)
}


calc_ratio <- function(x){
  return(sum(x < 0.05)/length(x))
}

# For confidence interval, litter
ConfLitFig <- function(csv.data, plot.title){
  # Rename & reorder litter ID
  csv.data$litter <- NaN
  csv.data$litter[csv.data$ID == "leaf"] <- "Leaf"
  csv.data$litter[csv.data$ID == "flower"] <- "Flower"
  csv.data$litter <- factor(csv.data$litter, levels = unique(csv.data$litter))
  csv.data <- csv.data[order(csv.data$litter),]
  
  conf.lit.bars.low <- csv.data$lower_ci_norm
  conf.lit.bars.up <- csv.data$upper_ci_norm
  conf.lit.bars.mean <- csv.data$spec_dom_norm
  conf.lit.bars.null <- csv.data$spec_null_dom_norm
  conf.lit.bars.avr <- rep(1, nrow(csv.data))
  
  sig.cycle.plot <- csv.data$sig_avr
  sig.cycle.plot[sig.cycle.plot] <- 3
  sig.cycle.plot[!sig.cycle.plot] <- 0
  
  sig.cycle.plot2 <- csv.data$sig_null
  sig.cycle.plot2[sig.cycle.plot2] <- 3
  sig.cycle.plot2[!sig.cycle.plot2] <- 0
  
  cycles.m.lit <- csv.data$cycle_dom
  cycles.m.lit[cycles.m.lit > 48] <- 48
  cycles.col <- cycles.m.lit
  cycles.col[cycles.col < 48] <- "black"
  cycles.col[cycles.col == 48] <- "darkgray"
  
  # title for the confidence interval
  conf.v1 <- ggplot(csv.data, aes(x = site, y = spec_dom_norm, group = litter))
  conf.v1 <- conf.v1  + geom_point(shape = 18, size = 3) +
    geom_errorbar(aes(ymin = conf.lit.bars.low, ymax = conf.lit.bars.up, width = 0.3)) +
    geom_point(aes(x = site, y = spec_null_dom_norm), shape = 95, size = 10, colour = "red3") +
    geom_point(aes(x = site, y = spec_avr_norm), shape = 95, size = 10, colour = "royalblue") +
    geom_point(aes(x = site, y = 0.5), shape = 8, size = sig.cycle.plot, colour = "royalblue") +
    geom_point(aes(x = site, y = 0.7), shape = 8, size = sig.cycle.plot2, colour = "red3")
  conf.v1 <- conf.v1 + ylab("Standardised\nspectrum") + ggtitle(plot.title) + facet_wrap(.~litter)
  conf.v1 <- style_plot(conf.v1) + theme(axis.title.x = element_blank(),
                                         axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(conf.v1)
}

CycLitFig <- function(csv.data, plot.title){
  # Rename & reorder litter ID
  csv.data$litter <- NaN
  csv.data$litter[csv.data$ID == "leaf"] <- "Leaf"
  csv.data$litter[csv.data$ID == "flower"] <- "Flower"
  csv.data$litter <- factor(csv.data$litter, levels = unique(csv.data$litter))
  csv.data <- csv.data[order(csv.data$litter),]
  
  sig.cycle.plot <- csv.data$sig_avr
  sig.cycle.plot[sig.cycle.plot] <- 3
  sig.cycle.plot[!sig.cycle.plot] <- 0
  
  sig.cycle.plot2 <- csv.data$sig_null
  sig.cycle.plot2[sig.cycle.plot2] <- 3
  sig.cycle.plot2[!sig.cycle.plot2] <- 0
  
  cycles.m.lit <- csv.data$cycle_dom
  cycles.m.lit[cycles.m.lit > 48] <- 48
  cycles.col <- cycles.m.lit
  cycles.col[cycles.col < 48] <- "black"
  cycles.col[cycles.col == 48] <- "darkgray"

  cycle.v1 <- ggplot(csv.data, aes(x = site, y = cycle_dom, group = litter))
  cycle.v1 <- cycle.v1 + ylim(0, 48) +
    geom_point(aes(x = site, y = cycles.m.lit), shape = 18, size = 4)  + facet_wrap(. ~ litter) +
    ylab("Length of\nthe dominant cycle (month)") +
    geom_abline(intercept = c(0, 12, 24, 36, 48), slope = 0, lwd = 0.5, linetype = 2) +
    geom_point(aes(x = site, y = 1), shape = 8, size = sig.cycle.plot, colour = "royalblue") +
    geom_point(aes(x = site, y = 5), shape = 8, size = sig.cycle.plot2, colour = "red3") +
    ggtitle(plot.title)
  cycle.v1 <- style_plot(cycle.v1) + theme(axis.title.x = element_blank(),
                                           axis.text.x = element_text(angle = 45, hjust = 1))

  return(cycle.v1)
}


#---------------- For confidence interval of climate ----------------#
ConfCliFig <- function(csv.data, plot.title){
  csv.data$ID2 <- NaN
  csv.data$ID2[csv.data$ID == "ATC"] <- "Mean temp."
  csv.data$ID2[csv.data$ID == "Act_VP_kPa"] <- "Actual VP"
  csv.data$ID2[csv.data$ID == "Corrected_SD_kPa"] <- "Sat. Def."
  csv.data$ID2[csv.data$ID == "PAR"] <- "PAR"
  csv.data$ID2[csv.data$ID == "ET0_potential_evp"] <- "Potential ET"
  csv.data$ID2[csv.data$ID == "rain"] <- "Rain"
  
  csv.data$ID2 <- factor(csv.data$ID2,
                         levels = c("Mean temp.",
                                    "Potential ET",
                                    "Actual VP",
                                    "Sat. Def.",
                                    "PAR",
                                    "Rain"))

  conf.lit.bars.low <- csv.data$lower_ci_norm
  conf.lit.bars.up <- csv.data$upper_ci_norm
  conf.lit.bars.mean <- csv.data$spec_dom_norm
  conf.lit.bars.null <- csv.data$spec_null_dom_norm
  conf.lit.bars.avr <- rep(1, 6)
  
  sig.cycle.plot <- csv.data$sig_avr
  sig.cycle.plot[sig.cycle.plot] <- 3
  sig.cycle.plot[!sig.cycle.plot] <- 0
  
  sig.cycle.plot2 <- csv.data$sig_null
  sig.cycle.plot2[sig.cycle.plot2] <- 3
  sig.cycle.plot2[!sig.cycle.plot2] <- 0
  
  cycles.m.lit <- csv.data$cycle_dom
  cycles.m.lit[cycles.m.lit > 48] <- 48
  cycles.col <- cycles.m.lit
  cycles.col[cycles.col < 48] <- "black"
  cycles.col[cycles.col == 48] <- "darkgray"
  
  conf.v1 <- ggplot(csv.data, aes(x = ID2, y = spec_dom_norm))
  conf.v1 <- conf.v1  + geom_point(shape = 18, size = 3) +
    geom_errorbar(aes(ymin = conf.lit.bars.low, ymax = conf.lit.bars.up, width = 0.3)) +
    geom_point(aes(x = ID2, y = spec_null_dom_norm), shape = 95, size = 10, colour = "red3") +
    geom_point(aes(x = ID2, y = spec_avr_norm), shape = 95, size = 10, colour = "royalblue") +
    geom_point(aes(x = ID2, y = 0.5), shape = 8, size = sig.cycle.plot, colour = "royalblue") +
    geom_point(aes(x = ID2, y = 0.7), shape = 8, size = sig.cycle.plot2, colour = "red3")
  conf.v1 <- conf.v1 + ylab("Standardised\nspectrum") + ggtitle(plot.title)
  conf.v1 <- style_plot(conf.v1) + theme(axis.title.x = element_blank(),
                                         axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(conf.v1)
}

CycCliFig <- function(csv.data, plot.title){
  csv.data$ID2 <- NaN
  csv.data$ID2[csv.data$ID == "ATC"] <- "Mean temp."
  csv.data$ID2[csv.data$ID == "Act_VP_kPa"] <- "Actual VP"
  csv.data$ID2[csv.data$ID == "Corrected_SD_kPa"] <- "Sat. Def."
  csv.data$ID2[csv.data$ID == "PAR"] <- "PAR"
  csv.data$ID2[csv.data$ID == "ET0_potential_evp"] <- "Potential ET"
  csv.data$ID2[csv.data$ID == "rain"] <- "Rain"
  
  csv.data$ID2 <- factor(csv.data$ID2,
                         levels = c("Mean temp.",
                                    "Potential ET",
                                    "Actual VP",
                                    "Sat. Def.",
                                    "PAR",
                                    "Rain"))
  
  sig.cycle.plot <- csv.data$sig_avr
  sig.cycle.plot[sig.cycle.plot] <- 3
  sig.cycle.plot[!sig.cycle.plot] <- 0
  
  sig.cycle.plot2 <- csv.data$sig_null
  sig.cycle.plot2[sig.cycle.plot2] <- 3
  sig.cycle.plot2[!sig.cycle.plot2] <- 0
  
  cycles.m.lit <- csv.data$cycle_dom
  cycles.m.lit[cycles.m.lit > 48] <- 48
  cycles.col <- cycles.m.lit
  cycles.col[cycles.col < 48] <- "black"
  cycles.col[cycles.col == 48] <- "darkgray"
  
  cycle.v1 <- ggplot(csv.data, aes(x = ID2, y = cycle_dom))
  cycle.v1 <- cycle.v1 + ylim(0, 48) +
    geom_point(aes(x = ID2, y = cycles.m.lit), shape = 18, size = 4, colour = cycles.col) +
    ylab("Length of\nthe dominant cycle (month)") +
    geom_abline(intercept = c(0, 12, 24, 36, 48), slope = 0, lwd = 0.5, linetype = 2) +
    geom_point(aes(x = ID2, y = 1), shape = 8, size = sig.cycle.plot, colour = "royalblue") +
    geom_point(aes(x = ID2, y = 5), shape = 8, size = sig.cycle.plot2, colour = "red3") +
    ggtitle(plot.title)
  cycle.v1 <- style_plot(cycle.v1) + theme(axis.title.x = element_blank(),
                                           axis.text.x = element_text(angle = 45, hjust = 1))
  return(cycle.v1)
}



