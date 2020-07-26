####
#### Kitayama et al. "Temperature is a dominant driver of distinct annual
#### seasonality of leaf litter production of equatorial tropical rain forests"
#### No.3: Fourier Analysis for climate variables
####

source("config.R")

#### load environments
load(config$out.02.rdata.path)
dir.create(config$out.03.dir, showWarnings = FALSE)
dir.create(sprintf("%s/fig_csv", config$out.03.dir), showWarnings = FALSE)

#### Load libraries ####
library(plyr)
library(ggplot2)
library(reshape)
library(RColorBrewer)
library(gtools)
library(reshape2)
library(gridExtra)
library(circular)
library(fields)
library(DescTools)
library(truncnorm)
library(timeSeries)
library(imputeTS)

#### replace object
conf.all.litter <- conf.all

## load functions
source('functions/PowerCIFunc_v1.R')
source('functions/Helper_functions_H1.R')
source('functions/ClimateDataCompiler_v1.R')

#### Select litter properties
conf.all.climate <- c()

for (i1 in 1:4) {
  data.ls <- DataCompiler02(climate.correct2, config$kSiteNamesClimate,
                            i1, config$kSelectedClimateCat, allow.NAs = 6, visualize = F)
  
  names(data.ls) <- config$kSelectedClimateCat
  individuals <- length(data.ls)
  ids <- names(data.ls)
  
  ####################################################
  #### Intitial observation of the Periodogram #######
  ###################################################
  
  ##Run fourier analysis using function "spectrum" on each list object (individual
  ##time series) to give Fourier outputs for each individual
  fourier.df <- data.frame()
  for (i in 1:length(data.ls)) {
    d <- data.frame(freq = SpecFun(data.ls[[i]])$freq / 24)
    d$spec <- SpecFun(data.ls[[i]])$spec
    d$spec_norm <- SpecFun(data.ls[[i]])$spec * (1 / mean(SpecFun(data.ls[[i]])$spec))
    d$ID <- as.factor(ids[i])
    fourier.df <- rbind(fourier.df, d)
  }
  
  # title for the periodgram
  title.periodgram <-
    sprintf("Smoothed periodgrams of %s", config$kSiteNamesClimate[i1])
  # Periogorams for each individual on one plot - Figure 1c
  periodograms <- ggplot(data = fourier.df, aes(x = freq, y = spec_norm, group = ID, colour = ID)) +
    geom_line() +
    theme_minimal(base_size = 14) +
    ylab("Power (standardised spectrum)") +
    xlab("Cycle frequency (cycles per 2 week)") +
    ggtitle(title.periodgram)
  
  #Summarise dominant peaks
  fourier.df.sample.summary <- ddply(fourier.df, .(ID), summarize, maxFreq = freq[which.max(spec)], maxSpec_norm = max(spec_norm))
  
  #Dominant phenology cycle for sample (month)
  median.cycle <- 1 / fourier.df.sample.summary$maxFreq / 2
  fourier.df.sample.summary$cycles <- median.cycle
  
  #####################################################################################
  #### Periodogram analysis with confidence intervals to find frequency and phase #####
  #####################################################################################
  
  ##Find frequency of dominant cycles and test against null hypothesis using 95%
  ##confidence interval derived from #Bloomfield et al. (2004)
  ###### Run loop to apply spectrum functions to each individual time series in data.ls
  
  confidence.results <- ldply(1:length(data.ls), .fun = ConfidenceFun, data.ls, ids)
  confidence.results$site <- config$kSiteNamesClimate[i1]
  conf.all.climate <- rbind(conf.all.climate, confidence.results)
  
  #### Visualize confidence interval and null distribution of periodgrams
  # rename
  conf.cli <- confidence.results
  conf.cli.bars.low <- conf.cli$lower_ci_norm
  conf.cli.bars.up <- conf.cli$upper_ci_norm
  conf.cli.bars.mean <- conf.cli$spec_dom_norm
  conf.cli.bars.null <- conf.cli$spec_null_dom_norm
  conf.cli.bars.avr <- rep(1, 6)
  
  sig.cycle.plot <- conf.cli$sig_avr
  sig.cycle.plot[sig.cycle.plot] <- 3
  sig.cycle.plot[!sig.cycle.plot] <- 0
  
  sig.cycle.plot2 <- conf.cli$sig_null
  sig.cycle.plot2[sig.cycle.plot2] <- 3
  sig.cycle.plot2[!sig.cycle.plot2] <- 0
  
  cycles.m.cli <- conf.cli$cycle_dom
  cycles.m.cli[cycles.m.cli > 48] <- 48
  cycles.col <- cycles.m.cli
  cycles.col[cycles.col < 48] <- 1
  cycles.col[cycles.col == 48] <- 2
  
  # title for the confidence interval
  title.conf <- sprintf("Confidence intervals of %s", config$kSiteNamesClimate[i1])
  
  conf.v2 <- ggplot(conf.cli, aes(x = ID, y = spec_dom_norm))
  conf.v2 <- conf.v2 + geom_point(shape = 18, size = 3) +
    geom_errorbar(aes(ymin = conf.cli.bars.low, ymax = conf.cli.bars.up, width = 0.3)) +
    geom_point(aes(x = ID, y = spec_null_dom_norm), shape = 95, size = 10, colour = "royalblue") +
    geom_point(aes(x = ID, y = 0.5), shape = 8, size = sig.cycle.plot2, colour = "royalblue")
  conf.v2 <- conf.v2 + ylab("Standardised spectrum") + ggtitle(title.conf)
  conf.v2 <- StylePlot(conf.v2)
  
  # Periodogram
  period.v2 <- StylePlot(periodograms) + geom_abline(intercept = 0, slope = 0, lwd = 0.5, linetype = 2)
  
  # Dominant cycle
  title.cycle <- sprintf("Dominant cycle of %s", config$kSiteNamesClimate[i1])
  cycle.v2 <- ggplot(conf.cli, aes(x = ID, y = cycle_dom))
  cycle.v2 <- cycle.v2 + ylim(0, 48) +
    geom_point(aes(x = ID, y = cycles.m.cli), shape = 18, size = 4, colour = cycles.col) +
    ylab("Length of the dominant cycle (month)") +
    geom_abline(intercept = c(0, 12, 24, 36, 48), slope = 0, lwd = 0.5, linetype = 2) +
    geom_point(aes(x = ID, y = 0), shape = 8, size = sig.cycle.plot2, colour = "royalblue") +
    ggtitle(title.cycle)
  cycle.v2 <- StylePlot(cycle.v2)
  
  if(T){ # Save figures and data
    dir.create(config$out.03.fig.dir, showWarnings = FALSE)
    pdf(width = 5, height = 9, file = config$Out03PdfPath(config$kSiteNamesLitter[i1]))
    grid.arrange(period.v2, cycle.v2, conf.v2, ncol = 1)
    dev.off()
    
    csv.file.name3 <- sprintf("%s/fig_csv/data_climate_period_%s.csv", config$out.03.dir, i1)
    csv.file.name4 <- sprintf("%s/fig_csv/data_climate_cycle_%s.csv", config$out.03.dir, i1)
    write.csv(fourier.df, file = csv.file.name3, row.names = F)
    write.csv(conf.cli, file = csv.file.name4, row.names = F)
  }
}

config$End()

config.03 <- config
rm("config")

#### save image
save.image(file = config.03$out.03.rdata.path)

#### save session info
writeLines(capture.output(sessionInfo()),
           sprintf("00_0_SessionInfo/03_SessionInfo_Climate_%s.txt", substr(Sys.time(), 1, 10)))
