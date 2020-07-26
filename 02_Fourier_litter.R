####
#### Kitayama et al. "Temperature is a dominant driver of distinct annual
#### seasonality of leaf litter production of equatorial tropical rain forests"
#### No.2: Fourier Analysis
####

source("config.R")

#### load environments
load(config$out.01.rdata.path)
dir.create(config$out.02.dir, showWarnings = FALSE)
dir.create(sprintf("%s/fig_csv", config$out.02.dir), showWarnings = FALSE)

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

#### Object information
## Original data (not averaged)
# litter data = lit.d
# original data = climate.not.correct
# corrected data = climate.correct
## Original data (averaged for one date category; basically same with the lit_d, but USE this data!)
# litter data = lit.d2
# original data = climate.not.correct2
# corrected data = climate.correct2

# site.names.climate <- c("car", "lab", "phq", "por")
# site_names_litter <- c("POR", "PHQ", "RTM", "PAK", "NAL", "BAB", "CAR", "HEL", "ULA")
# data.cat1 <- c("leaf", "flower", "branch", "epi", "bamboo", "dust")
# data.cat2 <- c("mean", "sd")
# litter.cat2.name <- "mean"

#### Load functions
source('functions/LitterDataCompiler_v1.R')
source('functions/PowerCIFunc_v1.R')
source('functions/Helper_functions_H1.R')

#### prepare object to incude all results
conf.all <- c()

#### Extract data set
for (i1 in 1:9) {
  # specify study site
  data.ls <- DataCompiler01(lit.d2, config$kSiteNamesLitter, config$kLitterDataCat1,
                            config$kLitterDataCat2, i1, visualize = F)

  ## add names to litter species (referred to as individuals here)
  names(data.ls) <- config$kLitterDataCat1
  individuals <- length(data.ls)
  ids <- names(data.ls)
  
  #### Generate data for Fourier analysis
  #individuals <- 6 # the number of litter species
  #months <- rep(length(unique(lit.d2$date_cat)), length.out=individuals) #length of timeseries (months)
  #wavelength <- rep(24,length.out=individuals) # Dominant wavelength (months)
  
  ####################################################
  #### Intitial observation of the Periodogram #######
  ###################################################
  
  ## Run fourier analysis using function "spectrum" on each list object (individual
  ## time series) to give Fourier outputs for each individual
  
  fourier.df <- data.frame()
  for(i in 1:length(data.ls)){
    d <- data.frame(freq = SpecFun(data.ls[[i]])$freq / 24)
    d$spec <- SpecFun(data.ls[[i]])$spec
    d$spec_norm <- SpecFun(data.ls[[i]])$spec * (1 / mean(SpecFun(data.ls[[i]])$spec))
    d$ID <- as.factor(ids[i])
    fourier.df <- rbind(fourier.df, d)
    }
  
  # Title for the periodgram
  title.periodgram <- sprintf("Smoothed periodgrams of %s", config$kSiteNamesLitter[i1])
  # Periogorams for each individual on one plot - Figure 1c
  periodogram <- ggplot(data = fourier.df, aes(x = freq, y = spec_norm, group = ID, colour = ID)) +
    geom_line() + theme_minimal(base_size = 14) +
    ylab("Power (standardised spectrum)") +
    xlab("Cycle frequency (cycles per 2 week)") + ggtitle(title.periodgram)
  
  # Summarise dominant peaks
  fourier.df.sample.summary <- ddply(fourier.df, .(ID), summarize, maxFreq = freq[which.max(spec)], maxSpec_norm = max(spec_norm))
  
  # Dominant phenology cycle for sample (month)
  median.cycle <- 1 / fourier.df.sample.summary$maxFreq / 2
  fourier.df.sample.summary$cycles <- median.cycle
  
  #####################################################################################
  #### Periodogram analysis with confidence intervals to find frequency and phase #####
  #####################################################################################
  
  ##Find frequency of dominant cycles and test against null hypothesis using 95%
  ##confidence interval derived from #Bloomfield et al. (2004)
  ###### Run loop to apply spectrum functions to each individual time series in data.ls
  confidence.results <- ldply(1:length(data.ls), .fun = ConfidenceFun, data.ls, ids)
  confidence.results$site <- config$kSiteNamesLitter[i1]
  conf.all <- rbind(conf.all, confidence.results)
  
  #### Visualize confidence interval and null distribution of periodgrams
  # rename
  conf.lit <- confidence.results
  conf.lit.bars.low <- conf.lit$lower_ci_norm
  conf.lit.bars.up <- conf.lit$upper_ci_norm
  conf.lit.bars.mean <- conf.lit$spec_dom_norm
  conf.lit.bars.null <- conf.lit$spec_null_dom_norm
  conf.lit.bars.avr <- rep(1, 6)
  
  sig.cycle.plot <- conf.lit$sig_avr
  sig.cycle.plot[sig.cycle.plot] <- 3
  sig.cycle.plot[!sig.cycle.plot] <- 0
  
  sig.cycle.plot2 <- conf.lit$sig_null
  sig.cycle.plot2[sig.cycle.plot2] <- 3
  sig.cycle.plot2[!sig.cycle.plot2] <- 0
  
  cycles.m.lit <- conf.lit$cycle_dom
  cycles.m.lit[cycles.m.lit > 48] <- 48
  cycles.col <- cycles.m.lit
  cycles.col[cycles.col < 48] <- 1
  cycles.col[cycles.col == 48] <- 2
  
  # title for the confidence interval
  title.conf <- sprintf("Confidence intervals of %s", config$kSiteNamesLitter[i1])
  
  conf.v1 <- ggplot(conf.lit, aes(x = ID, y = spec_dom_norm))
  conf.v1 <- conf.v1  + geom_point(shape = 18, size = 3) +
    geom_errorbar(aes(ymin = conf.lit.bars.low, ymax = conf.lit.bars.up, width = 0.3)) +
    geom_point(aes(x = ID, y = spec_null_dom_norm), shape = 95, size = 10, colour = "royalblue") +
    geom_point(aes(x = ID, y = 0.5), shape = 8, size = sig.cycle.plot2, colour = "royalblue")
  conf.v1 <- conf.v1 + ylab("Standardised spectrum") + ggtitle(title.conf)
  conf.v1 <- StylePlot(conf.v1)
  
  # Periodogram
  period.v1 <- StylePlot(periodogram) + geom_abline(intercept = 0, slope = 0, lwd = 0.5, linetype = 2)
  
  # Dominant cycle
  title.cycle <- sprintf("Dominant cycle of %s", config$kSiteNamesLitter[i1])
  cycle.v1 <- ggplot(conf.lit, aes(x = ID, y = cycle_dom))
  cycle.v1 <- cycle.v1 + ylim(0, 48) +
    geom_point(aes(x = ID, y = cycles.m.lit), shape = 18, size = 4, colour = cycles.col) +
    ylab("Length of the dominant cycle (month)") +
    geom_abline(intercept = c(0, 12, 24, 36, 48), slope = 0, lwd = 0.5, linetype = 2) +
    geom_point(aes(x = ID, y = 0), shape = 8, size = sig.cycle.plot2, colour = "royalblue") +
    ggtitle(title.cycle)
  cycle.v1 <- StylePlot(cycle.v1)
  
  if(T){ # Save figures and data
    dir.create(config$out.02.fig.dir, showWarnings = FALSE)
    pdf(width = 5, height = 9, file = config$Out02PdfPath(config$kSiteNamesLitter[i1]))
    grid.arrange(period.v1, cycle.v1, conf.v1, ncol = 1)
    dev.off()
    
    csv.file.name1 <- sprintf("%s/fig_csv/data_litter_period_%s.csv", config$out.02.dir, i1)
    csv.file.name2 <- sprintf("%s/fig_csv/data_litter_cycle_%s.csv", config$out.02.dir, i1)
    write.csv(fourier.df, file = csv.file.name1, row.names = F)
    write.csv(conf.lit, file = csv.file.name2, row.names = F)
  }
}

config$End()

config.02 <- config
rm("config")

#### save image
save.image(file = config.02$out.02.rdata.path)

#### save session info
writeLines(capture.output(sessionInfo()),
           sprintf("00_0_SessionInfo/02_SessionInfo_LitterRev_%s.txt", substr(Sys.time(), 1, 10)))

