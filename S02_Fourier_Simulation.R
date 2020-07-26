####
#### Kitayama et al. "Temperature is a dominant driver of distinct annual
#### seasonality of leaf litter production of equatorial tropical rain forests"
#### No.S2: Testing the performance of Fourier-CCM analysis using simulation data
####

source("config_sim.R")

#### load environments
load(config$out.07.rdata.path)
dir.create(config$out.08.dir, showWarnings = FALSE)

#### Load libraries ####
library(ggplot2)
library(RColorBrewer)
library(grid)
library(timeSeries)
library(cowplot)
library(reshape2)
library(rEDM)
library(zoo)
library(pforeach)
library(psych)
library(imputeTS)

#### Preparation of simulation time series
source('functions/seasonality_ts.R')
source('functions/SimDataCompiler_Window_v1.R')
source('functions/Parallel_Surrogate95CI.R')
source('functions/Helper_functions_H2.R')
source('functions/Helper_functions_H3.R')

#### Call parameter sets
sim.conditions <- CallSimParms(config$k08ParamSetName)

###############################
yl <- sim.conditions$data.length
num.iter <- sim.conditions$num.iter.surrogate
ccm.tp <- sim.conditions$ccm.tp
error.rate.seq <- sim.conditions$error.rates
force.seasonality.seq <- sim.conditions$force.seasonalities
sim.replication <- sim.conditions$simulation.reps
total.sim <- sim.conditions$total.simulations
Erange <- config$k08Erange
###############################

repID.i <- 1 # ID to identify replication
sink(config$out.08.sim.conditions.path)
print(sim.conditions)
sink()


#### Main Simulation Loop ####
for (force.seasonality.i in force.seasonality.seq) {
  #### Prepare data frame for each seasonality set
  sim.res.all <- data.frame()
  
  for (error.rate.i in error.rate.seq) {
    for (rep.i in 1:sim.replication) {
      t.start <- proc.time()
      
      parms.list <- list(yl = yl,
                         num.iter = num.iter,
                         Erange = Erange,
                         ccm.tp = ccm.tp,
                         error.rate.seq = error.rate.i,
                         force.seasonality.seq = force.seasonality.i,
                         cores = config$cores,
                         seed = config$kRndSeed)
      
      sim.res.tmp <- FourierCcmSim(parms.list)
      sim.res.tmp$repID <- repID.i
      sim.res.all <- rbind(sim.res.all, sim.res.tmp)
      
      t.used <- proc.time() - t.start
      message <- sprintf("%s / %s simulation finished: %s sec \n\n",
                         repID.i, total.sim, round(t.used[3], 1))
      cat(message)
      
      repID.i <- repID.i + 1
    }
  }
  
  csv.name <- config$Out08SimResPath(force.seasonality.i)
  write.csv(sim.res.all, csv.name, row.names = F)
}


config$End()

config.08 <- config
rm("config")

#### save image
save.image(file = config.08$out.08.rdata.path)

#### save session info
writeLines(capture.output(sessionInfo()),
           sprintf("00_0_SessionInfo/S02_SessionInfo_Simulation_%s.txt", substr(Sys.time(), 1, 10)))
