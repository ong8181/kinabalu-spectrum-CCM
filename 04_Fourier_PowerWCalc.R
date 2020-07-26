####
#### Kitayama et al. "Temperature is a dominant driver of distinct annual
#### seasonality of leaf litter production of equatorial tropical rain forests"
#### No. 4: Moving-Window Fourier analysis
####

source("config.R")

#### load environments
load(config$out.03.rdata.path)
dir.create(config$out.04.dir, showWarnings = FALSE)

#### Load libraries ####
library(plyr)
library(ggplot2)
library(reshape2)
library(reshape)
library(gtools)
library(gridExtra)
library(RColorBrewer)
library(circular)
library(fields)
library(DescTools)
library(truncnorm)
library(timeSeries)
library(cowplot)
library(zoo)

############### Litter analysis ##################
#### Calculate Moving-Window-Seasonality
## extract the power of annual seasonality
# load function
source('functions/LitterDataCompiler_Window_v1.R')

# specify window size
window.size <- 24 * config$kWindowYears
window.size.NAL <- 24 * 2

litter.power.01 <- LitterMwPower(lit.d2, config$kSiteNamesLitter, config$kLitterDataCat1, config$kLitterDataCat2, 1, trim.length = window.size)
litter.power.02 <- LitterMwPower(lit.d2, config$kSiteNamesLitter, config$kLitterDataCat1, config$kLitterDataCat2, 2, trim.length = window.size)
litter.power.03 <- LitterMwPower(lit.d2, config$kSiteNamesLitter, config$kLitterDataCat1, config$kLitterDataCat2, 3, trim.length = window.size)
litter.power.04 <- LitterMwPower(lit.d2, config$kSiteNamesLitter, config$kLitterDataCat1, config$kLitterDataCat2, 4, trim.length = window.size)
litter.power.05 <- LitterMwPower(lit.d2, config$kSiteNamesLitter, config$kLitterDataCat1, config$kLitterDataCat2, 5, trim.length = window.size.NAL)
litter.power.06 <- LitterMwPower(lit.d2, config$kSiteNamesLitter, config$kLitterDataCat1, config$kLitterDataCat2, 6, trim.length = window.size)
litter.power.07 <- LitterMwPower(lit.d2, config$kSiteNamesLitter, config$kLitterDataCat1, config$kLitterDataCat2, 7, trim.length = window.size)
litter.power.08 <- LitterMwPower(lit.d2, config$kSiteNamesLitter, config$kLitterDataCat1, config$kLitterDataCat2, 8, trim.length = window.size)
litter.power.09 <- LitterMwPower(lit.d2, config$kSiteNamesLitter, config$kLitterDataCat1, config$kLitterDataCat2, 9, trim.length = window.size)

#### check results
plot(litter.power.01[[1]]$spec_norm, type = "l")

plot(litter.power.03[[1]]$spec_norm, type = "l")
plot(litter.power.04[[1]]$spec_norm, type = "l")
plot(litter.power.05[[1]]$spec_norm, type = "l")
plot(litter.power.07[[1]]$spec_norm, type = "l")
plot(litter.power.08[[1]]$spec_norm, type = "l")

plot(litter.power.02[[1]]$spec_norm, type = "l")
plot(litter.power.06[[1]]$spec_norm, type = "l")
plot(litter.power.09[[1]]$spec_norm, type = "l")


############### Climate analysis ##################
#### Calculate Moving-Window-Seasonality
## extract the power of annual seasonality
source('functions/ClimateDataCompiler_Window_v1.R')

# specify window size
window.size.cli <- 24 * config$kWindowYears

climate.power.01 <- ClimateMwPower(climate.correct2, config$kSiteNamesClimate, 1, trim.length = window.size.cli, allow.NAs = 6, selected.climate = config$kSelectedClimateCat)
climate.power.02.2nd <- ClimateMwPower(climate.correct2, config$kSiteNamesClimate, 2, trim.length = window.size.cli, allow.NAs = 6, selected.climate = config$kSelectedClimateCat, ts.length = "second_longest")
climate.power.03 <- ClimateMwPower(climate.correct2, config$kSiteNamesClimate, 3, trim.length = window.size.cli, allow.NAs = 6, selected.climate = config$kSelectedClimateCat)
climate.power.03.2nd <- ClimateMwPower(climate.correct2, config$kSiteNamesClimate, 3, trim.length = window.size.cli, allow.NAs = 6, selected.climate = config$kSelectedClimateCat, ts.length = "second_longest")
climate.power.04 <- ClimateMwPower(climate.correct2, config$kSiteNamesClimate, 4, trim.length = window.size.cli, allow.NAs = 6, selected.climate = config$kSelectedClimateCat)

#### check results
plot(climate.power.01[[1]]$spec_norm, type = "l")
plot(climate.power.02.2nd[[1]]$spec_norm, type = "l")
plot(climate.power.03[[1]]$spec_norm, type = "l")
plot(climate.power.03.2nd[[1]]$spec_norm, type = "l")
plot(climate.power.04[[1]]$spec_norm, type = "l")


config$End()

config.04 <- config
rm("config")

#### save image
save.image(file = config.04$out.04.rdata.path)

#### save session info
writeLines(capture.output(sessionInfo()),
           sprintf("00_0_SessionInfo/04_SessionInfo_PowerWCalc_%s.txt", substr(Sys.time(), 1, 10)))
