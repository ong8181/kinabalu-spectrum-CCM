####
#### Kitayama et al. "Temperature is a dominant driver of distinct annual
#### seasonality of leaf litter production of equatorial tropical rain forests"
#### No. 8: Wavelet analysis for climate
####

source("config.R")

# load environments
load("07_out/07_WaveletLitterOut.RData")
out.08.dir <- "08_out"
dir.create(out.08.dir)

# Load libraries
library(WaveletComp); packageVersion("WaveletComp") # 1.1, 2020.5.25

# Load functions
source('functions/LitterDataCompiler_Window_v1.R')
source('functions/ClimateDataCompiler_v1.R')
source('functions/ClimateDataCompiler_Window_v1.R')

# specify window size
window.size.cli <- 24 * config$kWindowYears
select.climate <- c("ATC", "ET0_potential_evp", "PAR", "Corrected_SD_kPa")

# Wavelet analysis for litter
## Extract time series
cli01 <- DataCompiler02(climate.correct2, config$kSiteNamesClimate, 1, allow.NAs = 6,
                        selected.climate.vars = select.climate, continuous.ts = "first_longest", visualize = T)
cli02 <- DataCompiler02(climate.correct2, config$kSiteNamesClimate, 2, allow.NAs = 6,
                        selected.climate.vars = select.climate, continuous.ts = "first_longest", visualize = T)
cli03 <- DataCompiler02(climate.correct2, config$kSiteNamesClimate, 3, allow.NAs = 6,
                        selected.climate.vars = select.climate, continuous.ts = "first_longest", visualize = T)
cli04 <- DataCompiler02(climate.correct2, config$kSiteNamesClimate, 4, allow.NAs = 6,
                        selected.climate.vars = select.climate, continuous.ts = "first_longest", visualize = T)
names(cli01) <- sprintf("%s_%s", config$kSiteNamesClimate[1], select.climate)
names(cli02) <- sprintf("%s_%s", config$kSiteNamesClimate[2], select.climate)
names(cli03) <- sprintf("%s_%s", config$kSiteNamesClimate[3], select.climate)
names(cli04) <- sprintf("%s_%s", config$kSiteNamesClimate[4], select.climate)

# Wavelet analysis
## Air temperature
cli.wave01.temp <- analyze.wavelet(data.frame(variable = cli01[[1]]), "variable", loess.span = 0,
                                   dt = 1/24, dj = 1/120, lowerPeriod = 1/12, upperPeriod = 6, make.pval = FALSE)
cli.wave02.temp <- analyze.wavelet(data.frame(variable = cli02[[1]]), "variable", loess.span = 0,
                                   dt = 1/24, dj = 1/120, lowerPeriod = 1/12, upperPeriod = 6, make.pval = FALSE)
cli.wave03.temp <- analyze.wavelet(data.frame(variable = cli03[[1]]), "variable", loess.span = 0,
                                   dt = 1/24, dj = 1/120, lowerPeriod = 1/12, upperPeriod = 6, make.pval = FALSE)
cli.wave04.temp <- analyze.wavelet(data.frame(variable = cli04[[1]]), "variable", loess.span = 0,
                                   dt = 1/24, dj = 1/120, lowerPeriod = 1/12, upperPeriod = 6, make.pval = FALSE)

## Potential evapotranspiration
cli.wave01.evp <- analyze.wavelet(data.frame(variable = cli01[[2]]), "variable", loess.span = 0,
                                     dt = 1/24, dj = 1/120, lowerPeriod = 1/12, upperPeriod = 6, make.pval = FALSE)
cli.wave02.evp <- analyze.wavelet(data.frame(variable = cli02[[2]]), "variable", loess.span = 0,
                                     dt = 1/24, dj = 1/120, lowerPeriod = 1/12, upperPeriod = 6, make.pval = FALSE)
cli.wave03.evp <- analyze.wavelet(data.frame(variable = cli03[[2]]), "variable", loess.span = 0,
                                     dt = 1/24, dj = 1/120, lowerPeriod = 1/12, upperPeriod = 6, make.pval = FALSE)
cli.wave04.evp <- analyze.wavelet(data.frame(variable = cli04[[2]]), "variable", loess.span = 0,
                                     dt = 1/24, dj = 1/120, lowerPeriod = 1/12, upperPeriod = 6, make.pval = FALSE)

# Save wavelet figures
## Air temperature wavelet
dir.create(sprintf("%s/JPEG", out.08.dir))
jpeg(sprintf("%s/JPEG/TempWave700.jpg", out.08.dir), width = 700, height = 500, res = 100)
wt.image(cli.wave01.temp, n.levels = 250, legend.params = list(lab = "Wavelet power levels"),
         main = "700m, Temperature", timelab = "Time index", periodlab = "Period (year)")
dev.off()
jpeg(sprintf("%s/JPEG/TempWave1700.jpg", out.08.dir), width = 700, height = 500, res = 100)
wt.image(cli.wave02.temp, n.levels = 250, legend.params = list(lab = "Wavelet power levels"),
         main = "1700m, Temperature", timelab = "Time index", periodlab = "Period (year)")
dev.off()
jpeg(sprintf("%s/JPEG/TempWave2700.jpg", out.08.dir), width = 700, height = 500, res = 100)
wt.image(cli.wave03.temp, n.levels = 250, legend.params = list(lab = "Wavelet power levels"),
         main = "2700m, Temperature", timelab = "Time index", periodlab = "Period (year)")
dev.off()
jpeg(sprintf("%s/JPEG/TempWave3100.jpg", out.08.dir), width = 700, height = 500, res = 100)
wt.image(cli.wave04.temp, n.levels = 250, legend.params = list(lab = "Wavelet power levels"),
         main = "3100m, Temperature", timelab = "Time index", periodlab = "Period (year)")
dev.off()


## Flower litter wavelet 
jpeg(sprintf("%s/JPEG/EvpWave700.jpg", out.08.dir), width = 700, height = 500, res = 100)
wt.image(cli.wave01.evp, n.levels = 250, legend.params = list(lab = "Wavelet power levels"),
         main = "700m, Potential ET", timelab = "Time index", periodlab = "Period (year)")
dev.off()
jpeg(sprintf("%s/JPEG/EvpWave1700.jpg", out.08.dir), width = 700, height = 500, res = 100)
wt.image(cli.wave02.evp, n.levels = 250, legend.params = list(lab = "Wavelet power levels"),
         main = "1700m, Potential ET", timelab = "Time index", periodlab = "Period (year)")
dev.off()
jpeg(sprintf("%s/JPEG/EvpWave2700.jpg", out.08.dir), width = 700, height = 500, res = 100)
wt.image(cli.wave03.evp, n.levels = 250, legend.params = list(lab = "Wavelet power levels"),
         main = "2700m, Potential ET", timelab = "Time index", periodlab = "Period (year)")
dev.off()
jpeg(sprintf("%s/JPEG/EvpWave3100.jpg", out.08.dir), width = 700, height = 500, res = 100)
wt.image(cli.wave04.evp, n.levels = 250, legend.params = list(lab = "Wavelet power levels"),
         main = "3100m, Potential ET", timelab = "Time index", periodlab = "Period (year)")
dev.off()

#### save image
save.image(file = sprintf("%s/08_WaveletClimateOut.RData", out.08.dir))

#### save session info
writeLines(capture.output(sessionInfo()),
           sprintf("00_0_SessionInfo/08_SessionInfo_%s.txt", substr(Sys.time(), 1, 10)))
