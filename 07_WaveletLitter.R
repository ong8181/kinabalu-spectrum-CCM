####
#### Kitayama et al. "Temperature is a dominant driver of distinct annual
#### seasonality of leaf litter production of equatorial tropical rain forests"
#### No. 7: Wavelet analysis for litter
####

source("config.R")

# load environments
load(config$out.03.rdata.path)
out.07.dir <- "07_out"
dir.create(out.07.dir)

# Load libraries
library(WaveletComp); packageVersion("WaveletComp") # 1.1, 2020.5.25

# Load functions
source('functions/LitterDataCompiler_Window_v1.R')
source('functions/ClimateDataCompiler_v1.R')
source('functions/ClimateDataCompiler_Window_v1.R')

# Wavelet analysis for litter
## Extract time series
lit01 <- DataCompiler01(lit.d2, config$kSiteNamesLitter, config$kLitterDataCat1, "mean", 1, visualize = T)
lit02 <- DataCompiler01(lit.d2, config$kSiteNamesLitter, config$kLitterDataCat1, "mean", 2, visualize = T)
lit03 <- DataCompiler01(lit.d2, config$kSiteNamesLitter, config$kLitterDataCat1, "mean", 3, visualize = T)
lit04 <- DataCompiler01(lit.d2, config$kSiteNamesLitter, config$kLitterDataCat1, "mean", 4, visualize = T)
lit05 <- DataCompiler01(lit.d2, config$kSiteNamesLitter, config$kLitterDataCat1, "mean", 5, visualize = T)
lit06 <- DataCompiler01(lit.d2, config$kSiteNamesLitter, config$kLitterDataCat1, "mean", 6, visualize = T)
lit07 <- DataCompiler01(lit.d2, config$kSiteNamesLitter, config$kLitterDataCat1, "mean", 7, visualize = T)
lit08 <- DataCompiler01(lit.d2, config$kSiteNamesLitter, config$kLitterDataCat1, "mean", 8, visualize = T)
lit09 <- DataCompiler01(lit.d2, config$kSiteNamesLitter, config$kLitterDataCat1, "mean", 9, visualize = T)
names(lit01) <- sprintf("%s_%s_mean", config$kSiteNamesLitter[1], config$kLitterDataCat1)
names(lit02) <- sprintf("%s_%s_mean", config$kSiteNamesLitter[2], config$kLitterDataCat1)
names(lit03) <- sprintf("%s_%s_mean", config$kSiteNamesLitter[3], config$kLitterDataCat1)
names(lit04) <- sprintf("%s_%s_mean", config$kSiteNamesLitter[4], config$kLitterDataCat1)
names(lit05) <- sprintf("%s_%s_mean", config$kSiteNamesLitter[5], config$kLitterDataCat1)
names(lit06) <- sprintf("%s_%s_mean", config$kSiteNamesLitter[6], config$kLitterDataCat1)
names(lit07) <- sprintf("%s_%s_mean", config$kSiteNamesLitter[7], config$kLitterDataCat1)
names(lit08) <- sprintf("%s_%s_mean", config$kSiteNamesLitter[8], config$kLitterDataCat1)
names(lit09) <- sprintf("%s_%s_mean", config$kSiteNamesLitter[9], config$kLitterDataCat1)

# Wavelet analysis
## Leaf litter
lit.wave01.leaf <- analyze.wavelet(data.frame(variable = lit01[[1]]), "variable", loess.span = 0,
                              dt = 1/24, dj = 1/120, lowerPeriod = 1/12, upperPeriod = 6, make.pval = FALSE)
lit.wave02.leaf <- analyze.wavelet(data.frame(variable = lit02[[1]]), "variable", loess.span = 0,
                                   dt = 1/24, dj = 1/120, lowerPeriod = 1/12, upperPeriod = 6, make.pval = FALSE)
lit.wave03.leaf <- analyze.wavelet(data.frame(variable = lit03[[1]]), "variable", loess.span = 0,
                                   dt = 1/24, dj = 1/120, lowerPeriod = 1/12, upperPeriod = 6, make.pval = FALSE)
lit.wave04.leaf <- analyze.wavelet(data.frame(variable = lit04[[1]]), "variable", loess.span = 0,
                                   dt = 1/24, dj = 1/120, lowerPeriod = 1/12, upperPeriod = 6, make.pval = FALSE)
lit.wave05.leaf <- analyze.wavelet(data.frame(variable = lit05[[1]]), "variable", loess.span = 0,
                                   dt = 1/24, dj = 1/120, lowerPeriod = 1/12, upperPeriod = 6, make.pval = FALSE)
lit.wave06.leaf <- analyze.wavelet(data.frame(variable = lit06[[1]]), "variable", loess.span = 0,
                                   dt = 1/24, dj = 1/120, lowerPeriod = 1/12, upperPeriod = 6, make.pval = FALSE)
lit.wave07.leaf <- analyze.wavelet(data.frame(variable = lit07[[1]]), "variable", loess.span = 0,
                                   dt = 1/24, dj = 1/120, lowerPeriod = 1/12, upperPeriod = 6, make.pval = FALSE)
lit.wave08.leaf <- analyze.wavelet(data.frame(variable = lit08[[1]]), "variable", loess.span = 0,
                                   dt = 1/24, dj = 1/120, lowerPeriod = 1/12, upperPeriod = 6, make.pval = FALSE)
lit.wave09.leaf <- analyze.wavelet(data.frame(variable = lit09[[1]]), "variable", loess.span = 0,
                                   dt = 1/24, dj = 1/120, lowerPeriod = 1/12, upperPeriod = 6, make.pval = FALSE)

# Flower litter
lit.wave01.flower <- analyze.wavelet(data.frame(variable = lit01[[2]]), "variable", loess.span = 0,
                                     dt = 1/24, dj = 1/120, lowerPeriod = 1/12, upperPeriod = 6, make.pval = FALSE)
lit.wave02.flower <- analyze.wavelet(data.frame(variable = lit02[[2]]), "variable", loess.span = 0,
                                     dt = 1/24, dj = 1/120, lowerPeriod = 1/12, upperPeriod = 6, make.pval = FALSE)
lit.wave03.flower <- analyze.wavelet(data.frame(variable = lit03[[2]]), "variable", loess.span = 0,
                                     dt = 1/24, dj = 1/120, lowerPeriod = 1/12, upperPeriod = 6, make.pval = FALSE)
lit.wave04.flower <- analyze.wavelet(data.frame(variable = lit04[[2]]), "variable", loess.span = 0,
                                     dt = 1/24, dj = 1/120, lowerPeriod = 1/12, upperPeriod = 6, make.pval = FALSE)
lit.wave05.flower <- analyze.wavelet(data.frame(variable = lit05[[2]]), "variable", loess.span = 0,
                                     dt = 1/24, dj = 1/120, lowerPeriod = 1/12, upperPeriod = 6, make.pval = FALSE)
lit.wave06.flower <- analyze.wavelet(data.frame(variable = lit06[[2]]), "variable", loess.span = 0,
                                     dt = 1/24, dj = 1/120, lowerPeriod = 1/12, upperPeriod = 6, make.pval = FALSE)
lit.wave07.flower <- analyze.wavelet(data.frame(variable = lit07[[2]]), "variable", loess.span = 0,
                                     dt = 1/24, dj = 1/120, lowerPeriod = 1/12, upperPeriod = 6, make.pval = FALSE)
lit.wave08.flower <- analyze.wavelet(data.frame(variable = lit08[[2]]), "variable", loess.span = 0,
                                     dt = 1/24, dj = 1/120, lowerPeriod = 1/12, upperPeriod = 6, make.pval = FALSE)
lit.wave09.flower <- analyze.wavelet(data.frame(variable = lit09[[2]]), "variable", loess.span = 0,
                                     dt = 1/24, dj = 1/120, lowerPeriod = 1/12, upperPeriod = 6, make.pval = FALSE)

# Save wavelet figures
## Leaf litter wavelet 
dir.create(sprintf("%s/JPEG", out.07.dir))
jpeg(sprintf("%s/JPEG/LeafWave700S.jpg", out.07.dir), width = 700, height = 500, res = 100)
wt.image(lit.wave01.leaf, n.levels = 250, legend.params = list(lab = "Wavelet power levels"),
         main = "700m Sediment, Leaf", timelab = "Time index", periodlab = "Period (year)")
dev.off()
png(sprintf("%s/JPEG/LeafWave1700S.jpg", out.07.dir), width = 700, height = 500, res = 100)
wt.image(lit.wave02.leaf, n.levels = 250, legend.params = list(lab = "Wavelet power levels"),
         main = "1700m Sediment, Leaf", timelab = "Time index", periodlab = "Period (year)")
dev.off()
png(sprintf("%s/JPEG/LeafWave2700S.jpg", out.07.dir), width = 700, height = 500, res = 100)
wt.image(lit.wave03.leaf, n.levels = 250, legend.params = list(lab = "Wavelet power levels"),
         main = "2700m Sediment, Leaf", timelab = "Time index", periodlab = "Period (year)")
dev.off()
png(sprintf("%s/JPEG/LeafWave3100S.jpg", out.07.dir), width = 700, height = 500, res = 100)
wt.image(lit.wave04.leaf, n.levels = 250, legend.params = list(lab = "Wavelet power levels"),
         main = "3100m Sediment, Leaf", timelab = "Time index", periodlab = "Period (year)")
dev.off()
png(sprintf("%s/JPEG/LeafWave700U.jpg", out.07.dir), width = 700, height = 500, res = 100)
wt.image(lit.wave05.leaf, n.levels = 250, legend.params = list(lab = "Wavelet power levels"),
         main = "700m Ultrabasic, Leaf", timelab = "Time index", periodlab = "Period (year)")
dev.off()
png(sprintf("%s/JPEG/LeafWave1700U.jpg", out.07.dir), width = 700, height = 500, res = 100)
wt.image(lit.wave06.leaf, n.levels = 250, legend.params = list(lab = "Wavelet power levels"),
         main = "1700m Ultrabasic, Leaf", timelab = "Time index", periodlab = "Period (year)")
dev.off()
png(sprintf("%s/JPEG/LeafWave2700U.jpg", out.07.dir), width = 700, height = 500, res = 100)
wt.image(lit.wave07.leaf, n.levels = 250, legend.params = list(lab = "Wavelet power levels"),
         main = "2700m Ultrabasic, Leaf", timelab = "Time index", periodlab = "Period (year)")
dev.off()
png(sprintf("%s/JPEG/LeafWave3100U.jpg", out.07.dir), width = 700, height = 500, res = 100)
wt.image(lit.wave08.leaf, n.levels = 250, legend.params = list(lab = "Wavelet power levels"),
         main = "3100m Ultrabasic, Leaf", timelab = "Time index", periodlab = "Period (year)")
dev.off()
png(sprintf("%s/JPEG/LeafWave1700Q.jpg", out.07.dir), width = 700, height = 500, res = 100)
wt.image(lit.wave09.leaf, n.levels = 250, legend.params = list(lab = "Wavelet power levels"),
         main = "1700m Quaternary, Leaf", timelab = "Time index", periodlab = "Period (year)")
dev.off()

## Flower litter wavelet 
png(sprintf("%s/JPEG/FlowerWave700S.jpg", out.07.dir), width = 700, height = 500, res = 100)
wt.image(lit.wave01.flower, n.levels = 250, legend.params = list(lab = "Wavelet power levels"),
         main = "700m Sediment, Flower", timelab = "Time index", periodlab = "Period (year)")
dev.off()
png(sprintf("%s/JPEG/FlowerWave1700S.jpg", out.07.dir), width = 700, height = 500, res = 100)
wt.image(lit.wave02.flower, n.levels = 250, legend.params = list(lab = "Wavelet power levels"),
         main = "1700m Sediment, Flower", timelab = "Time index", periodlab = "Period (year)")
dev.off()
png(sprintf("%s/JPEG/FlowerWave2700S.jpg", out.07.dir), width = 700, height = 500, res = 100)
wt.image(lit.wave03.flower, n.levels = 250, legend.params = list(lab = "Wavelet power levels"),
         main = "2700m Sediment, Flower", timelab = "Time index", periodlab = "Period (year)")
dev.off()
png(sprintf("%s/JPEG/FlowerWave3100S.jpg", out.07.dir), width = 700, height = 500, res = 100)
wt.image(lit.wave04.flower, n.levels = 250, legend.params = list(lab = "Wavelet power levels"),
         main = "3100m Sediment, Flower", timelab = "Time index", periodlab = "Period (year)")
dev.off()
png(sprintf("%s/JPEG/FlowerWave700U.jpg", out.07.dir), width = 700, height = 500, res = 100)
wt.image(lit.wave05.flower, n.levels = 250, legend.params = list(lab = "Wavelet power levels"),
         main = "700m Ultrabasic, Flower", timelab = "Time index", periodlab = "Period (year)")
dev.off()
png(sprintf("%s/JPEG/FlowerWave1700U.jpg", out.07.dir), width = 700, height = 500, res = 100)
wt.image(lit.wave06.flower, n.levels = 250, legend.params = list(lab = "Wavelet power levels"),
         main = "1700m Ultrabasic, Flower", timelab = "Time index", periodlab = "Period (year)")
dev.off()
png(sprintf("%s/JPEG/FlowerWave2700U.jpg", out.07.dir), width = 700, height = 500, res = 100)
wt.image(lit.wave07.flower, n.levels = 250, legend.params = list(lab = "Wavelet power levels"),
         main = "2700m Ultrabasic, Flower", timelab = "Time index", periodlab = "Period (year)")
dev.off()
png(sprintf("%s/JPEG/FlowerWave3100U.jpg", out.07.dir), width = 700, height = 500, res = 100)
wt.image(lit.wave08.flower, n.levels = 250, legend.params = list(lab = "Wavelet power levels"),
         main = "3100m Ultrabasic, Flower", timelab = "Time index", periodlab = "Period (year)")
dev.off()
png(sprintf("%s/JPEG/FlowerWave1700Q.jpg", out.07.dir), width = 700, height = 500, res = 100)
wt.image(lit.wave09.flower, n.levels = 250, legend.params = list(lab = "Wavelet power levels"),
         main = "1700m Quaternary, Flower", timelab = "Time index", periodlab = "Period (year)")
dev.off()

#### save image
save.image(file = sprintf("%s/07_WaveletLitterOut.RData", out.07.dir))

#### save session info
writeLines(capture.output(sessionInfo()),
           sprintf("00_0_SessionInfo/07_SessionInfo_%s.txt", substr(Sys.time(), 1, 10)))
