####
#### Kitayama et al. "Temperature is a dominant driver of distinct annual
#### seasonality of leaf litter production of equatorial tropical rain forests"
#### No.5: CCM between climate annual power and litter annual power
####

source("config.R")

#### load environments
load(config$out.04.rdata.path)
dir.create(config$out.05.dir, showWarnings = FALSE)

##########################
#### Upload libraries ####
##########################

library(plyr)
library(RColorBrewer)
library(circular)
library(fields)
library(DescTools)
library(truncnorm)
library(timeSeries)
library(zoo)
library(deSolve)
library(pforeach)
library(psych)
library(rEDM)

#### load functions
source('functions/Helper_functions_H1.R')
source('functions/Helper_functions_H2.R')
source('functions/Parallel_Surrogate95CI.R')


#### Compile time series
## just adjust litter (leaf) and climate time series
## lower montane sites (litter_site = 2 phq, 6 babi, 9 ular, climate_site = 3 phq)
## lowland forest sites (litter_site = 1 por, 5 nal, climate = 4 por)
## lowland forest sites (litter_site = 3 rtm, 7 car, climate = 1 car)
## lowland forest sites (litter_site = 4 pak, 8 hel, climate = 2 lab)
#[1] "ATC"               "relative_humidity" "Act_VP_kPa"
#[4] "SD_kPa"            "Corrected_SD_kPa"  "PAR"

#### define climate and litter combinations
all.site <- config$kClimateLitterCombAllSite

SiteSelectFunc <- function(region = c("lowland", "lower_montane1", "lower_montane2", "higher_montane", "laban_rata")) {
    region <- match.arg(region)
    # region <- all.site
    if (region == "lowland") {
      litter.data.set <- list(litter.power.01) # litter.power.05 = NAL = inappropriate to analyze
      climate.data.set <- climate.power.04
      site.name.sub <- c("POR")
    } else if (region == "lower_montane1") {
      litter.data.set <- list(litter.power.02, litter.power.09)
      climate.data.set <- climate.power.03
      site.name.sub <- c("PHQ", "ULA")
    } else if (region == "lower_montane2") {
      litter.data.set <- list(litter.power.06)
      climate.data.set <- climate.power.03.2nd
      site.name.sub <- c("BAB")
    } else if (region == "higher_montane") {
      litter.data.set <- list(litter.power.03, litter.power.07)
      climate.data.set <- climate.power.01
      site.name.sub <- c("RTM", "CAR")
    } else if (region == "laban_rata") {
      litter.data.set <- list(litter.power.04, litter.power.08)
      climate.data.set <- climate.power.02.2nd # NO overlap between climate and litter data
      site.name.sub <- c("PAK", "HEL")
    }
    return(list(litter.data.set, climate.data.set, site.name.sub))
}


#### set paramters for (lag) CCM ######
num.iter <- config$kCCMFFTNumIter  # the number of surrogate
Erange <- config$kCCMFFTErange
ccm.tp <- config$kCCMFFTTp
########################################

seasonality.ccm.output <- data.frame()

#climate.for.ccm <- c("ATC")
climate.for.ccm <- config$kCCMFFTSelectClimateCat
litter.for.ccm <- config$kCCMFFTSelectLitterCat

all.ETs <- data.frame(site = NaN, xmap_from = NaN, xmap_to = NaN, xmap_from_E = NaN, xmap_from_theta = NaN)
climate.index <- match(climate.for.ccm, config$kSelectedClimateCat)
total.cycle <- sum(sapply(all.site, function(site.i) length(SiteSelectFunc(site.i)[[1]]))) * length(ccm.tp) * length(litter.for.ccm) * length(climate.index)

####### Main loop #############
current.cycle <- 1
for (site.i in all.site) {
  ## extract site data
  litter.climate.data.set <- SiteSelectFunc(site.i)
  litter.data.site <- litter.climate.data.set[[1]]
  climate.data.site <- litter.climate.data.set[[2]]
  site.name.sub <- litter.climate.data.set[[3]]
  
  ## use normalized spectrum (if want to use rawa spectrum values, use spec.col=2)
  spec.col <- 3
  
  for (site.i2 in 1:length(litter.data.site)) {
    for (litter.i in 1:length(litter.for.ccm)) {
      for (climate.i in climate.index) {
        t.start <- proc.time()
        
        litter.data <- litter.data.site[[site.i2]][[litter.i]]
        climate.data <- climate.data.site[[climate.i]]
        
        ## match date
        index.date.l <- litter.data$index_date
        index.date.c <- climate.data$index_date
        match.lc <- na.omit(match(index.date.l, index.date.c))
        match.cl <- na.omit(match(index.date.c, index.date.l))
        ts1.df <- litter.data[match.cl, ]
        ts2.df <- climate.data[match.lc, ]

        #### take first-differences
        ts1 <- as.numeric(scale(ts1.df[, spec.col]))
        ts1.embed <- embed(ts1.df[, spec.col], dimension = 2)
        ts1.dif <- as.numeric(scale(ts1.embed[, 1] - ts1.embed[, 2]))
        
        ts2 <- as.numeric(scale(ts2.df[, spec.col]))
        ts2.embed <- embed(ts2.df[, spec.col], dimension = 2)
        ts2.dif <- as.numeric(scale(ts2.embed[, 1] - ts2.embed[, 2]))
        
        simp.res1 <- rEDM::simplex(ts1.dif, E = Erange, silent = T)
        E1 <- simp.res1[which.min(simp.res1$mae), 'E']
        simp.res2 <- rEDM::simplex(ts2.dif, E = Erange, silent = T)
        E2 <- simp.res2[which.min(simp.res2$mae), 'E']
        
        smap.res <- s_map(ts1.dif, E = E1, theta = seq(0, 10, by = 0.1), silent = T)
        T1 <- smap.res[which.min(smap.res$mae), 'theta']
        ETs.tmp <- data.frame(site = site.name.sub[site.i2],
                              xmap_from = litter.for.ccm[litter.i],
                              xmap_to = climate.for.ccm[climate.i],
                              xmap_from_E = E1,
                              xmap_from_theta = T1)
        all.ETs <- rbind(all.ETs, ETs.tmp)
        
        #### set CCM paramters and generate twin surrogate
        m <- length(ts1.dif)
        block.ccm <- cbind(ts1.dif, ts2.dif)
        lib.set <- c(E1 + 1, max(Erange) + 2, m)
        # generate surrogate
        t.sur.start <- proc.time()
        sur.ts1 <- as.data.frame(make_surrogate_data(ts1.dif, method = "ebisuzaki", num_surr = num.iter))
        
        surrogate.check <- all(!is.na(sur.ts1))
        t.sur.end <- proc.time() - t.sur.start
        sur.message <- sprintf("--- surrogate generation finished (quality=%s): %s sec\n", surrogate.check, round(t.sur.end[3]))
        cat(sur.message)
        
        #### do CCM
        #### different tp
        for (ccm.tp.i in ccm.tp) {
          # ccm.tp.i <- ccm_tp[1]
          
          #### start message
          cycle.start.message <- sprintf( "--- [%s/%s]  %s cross-map %s (tp=%s) in %s started...\n",
              current.cycle, total.cycle, litter.for.ccm[litter.i], climate.for.ccm[climate.i], ccm.tp.i, site.name.sub[site.i2])
          cat(cycle.start.message)
          
          #### do original CCM
          ccm.12.raw	<- ccm(block.ccm, E = E1, lib_sizes = lib.set,
              tp = ccm.tp.i, silent = T, RNGseed = config$kRndSeed)
          ccm.12 <- ccm_means(ccm.12.raw, na.rm = T)
          
          #### do surrogate CCM
          if (surrogate.check) {
            sur.ci.res	<- ParSurciV3(ts1.dif, ts2.dif, sur.ts1,
              lib.parms = lib.set, E.range = Erange, tp = ccm.tp.i,
              calc.original.ccm.p = T, original.ccm.res = ccm.12, cores = config$cores, seed = config$kRndSeed)

            #### save the CCM plot
            fig.ymin <- (min(ts1.dif, ts2.dif, na.rm = T) - 2)
            fig.ymax <- (max(ts1.dif, ts2.dif, na.rm = T) + 2)
            min.row <- min(which(!is.na(ccm.12$rho) & is.finite(ccm.12$rho)))
            max.row <- max(which(!is.na(ccm.12$rho) & is.finite(ccm.12$rho)))
            t.rho.oriccm <- ccm.12[max.row, 'rho']
            d.rho.oriccm <- t.rho.oriccm - ccm.12[min.row, 'rho']
            
            dir.create(config$Out05FFTFigsDir(site.i), showWarnings = FALSE, recursive = TRUE)
            plot.title <- config$Out05FFTPdfPath(site.i, site.name.sub[site.i2], litter.for.ccm[litter.i], climate.for.ccm[climate.i], ccm.tp.i)
            pdf(plot.title, width = 9, height = 5)
            op <- par(mfrow = c(1, 2))
            plot(ts1.dif + 1, type = "l", col = "red3", lwd = 0.6, ylab = "Relative value (shifted)",
              xlab = "Time index", main = "Litter (red) or Climate (blue)", ylim = range(fig.ymin, fig.ymax))
            lines(ts2.dif - 1, col = "royalblue", lwd = 0.6)
            plot(c(sur.ci.res$conv_quantile['95%'], sur.ci.res$ter_rho_quantile['95%']) ~ as.factor(c('Terminal rho', 'Delta rho')),
              pch = 1, ylab = "Forecasting skill (and its improvement)", xlab = NA, ylim = c(0, 1))
            points(c(1, 2), c(d.rho.oriccm, t.rho.oriccm), pch = 8, cex = 2)
            par(op)
            dev.off()
            
            #### Do Z-test #####
            num.pred <- ccm.12$num_pred[1]
            z.result <- r.test(num.pred, ccm.12$rho[1], ccm.12$rho[nrow(ccm.12)])

            #### combine CCM results
            meta.data <- data.frame(
                litter_site = site.name.sub[site.i2],
                climate_site = site.i,
                litter_data = litter.for.ccm[litter.i],
                climate_data = climate.for.ccm[climate.i],
                min_lib = min(lib.set),
                init_rho = ccm.12$rho[min.row],
                z_value = z.result$z,
                z_p_value = z.result$p)
            ccm.res.tmp <- cbind(
              meta.data,
              ccm.12[max.row, ],
              sur.ci.res$ter_rho_quantile['50%'],
              sur.ci.res$ter_rho_quantile['90%'],
              sur.ci.res$ter_rho_quantile['95%'],
              sur.ci.res$ter_rho_quantile['97.5%'],
              sur.ci.res$conv_quantile['50%'],
              sur.ci.res$conv_quantile['90%'],
              sur.ci.res$conv_quantile['95%'],
              sur.ci.res$conv_quantile['97.5%'],
              sur.ci.res$conv_ter90mid,
              sur.ci.res$conv_ter95mid,
              sur.ci.res$conv_ter975mid,
              sur.ci.res$oriccm_d_rho_p,
              sur.ci.res$oriccm_t_rho_p,
              sur.ci.res$oriccm_comb_p,
              sur.ci.res$ident_lib,
              d.rho.oriccm)
            nnt <- ncol(ccm.res.tmp)
            colnames(ccm.res.tmp)[(nnt - 15):nnt] <- c("ter_rho50", "ter_rho90", "ter_rho95", "ter_rho975",
              "conv_50", "conv_90", "conv_95", "conv_975",
              "conv_ter90mid", "conv_ter95mid", "conv_ter975mid",
              "oriccm_d_rho_p", "oriccm_t_rho_p", "oriccm_comb_p", "identical_lib", "d_rho")
          } else{
            #### Do Z-test #####
            num.pred <- ccm.12$num_pred[1]
            z.result <- r.test(num.pred, ccm.12$rho[1], ccm.12$rho[nrow(ccm.12)])

            #### combine CCM results
            meta.data <-
              data.frame(
                litter_site = site.name.sub[site.i2],
                climate_site = site.i,
                litter_data = litter.for.ccm[litter.i],
                climate_data = climate.for.ccm[climate.i],
                min_lib = min(lib.set),
                init_rho = ccm.12$rho[min.row],
                z_value = z.result$z,
                z_p_value = z.result$p)
            ccm.res.tmp <- cbind(
              meta.data, ccm.12[max.row, ],
              NaN, NaN, NaN, NaN, NaN,
              NaN, NaN, NaN, NaN, NaN,
              NaN, NaN, NaN, NaN, NaN, NaN)
            nnt <- ncol(ccm.res.tmp)
            colnames(ccm.res.tmp)[(nnt - 15):nnt] <- c("ter_rho50", "ter_rho90", "ter_rho95", "ter_rho975",
                                                       "conv_50", "conv_90", "conv_95", "conv_975",
                                                       "conv_ter90mid", "conv_ter95mid", "conv_ter975mid",
                                                       "oriccm_d_rho_p", "oriccm_t_rho_p", "oriccm_comb_p", "identical_lib", "d_rho")
            ccm.res.tmp$comment <- "surrogate_not_generated"
          }
          
          #### combine all data
          seasonality.ccm.output <-
            rbind(seasonality.ccm.output, ccm.res.tmp)
          
          # end message
          t.end <- proc.time() - t.start
          
          cat(sur.message)
          cycle.finish <-
            sprintf(
              "%s cross-map %s in %s finished: %s sec\n\n",
              litter.for.ccm[litter.i],
              climate.for.ccm[climate.i],
              site.name.sub[site.i2],
              round(t.end[3])
            )
          cat(cycle.finish)
          current.cycle <- current.cycle + 1
        }
      }
    }
  }
}

config$End()

config.05.fft <- config
rm("config")

#### save image
save.image(file = config.05.fft$out.05.fft.rdata.path)

#### save session info
writeLines(capture.output(sessionInfo()),
           sprintf("00_0_SessionInfo/05_SessionInfo_CCMFFT_%s.txt", substr(Sys.time(), 1, 10)))
