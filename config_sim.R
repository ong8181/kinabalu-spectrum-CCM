


config <- local(function() {
  start.time <- proc.time()
  end.time <- NULL
  run.time <- NULL
  
  kCores <- NULL
  kRndSeed <- NULL
  
  kBaseDir <- '.'
  kDataDir <- './data'
  
  kLitDataFile <- 'Kinabalu_litter_original_data_std.csv'
  kCliPorAllFile <- 'Poring_climate_daily_std.csv'
  kCliPhqAllFile <- 'PHQ_climate_daily_std.csv'
  kClilabAllFile <- 'LabanRata_climate_daily_std.csv'
  kCliCarAllFile <- 'CarsonCamp_climate_daily_std.csv'
  
  kOut001Dir <- '00_1_DataToBiweekly'
  kOut002Dir <- '00_2_DataCorrectGAM'
  kOut01Dir <- '01_out'
  kOut02Dir <- '02_out'
  kOut03Dir <- '03_out'
  kOut04Dir <- '04_out'
  kOut05Dir <- '05_out'
  kOut06Dir <- '06_out'
  kOut07Dir <- 'S01_out'
  kOut08Dir <- 'S02_out'
  kOut09Dir <- 'S03_out'
  
  kOut001PointRDataFile <- 'biweekly_converted_climate.RData'
  kOut001AverageRDataFile <- 'biweekly_converted_climate_avr.RData'
  kOut002RDataFile <- 'kinabalu_corrected_climate.RData'
  kOut01RDataFile <- '01_DataCompile_ForFourier.RData'
  kOut02RDataFile <- '02_LitterFourierRes.RData'
  kOut03RDataFile <- '03_ClimateFourierRes_NA6.RData'
  kOut04RDataFile <- '04_WindowPowerCalc_3yearW.RData'
  kOut05FFTRDataFile <- '05_LagCCM_FFT.RData'
  kOut06RDataFile <- '06_Fourier_CCMcompile_FFT.RData'
  kOut07RDataFile <- 'S01_Fourier_SimulationFunc.RData'
  kOut08RDataFile <- 'S02_Fourier_SimulationTpTest.RData'
  kOut09RDataFile <- 'S03_Fourier_SimulationOriCCM.RData'
  
  kOut002SubDir <- 'kinabalu_corrected_TS'
  kOut002SubSiteDir <- 'climate_%s_correct'
  kOut002PdfFile <- '%s_%s_correct.pdf'
  kOut02FigsDir <- '02_figs'
  kOut02PdfFile <- '%s_Periodogram.pdf'
  kOut03FigsDir <- '03_figs_NAs6'
  kOut03PdfFile <- '%s_Periodogram.pdf'
  kOut05FFTFigsDir <- '05_KinabaluSeasonality_CCM_FFT'
  kOut05FigsSubDir <- '%s_CCMres'
  kOut05PdfFile <- '%s_%s_xmap_%s_tp%s.pdf'
  kOut08SimCondFile <- 'S02_SimConditions.txt'
  kOut08SimResFile <- 'S02_Fourier_SimRes_Seasonality_%s.csv'
  kOut09SimCondFile <- 'S03_SimConditions_OriCCM.txt'
  kOut09SimResFile <- 'S03_Fourier_SimResOriCCM_%s.csv'
  
  kSiteNamesClimate <- c("car", "lab", "phq", "por")
  kSiteNamesLitter <- c("POR", "PHQ", "RTM", "PAK", "NAL", "BAB", "CAR", "HEL", "ULA")
  kLitterDataCat1 <- c("leaf", "flower", "branch", "epi", "bamboo", "dust")
  kLitterDataCat2 <- c("mean", "sd")
  kSelectedClimateCat <- c("ATC", "relative_humidity",
                           "Act_VP_kPa", "SD_kPa",
                           "Corrected_SD_kPa",
                           "PAR", "wind", "rain")
  kWindowYears <- 3
  kErange <- 2:24
  
  #### define climate and litter combinations
  kClimateLitterCombAllSite <- c("lowland",
                                 "lower_montane1",
                                 "lower_montane2",
                                 "higher_montane",
                                 "laban_rata")
  
  #### 05_Fourier_CCM_FFT: set paramters for (lag) CCM ######
  kCCMFFTNumIter <- 1000  # the number of surrogate
  kCCMFFTErange <- kErange
  kCCMFFTTp <- c(-24:6)
  kCCMFFTSelectClimateCat <- kSelectedClimateCat
  kCCMFFTSelectLitterCat <- c("leaf")
  
  #### 08_Fourier_Simulation ######
  k08Erange <- kErange
  k08ParamSetName <- 'main_fCCM'
  
  #### 09_Fourier_SimOriCCM ######
  k09Erange <- kErange
  k09ParamSetName <- 'main_normalCCM'
  
  #####################
  
  base.dir <- normalizePath(kBaseDir)
  data.dir <- normalizePath(kDataDir)
  
  lit.d.path <- file.path(data.dir, kLitDataFile)
  cli.por.all.path <- file.path(data.dir, kCliPorAllFile)
  cli.phq.all.path <- file.path(data.dir, kCliPhqAllFile)
  cli.lab.all.path <- file.path(data.dir, kClilabAllFile)
  cli.car.all.path <- file.path(data.dir, kCliCarAllFile)
  
  out.001.point.rdata.path <- file.path(base.dir, kOut001Dir, kOut001PointRDataFile)
  out.001.ave.rdata.path <- file.path(base.dir, kOut001Dir, kOut001AverageRDataFile)
  out.002.rdata.path <- file.path(base.dir, kOut002Dir, kOut002RDataFile)
  out.002.site.dir <- list()
  out.002.pdf.path <- list()
  for (name in kSiteNamesClimate) {
    out.002.site.dir[[name]] <- file.path(base.dir,
                                          kOut002Dir,
                                          kOut002SubDir,
                                          sprintf(kOut002SubSiteDir, name))
    out.002.pdf.path[[name]] <- file.path(out.002.site.dir[name], sprintf(kOut002PdfFile, '%s', name))
  }
  Out002PdfPath <- function(site.name, data.name) {
    sprintf(out.002.pdf.path[[site.name]], data.name)
  }
  out.01.dir <- file.path(base.dir, kOut01Dir)
  out.01.rdata.path <- file.path(out.01.dir, kOut01RDataFile)
  out.02.dir <- file.path(base.dir, kOut02Dir)
  out.02.rdata.path <- file.path(out.02.dir, kOut02RDataFile)
  out.02.fig.dir <- file.path(out.02.dir, kOut02FigsDir)
  Out02PdfPath <- function(site.name) {
    sprintf(file.path(out.02.fig.dir, kOut02PdfFile), site.name)
  }
  out.03.dir <- file.path(base.dir, kOut03Dir)
  out.03.rdata.path <- file.path(out.03.dir, kOut03RDataFile)
  out.03.fig.dir <- file.path(out.03.dir, kOut03FigsDir)
  Out03PdfPath <- function(site.name) {
    sprintf(file.path(out.03.fig.dir, kOut03PdfFile), site.name)
  }
  out.04.dir <- file.path(base.dir, kOut04Dir)
  out.04.rdata.path <- file.path(out.04.dir, kOut04RDataFile)
  out.05.dir <- file.path(base.dir, kOut05Dir)
  out.05.fft.rdata.path <- file.path(out.05.dir, kOut05FFTRDataFile)
  Out05FFTFigsDir <- function(site.name) {
    sprintf(file.path(out.05.dir, kOut05FFTFigsDir, kOut05FigsSubDir),
            site.name)
  }
  Out05FFTPdfPath <- function(site.name, site.name.sub, litter, climate, ccm.tp) {
      sprintf(file.path(Out05FFTFigsDir(site.name), kOut05PdfFile), site.name.sub, litter, climate, ccm.tp)
    }
  out.06.dir <- file.path(base.dir, kOut06Dir)
  out.06.rdata.path <- file.path(out.06.dir, kOut06RDataFile)
  CausalityCriteria <- function(ccm.out) {
    cond1 <- ccm.out$oriccm_comb_p <= 0.05
    #cond1 <- ccm.out$d_rho > ccm.out$d_rho_upper
    #cond1 <- ccm.out$d_rho > 0.1
    #cond1 <- ccm.out$z_p_value < 0.05
    #cond2 <- ccm.out$rho > ccm.out$upper95
    cond3 <- ccm.out$tp < 1
    list(cond = cond1 & cond3, comment = "combined p-value < 0.05") # 2018.6.11
  }
  out.07.dir <- file.path(base.dir, kOut07Dir)
  out.07.rdata.path <- file.path(out.07.dir, kOut07RDataFile)
  out.08.dir <- file.path(base.dir, kOut08Dir)
  out.08.rdata.path <- file.path(out.08.dir, kOut08RDataFile)
  out.08.sim.conditions.path <- file.path(out.08.dir, kOut08SimCondFile)
  Out08SimResPath <- function(i) {
    sprintf(file.path(out.08.dir, kOut08SimResFile), i)
  }
  out.09.dir <- file.path(base.dir, kOut09Dir)
  out.09.rdata.path <- file.path(out.09.dir, kOut09RDataFile)
  out.09.sim.conditions.path <- file.path(out.09.dir, kOut09SimCondFile)
  Out09SimResPath <- function(i) {
    sprintf(file.path(out.09.dir, kOut09SimResFile), i)
  }
  
  End <- function() {
    end.time <<- proc.time()
    run.time <<- end.time - start.time
  }
  
  .max.cores <- parallel::detectCores()
  if (is.null(kCores)) {
    cores <- .max.cores
  } else if (kCores > 0 && kCores < 1) {
    cores <- ceiling(.max.cores * kCores)
  } else {
    cores <- kCores
  }
  if (!is.null(kRndSeed)) {
    set.seed(kRndSeed)
  }
  environment()
})()
