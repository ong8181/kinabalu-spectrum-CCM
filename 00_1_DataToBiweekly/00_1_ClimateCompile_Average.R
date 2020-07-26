####
#### Kitayama et al. "Temperature is a dominant driver of distinct annual
#### seasonality of leaf litter production of equatorial tropical rain forests"
#### No.00.1: Daily data to biweekly data
####

dir.create("../00_0_SessionInfo")
source(file.path("..", "config.R"), chdir = TRUE)

### load dataset
lit.d <- read.csv(config$lit.d.path)
cli.por.all <- read.csv(config$cli.por.all.path)
cli.phq.all <- read.csv(config$cli.phq.all.path)
cli.lab.all <- read.csv(config$cli.lab.all.path)
cli.car.all <- read.csv(config$cli.car.all.path)

### climate data compiling
### daily data to biweekly data
litter.mid.date <- as.Date(lit.d$mid_date)
litter.ini.date <- as.Date(lit.d$census_from)
litter.end.date <- as.Date(lit.d$census_to) - 1

ToBiweeklyAvr <- function(convert.data) {
  cvtd <- convert.data
  extracted <- data.frame
  ini_tmp0 <- as.numeric(row.names(cvtd[as.Date(cvtd$date) == litter.ini.date[1], ]))
  end_tmp0 <- as.numeric(row.names(cvtd[as.Date(cvtd$date) == litter.end.date[1], ]))
  avr_tmp0 <- cvtd[ini_tmp0:end_tmp0, 6:ncol(cvtd)]
  extracted <- cvtd[as.Date(cvtd$date) == litter.mid.date[1], ]
  extracted[, 6:ncol(cvtd)] <- apply(avr_tmp0, 2, function(x) mean(as.numeric(x), na.rm = T))
  
  for (i in 2:length(litter.mid.date)) {
    if (!any(as.Date(cvtd$date) == litter.mid.date[i])) {
      extracted <- rbind(extracted, rep(NA, ncol(cvtd)))
    } else{
      extracted0 <- data.frame
      ini_tmp <- as.numeric(row.names(cvtd[as.Date(cvtd$date) == litter.ini.date[i], ]))
      end_tmp <- as.numeric(row.names(cvtd[as.Date(cvtd$date) == litter.end.date[i], ]))
      avr_tmp <- cvtd[ini_tmp:end_tmp, 6:ncol(cvtd)]
      extracted0 <- cvtd[as.Date(cvtd$date) == litter.mid.date[i], ]
      extracted0[, 6:ncol(cvtd)] <- apply(avr_tmp, 2, function(x) mean(as.numeric(x), na.rm = T))
      extracted <- rbind(extracted, extracted0)
    }
  }
  return(extracted)
}

cli.por.biw.avr <- ToBiweeklyAvr(cli.por.all)
cli.phq.biw.avr <- ToBiweeklyAvr(cli.phq.all)
cli.lab.biw.avr <- ToBiweeklyAvr(cli.lab.all)
cli.car.biw.avr <- ToBiweeklyAvr(cli.car.all)

config$End()

config.00.1.ave <- config
rm("config")

#### save workspace
save.image(file = config.00.1.ave$out.001.ave.rdata.path)

#### save session info
writeLines(capture.output(sessionInfo()),
           sprintf("../00_0_SessionInfo/01_SessionInfo_ClimateCompileAverage_%s.txt", substr(Sys.time(), 1, 10)))
