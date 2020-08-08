####
#### Kitayama et al. "Temperature is a dominant driver of distinct annual
#### seasonality of leaf litter production of equatorial tropical rain forests"
#### No.1: Data compile
####

#### A part of code is from the supplement of Bush et al. (2017)
#### "Fourier analysis to detect phenological cycles using tropical field data and simulations"
#### Methods in Ecology and Evolution doi:10.1111/2041-210X.12704

source("config.R")

#### load environments
# workspace load (litter data and baseline-corrected climate data set)
load(config$out.002.rdata.path)
dir.create(config$out.01.dir, showWarnings = F)

##############################
######## Date compile ########
##############################

#### Preparation (extract target data)
# litter data = lit.d
# original data = climate.not.correct
# corrected data = climate.correct

## generate date label
new.date.cat <- rep(NA, length(lit.d$mid_date))

for (i in 1:length(lit.d$mid_date)) {
  mid.split <- strsplit(as.character(lit.d$mid_date), "/")
  year0 <- mid.split[[i]][1]
  month0 <- mid.split[[i]][2]
  day0 <- as.numeric(mid.split[[i]][3])
  if (day0 < 15) date.cat0 <- "1" # label as "early" if date is before 14
  if (day0 > 14) date.cat0 <- "16" # label as "late" if date is after 15
  date.cat.tmp <- sprintf("%s-%s-%s", year0, month0, date.cat0)
  new.date.cat[i] <- date.cat.tmp
}

lit.d$date_cat <- new.date.cat

## generate new data (one value for one date category) for litter data
lit.d2 <- data.frame(time_index = 1:length(unique(new.date.cat)), date_cat = unique(new.date.cat))
for (i in 6:(ncol(lit.d) - 1)) {
  mean.tmp <- tapply(lit.d[, i], as.Date(lit.d$date_cat), function(x) mean(x, na.rm = T))
  lit.d2 <- cbind(lit.d2, mean.tmp)
  colnames(lit.d2)[ncol(lit.d2)] <- colnames(lit.d)[i]
}

## generate new data (one value for one date category) for climate data
climate.correct2 <- list()

for (i in 1:length(climate.correct)) {
  climate.correct2.tmp <- data.frame(time_index = 1:length(unique(new.date.cat)), date_cat = unique(new.date.cat))
  climate.correct[[i]]$date_cat <- new.date.cat
  for (j in 6:(ncol(climate.correct[[i]]) - 1)) {
    mean.tmp <- tapply(climate.correct[[i]][, j], as.Date(climate.correct[[i]]$date_cat), function(x) mean(x, na.rm = T))
    climate.correct2.tmp <- cbind(climate.correct2.tmp, mean.tmp)
    colnames(climate.correct2.tmp)[ncol(climate.correct2.tmp)] <- colnames(climate.correct[[i]])[j]
  }
  climate.correct2 <- c(climate.correct2, list(climate.correct2.tmp))
  names(climate.correct2)[length(climate.correct2)] <- names(climate.correct)[i]
}


## generate new data (one value for one date category) for climate data
climate.not.correct2 <- list()

for (i in 1:length(climate.not.correct)) {
  climate.not.correct2.tmp <- data.frame(time_index = 1:length(unique(new.date.cat)), date_cat = unique(new.date.cat))
  climate.not.correct[[i]]$date_cat <- new.date.cat
  for (j in 6:(ncol(climate.not.correct[[i]]) - 1)) {
    mean.tmp <- tapply(climate.not.correct[[i]][, j], as.Date(climate.not.correct[[i]]$date_cat), function(x) mean(x, na.rm = T))
    climate.not.correct2.tmp <- cbind(climate.not.correct2.tmp, mean.tmp)
    colnames(climate.not.correct2.tmp)[ncol(climate.not.correct2.tmp)] <- colnames(climate.not.correct[[i]])[j]
  }
  climate.not.correct2 <- c(climate.not.correct2, list(climate.not.correct2.tmp))
  names(climate.not.correct2)[length(climate.not.correct2)] <- names(climate.not.correct)[i]
}


config$End()
config.01 <- config
rm("config")

#### save image
save.image(file = config.01$out.01.rdata.path)

#### save session info
writeLines(capture.output(sessionInfo()),
           sprintf("00_0_SessionInfo/01_SessionInfo_DataCompile_%s.txt", substr(Sys.time(), 1, 10)))
