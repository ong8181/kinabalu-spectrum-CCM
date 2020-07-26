####
#### Kitayama et al. "Temperature is a dominant driver of distinct annual
#### seasonality of leaf litter production of equatorial tropical rain forests"
####

#### Data compiler of litter data for Fourier analysis
#### Specify site name or no, and mean or sd
DataCompiler01 <- function(lit.d2,
                           site.names.litter,
                           data.cat1,
                           data.cat2,
                           study.site.no,
                           mean.or.sd.no = 1,
                           visualize = T) {
  i1 <- study.site.no # study site
  i3 <- mean.or.sd.no # mean or sd
  
  data.ls <- list()
  
  for (i2 in 1:6) {
    target.name <- sprintf("%s_%s_%s", site.names.litter[i1], data.cat1[i2], data.cat2[i3])
    lit.d.ts.tmp <- lit.d2[, c("date_cat", target.name)]
    
    #### convert the data.frame to class "time-series"
    #### short (< 4) NAs will be interpolated by the simple linear estimator
    
    ## check consecutive NAs (more than or equal to 4 NAs)
    na.n <- 4 # specify the acceptable number of consecutive NAs
    na.lit <- is.na(lit.d.ts.tmp)[, 2]
    lit.d.ts.tmp$na_sum <- NA
    lit.d.ts.tmp$num <- 1:nrow(lit.d.ts.tmp)
    ## calculate the number of NAs
    for (i in na.n:nrow(lit.d.ts.tmp))
      lit.d.ts.tmp$na_sum[i] <- sum(na.lit[(i - na.n + 1):i])
    ## find the consecutive NAs >= na.n
    consecutiveNAs.index <- lit.d.ts.tmp$na_sum > (na.n - 1)
    lit.d.ts.tmp$na_consecutive <- NA
    lit.d.ts.tmp$na_consecutive[!consecutiveNAs.index] <- 1
    longest.range <- attributes(na.contiguous(lit.d.ts.tmp$na_consecutive))$tsp[1:2]
    
    ## extract the longest consecutive range for
    ##         the data and replace the shrot NAs with linear interpolation
    dates <- as.character(lit.d.ts.tmp$date_cat[longest.range[1]:longest.range[2]])
    dates <- gsub("early", "1", dates)
    dates <- gsub("late", "16", dates)
    lit.d.ts0 <- timeSeries::timeSeries(matrix(lit.d.ts.tmp[longest.range[1]:longest.range[2], 2]), as.Date(dates))
    
    lit.d.ts1 <- na.omit(timeSeries::interpNA(lit.d.ts0, method = "linear"))
    lit.d.ts2 <- data.frame(date_cat = row.names(lit.d.ts1))
    lit.d.ts2$tmp <- as.numeric(lit.d.ts1)
    colnames(lit.d.ts2)[2] <- target.name
    lit.d.ts3 <- na.omit(lit.d.ts2)
    
    ## judge starting date
    start.date0 <- as.character(lit.d.ts3[1, 1])
    split.date <- strsplit(start.date0, "-")
    year0 <- as.numeric(split.date[[1]][1])
    month0 <- as.numeric(split.date[[1]][2])
    cat0 <-  as.character(split.date[[1]][3])
    if (cat0 == "01")
      cat1 <- 1
    if (cat0 == "16")
      cat1 <- 2
    start.n <- (month0 - 1) * 2 + cat1
    
    ## data (class "time series")
    data.ls.tmp <- ts(as.numeric(lit.d.ts3[, 2]), frequency = 24, start = c(year0, start.n))
    data.ls <- c(data.ls, list(data.ls.tmp))
  }
  
  if (visualize) {
    ## result check
    plot(as.numeric(scale(data.ls[[1]])), type = "l", ylim = c(-1, 15))
    for (i in 2:6)
      lines(as.numeric(scale(data.ls[[i]])) + i, col = i)
  }

  return(data.ls)
}
