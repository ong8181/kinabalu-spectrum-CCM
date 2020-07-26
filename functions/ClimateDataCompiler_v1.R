####
#### Kitayama et al. "Temperature is a dominant driver of distinct annual
#### seasonality of leaf litter production of equatorial tropical rain forests"
####

#### Specify site name or no, and mean or sd
DataCompiler02 <- function(climate.correct3,
                           site.names.climate,
                           study.site.no,
                           selected.climate.vars = c(
                             "ATC",
                             "relative_humidity",
                             "Act_VP_kPa",
                             "SD_kPa",
                             "Corrected_SD_kPa",
                             "PAR",
                             "wind",
                             "rain"
                           ),
                           allow.NAs = 3,
                           continuous.ts = "first_longest",
                           visualize = T) {
  i1 <- study.site.no # study site
  selected.climate <- selected.climate.vars
  cli.n <- length(selected.climate)
  
  data.ls <- list()
  
  for (i2 in 1:cli.n) {
    # target.name <- sprintf("cli_%s_biw_avr", site.names.climate[i1])
    target.name <- site.names.climate[i1]
    cli.d.ts.tmp <- climate.correct3[[target.name]][, c("date_cat", selected.climate[i2])]
    
    #### convert the data.frame to class "time-series"
    #### short (< 4) NAs will be interpolated by the simple linear estimator
    
    ## check consecutive NAs (more than or equal to 4 NAs)
    na.n <- allow.NAs + 1 # specify the acceptable number of consecutive NAs
    na.lit <- is.na(cli.d.ts.tmp)[, 2]
    cli.d.ts.tmp$na_sum <- NA
    cli.d.ts.tmp$num <- 1:nrow(cli.d.ts.tmp)
    ## calculate the number of NAs
    for (i in na.n:nrow(cli.d.ts.tmp)) cli.d.ts.tmp$na_sum[i] <- sum(na.lit[(i - na.n + 1):i])
    ## find the consecutive NAs >= na.n
    consecutiveNAs.index <- cli.d.ts.tmp$na_sum > (na.n - 1)
    cli.d.ts.tmp$na_consecutive <- NA
    cli.d.ts.tmp$na_consecutive[!consecutiveNAs.index] <- 1
    longest.range <- attributes(na.contiguous(cli.d.ts.tmp$na_consecutive))$tsp[1:2]
    
    if (continuous.ts == "second_longest") {
      ## identify the second longest continuous term
      
      ## repeat the same procedure
      na.lit[longest.range[1]:longest.range[2]] <- T
      cli.d.ts.tmp$na_sum <- NA
      cli.d.ts.tmp$num <- 1:nrow(cli.d.ts.tmp)
      ## calculate the number of NAs
      for (i in na.n:nrow(cli.d.ts.tmp))
        cli.d.ts.tmp$na_sum[i] <- sum(na.lit[(i - na.n + 1):i])
      ## find the consecutive NAs >= na.n
      consecutiveNAs.index <- cli.d.ts.tmp$na_sum > (na.n - 1)
      cli.d.ts.tmp$na_consecutive <- NA
      cli.d.ts.tmp$na_consecutive[!consecutiveNAs.index] <- 1
      longest.range <- attributes(na.contiguous(cli.d.ts.tmp$na_consecutive))$tsp[1:2]
    }
    
    ## extract the longest consecutive range for
    ##         the data and replace the shrot NAs with linear interpolation
    dates <- as.character(cli.d.ts.tmp$date_cat[longest.range[1]:longest.range[2]])
    cli.d.ts0 <- timeSeries::timeSeries(matrix(cli.d.ts.tmp[longest.range[1]:longest.range[2], 2]), as.Date(dates))
    cli.d.ts1 <- na.omit(timeSeries::interpNA(cli.d.ts0, method = "linear"))
    cli.d.ts2 <- data.frame(date_cat = row.names(cli.d.ts1))
    cli.d.ts2$tmp <- as.numeric(cli.d.ts1)
    colnames(cli.d.ts2)[2] <- target.name
    cli.d.ts3 <- na.omit(cli.d.ts2)
    
    ## judge starting date
    start.date0 <- as.character(cli.d.ts3[1, 1])
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
    data.ls.tmp <- ts(as.numeric(cli.d.ts3[, 2]), frequency = 24, start = c(year0, start.n))
    data.ls <- c(data.ls, list(data.ls.tmp))
  }
  
  if (visualize) {
    ## result check
    plot(as.numeric(scale(data.ls[[1]])), type = "l", ylim = c(-1, 15))
    for (i in 2:cli.n)
      lines(as.numeric(scale(data.ls[[i]])) + i, col = i)
  }
  
  return(data.ls)
}
