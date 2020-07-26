####
#### Kitayama et al. "Temperature is a dominant driver of distinct annual
#### seasonality of leaf litter production of equatorial tropical rain forests"
####

#### Data compiler of simulated data for Fourier analysis
####
SimMwPower <- function(ts.data,
                       # equal to 6 years
                       trim.length = 6 * 24) {
  # extract data from original data set (=lit.d2)
  data.ls <- ts.data
  annual.power.all <- data.frame()
  max.trim.no <- min(unlist(lapply(data.ls, length))) - trim.length
  
  # category names
  cat.names <- c("target", "force1", "force2", "force3")
  
  for (trim.i in 1:max.trim.no) {
    # Triming time series to a selected length (6 years)
    data.ls.trim <- lapply(data.ls, function(x) as.ts(zoo::as.zoo(x)[trim.i:(trim.length + trim.i - 1)]))
    fourier.df <- PerformFourier(data.ls.trim, data.variables = cat.names)
    
    # Extract the power of annual seasonality
    low.freq <- 1 / 24 - 1 / 24 * 0.1
    up.freq <- 1 / 24 + 1 / 24 * 0.1
    annual.power <- fourier.df[low.freq < fourier.df$freq & up.freq > fourier.df$freq, ]
    min.id <- min(abs(annual.power$freq - 1 / 24)) == abs(annual.power$freq - 1 / 24)
    annual.power <- annual.power[min.id, ]
    
    # Add index dates to the results of Fourier analysis
    index.dates <- lapply(data.ls.trim, function(x) time(x)[1])
    annual.power$index_date <- unlist(index.dates)
    annual.power.all <- rbind(annual.power.all, annual.power)
  }
  
  ## return data frame for CCM analysis
  df.seasonality <- list(annual.power.all[annual.power.all$ID == cat.names[1],])
  for (i in 2:length(cat.names)) {
    df.seasonality <- append(df.seasonality, list(annual.power.all[annual.power.all$ID == cat.names[i],]))
  }
  
  return(df.seasonality)
}
