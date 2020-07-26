####
#### Kitayama et al. "Temperature is a dominant driver of distinct annual
#### seasonality of leaf litter production of equatorial tropical rain forests"
####

#### Data compiler of litter data for Fourier analysis
#### Specify site name or no, and mean or sd
ClimateMwPower <- function(climate.correct3,
                           site.names.climate,
                           study.site.no,
                           trim.length = 3 * 24,
                           # equal to 3 years
                           allow.NAs = 3,
                           selected.climate = c(
                             "ATC",
                             "relative_humidity",
                             "Act_VP_kPa",
                             "SD_kPa",
                             "Corrected_SD_kPa",
                             "PAR",
                             "wind",
                             "rain"
                           ),
                           ts.length = "first_longest",
                           visualize = T) {
  # extract data from original data set (=lit.d2)
  data.ls <- DataCompiler02(
    climate.correct3,
    site.names.climate,
    study.site.no,
    allow.NAs = allow.NAs,
    selected.climate = selected.climate,
    continuous.ts = ts.length,
    visualize = F
  )
  
  annual.power.all <- data.frame()
  max.trim.no <- max(unlist(lapply(data.ls, length))) - trim.length
  max.trim.no.all <- unlist(lapply(data.ls, length)) - trim.length
  
  for (trim.i in 1:max.trim.no) {
    # check time series length
    ok.n <- which(max.trim.no.all >= trim.i)
    
    # Triming time series to a selected length (6 years)
    data.ls.trim <- lapply(data.ls[ok.n], function(x) as.ts(as.zoo(x)[trim.i:(trim.length + trim.i - 1)]))
    fourier.df <- PerformFourier(data.ls.trim, data.variables = selected.climate[ok.n])
    
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
  df.seasonality <- list(annual.power.all[annual.power.all$ID == selected.climate[1],])
  for (i in 2:length(selected.climate)) {
    df.seasonality <- append(df.seasonality, list(annual.power.all[annual.power.all$ID == selected.climate[i],]))
  }
  
  return(df.seasonality)
}


PerformFourier <- function(data.for.fourier, data.variables) {
  dff <- data.for.fourier
  
  ## add names to litter species (referred to as individuals here)
  names(dff) <- data.variables
  individuals <- length(dff)
  ids <- names(dff)
  
  ##Run fourier analysis using function "spectrum" on each list object (individual
  ##time series) to give Fourier outputs for each individual
  
  fourier.df <- data.frame()
  for (i in 1:length(dff)) {
    dff.spec <- SpecFun(dff[[i]])
    d <- data.frame(freq = dff.spec$freq / 24)
    d$spec <- dff.spec$spec
    d$spec_norm <- dff.spec$spec * (1 / mean(dff.spec$spec))
    d$ID <- as.factor(ids[i])
    fourier.df <- rbind(fourier.df, d)
  }
  
  return(fourier.df)
}
