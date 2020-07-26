####
#### Kitayama et al. "Temperature is a dominant driver of distinct annual
#### seasonality of leaf litter production of equatorial tropical rain forests"
####

#### Data compiler of litter data for Fourier analysis
#### Specify site name or no, and mean or sd
LitterMwPower <- function(lit.d2,
                          site.names.litter,
                          data.cat1,
                          data.cat2,
                          study.site.no,
                          trim.length = 6 * 24,
                          # equal to 6 years
                          mean.or.sd.no = 1,
                          visualize = F) {
  # extract data from original data set (=lit.d2)
  data.ls <- DataCompiler01(lit.d2,
                            site.names.litter,
                            data.cat1,
                            data.cat2,
                            study.site.no,
                            visualize = visualize)
  
  annual.power.all <- data.frame()
  max.trim.no <- min(unlist(lapply(data.ls, length))) - trim.length
  
  for (trim.i in 1:max.trim.no) {
    # Specify the start date
    #trim.i <- 1
    
    # Triming time series to a selected length (6 years)
    data.ls.trim <- lapply(data.ls, function(x) as.ts(zoo::as.zoo(x)[trim.i:(trim.length + trim.i - 1)]))
    
    fourier.df <- PerformFourier(data.ls.trim, data.variables = data.cat1)
    
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
  df.seasonality <- list(annual.power.all[annual.power.all$ID == data.cat1[1],])
  for (i in 2:length(data.cat1)) {
    df.seasonality <- append(df.seasonality, list(annual.power.all[annual.power.all$ID == data.cat1[i],]))
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
