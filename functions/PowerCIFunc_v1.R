####
#### Kitayama et al. "Temperature is a dominant driver of distinct annual
#### seasonality of leaf litter production of equatorial tropical rain forests"
####

##Functions for Fourier analysis ##
##Parameters ##
#The following list allows the SpecFun function to find appropriate spans for
#the Daniel kernel to successively apply to the spectrum to give a smoothed
#periodogram with bandwidth similar to 0.1, and a super-smoothed spectrum (the
#null continuum) with bandwidth similar to 1. Span size is linked to the length
#of the oringal timeseries data, we give options here for data from 24 to 360
#months long.

kSPANS_LOOKUP <- list(months = c(24, 48, 72, 96, 120, 144, 168, 192,
                                 216, 240, 264, 288, 312, 336, 360) * 2,
                      spans_smooth = list( 3, 3, 3, 3, c(3, 3), c(3, 5), c(3, 5),
                                           c(5, 5), c(5, 5), c(5, 7), c(5, 7),
                                           c(7, 7), c(7, 9), c(7, 9), c(7, 9)),
                      spans_super_smooth = list(c(5, 7), c(11, 11), c(15, 17), c(19, 21), c(25, 27),
                                                c(29, 31), c(35, 37), c(39, 41), c(43, 45), c(47, 49),
                                                c(55, 57), c(59, 61), c(65, 67), c(73, 75), c(75, 79)))

## Functions ##
# Spectrum fucntion
SpecFun <- function(x) spectrum(x, spans = kSPANS_LOOKUP$spans_smooth[[which.min(abs(kSPANS_LOOKUP$months - length(x)))]],
                                plot = F, demean = T, detrend = T) #spectrum function for normal smoother periodogram

#Function to calculate smoothed spectrum
SpecFun2 <- function(x) spectrum(x, spans = kSPANS_LOOKUP$spans_smooth[[which.min(abs(kSPANS_LOOKUP$months - length(x)))]],
                                 plot = F, demean = T, detrend = T) #spectrum function for normal smoother periodogram

#Function to calculate the null continuum (super-smoothed spectrum of data) for null hypothesis test
SpecNullFun1 <- function(x) spectrum(x, spans = kSPANS_LOOKUP$spans_super_smooth[[which.min(abs(kSPANS_LOOKUP$months - length(x)))]],
                                     plot = F, demean = T, detrend = T) #spectrum function for null hypothesis spectrum
SpecNullFun2 <- function(x) spectrum(x, plot = F, demean = T, detrend = T) #average spectrum


#### Function to extract key variables from spectrum outputs
ConfidenceFun <- function(p, data.ls, ids) {
  ts <- data.ls[[p]]
  d <- data.frame(ID = ids[p])
  d$length <- length(ts)
  spec.res <- SpecFun2(ts)
  
  d$freq_dom <- (spec.res$freq[which.max(SpecFun(ts)$spec)]) / 24 #frequency of the dominant peak
  d$cycle_dom <- 1 / d$freq_dom / 2
  d$spec_dom <- max(spec.res$spec) #spectrum of dominant peak
  d$spec_dom_norm <-  d$spec_dom * (1 / mean(spec.res$spec))
  df <- spec.res$df #degrees of freedom of the smoothed periodogram
  lower.ci <- (df * d$spec_dom) / (qchisq(c(0.975), df)) #lower CI of dominant peak of smoothed periodogram
  upper.ci <- (df * d$spec_dom) / (qchisq(c(0.025), df)) #lower CI of dominant peak of smoothed periodogram
  
  spec.null.1 <- SpecNullFun1(ts) # null distribution of extremely smooth spectrum
  spec.null.dom1 <- spec.null.1$spec[which(abs((spec.null.1$freq) / 24 - d$freq_dom) == min(abs((spec.null.1$freq) / 24 - d$freq_dom)))]
  spec.null.avr <- mean(SpecNullFun2(ts)$spec) # average spectrum, by Ushio
  
  d$spec_null_dom <- spec.null.dom1 # by Ushio
  d$spec_null_avr <- spec.null.avr # average spectrum, by Ushio
  
  d$sig_null <- lower.ci > spec.null.dom1
  d$sig_avr <- lower.ci > spec.null.avr
  d$lower_ci <- lower.ci # by Ushio
  d$upper_ci <- upper.ci # by Ushio
  
  d$lower_ci_norm <- lower.ci * (1 / mean(spec.res$spec)) # by Ushio
  d$upper_ci_norm <- upper.ci * (1 / mean(spec.res$spec)) # by Ushio
  d$spec_null_dom_norm <- d$spec_null_dom * (1 / mean(spec.res$spec))
  d$spec_avr_norm <- d$spec_null_avr * (1 / spec.null.avr)
  d$bw_smooth <- spec.res$bandwidth #bandwidth of smoothed periodogram
  d$bw_super_smooth <- spec.null.1$bandwidth #bandwidth of super-smoothed periodogram
  data.frame(d)
}
