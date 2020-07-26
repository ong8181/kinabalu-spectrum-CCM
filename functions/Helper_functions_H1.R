####
#### Kitayama et al. "Temperature is a dominant driver of distinct annual
#### seasonality of leaf litter production of equatorial tropical rain forests"
#### Helper functions H1
####

#### define functions
## check time points of the two species (genus) data
# currently only one month interval is accepted
CheckTimeCorresp <- function(data1, data2, interval = "month") {
  name1 <- data1$gen_name
  name2 <- data2$gen_name
  
  if (length(name1) < 2) name1 <- "NA1"
  if (length(name2) < 2) name2 <- "NA2"
  
  # correct sampling time point to match the two time series
  initial.month <- min(as.Date(data1$data$date), as.Date(data2$data$date))
  final.month <- max(as.Date(data1$data$date), as.Date(data2$data$date))
  
  ## generate time sequence with one month interval
  # prepare dataframe
  sampling.time.seq <- seq(as.Date(initial.month), as.Date(final.month + 31), by = "month")
  ns <- length(sampling.time.seq)
  corrected.ts <- data.frame(date = sampling.time.seq, y_m = rep(NA, ns), data1 = rep(NA, ns), data2 = rep(NA, ns))
  colnames(corrected.ts) <- c("date", "year_month", name1, name2)
  corrected.ts$year_month <- substr(corrected.ts$date, 1, 7)
  
  data1$data$year_month <- substr(data1$data$date, 1, 7)
  data2$data$year_month <- substr(data2$data$date, 1, 7)
  
  # input the data of the same time point
  for (i in 1:ns) {
    target.t <- corrected.ts[i, "year_month"]
    data1.cor <- data1$data[data1$data$year_month == target.t, ]
    data2.cor <- data2$data[data2$data$year_month == target.t, ]
    
    # set candidate values
    data1.to.val <- NA
    data2.to.val <- NA
    if (nrow(data1.cor) != 0) data1.to.val <- data1.cor$biomass
    if (nrow(data2.cor) != 0) data2.to.val <- data2.cor$biomass
    
    # input values
    corrected.ts[i, name1] <- data1.to.val
    corrected.ts[i, name2] <- data2.to.val
  }
  
  # remove the last row if an unnecessary row is present
  if (is.na(corrected.ts[ns, 3]) && is.na(corrected.ts[ns, 4])) {
    corrected.ts <- corrected.ts[-ns, ]
  }
  
  # output corrected and combined time series
  return(corrected.ts)
}


#### function to generage 95% CI by twin-surrogate method
CcmSurPlot <- function(ccm.res,
                       sur.res,
                       name.from = "X",
                       name.to = "Y") {
  rhos <- sur.res$rho
  d.rhos <- sur.res$delta_rho
  d.rho.ccm <- rev(ccm.res$rho)[1] - ccm.res$rho[1]
  
  #quartz(width=4, height=4)
  plot(ccm.res$lib_size, ccm.res$rho,
    type = "l", lwd = 2, ylim = range(-.2, 1), col = "red3",
    ylab = expression(rho), xlab = "L", cex.main = 0.8,
    main = sprintf("%s xmap to %s", name.from, name.to))
  polygon(c(rhos$L, rev(rhos$L)), c(rhos$lower95, rev(rhos$upper95)),
    col = rgb(.5, .5, .5, .5), border = NA)
  segments(15, 0.6, 15, 0.6 + as.numeric(d.rhos$d_rho_upper), lwd = 4, col = "black")
  segments(22, 0.6, 22, 0.6 + d.rho.ccm, lwd = 4, col = "red3")
  abline(h = 0, lty = 2)
}


#### Define ggplot style
StylePlot <-  function(ggobject) {
  return(
    ggobject + theme_bw() + theme(axis.text.x = element_text(angle = 0),
                                  panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  axis.text = element_text(size = 12),
                                  axis.title = element_text(size = 12),
                                  panel.background = element_rect(colour = "black", fill = NA, size = 0.8))
  )
}

