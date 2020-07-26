####
#### Seasonality time-series generation
####

#### depends = "imputeTS"
MakeSeasonalTS <- function(year = 12,
                           t = year * 24,
                           sin.lag = 0,
                           inits = runif(2, 0.49, 0.51),
                           show.fig = T,
                           x.parms = c(3.8,-3.8),
                           # intrinsic litter dynamics
                           intrinsic.effect = 1,
                           constant.effect = 2,
                           ry.parms = c(1, 1, 1),
                           sea.parms = c(1, 1, 1),
                           error = F,
                           error.rate = 0.1,
                           fluctuate.s = T,
                           # 1 point per 2 year
                           fluctuate.p = year / 2 + 1) {
  ## define fundamental model paramters
  r <- x.parms[1]
  s <- x.parms[2]
  
  ## define climate influences
  # constant (= seasonality driver)
  c.e <- constant.effect
  # random effect (= seasonality driver)
  r.e.term <- rep(c(rep(0, t/year*3/4), rep(1, t/year*1/4)), year)

  ry1 <- ry.parms[1]
  ry2 <- ry.parms[2]
  ry3 <- ry.parms[3]
  sea1 <- sea.parms[1]
  sea2 <- sea.parms[2]
  sea3 <- sea.parms[3]
  
  
  ## preparation for seasonality (sin function)
  x.sins <- seq(0 + sin.lag, 2*pi*year + sin.lag - 2*pi*year/t, by = 2*pi*year/t)
  y.sins <- sin(x.sins)
  d <- data.frame(time = 1:t, season = y.sins,
                  x = rep(NaN, t),
                  y1 = exp(-0.5*rnorm(t)),
                  y2 = exp(-0.5*rnorm(t)),
                  y3 = exp(-0.5*rnorm(t)),
                  xs = rep(NaN, t),
                  ys1 = rep(NaN, t),
                  ys2 = rep(NaN, t),
                  ys3 = rep(NaN, t))
  
  ## time series generation
  d[1, c('x', 'xs')] <- inits
  
  ## define fluctuating seasonality function
  if (fluctuate.s){
    flu.s1 <- flu.s2 <- flu.s3 <- rep(NaN, t)
    flu.p.index <- round(seq(1, t, length = fluctuate.p))
    flu_sd <- 0.2
    
    flu.s1[flu.p.index] <- rnorm(fluctuate.p, mean = 1, sd = flu_sd)
    flu.s2[flu.p.index] <- rnorm(fluctuate.p, mean = 1, sd = flu_sd)
    flu.s3[flu.p.index] <- rnorm(fluctuate.p, mean = 1, sd = flu_sd)
    
    ## avoid negative amplification
    flu.s1[flu.s1 < 0.2] <- flu.s2[flu.s2 < 0.2] <- flu.s3[flu.s3 < 0.2] <- 0.2
    
    ## smooth using spline (depends = imputeTS)
    flu.s1 <- na.interpolation(flu.s1, option = "stine")
    flu.s2 <- na.interpolation(flu.s2, option = "stine")
    flu.s3 <- na.interpolation(flu.s3, option = "stine")
  }
  
  for (i in 1:(t - 1)) {
    if (fluctuate.s) {
      d$ys1[i] <- (ry1 * d$y1[i] + sea1 * flu.s1[i] * d$season[i])
      d$ys2[i] <- (ry2 * d$y2[i] + sea2 * flu.s2[i] * d$season[i])
      d$ys3[i] <- (ry3 * d$y3[i] + sea3 * flu.s3[i] * d$season[i])
    } else{
      d$ys1[i] <- (ry1 * d$y1[i] + sea1 * d$season[i])
      d$ys2[i] <- (ry2 * d$y2[i] + sea2 * d$season[i])
      d$ys3[i] <- (ry3 * d$y3[i] + sea3 * d$season[i])
    }
    d$x[i + 1] <- d$x[i]*(r + s*d$x[i])
    d$xs[i + 1] <- intrinsic.effect*d$x[i + 1] + c.e*d$x[i]*(d$ys1[i] + r.e.term[i]*d$ys2[i])
  }
  
  if (all(!is.na(d$xs)) & all(!is.infinite(d$xs))) {
    if (error) {
      x.errors <- rnorm(t, mean = 0, sd = error.rate * sd(d$xs, na.rm = T))
      y1.errors <- rnorm(t, mean = 0, sd = error.rate * sd(d$ys1, na.rm = T))
      y2.errors <- rnorm(t, mean = 0, sd = error.rate * sd(d$ys2, na.rm = T))
      y3.errors <- rnorm(t, mean = 0, sd = error.rate * sd(d$ys3, na.rm = T))
      
      d$xs_er <- d$xs + x.errors
      d$ys1_er <- d$ys1 + y1.errors
      d$ys2_er <- d$ys2 + y2.errors
      d$ys3_er <- d$ys3 + y3.errors
      
      d$xs <- d$xs_er
      d$ys1 <- d$ys1_er
      d$ys2 <- d$ys2_er
      d$ys3 <- d$ys3_er
      
      d <- d[, 1:10]
      d <- na.omit(d)
    } else{
      d <- na.omit(d)
    }
  } else{
    warnings("Time series includes infinite values!")
    d <- NULL
  }
  
  if (show.fig) {
    xs.r <- d$xs[is.finite(d$xs)]
    ys_r1 <- d$ys1[is.finite(d$ys1)]
    
    op <- par(mfrow = c(2, 1), mar = c(4, 4, 1, 1))
    plot(d$t, 0.2 * (d$season) + mean(d$xs),
         type = "l", col = 2, xlab = "time", ylab = "X value",
         ylim = range((min(xs.r) - 0.2), (max(xs.r) + 0.2)))
    lines(d$t, d$xs, col = 1)
    plot(d$t, d$season + mean(d$ys1, na.rm = T),
         type = "l", col = 2, xlab = "time", ylab = "Y value",
         ylim = range((min(ys_r1) - 1.6), (max(ys_r1) + 9.2)))
    lines(d$t, as.numeric(scale(d$x)) * 1.3)
    lines(d$t, d$ys1 + 3, col = 1)
    lines(d$t, d$season + mean(d$ys1, na.rm = T) + 3, col = 2)
    lines(d$t, d$ys2 + 6, col = 3)
    lines(d$t, d$ys3 + 9, col = 4)
    par(op)
  }
  return(d)
}
