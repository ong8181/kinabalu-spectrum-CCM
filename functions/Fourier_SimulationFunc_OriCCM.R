####
#### Kitayama et al. "Temperature is a dominant driver of distinct annual
#### seasonality of leaf litter production of equatorial tropical rain forests"
####

## define function
FourierOriCcmSim <- function(parameter.list) {
  #### Prepare output data.frame
  sim.ccm.output <- data.frame()
  
  ## time series length
  ###############################
  yl <- parameter.list$yl # define year length
  num.iter <- parameter.list$num.iter
  Erange <- parameter.list$Erange
  ccm.tp <- parameter.list$ccm.tp
  error.rate.seq <- parameter.list$error.rate.seq
  force.seasonality.seq <- parameter.list$force.seasonality.seq
  cores = parameter.list$cores
  seed = parameter.list$seed
  ################################
  
  tl <- yl * 24 + 1
  d1 <- MakeSeasonalTS(t = tl, year = yl,
                       sea.parms = rep(force.seasonality.seq, 3),
                       constant.effect = 2,
                       error = T,
                       error.rate = error.rate.seq,
                       show.fig = T)
  
  ## check time series length
  while (is.null(d1)) {
    d1 <- MakeSeasonalTS(t = tl, year = yl,
                         sea.parms = rep(force.seasonality.seq, 3),
                         constant.effect = 2,
                         error = T,
                         error.rate = error.rate.seq,
                         show.fig = T)
  }
  
  ## perform Fourier analysis
  target <- as.numeric(scale(d1$xs))
  force1 <- as.numeric(scale(d1$ys1))
  force2 <- as.numeric(scale(d1$ys2))
  force3 <- as.numeric(scale(d1$ys3))
  
  all.ts <- list(target, force1, force2, force3)
  ts.names <- c("target", "force1", "force2", "force3")
  
  #### CCM for power spectrum time series
  all.simp <- lapply(all.ts, function(x) rEDM::simplex(x, E = Erange , silent = T))
  Es <- lapply(all.simp, function(x) x[which.min(x$mae), 'E'])
  
  ## Do CCM for a particlar set of time series
  ccm.set.all <- list(force1 = c(1, 2), force2 = c(1, 3), force3 = c(1, 4))
  
  for (ccm.set.i in ccm.set.all) {
    #### set CCM paramters and generate twin surrogate
    m <- length(all.ts[[ccm.set.i[1]]])
    lib.set <- c(Es[[ccm.set.i[1]]] + 1, max(Erange) + 2, m)
    
    block.ccm <- cbind(all.ts[[ccm.set.i[1]]], all.ts[[ccm.set.i[2]]])
    # generate surrogate
    sur.ts1 <- as.data.frame(rEDM::make_surrogate_data(all.ts[[ccm.set.i[1]]],
                                                       method = "seasonal", num_surr = num.iter, T_period = 24))

    #### do CCM
    #### different tp
    for (ccm.tp.i in ccm.tp) {
      #### do original CCM
      ccm.12.raw	<- rEDM::ccm(block.ccm, E = Es[[ccm.set.i[1]]],
                              lib_sizes = lib.set, tp = ccm.tp.i,
                              silent = T, RNGseed = seed)
      ccm.12	<- rEDM::ccm_means(ccm.12.raw, na.rm = T)
      
      #### do surrogate CCM
      sur.ci.res	<- ParSurciV3(all.ts[[ccm.set.i[1]]], all.ts[[ccm.set.i[2]]],
          sur.ts1, lib.parms = lib.set, E.range = Erange,
          tp = ccm.tp.i, calc.original.ccm.p = T,
          original.ccm.res = ccm.12, cores = cores, seed = seed)
      
      #### Do Z-test #####
      num.pred <- ccm.12$num_pred[1]
      z.result <- psych::r.test(num.pred, ccm.12$rho[1], ccm.12$rho[nrow(ccm.12)])

      #### combine CCM results
      ## NA check
      min.row <- min(which(!is.na(ccm.12$rho) & is.finite(ccm.12$rho)))
      max.row <- max(which(!is.na(ccm.12$rho) & is.finite(ccm.12$rho)))
      meta.data <- data.frame(
        target_var = ts.names[ccm.set.i[1]],
        force_var = ts.names[ccm.set.i[2]],
        min_lib = min(lib.set),
        init_rho = ccm.12$rho[min.row],
        z_value = z.result$z,
        z_p_value = z.result$p)
      delta.rho.tmp <- ccm.12$rho[max.row] - ccm.12$rho[min.row]
      ccm.res.tmp <- cbind(
        meta.data,
        ccm.12[max.row, ],
        sur.ci.res$ter_rho_quantile['50%'],
        sur.ci.res$ter_rho_quantile['90%'],
        sur.ci.res$ter_rho_quantile['95%'],
        sur.ci.res$ter_rho_quantile['97.5%'],
        sur.ci.res$conv_quantile['50%'],
        sur.ci.res$conv_quantile['90%'],
        sur.ci.res$conv_quantile['95%'],
        sur.ci.res$conv_quantile['97.5%'],
        sur.ci.res$conv_ter90mid,
        sur.ci.res$conv_ter95mid,
        sur.ci.res$conv_ter975mid,
        sur.ci.res$oriccm_d_rho_p,
        sur.ci.res$oriccm_t_rho_p,
        sur.ci.res$oriccm_comb_p,
        sur.ci.res$ident_lib,
        delta.rho.tmp)
      nnt <- ncol(ccm.res.tmp)
      colnames(ccm.res.tmp)[(nnt - 15):nnt] <- c(
        "ter_rho50", "ter_rho90", "ter_rho95", "ter_rho975",
        "conv_50", "conv_90", "conv_95", "conv_975",
        "conv_ter90mid", "conv_ter95mid", "conv_ter975mid",
        "oriccm_d_rho_p", "oriccm_t_rho_p", "oriccm_comb_p",
        "identical_lib", "d_rho")
      ccm.res.tmp$force_seasonality <- force.seasonality.seq
      ccm.res.tmp$error_rate <- error.rate.seq
      
      #### combine all data
      sim.ccm.output <- rbind(sim.ccm.output, ccm.res.tmp)
    }
  }
  return(sim.ccm.output)
}
