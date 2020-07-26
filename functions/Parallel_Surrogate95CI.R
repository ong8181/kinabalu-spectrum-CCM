####
#### Kitayama et al. "Temperature is a dominant driver of distinct annual
#### seasonality of leaf litter production of equatorial tropical rain forests"
####

#### Calculate 95% CI for the surrogate (Parallel version)
## define function
ParSurciV3 <- function(effect.ts,
                       cause.ts,
                       surrogate.ts,
                       lib.parms = c(11, 15, 24, length(effect.ts)),
                       surrogate = c("effect", "cause"),
                       E.range = 2:12,
                       tp = 0,
                       calc.original.ccm.p = F,
                       original.ccm.res = NULL,
                       cores = parallel::detectCores(),
                       seed = NULL) {
  surrogate = match.arg(surrogate)
  Etar <- BestE(effect.ts, E = E.range, fig = F)
  Ecau <- BestE(cause.ts, E = E.range, fig = F)
  lib.m <- length(effect.ts)
  lib.size.s <- lib.parms
  surrogate.sum <- data.frame(L = lib.size.s)
  #delta.rho.sum <- data.frame(L=c(11,lib.size.s[2]))
  
  # do CCM for the surrogate data
  # for parallel version (2015.11.18)
  surrogate.all <- pforeach::pforeach(i = 1:ncol(surrogate.ts), .c = cbind,
                                      .cores = cores, .seed = seed)({
    if (surrogate == "effect") {
      target.sur <- surrogate.ts[, i]
      block <- cbind(target.sur, cause.ts)
    } else if (surrogate == "cause") {
      cause.sur <-  surrogate.ts[, i]
      block <- cbind(effect.ts, cause.sur)
    }
    m <- nrow(block)
    ccm.tar.cau <- rEDM::ccm(block, E = Etar, lib_sizes = lib.size.s,
        silent = T, tp = tp, RNGseed = seed)
    ccm.m <- rEDM::ccm_means(ccm.tar.cau, na.rm = T)[, c('lib_size', 'rho')]
    
    ## output
    rhos.tmp <- ccm.m$rho
  })
  
  #### calculate quantiles
  prob.seq <- c(0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.975, 0.99, 0.995, 1)
  na.check <- apply(surrogate.all, 1, function(x) {all(!is.na(x)) & all(is.finite(x))})
  min.lib <- which(na.check)[1]
  max.lib <- rev(which(na.check))[1]
  minL <- surrogate.sum$L[min.lib]
  maxL <- surrogate.sum$L[max.lib]
  rho.df <- data.frame(ter_rho = as.numeric(surrogate.all[max.lib, ]),
                       delta_rho = as.numeric(surrogate.all[max.lib, ] - surrogate.all[min.lib, ]))
  ter.rho.q <- quantile(rho.df$ter_rho, prob = prob.seq)
  conv.q <- quantile(rho.df$delta_rho, prob = prob.seq)
  conv.ter90mid <- median(rho.df$delta_rho[rho.df$ter_rho > ter.rho.q['90%']])
  conv.ter95mid <- median(rho.df$delta_rho[rho.df$ter_rho > ter.rho.q['95%']])
  conv.ter975mid <- median(rho.df$delta_rho[rho.df$ter_rho > ter.rho.q['97.5%']])
  
  if (calc.original.ccm.p) {
    ccm.res <- original.ccm.res
    ## use the identical maximum and minimum library length with the surrogate
    na.check0 <- !is.na(ccm.res$rho) & is.finite(ccm.res$rho)
    min.lib0 <- which(na.check0)[1]
    max.lib0 <- rev(which(na.check0))[1]
    minL0 <- ccm.res$lib_size[min.lib0]
    maxL0 <- ccm.res$lib_size[max.lib0]
    
    if (minL0 == minL & maxL0 == maxL) {
      d.rho.ori <- ccm.res$rho[max.lib0] - ccm.res$rho[min.lib0]
      t.rho.ori <- ccm.res$rho[max.lib0]
      ## calculate p-value of the original CCM
      d.rho.p <- sum(rho.df$delta_rho > d.rho.ori) / length(rho.df$delta_rho)
      t.rho.p <- sum(rho.df$ter_rho > t.rho.ori) / length(rho.df$ter_rho)
      combined.p <- sum(rho.df$delta_rho > d.rho.ori & rho.df$ter_rho > t.rho.ori) / length(rho.df$delta_rho)
      identical.libs <- T
    } else{
      warning("Library length of the original CCM should match that of surrogate CCM!")
      d.rho.ori <- ccm.res$rho[max.lib0] - ccm.res$rho[min.lib0]
      t.rho.ori <- ccm.res$rho[max.lib0]
      ## calculate p-value of the original CCM
      d.rho.p <- sum(rho.df$delta_rho > d.rho.ori) / length(rho.df$delta_rho)
      t.rho.p <- sum(rho.df$ter_rho > t.rho.ori) / length(rho.df$ter_rho)
      combined.p <- sum(rho.df$delta_rho > d.rho.ori & rho.df$ter_rho > t.rho.ori) / length(rho.df$delta_rho)
      identical.libs <- F
    }
    
    #### output results
    result.conf <- list(
      ter_rho_quantile = ter.rho.q,
      conv_quantile = conv.q,
      conv_ter90mid = conv.ter90mid,
      conv_ter95mid = conv.ter95mid,
      conv_ter975mid = conv.ter975mid,
      oriccm_d_rho_p = d.rho.p,
      oriccm_t_rho_p = t.rho.p,
      oriccm_comb_p = combined.p,
      ident_lib = identical.libs
    )
  } else{
    #### output results
    result.conf <- list(
      ter_rho_quantile = ter.rho.q,
      conv_quantile = conv.q,
      conv_ter90mid = conv.ter90mid,
      conv_ter95mid = conv.ter95mid,
      conv_ter975mid = conv.ter975mid
    )
  }
  
  return(result.conf)
}
