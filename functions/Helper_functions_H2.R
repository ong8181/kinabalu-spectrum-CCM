####
#### Kitayama et al. "Temperature is a dominant driver of distinct annual
#### seasonality of leaf litter production of equatorial tropical rain forests"
#### Helper functions H2
####

#### check best embedding dimension
BestE <- function(time.series,
                  E = 1:30,
                  fig = T,
                  ylim = range(-0.5, 1.2),
                  save.raw.data = F) {
  #time.series <- ts
  simplex.res <- rEDM::simplex(time.series, E = E, exclusion_radius = 0)
  if (fig) {
    plot(simplex.res$E, simplex.res$rho, ylim = ylim,
      type = "b", ylab = "Rho, MAE, RSME", xlab = "E")
    lines(simplex.res$E, simplex.res$mae, col = "red3", type = "b")
    lines(simplex.res$E, simplex.res$rmse, col = "royalblue", type = "b")
    legend(2, 1.5, c("rho", "mae", "rmse"), lty = 1, pch = 1, col = c("black", "red3", "royalblue"))
  }
  
  if (save.raw.data) {
    return(simplex.res)
  } else{
    bestE <- simplex.res[simplex.res$mae == min(simplex.res$mae), 'E']
    return(bestE)
  }
  ##return(simplex.res)
}

#### check best theta
bestT <- function(time.series,
                  th = c(0, 1e-04, 3e-04, 0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 0.5,
                         0.75, 1, 1.5, 2, 3, 4, 6, 8),
                  fig = F, tp = 1) {
  res.tmp <- rEDM::block_lnlp(time.series, method = "s-map", theta = 0, exclusion_radius = 0, num_neighbors = 0)
  smap.res <- data.frame(matrix(rep(NaN, length(th) * length(res.tmp)), ncol = length(res.tmp)))
  colnames(smap.res) <- names(res.tmp)
  for (i in 1:length(th)) {
    smap.res[i, ] <- rEDM::block_lnlp(time.series, method = "s-map", theta = th[i], exclusion_radius = 0, num_neighbors = 0, tp = tp)
  }
  if (fig) {
    plot(smap.res$theta, smap.res$mae, type = "b")
  }
  bestT <- smap.res[smap.res$mae == min(smap.res$mae), 'theta']
  return(bestT)
}



#### TE-map (theta and best E check)
TEMap <- function(time.series,
                  E = 1:30,
                  theta = c(0, 1e-04, 3e-04, 0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 0.5,
                         0.75, 1, 1.5, 2, 3, 4, 6, 8),
                  predict.skill = c("mae", "rho", "rmse")) {
  predict.skill = match.arg(predict.skill)
  smap.matrix <- matrix(rep(NaN, length(E) * length(theta)), ncol = length(E))
  if (predict.skill == "mae") {
    for (i in E)
      smap.matrix[, i] <- rEDM::s_map(time.series, E = i, exclusion_radius = 0)$mae
  }
  
  if (predict.skill == "rho") {
    for (i in E)
      smap.matrix[, i] <- rEDM::s_map(time.series, E = i, exclusion_radius = 0)$rho
  }
  
  if (predict.skill == "rmse") {
    for (i in E)
      smap.matrix[, i] <- rEDM::s_map(time.series, E = i, exclusion_radius = 0)$rmse
  }
  
  return(smap.matrix)
}

TEPlot <- function(smap.matrix) {
  smap.3d <- reshape2::melt(smap.matrix)
  names(smap.3d) <- c("theta", "E", "pred_skill")
  smap.3d$theta <- as.numeric(smap.3d$theta)
  smap.3d$E <- as.numeric(smap.3d$E)
  v <- ggplot2::ggplot(smap.3d, ggplot2::aes(E, theta, z = "pred_skill"))
  v <- v + ggplot2::geom_tile(ggplot2::aes(fill = pred_skill)) + ggplot2::stat_contour()
  return(v)
}

TEMelt <- function(smap.matrix) {
  smap.3d <- reshape2::melt(smap.matrix)
  names(smap.3d) <- c("theta", "E", "pred_skill")
  smap.3d$theta <- as.numeric(smap.3d$theta)
  smap.3d$E <- as.numeric(smap.3d$E)
  return(smap.3d)
}
