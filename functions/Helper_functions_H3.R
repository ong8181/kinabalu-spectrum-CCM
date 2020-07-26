####
#### Kitayama et al. "Temperature is a dominant driver of distinct annual
#### seasonality of leaf litter production of equatorial tropical rain forests"
#### Helper functions H3
####

#### depends = "imputeTS"
CallSimParms <- function(parm.set.name = c("set1",
                                           "tp_test",
                                           "tp_test_normalCCM",
                                           "main_fCCM",
                                           "main_normalCCM")) {
  parm.set.name <- match.arg(parm.set.name)
  if (parm.set.name == "set1") {
    yl <- 10 # define year length
    num.iter <- 1000
    ccm.tp <- -1
    error.rate.seq <- seq(0, 0.6, by = 0.1)
    force.seasonality.seq <- seq(0, 3, by = 0.3)
    sim.replication <- 10
    
  } else if (parm.set.name == "tp_test") {
    yl <- 10 # define year length
    num.iter <- 1000
    ccm.tp <- -36:12
    error.rate.seq <- 0
    force.seasonality.seq <- 1
    sim.replication <- 100
    
  } else if (parm.set.name == "tp_test_normalCCM") {
    yl <- 10 # define year length
    num.iter <- 1000
    ccm.tp <- -36:12
    error.rate.seq <- 0
    force.seasonality.seq <- 1
    sim.replication <- 100
    
  } else if (parm.set.name == "main_fCCM") {
    yl <- 10 # define year length
    num.iter <- 1000
    ccm.tp <- -1
    error.rate.seq <- seq(0, 0.5, by = 0.05)
    force.seasonality.seq <- seq(0, 2, by = 0.2)
    sim.replication <- 100
    
  } else if (parm.set.name == "main_normalCCM") {
    yl <- 10 # define year length
    num.iter <- 1000
    ccm.tp <- -1
    error.rate.seq <- seq(0, 0.5, by = 0.05)
    force.seasonality.seq <- seq(0, 2, by = 0.2)
    sim.replication <- 100
  }
  
  total.sim <- length(error.rate.seq) * length(force.seasonality.seq) * sim.replication
  
  #### output simulation conditions
  sim.conditions <- list(
    data.length = yl,
    num.iter.surrogate = num.iter,
    ccm.tp = ccm.tp,
    error.rates = error.rate.seq,
    force.seasonalities = force.seasonality.seq,
    simulation.reps = sim.replication,
    total.simulations = total.sim
  )
  
  return(sim.conditions)
}
