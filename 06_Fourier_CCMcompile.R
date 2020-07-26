####
#### Kitayama et al. "Temperature is a dominant driver of distinct annual
#### seasonality of leaf litter production of equatorial tropical rain forests"
#### No.6: Compile CCM results
####

source("config.R")

#### load environments
load(config$out.05.fft.rdata.path)

##########################
#### Upload libraries ####
##########################
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(timeSeries)
library(cowplot)
library(reshape2)
dir.create(config$out.06.dir, showWarnings = FALSE)

#### object information
## all ccm data output
# seasonality ccm output
ccm.out <- seasonality.ccm.output # replace object

# read seasonality result
conf.all.climate # climate seasonality
conf.all.litter # litter seasonality

## causality criteria
causality <- config$CausalityCriteria(ccm.out)$cond
causality.comment <- config$CausalityCriteria(ccm.out)$comment

#### Extract significant interaction between litter and climate
strongest.causality <- data.frame()
## porling
ccm.cli.sites <- list(
  c("por", "POR"),
  c("phq", "PHQ"),
  c("phq", "BAB"),
  c("phq", "ULA"),
  c("car", "RTM"),
  c("car", "CAR"),
  c("lab", "PAK"),
  c("lab", "HEL"))

for (i in 1:length(ccm.cli.sites)) {
  cli.site <- ccm.cli.sites[[i]][1]
  ccm.site <- ccm.cli.sites[[i]][2]
  
  sig.climate <- conf.all.climate$sig_null == T # significantly larger than the null spectrum
  annual.cycle <- conf.all.climate$cycle_dom > 11 & conf.all.climate$cycle_dom < 13 # climage must show annual seasonality

  sig.IDs <- conf.all.climate[sig.climate & conf.all.climate$site == cli.site & annual.cycle, "ID"]

  ccm.causality.0 <- ccm.out$climate_data
  match.climate <- !is.na(match(ccm.causality.0, sig.IDs))
  ccm.causality.1 <- ccm.out[causality & ccm.out$litter_site == ccm.site & match.climate, ]
  
  ## Extract the strongest causality
  unique.climate <- unique(ccm.causality.1$climate_data)
  strongest.causality.0 <- data.frame()
  for (i in unique.climate) {
    tmp1 <- ccm.causality.1[ccm.causality.1$climate_data == i, ]
    strongest.causality.0 <- rbind(strongest.causality.0, tmp1[which.max(tmp1$rho), ])
  }
  
  strongest.causality <- rbind(strongest.causality, strongest.causality.0)
}

st.cause <- strongest.causality

#### Generate plot
g1 <- ggplot(st.cause, aes( y = rho, x = climate_data, group = litter_site, colour = climate_site))
g1 <- g1 + geom_point(shape = "*", size = 10) + geom_text(aes(label = tp / 2), vjust = 3, colour = 1)
g1 <- StylePlot(g1) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
g1 <- g1 + facet_grid(. ~ litter_site) + ylab("Cross-map skill (ρ)") + xlab("Climate variable") + ylim(0, 1)
g1 <- g1 + geom_hline(yintercept = 0, linetype = 2)
comment(g1) <- causality.comment

config$End()

config.06 <- config
rm("config")

#### save image
save.image(file = config.06$out.06.rdata.path)

#### save session info
writeLines(capture.output(sessionInfo()),
           sprintf("00_0_SessionInfo/06_SessionInfo_CCMCompile_%s.txt", substr(Sys.time(), 1, 10)))

