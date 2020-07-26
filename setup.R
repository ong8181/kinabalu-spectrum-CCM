####
#### Setup codes
####

req.packages <- c(
  "DescTools",
  "RColorBrewer",
  "circular",
  "cowplot",
  "deSolve",
  "fields",
  "ggplot2",
  "grid",
  "gridExtra",
  "gtools",
  "imputeTS",
  "mgcv",
  "pforeach",
  "plyr",
  "psych",
  "rEDM",
  "reshape",
  "reshape2",
  "timeSeries",
  "truncnorm",
  "tseriesChaos",
  "zoo"
)

all.packages <- installed.packages()[, 1]
req.install.packages <- req.packages[!req.packages %in% all.packages]

if ("pforeach" %in% req.install.packages) {
  if (!"devtools" %in% all.packages) {
    install.packages("devtools")
  }
  devtools::install_github("hoxo-m/pforeach")
  req.install.packages <- req.install.packages[!req.install.packages %in% "pforeach"]
}

if (length(req.install.packages)) {
  install.packages(req.install.packages)
}
