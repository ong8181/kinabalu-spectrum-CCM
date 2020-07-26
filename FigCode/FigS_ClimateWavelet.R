####
#### Kitayama et al. "Temperature is a dominant driver of distinct annual
#### seasonality of leaf litter production of equatorial tropical rain forests"
#### Figure code for Supplementary Information: Wavelet analysis of leaf litter
####

# Load environments
load("../08_out/08_WaveletClimateOut.RData")

# Load libraries
library(ggplot2); packageVersion("ggplot2")
library(cowplot); packageVersion("cowplot"); theme_set(theme_cowplot())
library(gridExtra); packageVersion("gridExtra")
library(reshape2); packageVersion("reshape2")
library(ggsci); packageVersion("ggsci")
library(lubridate); packageVersion("lubridate")
library(ggimage); packageVersion("ggimage") # 0.2.7, 2020.5.27

# Load helper functions for visualization
source('0_FigFunctions/FigFunctions.R')

# Load saved wavelet images
temp.w1 <- image_read2("../08_out/JPEG/TempWave700.jpg")
temp.w2 <- image_read2("../08_out/JPEG/TempWave1700.jpg")
temp.w3 <- image_read2("../08_out/JPEG/TempWave2700.jpg")
temp.w4 <- image_read2("../08_out/JPEG/TempWave3100.jpg")

# Combert images to ggplot objects
t1 <- ggdraw() + draw_image(temp.w1) + theme(plot.margin = margin(14,2,2,2, "pt"))
t2 <- ggdraw() + draw_image(temp.w2) + theme(plot.margin = margin(14,2,2,2, "pt"))
t3 <- ggdraw() + draw_image(temp.w3) + theme(plot.margin = margin(14,2,2,2, "pt"))
t4 <- ggdraw() + draw_image(temp.w4) + theme(plot.margin = margin(14,2,2,2, "pt"))


# Load saved wavelet images
evp.w1 <- image_read2("../08_out/JPEG/EvpWave700.jpg")
evp.w2 <- image_read2("../08_out/JPEG/EvpWave1700.jpg")
evp.w3 <- image_read2("../08_out/JPEG/EvpWave2700.jpg")
evp.w4 <- image_read2("../08_out/JPEG/EvpWave3100.jpg")

# Combert images to ggplot objects
e1 <- ggdraw() + draw_image(evp.w1) + theme(plot.margin = margin(14,2,2,2, "pt"))
e2 <- ggdraw() + draw_image(evp.w2) + theme(plot.margin = margin(14,2,2,2, "pt"))
e3 <- ggdraw() + draw_image(evp.w3) + theme(plot.margin = margin(14,2,2,2, "pt"))
e4 <- ggdraw() + draw_image(evp.w4) + theme(plot.margin = margin(14,2,2,2, "pt"))

# Combine figures
climate.w.all <- plot_grid(t4, e4,
                           t3, e3,
                           t2, e2,
                           t1, e1,
                           ncol = 2, align = "hv")

# Figure output
ggsave(file = "0_RawFigs/FigS_ClimateWavelet.pdf", plot = climate.w.all, width = 6, height = 7)

