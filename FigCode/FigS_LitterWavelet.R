####
#### Kitayama et al. "Temperature is a dominant driver of distinct annual
#### seasonality of leaf litter production of equatorial tropical rain forests"
#### Figure code for Supplementary Information: Wavelet analysis
####

# Load environments
load("../07_out/07_WaveletLitterOut.RData")

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
leaf.w1 <- image_read2("../07_out/JPEG/LeafWave700S.jpg")
leaf.w2 <- image_read2("../07_out/JPEG/LeafWave1700S.jpg")
leaf.w3 <- image_read2("../07_out/JPEG/LeafWave2700S.jpg")
leaf.w4 <- image_read2("../07_out/JPEG/LeafWave3100S.jpg")
leaf.w5 <- image_read2("../07_out/JPEG/LeafWave700U.jpg")
leaf.w6 <- image_read2("../07_out/JPEG/LeafWave1700U.jpg")
leaf.w7 <- image_read2("../07_out/JPEG/LeafWave2700U.jpg")
leaf.w8 <- image_read2("../07_out/JPEG/LeafWave3100U.jpg")
leaf.w9 <- image_read2("../07_out/JPEG/LeafWave1700Q.jpg")

# Combert images to ggplot objects
w1 <- ggdraw() + draw_image(leaf.w1) + theme(plot.margin = margin(14,2,2,2, "pt"))
w2 <- ggdraw() + draw_image(leaf.w2) + theme(plot.margin = margin(14,2,2,2, "pt"))
w3 <- ggdraw() + draw_image(leaf.w3) + theme(plot.margin = margin(14,2,2,2, "pt"))
w4 <- ggdraw() + draw_image(leaf.w4) + theme(plot.margin = margin(14,2,2,2, "pt"))
w5 <- ggdraw() + draw_image(leaf.w5) + theme(plot.margin = margin(14,2,2,2, "pt"))
w6 <- ggdraw() + draw_image(leaf.w6) + theme(plot.margin = margin(14,2,2,2, "pt"))
w7 <- ggdraw() + draw_image(leaf.w7) + theme(plot.margin = margin(14,2,2,2, "pt"))
w8 <- ggdraw() + draw_image(leaf.w8) + theme(plot.margin = margin(14,2,2,2, "pt"))
w9 <- ggdraw() + draw_image(leaf.w9) + theme(plot.margin = margin(14,2,2,2, "pt"))


# Load saved wavelet images
flower.w1 <- image_read2("../07_out/JPEG/FlowerWave700S.jpg")
flower.w2 <- image_read2("../07_out/JPEG/FlowerWave1700S.jpg")
flower.w3 <- image_read2("../07_out/JPEG/FlowerWave2700S.jpg")
flower.w4 <- image_read2("../07_out/JPEG/FlowerWave3100S.jpg")
flower.w5 <- image_read2("../07_out/JPEG/FlowerWave700U.jpg")
flower.w6 <- image_read2("../07_out/JPEG/FlowerWave1700U.jpg")
flower.w7 <- image_read2("../07_out/JPEG/FlowerWave2700U.jpg")
flower.w8 <- image_read2("../07_out/JPEG/FlowerWave3100U.jpg")
flower.w9 <- image_read2("../07_out/JPEG/FlowerWave1700Q.jpg")

# Combert images to ggplot objects
f1 <- ggdraw() + draw_image(flower.w1) + theme(plot.margin = margin(14,2,2,2, "pt"))
f2 <- ggdraw() + draw_image(flower.w2) + theme(plot.margin = margin(14,2,2,2, "pt"))
f3 <- ggdraw() + draw_image(flower.w3) + theme(plot.margin = margin(14,2,2,2, "pt"))
f4 <- ggdraw() + draw_image(flower.w4) + theme(plot.margin = margin(14,2,2,2, "pt"))
f5 <- ggdraw() + draw_image(flower.w5) + theme(plot.margin = margin(14,2,2,2, "pt"))
f6 <- ggdraw() + draw_image(flower.w6) + theme(plot.margin = margin(14,2,2,2, "pt"))
f7 <- ggdraw() + draw_image(flower.w7) + theme(plot.margin = margin(14,2,2,2, "pt"))
f8 <- ggdraw() + draw_image(flower.w8) + theme(plot.margin = margin(14,2,2,2, "pt"))
f9 <- ggdraw() + draw_image(flower.w9) + theme(plot.margin = margin(14,2,2,2, "pt"))

# Combine figures
leaf.w.all <- plot_grid(w4, w8, NULL,
                        w3, w7, NULL,
                        w2, w6, w9,
                        w1, w5, NULL,
                        ncol = 3, align = "hv")

flower.w.all <- plot_grid(f4, f8, NULL,
                          f3, f7, NULL,
                          f2, f6, f9,
                          f1, f5, NULL,
                          ncol = 3, align = "hv")

# Figure output
ggsave(file = "0_RawFigs/FigS_LeafWavelet.pdf", plot = leaf.w.all, width = 9, height = 7)
ggsave(file = "0_RawFigs/FigS_FlowerWavelet.pdf", plot = flower.w.all, width = 9, height = 7)

