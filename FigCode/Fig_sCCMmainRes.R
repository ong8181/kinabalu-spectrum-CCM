####
#### Kitayama et al. "Temperature is a dominant driver of distinct annual
#### seasonality of leaf litter production of equatorial tropical rain forests"
#### Figure code for spectrum CCM, main result
####

# Load environments
load("../06_out/06_Fourier_CCMcompile_FFT.RData")

# Load libraries
library(ggplot2)
library(cowplot)
library(gridExtra)
library(reshape2)
library(ggsci)

# Load helper functions for visualization
source('0_FigFunctions/FigFunctions.R')

# Exclude relative humidity from the main figure
st.cause2 <- st.cause[st.cause$climate_data != "relative_humidity",]

# Replace site and climate name
st.cause2$litter_site2 <- NaN
st.cause2[st.cause2$litter_site == "POR",]$litter_site2 <- rep("700 m Sed.", 4)
st.cause2[st.cause2$litter_site == "PHQ",]$litter_site2 <- rep("1700 m Sed.", 2)
st.cause2[st.cause2$litter_site == "ULA",]$litter_site2 <- rep("1700 m Qua.", 2)
st.cause2[st.cause2$litter_site == "BAB",]$litter_site2 <- rep("1700 m Ult.", 1)
st.cause2[st.cause2$litter_site == "PAK",]$litter_site2 <- rep("3100 m Gra.", 1)
st.cause2[st.cause2$litter_site == "HEL",]$litter_site2 <- rep("3100 m Ult.", 1)
st.cause2$litter_site2 <- factor(st.cause2$litter_site2, levels = c("700 m Sed.", "1700 m Sed.", "1700 m Qua.", "1700 m Ult.", "3100 m Gra.", "3100 m Ult."))

st.cause2$climate_data2 <- NaN
st.cause2[st.cause2$climate_data == "ATC",]$climate_data2 <- rep("Mean temperature", 5)
st.cause2[st.cause2$climate_data == "Corrected_SD_kPa",]$climate_data2 <- rep("Sat. Def.", 2)
st.cause2[st.cause2$climate_data == "PAR",]$climate_data2 <- rep("PAR", 1)
st.cause2[st.cause2$climate_data == "ET0_potential_evp",]$climate_data2 <- rep("Potential ET", 3)
st.cause2$climate_data2 <- factor(st.cause2$climate_data2, levels = c("Mean temperature", "Potential ET", "Sat. Def.", "PAR"))

# Generate plot
g1 <- ggplot(st.cause2, aes(y = rho, x = climate_data2, group = litter_site2))
g1 <- g1 + geom_point(shape = 18, size = 4) + geom_text(aes(label = tp / 2), vjust = 3, colour = 1)
g1 <- style_plot(g1) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
g1 <- g1 + facet_grid(. ~ litter_site2) + ylab("Cross-map skill (ρ)") + xlab("Climate variable") + ylim(0, 1)
g1 <- g1 + geom_hline(yintercept = 0, linetype = 2)
comment(g1) <- causality.comment

# Figure output
quartz(type = "pdf", file = "0_RawFigs/Fig_fCCMResult.pdf", width = 10, height = 4)
g1
dev.off()
