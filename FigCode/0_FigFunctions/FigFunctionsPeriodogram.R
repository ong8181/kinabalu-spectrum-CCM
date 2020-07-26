####
#### Kitayama et al. "Temperature is a dominant driver of distinct annual
#### seasonality of leaf litter production of equatorial tropical rain forests"
#### Functions for periodogram figures
####

# Define functions
style_plot <-  function(ggobject){
  return(ggobject + theme_bw() + theme(axis.text.x = element_text(angle=0),
                                       panel.grid.major = element_blank(),
                                       panel.grid.minor = element_blank(),
                                       axis.text = element_text(size=12),
                                       axis.title = element_text(size=12),
                                       panel.background=element_rect(colour="black", fill=NA, size=0.8)))
}

# For periodogram of litter
PeriLitFig2 <- function(csv.data,
                        peri.signif.data,
                        plot.title,
                        r = c(0,3,16),
                        h = 0.8,
                        w = 0.5,
                        scolor = c("gray70", "red")){ # Heatmap version
  # Rename litter label
  csv.data$ID2 <- NaN
  csv.data$ID2[csv.data$ID == "leaf"] <- "Leaf"
  csv.data$ID2[csv.data$ID == "flower"] <- "Flower"
  csv.data$ID2 <- factor(csv.data$ID2, levels = unique(csv.data$ID2))
  colnames(csv.data)[4] <- "Power"
  
  # Extract dominant cycles
  dom.df <- data.frame()
  csv.list <- split(csv.data, csv.data$ID2:csv.data$site)
  max.Power.id <- sapply(csv.list, function(x) which.max(x$Power))
  
  for(list.i in 1:length(max.Power.id)){
    dom.x1 <- csv.list[[list.i]]$freq_inv[max.Power.id[list.i]]
    dom.x2 <- unique(csv.list[[list.i]]$ID2)
    dom.x3 <- unique(csv.list[[list.i]]$site)
    if(dom.x2 == "Flower") dom.y1 <- 1-h+0.1
    if(dom.x2 == "Leaf") dom.y1 <- 2-h+0.1
    dom.df0 <- data.frame(x = dom.x1,
                          y = dom.y1,
                          ID2 = dom.x2,
                          site = dom.x3)
    dom.df <- rbind(dom.df, dom.df0)
  }
  
  peri.signif.data <- peri.signif.data[order(peri.signif.data$ID),]

  dom.df$signif.col <- "NS"
  dom.df$signif.col[peri.signif.data$sig_null] <- "S"
  dom.df$signif.col <- factor(dom.df$signif.col, levels = c("NS", "S"))

  p1 <- ggplot(csv.data, aes(x = freq_inv, y = ID2, group = site))
  p1 <- p1 + geom_vline(xintercept = 12, linetype = 2, size = 0.5, colour = "black") +
    geom_tile(aes(fill=Power), height = h, width = w) + facet_wrap(.~site, scales = "fixed") + 
    geom_point(data = dom.df, aes(x, y, group = ID2, colour = signif.col), shape = 17, size = 3, show.legend = F) +
    scale_colour_manual(values = scolor) +
    scale_fill_gradientn(colours = c("royalblue", "white", "red3", "yellow"), limits = c(r[1], rev(r)[1]), values = rescale(r), oob = squish) +
    ggtitle(plot.title) + xlim(0,60) +
    ylab(NULL) + 
    xlab("Cycle length (month)") #+ theme(axis.text.x = element_text(angle = 45, hjust = 1))

  return(p1)
}


#---------------- For periodogram of climate ----------------#
# Heatmap version
PeriCliFig2 <- function(csv.data,
                        peri.signif.data,
                        plot.title,
                        r = c(0,3,16),
                        h = 0.8,
                        w = 0.5,
                        scolor = c("gray70", "red")){ # Heatmap version
  # Rename litter label
  csv.data$ID2 <- peri.signif.data$ID2 <- NaN
  csv.data$ID2[csv.data$ID == "ATC"] <- peri.signif.data$ID2[peri.signif.data$ID == "ATC"] <- "Mean temp."
  csv.data$ID2[csv.data$ID == "Act_VP_kPa"] <-  peri.signif.data$ID2[peri.signif.data$ID == "Act_VP_kPa"] <- "Actual VP"
  csv.data$ID2[csv.data$ID == "Corrected_SD_kPa"] <- peri.signif.data$ID2[peri.signif.data$ID == "Corrected_SD_kPa"] <- "Sat. Def."
  csv.data$ID2[csv.data$ID == "PAR"] <- peri.signif.data$ID2[peri.signif.data$ID == "PAR"] <-  "PAR"
  csv.data$ID2[csv.data$ID == "ET0_potential_evp"] <- peri.signif.data$ID2[peri.signif.data$ID == "ET0_potential_evp"] <-  "Potential ET"
  csv.data$ID2[csv.data$ID == "rain"] <- peri.signif.data$ID2[peri.signif.data$ID == "rain"] <-  "Rain"
  
  csv.data$ID2 <- factor(csv.data$ID2,
                         levels = c("Mean temp.",
                                   "Potential ET",
                                   "Actual VP",
                                   "Sat. Def.",
                                   "PAR",
                                   "Rain"))  
  peri.signif.data$ID2 <- factor(peri.signif.data$ID2,
                         levels = c("Mean temp.",
                                    "Potential ET",
                                    "Actual VP",
                                    "Sat. Def.",
                                    "PAR",
                                    "Rain"))  
  
  colnames(csv.data)[4] <- "Power"

  # Extract dominant cycles
  dom.df <- data.frame()
  csv.list <- split(csv.data, csv.data$ID2)
  max.Power.id <- sapply(csv.list, function(x) which.max(x$Power))
  
  for(list.i in 1:length(max.Power.id)){
    dom.x1 <- csv.list[[list.i]]$freq_inv[max.Power.id[list.i]]
    dom.x2 <- unique(csv.list[[list.i]]$ID2)
    if(dom.x2 == "Mean temp.")    dom.y1 <- 1-h+0.1
    if(dom.x2 == "Potential ET")  dom.y1 <- 2-h+0.1
    if(dom.x2 == "Actual VP")     dom.y1 <- 3-h+0.1
    if(dom.x2 == "Sat. Def.")     dom.y1 <- 4-h+0.1
    if(dom.x2 == "PAR")           dom.y1 <- 5-h+0.1
    if(dom.x2 == "Rain")          dom.y1 <- 6-h+0.1
    dom.df0 <- data.frame(x = dom.x1,
                          y = dom.y1,
                          ID2 = dom.x2)
    dom.df <- rbind(dom.df, dom.df0)
  }
  
  peri.signif.data <- peri.signif.data[order(peri.signif.data$ID2),]
  
  dom.df$signif.col <- "NS"
  dom.df$signif.col[peri.signif.data$sig_null] <- "S"
  dom.df$signif.col <- factor(dom.df$signif.col, levels = c("NS", "S"))
  
  p1 <- ggplot(csv.data, aes(x = freq_inv, y = ID2))
  p1 <- p1 + geom_vline(xintercept = 12, linetype = 2, size = 0.5, colour = "black") +
    geom_tile(aes(fill=Power), height = h, width = w) + #facet_wrap(.~site, scales = "fixed") + 
    geom_point(data = dom.df, aes(x, y, group = ID2, colour = signif.col), shape = 17, size = 3, show.legend = F) +
    scale_colour_manual(values = scolor) +
    scale_fill_gradientn(colours = c("royalblue", "white", "red3", "yellow"), limits = c(r[1], rev(r)[1]), values = rescale(r), oob = squish) +
    ggtitle(plot.title) + xlim(0,60) +
    ylab(NULL) + 
    xlab("Cycle length (month)") #+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p1)
}


#---------- For smoothing tile figure ----------#
Unzip <- function(...) rbind(data.frame(), ...)

PeriSmoothLitter <- function(csv.data, np = 1000, ver = "inv"){
  if(ver == "inv"){
    csv.data$freq_inv <- (1/csv.data$freq)/2
    csv.list <- split(csv.data, csv.data$ID:csv.data$site)
    csv.list <- csv.list[sapply(csv.list, nrow) > 0]
    
    # Linear interpolation
    csv.approx <- list()
    
    for(list.i in 1:length(csv.list)){
      csv.approx.tmp1 <- approx(x = csv.list[[list.i]]$freq_inv,
                                y = csv.list[[list.i]]$spec,
                                n = np, method = "linear")
      csv.approx.tmp2 <- approx(x = csv.list[[list.i]]$freq_inv,
                                y = csv.list[[list.i]]$spec_norm,
                                n = np, method = "linear")
      
      csv.approx.df <- data.frame(freq = 2/csv.approx.tmp2$x,
                                  freq_inv = csv.approx.tmp2$x,
                                  spec = csv.approx.tmp1$y,
                                  spec_norm = csv.approx.tmp2$y,
                                  ID = unique(csv.list[[list.i]]$ID),
                                  site = unique(csv.list[[list.i]]$site))
      csv.approx <- c(csv.approx, list(csv.approx.df))
    }    
  }else{
    csv.list <- split(csv.data, csv.data$ID:csv.data$site)
    csv.list <- csv.list[sapply(csv.list, nrow) > 0]
    
    # Linear interpolation
    csv.approx <- list()
    
    for(list.i in 1:length(csv.list)){
      csv.approx.tmp1 <- approx(x = csv.list[[list.i]]$freq,
                                y = csv.list[[list.i]]$spec,
                                n = np, method = "linear")
      csv.approx.tmp2 <- approx(x = csv.list[[list.i]]$freq,
                                y = csv.list[[list.i]]$spec_norm,
                                n = np, method = "linear")
      
      csv.approx.df <- data.frame(freq = csv.approx.tmp2$x,
                                  freq_inv = (1/csv.approx.tmp2$x)/2,
                                  spec = csv.approx.tmp1$y,
                                  spec_norm = csv.approx.tmp2$y,
                                  ID = unique(csv.list[[list.i]]$ID),
                                  site = unique(csv.list[[list.i]]$site))
      csv.approx <- c(csv.approx, list(csv.approx.df))
    }    
  }

  
  unzip.csv <- do.call(Unzip, csv.approx)

  return(unzip.csv)
}



PeriSmoothClimate <- function(csv.data, np = 1000, ver = "inv"){
  if(ver == "inv"){
    csv.data$freq_inv <- (1/csv.data$freq)/2
    csv.list <- split(csv.data, csv.data$ID)
    csv.list <- csv.list[sapply(csv.list, nrow) > 0]
    
    # Linear interpolation
    csv.approx <- list()
    
    for(list.i in 1:length(csv.list)){
      csv.approx.tmp1 <- approx(x = csv.list[[list.i]]$freq_inv,
                                y = csv.list[[list.i]]$spec,
                                n = np, method = "linear")
      csv.approx.tmp2 <- approx(x = csv.list[[list.i]]$freq_inv,
                                y = csv.list[[list.i]]$spec_norm,
                                n = np, method = "linear")
      
      csv.approx.df <- data.frame(freq = 2/csv.approx.tmp2$x,
                                  freq_inv = csv.approx.tmp2$x,
                                  spec = csv.approx.tmp1$y,
                                  spec_norm = csv.approx.tmp2$y,
                                  ID = unique(csv.list[[list.i]]$ID),
                                  site = unique(csv.list[[list.i]]$site))
      csv.approx <- c(csv.approx, list(csv.approx.df))
    }    
  }else{
    csv.list <- split(csv.data, csv.data$ID)
    csv.list <- csv.list[sapply(csv.list, nrow) > 0]
    
    # Linear interpolation
    csv.approx <- list()
    
    for(list.i in 1:length(csv.list)){
      csv.approx.tmp1 <- approx(x = csv.list[[list.i]]$freq,
                                y = csv.list[[list.i]]$spec,
                                n = np, method = "linear")
      csv.approx.tmp2 <- approx(x = csv.list[[list.i]]$freq,
                                y = csv.list[[list.i]]$spec_norm,
                                n = np, method = "linear")
      
      csv.approx.df <- data.frame(freq = csv.approx.tmp2$x,
                                  freq_inv = (1/csv.approx.tmp2$x)/2,
                                  spec = csv.approx.tmp1$y,
                                  spec_norm = csv.approx.tmp2$y,
                                  ID = unique(csv.list[[list.i]]$ID),
                                  site = unique(csv.list[[list.i]]$site))
      csv.approx <- c(csv.approx, list(csv.approx.df))
    }
    
  }
  unzip.csv <- do.call(Unzip, csv.approx)

  return(unzip.csv)
}



