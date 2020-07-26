####
#### Kitayama et al. "Temperature is a dominant driver of distinct annual
#### seasonality of leaf litter production of equatorial tropical rain forests"
#### No.00.2: Baseline correction : Additive model correction
####

source(file.path("..", "config.R"), chdir = TRUE)

#### load workspace
load(config$out.001.ave.rdata.path)

#### load library
library(mgcv)

#### additive model
## column = 6:...
## data = cli.car.biw.avr
##        cli.lab.biw.avr
##        cli.phq.biw.avr
##        cli.por.biw.avr
# composite all dataset
climate.not.correct <- list(car = cli.car.biw.avr,
                            lab = cli.lab.biw.avr,
                            phq = cli.phq.biw.avr,
                            por = cli.por.biw.avr)
## Prepare corrected list
climate.correct <- climate.not.correct


## select site
for (site.name in config$kSiteNamesClimate) {
  dir.create(config$out.002.site.dir[[site.name]],
             showWarnings = FALSE,
             recursive = TRUE)
  # extract data from one site
  d.tmp <- as.data.frame(climate.not.correct[[site.name]])
  
  #### Perform additive model (with GCV)
  # use GCV criteria
  for (i in 6:ncol(d.tmp)) {
    tmp.gam <- gam(d.tmp[, i] ~ s(seq(1, nrow(d.tmp))), data = d.tmp)
    # Alternative : define the number of knot (k)
    # tmp.gam <- gam(ATC~s(seq(1,nrow(d.tmp)), k=20), data=d.tmp)
    model.x <- tmp.gam$model[, 2]
    model.y <- tmp.gam$model[, 1]
    pred.y <- predict(tmp.gam)
    corrected.y <- resid(tmp.gam)
    
    if (T) {
      # pdf title
      quartz.title <- sprintf("kinabalu_corrected_TS/climate_%s_correct/%s_%s_correct.pdf",
          site.name, colnames(d.tmp)[i], site.name)
      if (Sys.info()['sysname'] == "Darwin") {
        quartz(file = quartz.title, type = "pdf", width = 7, height = 7)
      } else {
        pdf(file = quartz.title, width = 7, height = 7)
      }
      
      # show corrected and original values
      op <- par(mfrow = c(2, 1))
      plot(model.x, pred.y,
        type = "l", col = 2, lwd = 2,
        ylim = range(min(d.tmp[, i], na.rm = T), max(d.tmp[, i], na.rm = T)),
        ylab = colnames(d.tmp)[i], xlab = "Time index",
        main = sprintf("%s and regression (red line)", colnames(d.tmp)[i]))
      lines(model.x, model.y, col = 1)
      
      plot(model.x, resid(tmp.gam), type = "l",
        ylab = colnames(d.tmp)[i],
        xlab = "Time index",
        main = sprintf("%s, Corrected (Residuals)", colnames(d.tmp)[i]))
      par(op)
      dev.off()
    }
    
    
    # generate NA including data
    cor.na.data0 <- as.data.frame(cbind(model.x, model.y, corrected.y))
    cor.na.data <- data.frame(time_index = 1:nrow(d.tmp))
    cor.na.data$model.y <- NaN
    cor.na.data$corrected.y <- NaN
    
    for (i.na in 1:nrow(cor.na.data)) {
      select.data <- cor.na.data0[cor.na.data0[, "model.x"] == cor.na.data[i.na, "time_index"], ]
      if (nrow(select.data) < 1) {
        cor.na.data[i.na, "model.y"] <- NA
        cor.na.data[i.na, "corrected.y"] <- NA
      } else{
        select.data <- as.data.frame(select.data)
        cor.na.data[i.na, "model.y"] <- select.data[1, "model.y"]
        cor.na.data[i.na, "corrected.y"] <- select.data[1, "corrected.y"]
      }
    }
    
    # input corrected data
    climate.correct[[site.name]][, i] <- cor.na.data$corrected.y
  }
}

config$End()

config.00.2 <- config
rm("config")

#### save image
save.image(file = config.00.2$out.002.rdata.path)

#### save session info
writeLines(capture.output(sessionInfo()),
           sprintf("../00_0_SessionInfo/01_SessionInfo_CorrectData_%s.txt", substr(Sys.time(), 1, 10)))
