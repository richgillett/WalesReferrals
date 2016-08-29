# Referrals in NHS Wales, Rich Gillett
# https://statswales.gov.wales/v/BpON
# you'll have to export as csv from StatsWales, and clean the dates
# remove any summary totals
#rm(list = ls())

setwd("C:/Users/Rich/Documents/R/Referrals")

#read the data which is from statswales
raw <- read.csv("StatsWalesCleaned.csv", header=TRUE, na.string=c("."))
raw[1:5,1:5]

require(plyr)
#fix minor import problem
raw <-rename (raw, c("Betsi.Cadwaladr.University.Local.Health.Board"="HB"))

IndividualHB <- subset(raw,HB="Betsi.Cadwaladr.University.Local.Health.Board")

IndividualHB <- subset(IndividualHB, select = -c(HB))

numbercols <-ncol(IndividualHB)

IndividualHBts <- ts(IndividualHB,start=c(2012,4), end=c(2016,6), frequency=12)

forecastperiod <- 24

fcast <- matrix(NA, nrow=forecastperiod, ncol(IndividualHB))
fcastcolnames <- colnames(IndividualHB)
# export the image

for(i in 1:numbercols) {
  
  fcast[,i] <- forecast(IndividualHBts[,i], h=forecastperiod, robust=TRUE)$mean
  
  write((fcast[,i]),file=paste(fcastcolnames[i],".csv"),sep=",",ncol=numbercols)
  
  img <-    png(filename=paste(fcastcolnames[i],"plot.png"), 
                units="in", 
                width=5, 
                height=4, 
                pointsize=12, 
                res=72)
                plot.forecast(forecast(IndividualHBts[,i], 
                h=forecastperiod, 
                robust=TRUE),
                main=fcastcolnames[i])
                dev.off()
}
