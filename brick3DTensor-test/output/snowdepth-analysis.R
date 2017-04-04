# TODO: Add comment
# 
# Author: ecor
###############################################################################
library(ggplot2)

datacsv <-   '/home/ecor/Dropbox/R-packages/OpenRendena100/brick3DTensor-test/output/alt_snow50_vs_time_daily.csv' 

datas <- read.table(datacsv,sep=",",header=TRUE,stringsAsFactors=FALSE)

datas$time <- as.Date(datas$time)
datas$month <- months(datas$time,abbreviate=TRUE)
datas$monthsc <- as.character(datas$time,format="%Y-%m")


N <- datas[datas$aspect=="N",]
S <- datas[datas$aspect=="S",]
E <- datas[datas$aspect=="E",]
W <- datas[datas$aspect=="W",]


ll <-glm(datas$value ~ datas$time+datas$aspect+datas$month)
