# TODO: Add comment
# 
# Author: ecor
###############################################################################



library(geotopbricks)
library(reshape2)
library(ggplot2)
####wpath <- '/home/ecor/rendena100_sim/trial/rendena100_trial002_in'
wpath <- '/home/ecor/Dropbox/R-packages/geotopbricks_simulation/rendena'
mmapfolder <- '/home/ecor/activity/2017/rendena100/simulation/monthly_maps'
alt_map <-  get.geotop.inpts.keyword.value("DemFile",raster=TRUE,wpath=wpath)
aspect_map <- get.geotop.inpts.keyword.value("AspectMapFile",raster=TRUE,wpath=wpath)
aspect_map[aspect_map>360-45/2] <- aspect_map[aspect_map>360-45/2]-360

aspect_cat <- 1:8
names(aspect_cat) <- aspect_class <- c("N","NE","E","SE","S","SW","W","NW")

mapfiles <- list.files(mmapfolder,pattern=".asc",full.name=TRUE)
names(mapfiles) <- list.files(mmapfolder,pattern=".asc",full.name=FALSE)

mapfiles <- mapfiles[str_detect(mapfiles,"nday")]


month <- "-01"


mapfiles <- mapfiles[str_detect(mapfiles,month)]
time <- (str_replace_all(names(mapfiles),"[a-z,A-Z]",""))
time <- str_replace_all(time,"-","")

time <- str_replace_all(time,"[.]","")
time <- paste(time,"01",sep="")
time <- as.Date(time,format="%Y%m%d")
year <- as.numeric(as.character(time,format="%Y"))
###,format="SnowDepthMapFile-ndays-%Y-%m.asc")
names(year) <- names(mapfiles)
names(year) <- str_replace_all(names(year),"-",".")


years <- 2000:2009
mstack <- stack(mapfiles[year %in% years],alt_map)
mdf <- as.data.frame(mstack)

nyy <- sprintf("Y%04d",year[names(mdf)[names(mdf)!="layer"]])
names(mdf)[names(mdf)!="layer"] <- nyy
names(mdf)[names(mdf)=="layer"] <- "altitude"

mdfm <- melt(mdf,id="altitude")
mdfm <- mdfm[which(mdfm$altitude>=500 & mdfm$altitude<2500),]
mdfm$vale <- factor(mdfm$value)
gg <- ggplot(data = mdfm, aes(x=altitude, y=value, by=variable, color=variable))
gg <- gg+geom_ribbon()+facet_grid(variable ~ .)
	##	geom_point()+median_hilow(y=value,by=variable)

#alt_map_df <- as.data.frame(alt_map,xy=TRUE)
#
#snow_map_df <- as.data.frame(snow_map,xy=TRUE)


