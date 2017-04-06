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


years <- 1996:2015
mstack <- stack(mapfiles[year %in% years],alt_map)
mdf <- as.data.frame(mstack)

nyy <- sprintf("Y%04d",year[names(mdf)[names(mdf)!="layer"]])
names(mdf)[names(mdf)!="layer"] <- nyy
names(mdf)[names(mdf)=="layer"] <- "altitude"

mdfm <- melt(mdf,id="altitude")
mdfm <- mdfm[which(mdfm$altitude>=500 & mdfm$altitude<2500),]
##mdfm$value <- factor(mdfm$value)
index <- sprintf("%s_%02d",mdfm$variable,mdfm$value)
altitude_min <- tapply(X=mdfm$altitude,INDEX=index,FUN=min,na.rm=TRUE)
altitude_max <- tapply(X=mdfm$altitude,INDEX=index,FUN=max,na.rm=TRUE)
altitude_q25 <- tapply(X=mdfm$altitude,INDEX=index,FUN=quantile,probs=0.25,na.rm=TRUE)
altitude_median <- tapply(X=mdfm$altitude,INDEX=index,FUN=median,na.rm=TRUE)
altitude_q75 <- tapply(X=mdfm$altitude,INDEX=index,FUN=quantile,probs=0.75,na.rm=TRUE)
altitude_mean <- tapply(X=mdfm$altitude,INDEX=index,FUN=mean,na.rm=TRUE)

nnn <- names(altitude_min)

ss <- str_split(nnn,"_")

dafm <- data.frame(
		variable=sapply(X=ss,FUN=function(x){x[1]}),
		nday=as.numeric(sapply(X=ss,FUN=function(x){x[2]})),
		stringsAsFactors=FALSE
		)

		
dafm$altitude_max <- altitude_max[nnn]
dafm$altitude_q25 <- altitude_q25[nnn]
dafm$altitude_median <- altitude_median[nnn]
dafm$altitude_q75 <- altitude_q75[nnn]
dafm$altitude_mean <- altitude_mean[nnn]
dafm$year <- as.numeric(str_replace(dafm$variable,"Y",""))
dafm$period <- "none"
dafm$period[dafm$year>=1996 & dafm$year<=2005] <- "1996-2005"
dafm$period[dafm$year>=2006 & dafm$year<=2015] <- "2006-2015"

### 
### FARE UN TAPPLTY PER AGGREGARE I GIORNI 
#stop("")

gg <- ggplot(data = dafm, aes(ymin=altitude_q25,ymax=altitude_q75, x=nday, by=period, color=period,fill=period))+geom_ribbon()
##gg <- gg+facet_grid(variable ~ .) ##+geom_ribbon()
	##	geom_point()+median_hilow(y=value,by=variable)

#alt_map_df <- as.data.frame(alt_map,xy=TRUE)
#
#snow_map_df <- as.data.frame(snow_map,xy=TRUE)


