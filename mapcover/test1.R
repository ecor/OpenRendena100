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

monthn <- c(12)
month <- sprintf("-%02d",monthn)
mstack <- list()
mapfiles_ <- mapfiles

for (itm in month) {
	
	mapfiles <- mapfiles_
	mapfiles <- mapfiles[str_detect(mapfiles,itm)]




time <- (str_replace_all(names(mapfiles),"[a-z,A-Z]",""))
time <- str_replace_all(time,"-","")

time <- str_replace_all(time,"[.]","")
time <- paste(time,"01",sep="")
time <- as.Date(time,format="%Y%m%d")

year <- as.numeric(as.character(time,format="%Y"))
month_ <- as.numeric(as.character(time,format="%m"))
year[month_>=8] <- year[month_>=8]+1
###,format="SnowDepthMapFile-ndays-%Y-%m.asc")
names(year) <- names(mapfiles)
names(year) <- str_replace_all(names(year),"-",".")




years <- 1995:2015
mstack[[itm]] <- stack(mapfiles[year %in% years]) ##,alt_map)

	

}

###
mmstack <- mstack[[1]]

if (length(mstack)>1) {
	
	for (itt in 2:length(mstack)) {
		
		mmstack <- mstack[[itt]]+mmstack
		
		
	}
	
	
	
}

mmstack <- stack(mmstack,alt_map)


###


mdf <- as.data.frame(mmstack)

nyy <- sprintf("Y%04d",year[names(mdf)[names(mdf)!="layer"]])
names(mdf)[names(mdf)!="layer"] <- nyy
names(mdf)[names(mdf)=="layer"] <- "altitude"

mdfm <- melt(mdf,id="altitude")
mdfm <- mdfm[which(mdfm$altitude>=500 & mdfm$altitude<2500),]
##mdfm$value <- factor(mdfm$value)
index <- sprintf("%s_%02d",mdfm$variable,mdfm$value)

####
stop("STOP HERE AND MODIFY THE PAROCEDOURE BEYOND HERE")
###
altitude_min <- tapply(X=mdfm$altitude,INDEX=index,FUN=min,na.rm=TRUE)
altitude_max <- tapply(X=mdfm$altitude,INDEX=index,FUN=max,na.rm=TRUE)
altitude_q25 <- tapply(X=mdfm$altitude,INDEX=index,FUN=quantile,probs=0.25,na.rm=TRUE)
altitude_median <- tapply(X=mdfm$altitude,INDEX=index,FUN=median,na.rm=TRUE)
altitude_q75 <- tapply(X=mdfm$altitude,INDEX=index,FUN=quantile,probs=0.75,na.rm=TRUE)
altitude_mean <- tapply(X=mdfm$altitude,INDEX=index,FUN=mean,na.rm=TRUE)
altitude_sd <- tapply(X=mdfm$altitude,INDEX=index,FUN=sd,na.rm=TRUE)
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
dafm$altitude_sd <- altitude_sd[nnn]
dafm$year <- as.numeric(str_replace(dafm$variable,"Y",""))
dafm$period <- "none"
dafm$period[dafm$year>=1996 & dafm$year<=2005] <- "1996-2005"
dafm$period[dafm$year>=2006 & dafm$year<=2015] <- "2006-2015"

dafm <- dafm[dafm$period!="none",]

### 
### FARE UN TAPPLTY PER AGGREGARE I GIORNI 
##stop("")

index2 <- sprintf("%s_%02d",dafm$period,dafm$nday)
zaaltitude_q25 <- tapply(X=dafm$altitude_q25,INDEX=index2,FUN=mean,na.rm=TRUE)
zaaltitude_q75 <- tapply(X=dafm$altitude_q75,INDEX=index2,FUN=mean,na.rm=TRUE)
zaaltitude_median <- tapply(X=dafm$altitude_median,INDEX=index2,FUN=mean,na.rm=TRUE)
zaaltitude_mean <- tapply(X=dafm$altitude_mean,INDEX=index2,FUN=mean,na.rm=TRUE)
zaaltitude_sd <- tapply(X=dafm$altitude_sd,INDEX=index2,FUN=mean,na.rm=TRUE)
sss <- str_split(index2,"_")
dbfm <- data.frame(
		period=sapply(X=sss,FUN=function(x){x[1]}),
		nday=as.numeric(sapply(X=sss,FUN=function(x){x[2]})),
		stringsAsFactors=FALSE
		)
###
###

dbfm$altitude_q25 <- zaaltitude_q25[index2]
dbfm$altitude_q75 <- zaaltitude_q75[index2]
dbfm$altitude_median <- zaaltitude_median[index2]
dbfm$altitude_mean <- zaaltitude_mean[index2]
dbfm$altitude_sd <- zaaltitude_sd[index2]


###gg <- ggplot(data = dbfm, aes(ymin=altitude_q25,ymax=altitude_q75,y=altitude_median,x=nday, by=period, color=period,fill=period))+geom_ribbon() ## +geom_line()
titlem <- paste(sprintf("%02d",monthn),collapse=",")
nfilep <- sprintf('/home/ecor/Dropbox/R-packages/OpenRendena100/mapcover/plot/month%s',titlem)

## MEAN 

nfile <- paste(nfilep,"mean.png",sep="")
title <- sprintf("Mean Snow Coverage Duration (months %s) vs Elevation" ,titlem)
gmean <- ggplot(data = dbfm, aes(y=altitude_mean,x=nday, by=period, color=period,fill=period))+geom_line()+xlab("Duration [days]")+ylab("Elevation [m a.s.l.]")+ggtitle(title)
ggsave(file=nfile,plot=gmean)

## Q25

nfile <- paste(nfilep,"Q25.png",sep="")
title <- sprintf("Q25 Snow Coverage Duration (months %s) vs Elevation" ,titlem)
gq25 <- ggplot(data = dbfm, aes(y=altitude_q25,x=nday, by=period, color=period,fill=period))+geom_line()+xlab("Duration [days]")+ylab("Elevation [m a.s.l.]")+ggtitle(title)
ggsave(file=nfile,plot=gq25)

## MEDIAN

nfile <- paste(nfilep,"median.png",sep="")
title <- sprintf("Median Snow Coverage Duration (months %s) vs Elevation" ,titlem)
gmedian <- ggplot(data = dbfm, aes(y=altitude_median,x=nday, by=period, color=period,fill=period))+geom_line()+xlab("Duration [days]")+ylab("Elevation [m a.s.l.]")+ggtitle(title)
ggsave(file=nfile,plot=gmedian)




## Q75

nfile <- paste(nfilep,"q75.png",sep="")
title <- sprintf("Q75 Snow Coverage Duration (months %s) vs Elevation" ,titlem)
gq75 <- ggplot(data = dbfm, aes(y=altitude_q75,x=nday, by=period, color=period,fill=period))+geom_line()+xlab("Duration [days]")+ylab("Elevation [m a.s.l.]")+ggtitle(title)
ggsave(file=nfile,plot=gq75)



