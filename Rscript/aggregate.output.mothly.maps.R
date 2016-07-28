#!/usr/bin/env Rscript
# file aggregate.output.monthly.maps.R
#
# This file makes monthly aggregation of some GEOtop output maps returned by daily frequancy.
#
# author: Emanuele Cordano on 2015 (28-07-2016)
#
#This program is free software: you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation, either version 3 of the License, or
#(at your option) any later version.
#
#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.
#
#You should have received a copy of the GNU General Public License
#along with this program.  If not, see <http://www.gnu.org/licenses/>.

###############################################################################




rm(list=ls())

library(geotopbricks)
library(sp)

Sys.setenv(LANGUAGE='en')
Sys.setenv(LC_TIME='en_US.UTF-8')

wpath <- "/media/ecor/CAUCASO/GONGOLO/keu_cordano/rendena100_kore_v2"

tz <-  "Etc/GMT-1"  #set time zone
format_date <- "%Y-%m-%d %H:%M"

###

start <-  get.geotop.inpts.keyword.value("InitDateDDMMYYYYhhmm",date=TRUE,wpath=wpath,tz=tz)
end <- get.geotop.inpts.keyword.value("EndDateDDMMYYYYhhmm",date=TRUE,wpath=wpath,tz=tz)
###

# a 2D map:
x_e <- "SnowDepthMapFile"

## Aggregation function

quantilemod <- function(x,na.rm=TRUE,...) { if (na.rm) { quantile(x,na.rm=na.rm,...) } else { NA   }}

q25 <- function(x,...) 	{ as.numeric(quantile(x,probs=0.25,...))}
q75 <-  function(x,...) { as.numeric(quantile(x,probs=0.75,...))}
q90 <-  function(x,...) { as.numeric(quantile(x,probs=0.90,...))}
ndays <- function(x,valmin=50,...) { 
	
	if (any(is.na(x))) {
		
		out <- NA
	} else {
	
		out <- length(which(x>=valmin))
	
	}
	
	
	return(out)

}


aggregate_funs <- c("mean","sd","q25","q75","q90","median","max","min","ndays")
###aggregate_funs <- c("ndays")

wpathout <- "/home/ecor/Dropbox/R-packages/OpenRendena100/Rscript/test"

maggr <- list() 

###periods <- c("ND","JF","MA","MJ","JA","SO")






####years <- 1990:2015 ## 2000,2001,2002,2013,2014

time_index <- seq(from=start,to=end,by=3600*24)[-1] ### The 'start'  

i_month <-  as.character(time_index,format="%Y-%m")
i_months <- unique(i_month)

##starty <- as.POSIXct(sprintf("%d-11-01 00:00",years[1]),format=format_date,tz=tz)
###endy   <- as.POSIXct(sprintf("%d-10-31 00:00",year),format=format_date,tz=tz)

####stop("MI  FERMO QUI")
for (im in i_months[1:3]) {
		
	
#	winter_start <- as.POSIXct(sprintf("%d-11-01 00:00",year-1),format=format_date,tz=tz)
###	winter_end   <- as.POSIXct(sprintf("%d-05-01 00:00",year),format=format_date,tz=tz)
#	winter_end   <- as.POSIXct(sprintf("%d-10-31 00:00",year),format=format_date,tz=tz)
#	
#	winter <- seq(from=winter_start,to=winter_end,by=3600*24)
#	winter <- winter[winter>start]
#	winter <- winter[winter<=end]
#	ND <- winter[months(winter,abbreviate=TRUE) %in% c("Nov","Dec")]
#	JF <- winter[months(winter,abbreviate=TRUE) %in% c("Jan","Feb")]
#	MA <- winter[months(winter,abbreviate=TRUE) %in% c("Mar","Apr")]
#	
#	## Add long-summer months!
#	MJ <- winter[months(winter,abbreviate=TRUE) %in% c("May","Jun")]
#	JA <- winter[months(winter,abbreviate=TRUE) %in% c("Jul","Aug")]
#	SO <- winter[months(winter,abbreviate=TRUE) %in% c("Sep","Oct")]
#	
#	periods <- c("ND","JF","MA","MJ","JA","SO")

	
	
	### TO GO ON (CORRECT MAX AND MIN)
#	for (period in periods) {
#		
#		when <- get(period)
#		
#		if (length(when)<7) periods <- periods[periods!=period] 
#		
#	}
#	
#	
#	for (period in periods) {
#		
#		
	
	# import the maps in R of the dates = whentest
		when <- time_index[i_month==im]
	
		m <- rasterFromOutput2DMap(x_e,when=when,wpath=wpath,timestep="OutputSnowMaps",tz=tz,use.read.raster.from.url=FALSE)
	
####		yearplot <- year
		
		###if (period=="ND") yearplot <- year-1
	
	
	
		for (aggregate_fun in aggregate_funs) {
		
		
		##	filename <- paste(paste(x_e,yearplot,period,aggregate_fun,"winter",year-1,year,sep="-"),".asc",sep="")
			filename <- paste(paste(x_e,aggregate_fun,im,sep="-"),".asc",sep="")
			fileout <- paste(wpathout,filename,sep="/")
			label <- paste(im,aggregate_fun,sep="-")
			
			if (aggregate_fun=="min") {
				
				funx <-  function(x,na.rm=TRUE,...) {
					
					lx <- which(!is.na(x))
					if (length(lx)>0) {
						return(min(x,na.rm=na.rm,...))
					} else {
						
						return(NA)
					}
					
				} 
				
				
				
			} else if (aggregate_fun=="max") {
				
					funx <-  function(x,na.rm=TRUE,...) {
					
					lx <- which(!is.na(x))
					if (length(lx)>0) {
						return(max(x,na.rm=na.rm,...))
					} else {
						
						return(NA)
					}
				
				}
				
			} else {
				
				funx <- get(aggregate_fun)
			}
			
			
			maggr[[label]] <- stackApply(x=stack(m),indices=1,fun=funx,na.rm=TRUE) ###,filename=fileout,overwrite=TRUE)
			proj4string(maggr[[label]]) <- proj4string(m[[1]])
			writeRasterxGEOtop(x=maggr[[label]],filename=fileout,overwrite=TRUE)
		
		
		
		}
}

