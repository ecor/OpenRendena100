# TODO: Add comment
# 
# Author: ecor
###############################################################################
#!/usr/bin/env Rscript
# file aggregate.output.monthly.maps.R
#
# This file extracts value of some GEOtop output maps returned with daily frequency.
#
# author: Emanuele Cordano on 2015 (18-08-2016)
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

library(rgdal)
library(raster)
library(stringr)
library(geotopbricks)





library(sp)

Sys.setenv(LANGUAGE='en')
Sys.setenv(LC_TIME='en_US.UTF-8')

##wpath <- "/media/ecor/CAUCASO/GONGOLO/keu_cordano/rendena100_kore_v2"
wpath <- "/home/ecor/rendena100_sim/trial/rendena100_trial002_in"
tz <-  "Etc/GMT-1"  #set time zone
format_date <- "%Y-%m-%d %H:%M"

#dsn <- '/home/ecor/Dropbox/R-packages/OpenRendena100/maps/points'
dsn <- '/home/ecor/local/OpenRendena100/maps/points'
layer <- 'rdm_pts_elev' 


pp_o <- readOGR(dsn=dsn,layer=layer)

####funcs <- unique(maps$fun) 



###

start <-  get.geotop.inpts.keyword.value("InitDateDDMMYYYYhhmm",date=TRUE,wpath=wpath,tz=tz)
end <- get.geotop.inpts.keyword.value("EndDateDDMMYYYYhhmm",date=TRUE,wpath=wpath,tz=tz)
###

# a 2D map:
x_e <- "SnowDepthMapFile"

when <- as.POSIXct(seq(from=start,to=end,by=24*3600))[-1]



i_month <-  as.character(when,format="%Y_%m")
i_months <- unique(i_month)



for (mon in i_months) {
	
	
	indices <- which(i_month==mon)
	cnt <- 0
	pp <- pp_o 
	for (i in indices) {
			
	cnt <- cnt+1
		msnow <- rasterFromOutput2DMap(x_e,when=when[i],wpath=wpath,timestep="OutputSnowMaps",tz=tz,use.read.raster.from.url=FALSE)
		if (cnt==1) {
			iout <- cellFromXY(msnow, xy=pp)
			pp$cell <- iout
		}
	
	
		time <- when[i]
		timev <- as.character(time,format="HS%Y%m%d")
		pp$value <- msnow[iout]
		
			
		names(pp)[names(pp)=="value"] <- timev
		print(mon)
		print(timev)
		
	
	}
	
	######
	#dsn_out <- '/home/ecor/Dropbox/R-packages/OpenRendena100/maps/points_daily_output'
	dsn_out <- '/home/ecor/local/OpenRendena100/maps/points_daily_output'
	
	layer_out <- sprintf("HS_%s",mon)
	
	writeOGR(pp,dsn=dsn_out,layer_out,driver="ESRI Shapefile",overwrite_layer=TRUE)
	
	
	
	
	
	
	#######
	
	
	
	
	
	
}


