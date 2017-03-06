#!/usr/bin/env Rscript
# file aggregate.output.maps.R
#
# This file makes plays GEOtop output maps returned with daily frequancy.
#
# author: Emanuele Cordano on 2015 (06-03-2017)
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

###wpath <- "/media/ecor/CAUCASO/GONGOLO/keu_cordano/rendena100_kore_v2"
wpath <- "/home/ecor/rendena100_sim/trial/rendena100_trial002_in"



tz <-  "Etc/GMT-1"  #set time zone
format_date <- "%Y-%m-%d %H:%M"

###

start <-  get.geotop.inpts.keyword.value("InitDateDDMMYYYYhhmm",date=TRUE,wpath=wpath,tz=tz)
end <- get.geotop.inpts.keyword.value("EndDateDDMMYYYYhhmm",date=TRUE,wpath=wpath,tz=tz)
###

elev <- get.geotop.inpts.keyword.value("DemFile",raster=TRUE,wpath=wpath,tz=tz)

# a 2D map:
x_e <- "SnowDepthMapFile"



m <- rasterFromOutput2DMap(x_e,when=get(period),wpath=wpath,timestep="OutputSnowMaps",tz=tz,use.read.raster.from.url=FALSE)


