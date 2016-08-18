# TODO: Add comment
# 
# Author: ecor
###############################################################################
rm(list=ls())


library(rgdal)
library(raster)
library(stringr)

### MAPS 

map_folder <- '/home/ecor/Dropbox/R-packages/OpenRendena100/maps/monthly_maps' 
mapfiles <- list.files(map_folder,pattern=".asc",full.name=TRUE)
names(mapfiles) <- list.files(map_folder,pattern=".asc",full.name=FALSE)
names(mapfiles) <- str_replace(names(mapfiles),".asc","")
maps <- (str_split(names(mapfiles),"-"))
names(maps) <- names(mapfiles)
maps <- lapply(X=maps,FUN=t)
maps <- lapply(X=maps,FUN=as.data.frame,stringsAsFactors=FALSE)
maps <- do.call(what=rbind,args=maps)
names(maps) <- c("variable","fun","year","month")
maps$name <- names(mapfiles)
maps$file <- mapfiles
maps$yearmonth <- sprintf("%s_%s",maps$year,maps$month)

dsn <- '/home/ecor/Dropbox/R-packages/OpenRendena100/maps/points'
layer <- 'rdm_pts_elev' 


pp_o <- readOGR(dsn=dsn,layer=layer)

funcs <- unique(maps$fun) 

for (func in funcs)    {
	
	pp <- pp_o
	
	oo <- maps[maps$fun==func,]
	
	rr <- stack(oo$file)
	names(rr) <- sprintf("value_%s_%s",oo$year,oo$month)
	
	iout <- cellFromXY(rr, xy=pp)
	out <- as.data.frame(rr[iout])
	out$cell <- iout
	pp$cell <-  iout
	out <- merge(pp,out)
	
	
	dsn_out <- '/home/ecor/Dropbox/R-packages/OpenRendena100/maps/points_output' 
	
	layer_out <- sprintf("points_%s_%s",oo$variable[1],oo$fun[1])
	
	writeOGR(out,dsn=dsn_out,layer_out,driver="ESRI Shapefile",overwrite_layer=TRUE)
	
}