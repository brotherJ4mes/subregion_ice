#!/bin/Rscript
library(sf)
library(terra)

yr <- commandArgs(trail=T)
#yr <- 2019
dir_in <- sprintf('/mnt/projects/ipemf/ice/ct_1024/%s', yr)
merc_str <- '+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=-24 +datum=WGS84 +units=m +no_defs'
bnds <- st_read('shp/glahf_cw_mercator.shp', quiet=T) # boundaries

x_ice <- function(fin, fun='mean'){
		ice <- rast(fin)
		crs(ice) <- merc_str
		ice[ice==-1] <- NA
		ice_vals <- extract(ice, bnds, fun='mean', na.rm=T, ID=F)
		return(as.matrix(ice_vals))
}


flist <- list.files(dir_in, pattern='*.ct', full.names=T)
dts <- gsub('\\D', '', basename(flist))
dat_out <- matrix(NA, length(flist), nrow(bnds))
rownames(dat_out) <- dts

for (i in 1:length(dts)) dat_out[i,] <- x_ice(flist[i])
#for (i in 1:10) dat_out[i,] <- x_ice(flist[i])

fout <- sprintf('yrly_out/%s.txt', yr)
write.table(round(dat_out, digits=1), file=fout, quote=F, col.names=F)

