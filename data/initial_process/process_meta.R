#!/bin/Rscript
library(sf)
library(terra)

# pull meta from shp files
shp = st_read('/home/kessler/work/subregion_ice/shp/glahf_cw_mercator.shp') # TODO: read in native and project
shp_ll = st_transform(shp, '+proj=longlat +datum=WGS84')
shp_ll = st_make_valid(shp_ll)



out <- data.frame(code=shp$SUBBASI, name=shp$sbbsn_N, lake=shp$LAKE, area=sprintf('%.2e',shp$Shap_Ar))


# process bathy (very slow)
# data source: https://www.ncei.noaa.gov/products/great-lakes-bathymetry
#bathy_all <- vrt(c('bathy_ncei/michigan_lld.tif','bathy_ncei/huron_lld.tif','bathy_ncei/erie_lld.tif',
#				   'bathy_ncei/ontario_lld.tif','bathy_ncei/superior_lld.tif'))
#bathy_all_merc <- project(bathy_all, st_crs(shp)) # this step is VERY SLOW
#b_ext <- extract(bathy_all_merc, shp, 'mean', na.rm=TODO)  # this step is PRETTY slow
#save(b_ext, file='bathy_extract.Rdata')
load('/home/kessler/work/subregion_ice/initial_process/bathy_extract.Rdata')




out <- data.frame(code=shp$SUBBASI, name=shp$sbbsn_N, lake=shp$LAKE, 
				  area=sprintf('%.2e',shp$Shap_Ar), depth=sprintf('%.2e', -b_ext[,2]))


crds = st_coordinates(st_centroid(shp_ll))
out$lon = crds[,1]
out$lat = crds[,2]


# cleanup and abbreviate names
out$name <- gsub('Lake ', '', out$name)
out$name <- gsub('North Channel and Georgian Bay', 'N. Chan. & Geo. Bay', out$name)
if(T){
	out$name <- gsub('Superior', 'Sup.', out$name)
	out$name <- gsub('Michigan', 'Mich.', out$name)
	out$name <- gsub('Huron', 'Huron', out$name)
	out$name <- gsub('Ontario', 'Ont.', out$name)
}



write.table(out, quote=F, row.names=F, file='meta.txt', sep='\t')

