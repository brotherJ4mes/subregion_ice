#!/usr/bin/RScript
library(sf)
source('met_utils.R')

dist = 20e3

meta = read.table('meta.txt', sep='\t', head=T)
lakes = meta$lake
lk_idx = c(grep('Sup', lakes), grep('Mic', lakes), grep('Hur', lakes), grep('Eri', lakes), which(grepl('Ont',meta$name)))
meta = meta[lk_idx,]

stn_list_g = 'stnlist_ghcnd_2021mar16.txt'
stn_list_i = 'stnlist_isd_2021jan29.txt'
met_dir = 'stn'

ghcnd = create_pts(stn_list_g)
isd = create_pts(stn_list_i)
ghcnd$source = 'GHCND'
isd$source = 'ISH'
stns = rbind(ghcnd, isd)

lks = st_read('/home/kessler/work/geospatial_data/glahf_spatial_framework_v1d1/spatial_framework/GLAHF_spatial_framework_v1d1.gdb/', layer='subbasins_by_lake_area')
lks = st_transform(lks, st_crs(ghcnd))
lks = st_make_valid(lks)
# re-order to add mean depth
lks = lks[lk_idx,]
lks$depth = meta$depth


cat(sprintf('finding stations within %i M from shoreline...\n', dist))
sel_stn = st_is_within_distance(lks, stns, dist=dist, sparse=F)


# get ready to print out to a massive text file (rows=stns; cols=subregions)
# 1 indicates station within radius; 0 not within radius
sel_stn = t(sel_stn*1)
rownames(sel_stn) = stns$station_id
colnames(sel_stn) = meta$code
#write.table(sel_stn, quote=F, file='selected_stations.txt')
write.table(sel_stn, quote=F, file=sprintf('sel_stns_%ikm.txt',dist/1e3))



# now access the station list like:   `names(which(sel_stn[,'WSU']==1))`


# for debug; plot a subregion and corresponding stations
#plot(st_geometry(stns[which(sel_stn[,'SB']==1),]), pch=20, col='darkgreen')
#text(stns[which(sel_stn[,'SB']==1),], lab=stns[which(sel_stn[,'SB']==1),]$station_id)
#plot(st_geometry(lks[lks$SUBBASIN=='SB',]), add=T)

