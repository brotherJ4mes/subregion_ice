#!/usr/bin/Rscript
library(sf)
library(wesanderson)
source('met_utils.R')


out_dir <- '/home/kessler/work/subregion_ice/poster/67311e171949f1bd5174bf03/figures/'
meta <- read.table('../meta.txt', sep='\t', head=T)
lakes <- meta$lake
lk_idx <- c(grep('Sup', lakes), grep('Mic', lakes), grep('Hur', lakes), grep('Eri', lakes), which(grepl('Ont',meta$name)))
meta <- meta[lk_idx,]


if(T){
stn_list_g <- 'stnlist_ghcnd_2021mar16.txt'
stn_list_i <- 'stnlist_isd_2021jan29.txt'
met_dir <- 'stn'

ghcnd <- create_pts(stn_list_g)
isd <- create_pts(stn_list_i)
ghcnd$source <- 'GHCND'
isd$source <- 'ISH'
stns <- rbind(ghcnd, isd)


#dat <- read_stn(ids[1])


# plot LBRM stuff
#subs <- st_read('/home/kessler/work/geospatial_data/gllbrm-boundaries/shp/lbrm_subbasin_outlines.shp')
#plot(st_geometry(subs), ax=T, col=NA, border='black')
#plot(st_geometry(ghcnd), add=T, pch=20, col='red')
#plot(st_geometry(isd), add=T, pch=20, col='blue')



#	subs <- st_read('/home/kessler/work/geospatial_data/glahf_spatial_framework_v1d1/spatial_framework/GLAHF_spatial_framework_v1d1.gdb/', layer='subbasins_by_drainage_area')
	#subs <- st_transform(subs, st_crs(ghcnd))

	lks <- st_read('/home/kessler/work/geospatial_data/glahf_spatial_framework_v1d1/spatial_framework/GLAHF_spatial_framework_v1d1.gdb/', layer='subbasins_by_lake_area')
	lks[lks$SUBBASIN=='SMR',]$LAKE <- 'Superior'
	lks <- st_transform(lks, st_crs(ghcnd))

	# plot GLAHF subs
	#plot(st_geometry(lks), ax=T, col='lightblue', border=NA, extent=subs)
	#plot(st_geometry(subs), add=T, col=NA, border='black')
	#plot(st_geometry(ghcnd), add=T, pch=20, col='red')
	#plot(st_geometry(isd), add=T, pch=20, col='blue')
	#legend('topright', legend=c('GHCND', 'ISD'), text.col=c('red','blue'), pch=NA, cex=1.5)


	dist <- 20e3

	lks <- st_make_valid(lks)


	cat(sprintf('finding stations within %i M from shoreline...\n', dist))
	sel_pts <- st_is_within_distance(lks, stns, dist=dist, sparse=F)
	row.names(sel_pts) <- lks$SUBBASIN
 
print('stations found: (now lets just hope they have valid data))')
print(apply(sel_pts, 1, sum))
print(mean(apply(sel_pts, 1, sum)))

# subset only stations used
used_stns <- apply(sel_pts, 2, any)
stns <- stns[used_stns,]

# re-order to add mean depth
lks <- lks[lk_idx,]
lks$depth <- meta$depth
}

cidx <- match(lks$LAKE, unique(lks$LAKE))

#================= station map version 1 ==============================
stn_col <- 'dodgerblue'
stn_cex <- 2
#pal <- wes_palette('FantasticFox1')
pal <- rev(wes_palette('Darjeeling1'))
#pal <- wes_palette('Rushmore1')

png(paste(out_dir,'station_map.png', sep='/'), w=1800, h=1200)
par(cex.axis=2.0, mar=c(1,1,1,16))

# plot main graphic
plot(lks['LAKE'], axes=F, lwd=2, border='black', col=pal[cidx], reset=F, key.pos=4, extent=stns, main=NULL)
plot(st_geometry(stns), add=T, cex=stn_cex, lwd=2, col=stn_col, pch=ifelse(stns$source=='ISH', 1, 2))
text(lks, lab=lks$SUBBASIN, col='black', cex=2.0)
leg_str <- c(unique(meta$lake),'GHCND', 'ISD')
leg_col <- c(pal, stn_col, stn_col)
leg_pch <- c(rep(15,5), 2, 1)
leg_cex <- c(rep(5,5), 3,3)
legend('bottomleft', legend=leg_str, pch=leg_pch, col=leg_col,
	   cex=2.5, pt.cex=leg_cex, pt.lwd=2, inset=.1, box.lwd=0)
#legend('bottomleft', legend=c('GHCND', 'ISD'), pch=c(1,2), col=stn_col, 
#	   cex=2, title='met stations:', pt.cex=stn_cex, pt.lwd=2, inset=.1)
terra::sbar('bottomleft', d=100, lonlat=T)

#overlay bar chart
par(fig=c(0.55, 1, .65, 1), mar=c(1, 4, 6, 3), new=TRUE)
x <- barplot(meta$depth, names.arg=NULL, col=pal[cidx], width=meta$area, ylim=c(160,0))
axis(3, x, meta$code, las=2, cex.axis=1.0, gap.axis=.0)
mtext(side=2, 'mean depth (m)', cex=2.0, line=3.5)

#overlay key
#par(fig=c(0.85, 1, .45, .65), mar=c(0, 0, 0, 10), new=TRUE)
#plot(x=rep(1,5), y=1:5, pch=15, cex=5, col=rev(pal), xlab=NA, ylab=NA, ylim=c(0,6), xaxs='i', ax=F)
#axis(4, at=5:1, lab=unique(meta$lake), las=1, cex.axis=2, line=-2, lwd=0)


dev.off()


stop()


#================= station map version 2 ==============================
stn_col <- 'red'
stn_cex <- 2
#pal <- wes_palette('FantasticFox1')
#pal <- rev(wes_palette('Darjeeling1'))
#pal <- wes_palette('Rushmore1')

#png('../station_map_bathy.png', w=1600, h=1200)
png(paste(out_dir,'station_map_bathy.png', sep='/'), w=1800, h=1300)
par(cex.axis=2.0, mar=c(3,1,1,1))

if (all(lks$depth > 0)) lks$depth <- -lks$depth # invert for colormap 
# plot main graphic
plot(lks['depth'], axes=F, lwd=2, border='black', pal=viridis, reset=F, key.pos=1, 
	 at=seq(-150, -10, by=20), extent=stns, main=NULL)
plot(st_geometry(stns), add=T, cex=stn_cex, lwd=2, col=stn_col, pch=ifelse(stns$source=='ISH', 1, 2))
text(lks, lab=lks$SUBBASIN, col=ifelse(lks$depth < -110, 'white','black'), cex=2.0)
legend('bottomleft', legend=c('GHCND', 'ISD'), pch=c(1,2), col=stn_col, 
	   cex=2.5, title='met stations:', pt.cex=stn_cex, pt.lwd=2, inset=.1)

mtext(side=1, 'mean depth (m)', line=-0.5, cex=2.5) 
#terra::sbar('bottomleft', d=100, lonlat=T)
dev.off()

#x11(w=12, h=6)
#par(mar=c(2,4,10,3))

if(F){
for (lk in 1:nrow(lks)){
	x11()
	plot(st_geometry(lks[lk,]), col='lightblue', ax=T)
	plot(st_geometry(stns[sel_pts[lk,],]), add=T, pch=20)
	}
}


ids <- stns[sel_pts['GrB',],'station_id', drop=T]

