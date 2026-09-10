#!/usr/bin/Rscript
library(sf)
library(wesanderson)
source('met_utils.R')


out_dir <- '~/work/subregion_ice/manuscript/figures/'
meta <- read.table('data/meta.txt', sep='\t', head=T)
lakes <- meta$lake
lk_idx <- c(grep('Sup', lakes), grep('Mic', lakes), grep('Hur', lakes), grep('Eri', lakes), which(grepl('Ont',meta$name)))
meta <- meta[lk_idx,]


if(F){
	stn_list_g <- 'data/stn_meta/stnlist_ghcnd_2021mar16.txt'
	stn_list_i <- 'data/stn_meta/stnlist_isd_2021jan29.txt'
	ghcnd <- create_pts(stn_list_g)
	isd <- create_pts(stn_list_i)
	ghcnd$source <- 'GHCND'
	isd$source <- 'ISH'
	stns <- rbind(ghcnd, isd)


	lks <- st_read('/home/kessler/work/geospatial_data/glahf_spatial_framework_v1d1/spatial_framework/GLAHF_spatial_framework_v1d1.gdb/', layer='subbasins_by_lake_area')
	lks[lks$SUBBASIN=='SMR',]$LAKE <- 'Superior'
	lks <- st_transform(lks, st_crs(ghcnd))

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
#pal <- wes_palette('Moonrise3') # LIKE IT!
pal <- wes_palette('BottleRocket2') # Not bad
text_col = c(1,1,'white','white','white')
#pal <- wes_palette('Rushmore1')

png(paste(out_dir,'station_map.png', sep='/'), w=1800, h=1200)
par(cex.axis=2.0, mar=c(1,1,1,16))

# plot main graphic
plot(lks['LAKE'], axes=F, lwd=2, border='black', col=pal[cidx], reset=F, key.pos=4, extent=stns, main=NULL)
plot(st_geometry(stns), add=T, cex=stn_cex, lwd=2, col=stn_col, pch=ifelse(stns$source=='ISH', 1, 2))
text(lks, lab=lks$SUBBASIN, col=text_col, cex=2.0)
leg_str <- c(unique(meta$lake),'GHCND', 'ISD')
leg_col <- c(pal, stn_col, stn_col)
leg_pch <- c(rep(15,5), 2, 1)
leg_cex <- c(rep(5,5), 3,3)
legend('bottomleft', legend=leg_str, pch=leg_pch, col=leg_col,
	   cex=2.5, pt.cex=leg_cex, pt.lwd=2, inset=.1, box.lwd=0, text.col=leg_col)
#legend('bottomleft', legend=c('GHCND', 'ISD'), pch=c(1,2), col=stn_col, 
#	   cex=2, title='met stations:', pt.cex=stn_cex, pt.lwd=2, inset=.1)
terra::sbar('bottomleft', d=100, lonlat=T)

#overlay bar chart
par(fig=c(0.55, 1, .65, 1), mar=c(1, 4, 6, 3), new=TRUE)
x <- barplot(meta$depth, names.arg=NULL, col=pal[cidx], width=meta$area, ylim=c(160,0))
axis(3, x, meta$code, las=2, cex.axis=0.8, gap.axis=-.1)
mtext(side=2, 'mean depth (m)', cex=2.0, line=3.5)

#overlay key
#par(fig=c(0.85, 1, .45, .65), mar=c(0, 0, 0, 10), new=TRUE)
#plot(x=rep(1,5), y=1:5, pch=15, cex=5, col=rev(pal), xlab=NA, ylab=NA, ylim=c(0,6), xaxs='i', ax=F)
#axis(4, at=5:1, lab=unique(meta$lake), las=1, cex.axis=2, line=-2, lwd=0)

legend('bottomleft', legend=c('GHCND', 'ISD'), pch=c(1,2), col=stn_col, 
	   cex=2.5, title='met stations:', pt.cex=stn_cex, pt.lwd=2, inset=.1)

dev.off()



