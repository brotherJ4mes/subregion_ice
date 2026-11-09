#!/usr/bin/env Rscript
library(sf)
#library(wesanderson)
source('met_utils.R')
source('set_colors.R')

#TODO: add scale bar to map and scalebar (m^2) to barplot
#TODO: sort barplot by lake > longitude




out_dir = '~/work/subregion_ice/manuscript/figures/'
meta = read.table('data/meta.txt', sep='\t', head=T)
lakes = meta$lake
lk_idx = c(grep('Sup', lakes), grep('Mic', lakes), grep('Hur', lakes), grep('Eri', lakes), which(grepl('Ont',meta$name)))
meta = meta[lk_idx,]


#================= READ & MANIPULATE DATA  ==============================
if(!(exists('lks'))){
	stn_list_g = 'data/stn_meta/stnlist_ghcnd_2021mar16.txt'
	stn_list_i = 'data/stn_meta/stnlist_isd_2021jan29.txt'
	ghcnd = create_pts(stn_list_g)
	isd = create_pts(stn_list_i)
	ghcnd$source = 'GHCND'
	isd$source = 'ISH'
	stns = rbind(ghcnd, isd)
	#lks = st_read('/home/kessler/work/geospatial_data/glahf_spatial_framework_v1d1/spatial_framework/GLAHF_spatial_framework_v1d1.gdb/', layer='subbasins_by_lake_area')
	lks = st_read('data/shp/glahf_cw_mercator.shp')
	#lks[lks$SUBBASIN=='SMR',]$LAKE = 'Superior'
	lks = st_transform(lks, st_crs(ghcnd))

	dist = 20e3

	lks = st_make_valid(lks)


	cat(sprintf('finding stations within %i M from shoreline...\n', dist))
	sel_pts = st_is_within_distance(lks, stns, dist=dist, sparse=F)
	row.names(sel_pts) = lks$SUBBASIN

	print('stations found: (now lets just hope they have valid data))')
	print(apply(sel_pts, 1, sum))
	print(mean(apply(sel_pts, 1, sum)))

	# subset only stations used
	used_stns = apply(sel_pts, 2, any)
	stns = stns[used_stns,]

	# re-order to add mean depth
	lks = lks[lk_idx,]
	lks$depth = meta$depth


	moor_df = read.table('data/mooring_locations.txt', head=T)
	# consolidate GLERL-23 into GLERL-MI
	moor_df = moor_df[moor_df$name!='GLERL-23',]
	moor_df[moor_df$name=='GLERL-MI','num_winters'] = moor_df[moor_df$name=='GLERL-MI','num_winters'] + 1
	moor = st_as_sf(moor_df, coords=c('lon','lat'), crs='+proj=longlat +datum=WGS84')
	moor = st_transform(moor, st_crs(lks))
	moor = moor[moor$num_winters >= 8,]



}

cidx = match(lks$LAKE, unique(lks$LAKE))

#================= DRAW PLOT  ==============================
stn_col = 'dodgerblue'
stn_cex = 2
moor_col = 'darkslategrey'
moor_cex = 5

#pal = wes_palette('FantasticFox1')
#pal = rev(wes_palette('Darjeeling1')) # Love it # leave this set by set_colors.R
#pal = wes_palette('Moonrise3') # LIKE IT!
#pal = wes_palette('BottleRocket2') # Not bad
#text_col = c(1,1,'white','white','white')
text_colext_col = 'black'
#pal = wes_palette('Rushmore1')

#png(paste(out_dir,'station_map.png', sep='/'), w=1800, h=1200)
png(paste(out_dir,'station_map.png', sep='/'), w=1800, h=1200)
par(cex.axis=2.0, mar=c(1,1,1,16))

# plot main graphic
# subregions
plot(lks['LAKE'], axes=F, lwd=2.5, border='black', col=pal[cidx], reset=F, key.pos=4, extent=stns, main=NULL)
# stations
plot(st_geometry(stns), add=T, cex=stn_cex, lwd=2, col=stn_col, pch=ifelse(stns$source=='ISH', 1, 2))
# mooring
plot(moor, pch=1, cex=moor_cex, add=T, col=moor_col)

# text
text(st_coordinates(moor), lab=moor$num_winters, col=moor_col, cex=1.75)
text(lks, lab=lks$SUBBASIN, col=text_col, cex=2.0)


# legend that spells out lake names
#leg_str = c(unique(meta$lake),'GHCND', 'ISD')
#leg_col = c(pal, stn_col, stn_col)
#leg_pch = c(rep(15,5), 2, 1)
#leg_cex = c(rep(5,5), 3,3)
#legend('bottomleft', legend=leg_str, pch=leg_pch, col=leg_col,
#	   cex=2.5, pt.cex=leg_cex, pt.lwd=2, inset=.1, box.lwd=0, text.col=leg_col)

#legend('bottomleft', legend=c('GHCND', 'ISD'), pch=c(1,2), col=stn_col, 
#	   cex=2, title='met stations:', pt.cex=stn_cex, pt.lwd=2, inset=.25)
legend('bottomleft', legend=c('Mooring','GHCND Stn.', 'ISD Stn.'), pch=c(1,1,2), col=c(moor_col, stn_col, stn_col),
	   cex=2, pt.cex=c(moor_cex, stn_cex, stn_cex), pt.lwd=2, inset=.125)

terra::sbar('bottomleft', d=100, lonlat=T)

#overlay bar chart
par(fig=c(0.5, 1, .65, 1), mar=c(1, 4, 6, 3), new=TRUE)
x = barplot(meta$depth, names.arg=NULL, col=pal[cidx], width=meta$area, ylim=c(160,0), border=NA)
axis(3, x, meta$code, las=2, cex.axis=1.0, gap.axis=-.1)
mtext(side=2, 'mean depth (m)', cex=2.0, line=3.5)

#overlay key
#par(fig=c(0.85, 1, .45, .65), mar=c(0, 0, 0, 10), new=TRUE)
#plot(x=rep(1,5), y=1:5, pch=15, cex=5, col=rev(pal), xlab=NA, ylab=NA, ylim=c(0,6), xaxs='i', ax=F)
#axis(4, at=5:1, lab=unique(meta$lake), las=1, cex.axis=2, line=-2, lwd=0)


dev.off()



