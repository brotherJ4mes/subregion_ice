#!/usr/bin/Rscript
library(fields)
library(Kendall)
#library(latex2exp)
#graphics.off()

#out_dir = '/home/kessler/work/subregion_ice/poster/67311e171949f1bd5174bf03/figures'
#out_dir = '/home/kessler/work/subregion_ice/poster/67311e171949f1bd5174bf03/figures'
#out_dir = '/home/kessler/work/subregion_ice/figures'
out_dir = '/home/j4mes/work/subregion_ice/figures'
file_in = 'data/txt/yrly_out/all.txt'

thresh = 10
ice = read.table(file_in)
dts = as.Date(row.names(ice), format='%Y%m%d')
ice = ice[-c(grep('02-29', dts)),] # remove leap days
dts = dts[-c(grep('02-29', dts))]



plt_heat = F
plt_diff = F
plt_trend = T
cross_cor = F


meta = read.table('data/txt/meta.txt', sep='\t', head=T)
lks = meta$lake
# re-order lakes (Ont is handled differently to omit Niagara (no ice data)
lk_idx = c(grep('Sup', lks), grep('Mic', lks), grep('Hur', lks), grep('Eri', lks), which(grepl('Ont',meta$name)))
meta = meta[lk_idx,]
ice = ice[,lk_idx]
nsubs = length(lk_idx)
lks = lks[lk_idx]
lk_brks = c(0,max(grep('Sup',lks)), max(grep('Mic',lks)), max(grep('Hur',lks)), max(grep('Eri',lks)), max(grep('Ont',lks)))
lk_abbrevs = c('Sup.','Mich.','Huron','Erie','Ont.')
#lk_brks = c(0,max(grep('Sup',lks)), max(grep('Mic',lks)), max(grep('Hur',lks)), max(grep('Eri',lks)), max(grep('Ont',lks)))


# develop selection vectors
iyr = as.numeric(format(dts, '%Y')) # ice year  
yrs = unique(iyr)
jd = as.numeric(format(dts, '%j'))  # julian day
mon = format(dts, '%b')             # month 
iyr[jd>300] = iyr[jd>300] + 1


# determine jd range

#graphics.off()
#jdr = aggregate(jd, by=list(yr=iyr), function(x) cbind(x[1],rev(x)[1])) # JD range
#jdr = data.frame(yr=jdr$yr, t0=jdr$x[,1], tf=jdr$x[,2])
#jdr[,2:3][jdr[,2:3]>300] = jdr[,2:3][jdr[,2:3]>300] -365 # shift Nov/Dec days back
#plot(tf~yr, jdr, ylim=range(jdr[,-1]), pch=20, cex=4, ylab='Julian Day', xlab=NA, main='Great Lakes Wide')
#points(t0~yr, jdr, cex=3)
#abline(h=c(0,90))
#legend('right',legend=c('First Reported','Last Reported'), pch=c(1,20), pt.cex=c(3,4))
#stop()


amic = aggregate(ice, by=list(yr=iyr), 'max')
jfm = aggregate(ice[jd<91,], by=list(yr=iyr[jd<91]), 'mean')

ice_on  = ice >= thresh
print('restricting window for duration')
ice_on = ice_on[jd > 0 & jd <91,] # only consider JFM duration
onyr =  as.numeric(format(as.Date(row.names(ice_on),'%Y%m%d'),'%Y'))
dur = aggregate(ice_on, by=list(yr=onyr), sum)

#stop()

stop()

normalize = function(dat){ out = cbind(data.frame(yr=unique(iyr), sweep(dat[,-1], 2, apply(dat[,-1], 2, mean), '-'))) }

heat_map = function(dat, cmap, brks, ti_str, fout, linecol='black'){
	png(sprintf('%s/%s.png', out_dir, fout), w=1700, h=800)
	par(mar=c(2,12.75,4,8), cex.axis=1.5, cex.main=2.5)
	image.plot(x=dat$yr, y=1:nsubs, z=as.matrix(dat[,-1]), yaxt='n', main=ti_str,
		  ylab=NA,  xlab=NA, col=cmap, breaks=brks, xaxt='n', ylim=c(nsubs+.5,0.5))
	axis(2, at=1:nsubs, lab=meta$name, las=2)
	axis(1, at=dat$yr[c(F,F,T,F,F)])
	axis(1, at=dat$yr, lab=NA)
	axis(3, at=dat$yr[c(F,F,T,F,F)], lab=NA)
	axis(4, lk_brks+.5, lab=NA, tcl=-1, lwd=2)
	abline(h=lk_brks[c(-1,-6)]+.5, lwd=2, col=linecol)
	text(x=max(dat$yr)+1.5, y=lk_brks[-6] + diff(lk_brks)/2 + 0.5, lab=unique(lks), srt=270, xpd=NA, cex=1.75)
	dev.off()

	#axis(4, at=lk_brks[-6] + diff(lk_brks)/2 + 0.5, lab=c('Sup.','Mic.','Hur.','Erie','Ont.'), las=2)

}



# first and last reported vals
#first = aggregate(ice, by=list(yr=iyr), function(x) head(x, n=1))
#last = aggregate(ice, by=list(yr=iyr), function(x) tail(x, n=1))
#brks = seq(0,100,by=10)
#cmap = viridis(length(brks)-1)
#graphics.off()
#
##x11(w=12, h=6)
##yrs = 1973:2024
##heat_map(first, cmap, brks, 'first reported value (%)')
##labs = as.matrix(round(first[,-1], digits=0))
##labs[labs < 10] = NA
##text(x=rep(yrs, times=nsubs), y=rep(1:nsubs, each=length(yrs)), lab=labs, col='white')
##
##x11(w=12, h=6)
##heat_map(last, cmap, brks, 'last reported value (%)')
#labs = as.matrix(round(last[,-1], digits=0))
#labs[labs < 10] = NA
#text(x=rep(yrs, times=nsubs), y=rep(1:nsubs, each=length(yrs)), lab=labs, col='white')


if(plt_heat){
	brks = seq(0,100,by=12.5); 
	cmap = mako(length(brks)-1)
	heat_map(jfm, cmap, brks, 'Seasonal Average (JFM %)', 'jfm_ts', linecol='black')
	heat_map(amic, cmap, brks, 'Annual Max Ice Cover (%)','amic_ts', linecol='black')
	heat_map(dur, cmap, brks, 'Season Duration (days)', 'dur_ts', linecol='black')
}

if(plt_diff){
	brks = seq(-100,100,by=25); 
	cmap = rev(hcl.colors(length(brks)-1,'blue-Red 3'))
	jfm_diff = normalize(jfm)
	amic_diff = normalize(amic)
	dur_diff = normalize(dur)
	heat_map(jfm_diff, cmap, brks, 'JFM - JFM climatology (%)', 'jfm_diff')
	heat_map(amic_diff, cmap, brks, 'AMIC - AMIC climatology (%)','amic_diff')
	heat_map(dur_diff, cmap, brks, 'duration - duration climatology (days)', 'dur_diff')

}



#x11(w=12, h=6)
#ar(mar=c(2,4,10,3))
# = barplot(meta$depth, names.arg=NULL,col=match(lks, unique(lks))+1, width=meta$area, ylim=c(160,0), ylab='depth')
#xis(3, x, meta$name, las=2)


if(cross_cor){
print('================================================')
print('correlations (pearsons r) between ice metrics')
print('================================================')
cat(sprintf('lake, JFM/DUR, JFM/AMIC, DUR/AMIC\n'))
for (i in 2:ncol(jfm)){ 
	cat(sprintf('%s,\t ', meta[i-1, 'code']))
	cat(sprintf('%5.2f,', cor(jfm[,i],dur[,i])))
	cat(sprintf('%5.2f,', cor(jfm[,i], amic[,i])))
	cat(sprintf('%5.2f\n', cor(dur[,i], amic[,i]))) 
}
}



pvals = data.frame(jfm=apply(jfm, 2, function(x) as.numeric(MannKendall(x)$sl))[-1],
					amic=apply(amic, 2, function(x) as.numeric(MannKendall(x)$sl))[-1],
					dur=apply(dur, 2, function(x) as.numeric(MannKendall(x)$sl))[-1])

sig99 = which(pvals<.01, arr=T)
sig95 = which(pvals<.05, arr=T)

yrs = jfm$yr

slopes = data.frame(jfm=cbind(mapply(function(y) as.numeric(lm(y~yrs)$coefficients[2]), jfm[,-1])),
					amic=cbind(mapply(function(y) as.numeric(lm(y~yrs)$coefficients[2]), amic[,-1])),
					dur=cbind(mapply(function(y) as.numeric(lm(y~yrs)$coefficients[2]), dur[,-1])))




if(plt_trend){
	#graphics.off()

	brks = seq(-1.5,1.5,by=.25);
	cmap = rev(hcl.colors(length(brks)-1,'blue-Red 3'))

	#png(sprintf('%s/trends.png', out_dir), w=200, h=1000)
	png(sprintf('%s/trends.png', out_dir), w=300, h=1200)
	par(mar=c(5,3,4,2))
	image.plot(x=1:3, y=1:nsubs, z=t(as.matrix(slopes[nsubs:1,])), col=cmap, breaks=brks, main='Ice Cover Trends',
			   yaxt='n', xaxt='n', yaxt='n', xlab=NA, ylab=NA, horizontal=T)
	#axis(2, at=1:nsubs, lab=meta$name, las=2)
	axis(1, at=1:3, lab=c('JFM (%/yr)','AMIC (%/yr)', NA))
	axis(1, at=1:3, lab=c(NA, NA, 'Duration\n (days/yr)'), lwd=0, line=.5)
	#axis(4, nsubs-lk_brks+.5, lab=NA, tcl=1)
	#axis(4, nsubs-lk_brks+.5, lab=NA, tcl=-1, lwd=2)
	abline(h=nsubs-lk_brks[c(-1,-6)]+.5, lwd=2, col='black')
	text(x=0.25, y=nsubs-lk_brks[-6] + diff(nsubs-lk_brks)/2 + 0.5, lab=unique(lks), srt=90, xpd=NA, cex=1.5)
	points(sig95[,2], sig95[,1], pch=1, cex=2)
	points(sig99[,2], sig99[,1], pch=20, cex=2.5)

	sig99 = which(t(pvals)<.01, arr=T)
	sig95 = which(t(pvals)<.05, arr=T)
	png(sprintf('%s/trends_horz.png', out_dir), w=1500, h=350)
	par(mar=c(5,8,4,6), cex.axis=1.5, cex.main=2)
	image.plot(y=1:3, x=1:nsubs, z=as.matrix(slopes), col=cmap, breaks=brks, main='Ice Cover Trends',
			   yaxt='n', xaxt='n', yaxt='n', xlab=NA, ylab=NA)
	#axis(1, at=1:nsubs, lab=meta$name, las=2)
	axis(2, at=1:3, lab=c('JFM\n(%/year)','AMIC\n(%/year)', 'Duration\n(days/year)'), las=2)
	#axis(1, lk_brks+.5, lab=NA, tcl=-1)
	text(y=0.125, x=lk_brks[-6] + diff(lk_brks)/2 + 0.5, lab=unique(lks), xpd=NA, cex=1.75)
	abline(v=lk_brks[c(-1,-6)]+.5, lwd=2, col='black')
	#axis(4, lk_brks+.5, lab=NA, tcl=-1, lwd=2)
	points(sig95[,2], sig95[,1], pch=1, cex=1.5)
	points(sig99[,2], sig99[,1], pch=20, cex=2.00)

}






#x11()
slopes[pvals > 0.05] = NA
slopes = cbind(slopes, meta[c('area','depth','lon','lat')])
#slopes$lon = -slopes$lon
slopes$area = slopes$area/1e3/1e3


draw_scat = function(x, y, xlab=NA, data=slopes){
	pch = 20
	cex = 2
	formula = reformulate(x, y)
	r = cor(data[y], data[x], use='pairwise')
	mylm = lm(formula, data)
	#colors = c('lightgrey','darkgrey','black')
	colors = c('gray90','gray50','black')
	color_i = colors[findInterval(abs(r), c(0,0.3,0.6))]
	plot(formula,  data, pch=pch, cex=cex, ylim=c(-1,0), ylab=NA, xaxt='n', yaxt='n', col=color_i)
	mtext(side=1, xlab, line=2)
	legend('topright', legend=sprintf('%5.2f', r), adj=c(.25,.5), cex=1.5, box.lwd=0, bg=NA)

}



areas = c(0,35e3)
deps = c(0,150)
lons = c(90,78.0)
lats = c(42,47)

run_cor = function(var, xlab=c(NA,NA,NA,NA), ytick=F){ # response var as input
	draw_scat('area',var, xlab=xlab[1])
	if (ytick){ axis(3, at=pretty(areas), lab=F); axis(3, at=areas, lab=c('0',TeX('3.5E4')), lwd=0) }
	draw_scat('depth',var, xlab=xlab[2])
	if (ytick){ axis(3, at=pretty(deps), lab=F); axis(3, at=deps, lwd=0) }
	draw_scat('lon', var,  xlab=xlab[3])
	if (ytick){ axis(3, at=pretty(-lons), lab=F); axis(3, at=-lons, lab=lons, lwd=0) }
	draw_scat('lat', var,  xlab=xlab[4])
	if (ytick){ axis(3, at=pretty(lats), lab=F); axis(3, at=lats, lwd=0) }
	axis(4, at=c(-1.0,-0.5,0))

}



#graphics.off()
png('figures/morphoscatter.png', w=1650, h=1350, pointsize=36)
#x11(w=12,h=10)
layout(matrix(1:12,3,4, byrow=T))
par(mar=c(0.5,0,0.5,0), oma=c(5,4,4,4))
run_cor('dur', ytick=T)
mtext('duration (days/year)', outer=T, cex=1, side=2, line=1.5, adj=.975)
run_cor('jfm')
mtext('JFM (%/year)', outer=T, cex=1, side=2, line=1.5)
run_cor('amic', xlab=c(TeX('area (km$^2$)'), 'depth (m)', 'longitude (°W)', 'latitude (°N)'))
mtext('AMIC (%/year)', outer=T, cex=1, side=2, line=1.5, adj=.075)
dev.off()

#plot(dur~area,  slopes,   pch=pch,  cex=cex, ylim=c(-1,0))
#lot(dur~depth, slopes,   pch=pch,  cex=cex, ylim=c(-1,0))
#lot(dur~lon,   slopes,   pch=pch,  cex=cex, ylim=c(-1,0))
#lot(dur~lat,   slopes,   pch=pch,  cex=cex, ylim=c(-1,0))






#graphics.off()


#write.table(round(jfm, digits=1), file='txt/jfm.txt', quote=F, row.names=F)
#write.table(round(amic, digits=1), file='txt/amic.txt', quote=F, row.names=F)
#write.table(round(dur, digits=1), file='txt/dur.txt', quote=F, row.names=F)

