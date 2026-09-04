library(wesanderson)
out_dir = '~/work/subregion_ice/manuscript/figures/'
file_in = 'data/all_years_sub_ice.txt'

#thresh = 10
ice = read.table(file_in)  # 
dts = as.Date(row.names(ice), format='%Y%m%d') # 

jfm = read.table('data/jfm.txt')
amic = read.table('data/amic.txt')
ice0 = read.table('data/ice_on.txt')
icef = read.table('data/ice_off.txt')


meta = read.table('data/meta.txt', sep='\t', head=T)
lks = meta$lake
# re-order lakes (Ont is handled differently to omit Niagara (no ice data)
lk_idx = c(grep('Sup', lks), grep('Mic', lks), grep('Hur', lks), grep('Eri', lks), which(grepl('Ont',meta$name)))
meta = meta[lk_idx,]
ice = ice[,lk_idx]
nsubs = length(lk_idx)
lks = lks[lk_idx]
lk_brks = c(0,max(grep('Sup',lks)), max(grep('Mic',lks)), max(grep('Hur',lks)), max(grep('Eri',lks)), max(grep('Ont',lks)))
#lk_brks = c(0,max(grep('Sup',lks)), max(grep('Mic',lks)), max(grep('Hur',lks)), max(grep('Eri',lks)), max(grep('Ont',lks)))



# barplot of JFM and amic
if(F){
#	x11()
	fill_col='ivory'
	bar_col=wes_palette('Royal1')[2:3]
	line_col=wes_palette('Royal1')[c(1,4)]

	means = rbind(apply(jfm, 2, mean), apply(amic, 2, mean))
	sig = rbind(apply(jfm, 2, sd), apply(amic, 2, sd))
	bar_centers = barplot(means, beside=T, ylim=c(0,100))
	group_centers = apply(bar_centers, 2, mean)
	lk_brks = c(0,max(grep('Sup',lks)), max(grep('Mic',lks)), max(grep('Hur',lks)), max(grep('Eri',lks)), max(grep('Ont',lks)))
	brks_x = group_centers[lk_brks[-6]]+1.5
	#polygon(x=c(rep(brks_x[1],2), rep(brks_x[2],2)), y=c(0,100,100,0), col=fill_col, border=NA)
	#polygon(x=c(rep(brks_x[3],2), rep(brks_x[4],2)), y=c(0,100,100,0), col=fill_col, border=NA)
	grid(nx=NA, ny=NULL)
	#barplot(means, beside=T, ylim=c(0,100), add=T, col=bar_col, border=line_col)
	barplot(means, beside=T, ylim=c(0,100), add=T)
	arrows(x0=bar_centers, x1=bar_centers, y0=pmax(means-sig,0), y1=pmin(means+sig,100), code=3, angle=90, length=.05, col='grey')

	#axis(1, at=out[(round(lk_brks[-6] + diff(lk_brks)/2))+1], lab=unique(lks), line=3, lwd=0)
	axis(1, at=c(6.5,20,36.5,51.5,62), lab=unique(lks), line=2, lwd=0)
	axis(1, at=brks_x, lab=NA, line=2.5, tcl=-2,  lwd=0, lwd.ticks=1)
	axis(4, lab=NA)
	legend('topright', legend=c('JFM','AMIC'), pt.bg=grey.colors(2), pch=22, pt.cex=2, cex=1.5, inset=c(.01,.01), bg='white')

}




if(T){ # stacked bar plots
	#x11()
	par(mar=c(5,4,4,4))
	mu0 = apply(ice0, 2, mean, na.rm=T)
	muf = apply(icef, 2, mean, na.rm=T)
	sd0 = apply(ice0, 2, sd, na.rm=T)
	sdf = apply(icef, 2, sd, na.rm=T)
	min0 = apply(ice0, 2, min, na.rm=T)
	minf = apply(icef, 2, min, na.rm=T)
	max0 = apply(ice0, 2, max, na.rm=T)
	maxf = apply(icef, 2, max, na.rm=T)
	yat <- c(-30,1,32,60,91,121);
	ylabs <- c('Dec','Jan','Feb','Mar','Apr','May')

	xx = 1:nsubs



	# v1 with symbols
#	plot(x=1:nsubs, ylim=c(min(min0), max(maxf)), typ='n', xaxt='n', ylab=NA, xlab=NA, yaxt='n', las=3)
#	abline(h=yat, lty='dotted', col='lightgrey')
#	arrows(x=xx, x1=xx, y=mu0, y1=muf,   lwd=15, code=0, lend=3, col='darkslategrey')
#	matplot(xx, cbind(min0,max0), pch=1, add=T, col=c('darkslategrey','grey'))
#	matplot(xx, cbind(minf,maxf), pch=2, add=T, col=c('grey','darkslategrey'))
#    legend('top', xpd=T, inset=-.15, horiz=T, pt.bg='darkslategrey', pt.cex=c(2,1,1), cex=1,
#		   legend=c('mean duration','ice-on extrema','ice-off extrema'),
#		   pch=c(22,1,2), col=c(NA,'darkslategrey','darkslategrey'))


	# v2 with whiskers
	plot(x=1:nsubs, ylim=c(min(min0), max(maxf)), typ='n', xaxt='n', ylab=NA, xlab=NA, yaxt='n', las=3)
	abline(h=yat, lty='dotted', col='lightgrey')
	arrows(x=xx, x1=xx, y=mu0, y1=muf,   lwd=15, code=0, lend=3, col='darkslategrey')
#	arrows(x=xx, x1=xx, y=min0, y1=max0, lwd=2, code=3, lend=3, angle=90, length=.1, col=rgb(1,0,0,.5))
#	arrows(x=xx, x1=xx, y=minf, y1=maxf, lwd=2, code=3, lend=3, angle=90, length=.1, col=rgb(0,0,1,.5))
#	arrows(x=xx, x1=xx, y=mu0-sd0, y1=muf+sdf, lwd=15, code=0, lend=3, col='grey')
	arrows(x=xx, x1=xx, y=mu0-sd0, y1=mu0+sd0, lwd=1, code=3, lend=3, col='grey', angle=90, length=.1)
	arrows(x=xx, x1=xx, y=muf-sdf, y1=muf+sdf, lwd=1, code=3, lend=3, col='grey', angle=90, length=.1)
    #legend('top', xpd=T, inset=-.15, horiz=T, pt.bg='darkslategrey', pt.cex=c(2,1,1), cex=1,


	# do this for both versions
	axis(1, at=xx, lab=meta$code, las=3)
	axis(side=2, at=yat, labels=F); axis(side=2, at=yat+15, labels=ylabs, tick=F)
	axis(side=4, at=yat, labels=F); axis(side=4, at=yat+15, labels=ylabs, tick=F)


}
