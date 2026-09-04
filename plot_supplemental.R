library(wesanderson)
out_dir = '~/work/subregion_ice/manuscript/figures/'
file_in = 'data/all_years_sub_ice.txt'

#thresh = 10
ice = read.table(file_in)  # 
dts = as.Date(row.names(ice), format='%Y%m%d') # 


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


	# draw timeseries plots of all 22 subregions (may include in appendix)
if(F){
	tick_yr = seq.Date(dts[13], rev(dts)[1], by='year') # jan 01
	tick_five = seq.Date(dts[236], rev(dts)[1], by='5 years') # jan 01 every 5
	draw_ts = function(name){ 
		plot(dts, ice[,name], xaxt='n', pch=20, ylim=c(0,100)); 
		mtext(side=2, name, cex=1, line=-2); 
		axis(4, at=seq(0,100, by=20), lab=c(NA,20,40,60,80,NA), line=-4)
		axis.Date(1, at=tick_yr, lab=NA)
		grid(nx=NA, ny=NULL)
	}

	draw_multi = function(sub_0, sub_f){
		x11()
		layout(cbind(1:((sub_f-sub_0)+1))); 
		par(mar=c(0,0,0,0), oma=c(3,0,3,0))
		for (name in meta$code[sub_0:sub_f]) draw_ts(name)
		axis.Date(1, at=tick_yr, lab=NA)
		axis.Date(1, at=tick_five, lwd=0)
	}

	draw_multi(1,8)
	draw_multi(9,16)
	draw_multi(17,22)
}


# show time series to illustrate window
if(T){
t0=tick_yr # jan 01
tf=t0+89 # mar 31

draw_jfm_window = function(name, title, thresh){
	shaded_col=rgb(0,0,0,.10)
	plot(dts, ice[,name], pch=20, ylab=NA,
		 xlim=c(as.Date('1972-12-01'),as.Date('1990-09-01')), 
		 xlab=NA, xaxt='n', yaxt='n')
	polygon(x=as.Date(rep(interleave(t0,tf),each=2)), y=rep(c(-10,110,110,-10),54), col=shaded_col, border=NA)
	abline(h=thresh, col=shaded_col)
	mtext(side=2, title, cex=1.5)
	axis(4)
	#axis.Date(1, at=t0)
}

layout(cbind(1:4))
par(mar=c(1,4,1,2), oma=c(3,0,0,0))
draw_jfm_window('WB','Whitefish Bay', 50)
draw_jfm_window('SB','Saginaw Bay', 50)
draw_jfm_window('WER','Western Erie', 50)
draw_jfm_window('LSC','St Clair', 50)
axis.Date(1, at=t0)

x11()
layout(cbind(1:2))
par(mar=c(1,4,1,2), oma=c(3,0,0,0))
draw_jfm_window('GrB','Green Bay', 90)
draw_jfm_window('SMR','St Marys River', 90)
axis.Date(1, at=t0)

}




# barplot of JFM and amic
if(F){
	jfm = read.table('data/jfm.txt', head=T)
	amic = read.table('data/amic.txt', head=T)

	fill_col='ivory'
	bar_col=wes_palette('Royal1')[2:3]
	line_col=wes_palette('Royal1')[c(1,4)]

	means = rbind(apply(jfm[,-1], 2, mean), apply(amic[,-1], 2, mean))
	sig = rbind(apply(jfm[,-1], 2, sd), apply(amic[,-1], 2, sd))
	bar_centers = barplot(means, beside=T, ylim=c(0,100))
	group_centers = apply(bar_centers, 2, mean)
	lk_brks = c(0,max(grep('Sup',lks)), max(grep('Mic',lks)), max(grep('Hur',lks)), max(grep('Eri',lks)), max(grep('Ont',lks)))
	brks_x = group_centers[lk_brks[-6]]+1.5
	#polygon(x=c(rep(brks_x[1],2), rep(brks_x[2],2)), y=c(0,100,100,0), col=fill_col, border=NA)
	#polygon(x=c(rep(brks_x[3],2), rep(brks_x[4],2)), y=c(0,100,100,0), col=fill_col, border=NA)
	grid(nx=NA, ny=NULL)
	#barplot(means, beside=T, ylim=c(0,100), add=T, col=bar_col, border=line_col)
	barplot(means, beside=T, ylim=c(0,100), add=T)
	arrows(x0=bar_centers, x1=bar_centers, y0=pmax(means-se,0), y1=pmin(means+se,100), code=3, angle=90, length=.05, col='grey')



	#axis(1, at=out[(round(lk_brks[-6] + diff(lk_brks)/2))+1], lab=unique(lks), line=3, lwd=0)
	axis(1, at=c(6.5,20,36.5,51.5,62), lab=unique(lks), line=2, lwd=0)
	axis(1, at=brks_x, lab=NA, line=2.5, tcl=-2,  lwd=0, lwd.ticks=1)
	axis(4, lab=NA)
	legend('topright', legend=c('JFM','AMIC'), pt.bg=grey.colors(2), pch=22, pt.cex=3, cex=2, inset=c(.01,.025), bg='white')

}




if(T){ # stacked bar plots
mu0 = apply(ice0, 2, mean, na.rm=T)
muf = apply(icef, 2, mean, na.rm=T)
sd0 = apply(ice0, 2, sd, na.rm=T)
sdf = apply(icef, 2, sd, na.rm=T)
min0 = apply(ice0, 2, min, na.rm=T)
minf = apply(icef, 2, min, na.rm=T)
max0 = apply(ice0, 2, max, na.rm=T)
maxf = apply(icef, 2, max, na.rm=T)

xx = 1:nsubs

plot(x=1:nsubs, ylim=c(min(min0), max(maxf)), typ='n', xaxt='n', ylab=NA, xlab=NA, yaxt='n')
arrows(x=xx, x1=xx, y=mu0, y1=muf,   lwd=15, code=0, lend=3, col='darkslategrey')
#arrows(x=xx, x1=xx, y=min0, y1=max0, lwd=2, code=3, lend=3, angle=90, length=.1, col=rgb(1,0,0,.5))
#arrows(x=xx, x1=xx, y=minf, y1=maxf, lwd=2, code=3, lend=3, angle=90, length=.1, col=rgb(0,0,1,.5))
#arrows(x=xx, x1=xx, y=mu0-sd0, y1=muf+sdf, lwd=15, code=0, lend=3, col='grey')
#arrows(x=xx, x1=xx, y=mu0-sd0, y1=mu0+sd0, lwd=2, code=3, lend=3, col='red', angle=90)
#arrows(x=xx, x1=xx, y=muf-sdf, y1=muf+sdf, lwd=2, code=3, lend=3, col='blue', angle=90)
matplot(xx, cbind(minf,maxf), pch=2, add=T, col=c('grey','darkslategrey'))
matplot(xx, cbind(min0,max0), pch=1, add=T, col=c('darkslategrey','grey'))

axis(1, at=xx, lab=meta$code)
yat <- c(-30,1,32,60,91,121);
ylabs <- c('Dec','Jan','Feb','Mar','Apr','May')
axis(side=2, at=yat, labels=F); axis(side=2, at=yat+15, labels=ylabs, tick=F)
axis(side=4, at=yat, labels=F); axis(side=4, at=yat+15, labels=ylabs, tick=F)

}
