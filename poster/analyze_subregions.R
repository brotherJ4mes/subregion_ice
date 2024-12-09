#!/bin/Rscript
library(fields)
library(Kendall)
graphics.off()

out_dir <- '/home/kessler/work/subregion_ice/poster/tex/figures/'
in_dir <- 'define this!'


thresh <- 10
ice <- read.table('../txt/yrly_out/all.txt')
dts <- as.Date(row.names(ice), format='%Y%m%d')
ice <- ice[-c(grep('02-29', dts)),] # remove leap days
dts <- dts[-c(grep('02-29', dts))]


meta <- read.table('meta.txt', sep='\t', head=T)
lks <- meta$lake
# re-order lakes (Ont is handled differently to omit Niagara (no ice data)
lk_idx <- c(grep('Sup', lks), grep('Mic', lks), grep('Hur', lks), grep('Eri', lks), which(grepl('Ont',meta$name)))
meta <- meta[lk_idx,]
ice <- ice[,lk_idx]
nsubs <- length(lk_idx)
lks <- lks[lk_idx]
lk_brks <- c(0,max(grep('Sup',lks)), max(grep('Mic',lks)), max(grep('Hur',lks)), max(grep('Eri',lks)), max(grep('Ont',lks)))
lk_abbrevs <- c('Sup.','Mich.','Huron','Erie','Ont.')
#lk_brks <- c(0,max(grep('Sup',lks)), max(grep('Mic',lks)), max(grep('Hur',lks)), max(grep('Eri',lks)), max(grep('Ont',lks)))


# develop selection vectors
iyr <- as.numeric(format(dts, '%Y')) # ice year  
yrs <- unique(iyr)
jd <- as.numeric(format(dts, '%j'))  # julian day
mon <- format(dts, '%b')             # month 
iyr[jd>300] <- iyr[jd>300] + 1


# determine jd range

#graphics.off()
#jdr <- aggregate(jd, by=list(yr=iyr), function(x) cbind(x[1],rev(x)[1])) # JD range
#jdr <- data.frame(yr=jdr$yr, t0=jdr$x[,1], tf=jdr$x[,2])
#jdr[,2:3][jdr[,2:3]>300] <- jdr[,2:3][jdr[,2:3]>300] -365 # shift Nov/Dec days back
#plot(tf~yr, jdr, ylim=range(jdr[,-1]), pch=20, cex=4, ylab='Julian Day', xlab=NA, main='Great Lakes Wide')
#points(t0~yr, jdr, cex=3)
#abline(h=c(0,90))
#legend('right',legend=c('First Reported','Last Reported'), pch=c(1,20), pt.cex=c(3,4))
#stop()


amic <- aggregate(ice, by=list(yr=iyr), 'max')
jfm <- aggregate(ice[jd<91,], by=list(yr=iyr[jd<91]), 'mean')

ice_on  <- ice >= thresh
print('restricting window for duration')
ice_on <- ice_on[jd > 0 & jd <91,] # only consider JFM duration
onyr <-  as.numeric(format(as.Date(row.names(ice_on),'%Y%m%d'),'%Y'))
dur <- aggregate(ice_on, by=list(yr=onyr), sum)


normalize <- function(dat){ out <- cbind(data.frame(yr=unique(iyr), sweep(dat[,-1], 2, apply(dat[,-1], 2, mean), '-'))) }

heat_map <- function(dat, cmap, brks, ti_str, fout, linecol='black'){
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
#first <- aggregate(ice, by=list(yr=iyr), function(x) head(x, n=1))
#last <- aggregate(ice, by=list(yr=iyr), function(x) tail(x, n=1))
#brks <- seq(0,100,by=10)
#cmap <- viridis(length(brks)-1)
#graphics.off()
#
##x11(w=12, h=6)
##yrs <- 1973:2024
##heat_map(first, cmap, brks, 'first reported value (%)')
##labs <- as.matrix(round(first[,-1], digits=0))
##labs[labs < 10] <- NA
##text(x=rep(yrs, times=nsubs), y=rep(1:nsubs, each=length(yrs)), lab=labs, col='white')
##
##x11(w=12, h=6)
##heat_map(last, cmap, brks, 'last reported value (%)')
#labs <- as.matrix(round(last[,-1], digits=0))
#labs[labs < 10] <- NA
#text(x=rep(yrs, times=nsubs), y=rep(1:nsubs, each=length(yrs)), lab=labs, col='white')

brks <- seq(0,100,by=12.5); 
cmap <- mako(length(brks)-1)
heat_map(jfm, cmap, brks, 'Seasonal Average (JFM %)', 'jfm_ts', linecol='black')
heat_map(amic, cmap, brks, 'Annual Max Ice Cover (%)','amic_ts', linecol='black')
heat_map(dur, cmap, brks, 'Season Duration (days)', 'dur_ts', linecol='black')


brks <- seq(-100,100,by=25); 
cmap <- rev(hcl.colors(length(brks)-1,'blue-Red 3'))
jfm_diff <- normalize(jfm)
amic_diff <- normalize(amic)
dur_diff <- normalize(dur)
heat_map(jfm_diff, cmap, brks, 'JFM - JFM climatology (%)', 'jfm_diff')
heat_map(amic_diff, cmap, brks, 'AMIC - AMIC climatology (%)','amic_diff')
heat_map(dur_diff, cmap, brks, 'duration - duration climatology (days)', 'dur_diff')



#x11(w=12, h=6)
#ar(mar=c(2,4,10,3))
# <- barplot(meta$depth, names.arg=NULL,col=match(lks, unique(lks))+1, width=meta$area, ylim=c(160,0), ylab='depth')
#xis(3, x, meta$name, las=2)



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



pvals <- data.frame(jfm=apply(jfm, 2, function(x) as.numeric(MannKendall(x)$sl))[-1],
					amic=apply(amic, 2, function(x) as.numeric(MannKendall(x)$sl))[-1],
					dur=apply(dur, 2, function(x) as.numeric(MannKendall(x)$sl))[-1])

sig99 <- which(pvals<.01, arr=T)
sig95 <- which(pvals<.05, arr=T)

yrs <- jfm$yr

slopes <- data.frame(jfm=cbind(mapply(function(y) as.numeric(lm(y~yrs)$coefficients[2]), jfm[,-1])),
					amic=cbind(mapply(function(y) as.numeric(lm(y~yrs)$coefficients[2]), amic[,-1])),
					dur=cbind(mapply(function(y) as.numeric(lm(y~yrs)$coefficients[2]), dur[,-1])))



#graphics.off()

brks <- seq(-1.5,1.5,by=.25);
cmap <- rev(hcl.colors(length(brks)-1,'blue-Red 3'))

png(sprintf('%s/trends.png', out_dir), w=700, h=1000)
par(mar=c(5,10,4,5))
image.plot(x=1:3, y=1:nsubs, z=t(as.matrix(slopes)), col=cmap, breaks=brks, main='Ice Cover Trends',
		   yaxt='n', xaxt='n', yaxt='n', xlab=NA, ylab=NA)
axis(2, at=1:nsubs, lab=meta$name, las=2)
axis(1, at=1:3, lab=c('JFM (%/yr)','AMIC (%/yr)', NA))
axis(1, at=1:3, lab=c(NA, NA, 'Duration\n (days/yr)'), lwd=0, line=.5)
axis(4, lk_brks+.5, lab=NA, tcl=1)
text(3.75, y=lk_brks[-6] + diff(lk_brks)/2 + 0.5, lab=unique(lks), srt=270, xpd=NA, cex=1.5)
abline(h=lk_brks[c(-1,-6)]+.5, lwd=2, col='black')
axis(4, lk_brks+.5, lab=NA, tcl=-1, lwd=2)
points(sig95[,2], sig95[,1], pch=1)
points(sig99[,2], sig99[,1], pch=20)




sig99 <- which(t(pvals)<.01, arr=T)
sig95 <- which(t(pvals)<.05, arr=T)
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



graphics.off()


write.table(round(jfm, digits=1), file='txt/jfm.txt', quote=F, row.names=F)
write.table(round(amic, digits=1), file='txt/amic.txt', quote=F, row.names=F)
write.table(round(dur, digits=1), file='txt/dur.txt', quote=F, row.names=F)

