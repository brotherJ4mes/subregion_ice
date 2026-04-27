#!/bin/Rscript

out_dir <- '/home/kessler/work/subregion_ice/figures/'
dat <- read.table('txt/AMIC.txt', head=T)

png(paste(out_dir, 'amic.png', sep='/'), w=1200, h=350)
#x11()
yrs <- dat[['year']]
par(cex.axis=1.5, mar=c(3,3,4,4), cex.main=1.5)
plot(bas~year, dat, 'l', xaxt='n', yaxt='n', main='Great Lakes Annual Max Ice',
	 xlab=NA, ylab=NA, ylim=c(0,100), yaxs='i', xaxs='r')
points(bas~year, dat[yrs<2014,], pch=20, cex=2)
points(bas~year, dat[yrs>2013&yrs<2026,], pch=20, cex=2, col='dodgerblue')
points(bas~year, dat[yrs==2026,], pch=21, cex=2.5, bg='white', col='dodgerblue')
points(bas~year, dat[yrs==2026,], pch='?', cex=1, col='dodgerblue')
x_lab <- seq(1975, rev(yrs)[1], by=5)

axis(1, at=x_lab, cex=2, tcl=-1, line=.75, lwd=0)
axis(1, at=x_lab, lab=NA, cex=2, tcl=-1 )
axis(1, at=yrs[!yrs %in% x_lab], lab=NA)

y_lab <- seq(0,100, by=20)
axis(4, at=y_lab, tcl=-1, lab=NA)
axis(4, at=y_lab, tcl=-1, lwd=0, line=.75)
abline(h=seq(20,80, by=20), lty=2, col='lightgrey')
axis(4, at=seq(10,90, by=20), lab=NA)
mtext('Percent Ice Cover', side=2, cex=1.5, line=1.5)
axis(2, at=y_lab[-c(1,length(y_lab))], tcl=-1, lab=NA)
axis(2, at=seq(10,90, by=20), lab=NA)
dev.off()

