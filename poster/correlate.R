#!/bin/Rscript


meta <- read.table('meta.txt', sep='\t', head=T)
meta[meta$code=='SMR','lake'] <- 'Superior'
lks <- meta$lake
lk_idx <- c(grep('Sup', lks), grep('Mic', lks), grep('Hur', lks), grep('Eri', lks), which(grepl('Ont',meta$name)))
meta <- meta[lk_idx,]
lks <- lks[lk_idx]

dir_in <- 'txt'
dir_out <- '/home/kessler/work/subregion_ice/poster/67311e171949f1bd5174bf03/figures/scatter'

jfm <- read.table(sprintf('%s/%s.txt', dir_in, 'jfm'), head=T)
amic <- read.table(sprintf('%s/%s.txt', dir_in, 'amic'), head=T)
dur <- read.table(sprintf('%s/%s.txt', dir_in, 'dur'), head=T)

vars <- c('AirTemp','AirTempMax','AirTempMin','CloudCover','Dewpoint','WindSpeed', 'Precipitation')


for (v in 1:length(vars)) assign(vars[v], read.table(sprintf('%s/%s_jfm.txt', dir_in, vars[v]), head=T))
rowdict <- list(Superior=1, Michigan=2, Huron=3, Erie=4, Ontario=5)

draw_scatter <- function(myvar, ice, ylab, ylim=c(0,115)){
	varstr <- deparse(substitute(myvar))
	icestr <- deparse(substitute(ice))
	# drop yrs for simpler indexing
	myvar <- myvar[,-1]
	ice <- ice[,-1]
	savenm <- sprintf('%s/%s_%s.png', dir_out, varstr, icestr)
	png(savenm, w=800, h=700); layout(matrix(1:25, 5,5, byrow=T)); par(oma=c(5,6,4,4))
	lastlk <- meta$lake[1]
	for (lk in 1:nrow(meta)){
		lkname <- meta$lake[lk]
		par(mar=c(0,0,0,0))
		if(lastlk != lkname){axis(4);  par(mfg=c(rowdict[[lkname]], 1)) } # new lake; new row
		plot(myvar[,lk], ice[,lk], ylim=ylim, xlab=NA, ylab=NA, xaxt='n', yaxt='n', cex=1.5, lwd=1.5)
		r <- cor(myvar[,lk], ice[,lk], use='complete.obs')
		cor_mat[icestr, varstr, meta[lk,'code']] <<- r
		mtext(sprintf('%s\n%.2f',meta$name[lk], r), side=3, line=-3, cex=1, adj=.95, col='blue')
		if(par()$mfg[2] == 1) mtext(side=2, lkname, cex=1.25, line=1)
		lastlk <- meta$lake[lk]
	}
	axis(4)
	mtext(varstr, side=1, outer=T, cex=2, line=2)
	mtext(ylab, side=2, outer=T, cex=2, line=3)
	dev.off()
	return(cor_mat)
}

#cor_amic <- data.frame()
#cor_jfm <<- data.frame()
#cor_dur <- data.frame()
cor_mat <- array(NA, dim=c(3,length(vars),length(lks)))
dimnames(cor_mat) <-  list(metric=c('jfm','amic','dur'), metvar=vars, sub=meta$code)

lab <- 'JFM Ice (%)'
draw_scatter(AirTemp,       jfm, lab)
draw_scatter(AirTempMax,    jfm, lab)
draw_scatter(AirTempMin,    jfm, lab)
draw_scatter(CloudCover,    jfm, lab)
draw_scatter(Dewpoint,      jfm, lab)
draw_scatter(WindSpeed, 	jfm, lab)
draw_scatter(Precipitation, jfm, lab)

lab <- 'AMIC (%)'
draw_scatter(AirTemp,       amic, lab)
draw_scatter(AirTempMax,    amic, lab)
draw_scatter(AirTempMin,    amic, lab)
draw_scatter(CloudCover,    amic, lab)
draw_scatter(Dewpoint,      amic, lab)
draw_scatter(WindSpeed,     amic, lab)
draw_scatter(Precipitation, amic, lab)


lab <- 'Duration (days)'
draw_scatter(AirTemp,       dur,  lab)
draw_scatter(AirTempMax,    dur,  lab)
draw_scatter(AirTempMin,    dur,  lab)
draw_scatter(CloudCover,    dur,  lab)
draw_scatter(Dewpoint,      dur,  lab)
draw_scatter(WindSpeed,     dur,  lab)
draw_scatter(Precipitation, amic, lab)


png(sprintf('%s/cor_boxplot.png', dir_out), width=1200, h=250)
par(cex.main=2, cex.axis=2, las=1)
layout(rbind(1:4))
xnames <- c('JFM', 'AMIC', 'Duration')
abline_col <- 'darkslategray'
boxplot(t(cor_mat[,'AirTempMin',]),ylim=c(-1,0), names=xnames, main='Air Temp Min');abline(h=c(-1,0,1), lty=2, col=abline_col)
boxplot(t(cor_mat[,'AirTempMax',]),ylim=c(-1,0), names=xnames, main='Air Temp Max');abline(h=c(-1,0,1), lty=2, col=abline_col)
boxplot(t(cor_mat[,'CloudCover',]),     	 	 names=xnames, main='Cloud Cover');abline(h=c(-1,0,1), lty=2, col=abline_col)
boxplot(t(cor_mat[,'WindSpeed',]),ylim=c(-.25,1), names=xnames, main='Wind Speed')    ;abline(h=c(-1,0,1), lty=2, col=abline_col)
#boxplot(t(cor_mat[,'Precipitation',]),  		 names=xnames, main='Precipitation');abline(h=c(-1,0,1), lty=2, col=abline_col)
dev.off()

