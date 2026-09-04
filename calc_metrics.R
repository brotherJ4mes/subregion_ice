#!/usr/bin/Rscript
library(Kendall)
library(wesanderson)
file_in = 'data/all_years_sub_ice.txt'

ice = read.table(file_in) # leap days removed manually from file
dts = as.Date(row.names(ice), format='%Y%m%d')

meta = read.table('data/meta.txt', sep='\t', head=T)
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


yrs2rownames = function(df){
	rownames(df) = df[,'yrs']
	df = df[,-1]
	return(df)
}


# develop selection vectors
yrs = as.numeric(format(dts, '%Y')) # ice year  
jd = as.numeric(format(dts, '%j'))  # julian day
mon = format(dts, '%b')             # month 
sel_leap = (yrs %% 4 == 0 & yrs %% 100 != 0) | yrs %% 400 == 0
jd[sel_leap & jd>300] = jd[sel_leap & jd>300] - 1 # shift back nov dec leap years to avoid JD==366
iyr = yrs
iyr[jd>300] = iyr[jd>300] + 1 # push nov dec into next "ice year"
yrs = unique(iyr)


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


amic = aggregate(ice, by=list(yrs=iyr), 'max')
amic = yrs2rownames(amic)
jfm = aggregate(ice[jd<91,], by=list(yrs=iyr[jd<91]), 'mean')
jfm = yrs2rownames(jfm)


# define thresholds:
# 20% if mean JFM >= 20%
# 50% for WB, SB, WER, LSC
# 90% for GrB and SMR
# 10% for all other subregions

#thresh = ifelse(apply(jfm[,-1], 2, mean)>20, 20, 10)
thresh = ifelse(apply(jfm, 2, mean)>10, 10, 5)
thresh[c('WB','SB','WER','LSC')] = 50
thresh[c('GrB','SMR')] = 90


ice_on = t(t(ice)>thresh)
# now print how many winters "missed"
had_ice = aggregate(ice_on, by=list(yrs=iyr), function(x) !(any(x)))[,-1]
if(T){
	par(mar=c(5,8,4,6), cex.axis=1, cex.main=2)
	image(x=yrs, y=1:nsubs, z=as.matrix(had_ice), yaxt='n',  ylab=NA,  xlab=NA, col=rev(mako(2)), xaxt='n', ylim=c(nsubs+.5,0.5))
	axis(2, at=1:nsubs, lab=sprintf('%s (%i%%)', meta$code, thresh), las=2)
	axis(1, at=yrs[c(F,F,T,F,F)])
	axis(1, at=yrs, lab=NA)
	axis(3, at=yrs[c(F,F,T,F,F)], lab=NA)
	axis(4, lk_brks+.5, lab=NA, tcl=-1, lwd=2)
	abline(h=lk_brks[c(-1,-6)]+.5, lwd=2)
	text(x=max(yrs)+1.5, y=lk_brks[-6] + diff(lk_brks)/2 + 0.5, lab=unique(lks), srt=270, xpd=NA, cex=1)
	axis(4, at=1:nsubs, apply(had_ice, 2, sum), las=2, lwd=0, line=-.5)


jd[jd>300] = jd[jd>300]-365 # important to wait until NOW to do this 
dur = aggregate(ice_on, by=list(yrs=iyr), 'sum')
dur = yrs2rownames(dur)
ice0_idx = aggregate(ice_on, by=list(iyr), function(x) Position(I, x))[,-1]
icef_idx = aggregate(ice_on, by=list(iyr), function(x) Position(I, x, right=T))[,-1]
seas_len = table(iyr)
shift_days = c(0,cumsum(seas_len[-length(seas_len)]))
ice0 = matrix(jd[as.matrix(ice0_idx + shift_days)], 54, 22, dimnames = list(yrs, meta$code))
icef = matrix(jd[as.matrix(icef_idx + shift_days)]-1, 54, 22, dimnames = list(yrs, meta$code))

# testing ice on/off date precision (SAVE THIS for debug)
if (F){
	graphics.off()
	mysub = 'WSU'
	for (yr in 2026:1973){
		x11(w=12)

		# ice ON 
		#myline = as.Date(paste(yr,'01-01', sep='-')) + ice0[as.character(yr),mysub]
		#t0 = as.Date(paste(yr-1,'12-01', sep='-'))
		#tf = as.Date(paste(yr,'02-01', sep='-'))

		# ice ON 
		myline = as.Date(paste(yr,'01-01', sep='-')) + icef[as.character(yr),mysub]
		t0 = as.Date(paste(yr,'02-01', sep='-'))
		tf = as.Date(paste(yr,'06-01', sep='-'))

		plot(dts, ice[,'WSU'], xlim=c(t0,tf), xaxt='n', pch=4, cex=1.5, col=ifelse(ice[,'WSU']>10,'blue','black'))
		axis.Date(1, at=seq.Date('1970-01-01','2030-01-01', by='year'), format='%Y')
		abline(v=seq.Date('1970-01-01','2030-01-01', by='year'), col='lightgrey')
		abline(v=myline)
		abline(h=10, col='lightgrey')
	}

}

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

write.table(round(jfm, digits=1), file='data/jfm.txt', quote=F)
write.table(round(amic, digits=1), file='data/amic.txt', quote=F)
write.table(dur, file='data/dur.txt', quote=F)
write.table(ice0, file='data/ice_on.txt', quote=F)
write.table(icef, file='data/ice_off.txt', quote=F)

