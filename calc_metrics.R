#!/usr/bin/Rscript
library(fields)
library(Kendall)
out_dir = '/home/j4mes/work/subregion_ice/figures'
file_in = 'data/txt/all_years_sub_ice.txt'

thresh = 10
ice = read.table(file_in)
dts = as.Date(row.names(ice), format='%Y%m%d')
ice = ice[-c(grep('02-29', dts)),] # remove leap days
dts = dts[-c(grep('02-29', dts))]




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



#write.table(round(jfm, digits=1), file='txt/jfm.txt', quote=F, row.names=F)
#write.table(round(amic, digits=1), file='txt/amic.txt', quote=F, row.names=F)
#write.table(round(dur, digits=1), file='txt/dur.txt', quote=F, row.names=F)
