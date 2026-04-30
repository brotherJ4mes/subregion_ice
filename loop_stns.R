#!/bin/Rscript
library(corrplot)
source('met_utils.R')
sel_stn_fn = 'stn_meta/sel_stns_20km.txt'
met_dir = 'stn_csv'
#tf = '2024-06-01'
dir_out = '/home/jemes/work/subregion_ice/txt'

sel_stn = read.table(sel_stn_fn)
stn_ids = row.names(sel_stn)
lks = colnames(sel_stn)
sel_stn = sapply(sel_stn, as.logical)


sub_name='SB'
if (!exists('sub_name')) sub_name = commandArgs(trail=T)
print(sub_name)
meta = read.table('txt/meta.txt', head=T, sep='\t', row.names=1)
fullname = meta[sub_name,'name']
lkname = meta[sub_name,'lake']




aggregate_stn_data = function(sub_name, varstr, method='mean', prnt_stats=F, plt_variance=F){
	var_daily = read_var(varstr, stn_ids[sel_stn[,sub_name]], tf='2024-12-31')

	# aggregate  to monthly
	dts = var_daily$dts
	yrmon = format(dts, '%Y-%m')
	if (method=='mean') mthy = aggregate(var_daily[,-1], by=list(yrmon=yrmon), mean, na.rm=T)[,-1]
	if (method=='FDD') mthy = aggregate(var_daily[,-1], by=list(yrmon=yrmon), function(x) sum(x<=0))[,-1]
	if (method=='max') mthy = aggregate(var_daily[,-1], by=list(yrmon=yrmon), max)[,-1]


	# print stats about sigma 
	if (prnt_stats){
		rng = range(apply(mly[,-1], 1, sd, na.rm=T), na.rm=T)
		mu = mean(apply(mly[,-1], 1, sd, na.rm=T), na.rm=T)
		cat(sprintf('%s, %5.1f, %5.1f, %5.1f \n', varstr, rng[1], rng[2], mu))
	}

	if (plt_variance){
		plot(ym, apply(mly[,-1], 1, sd, na.rm=T, ylim=))
	}


	yrs = as.numeric(substr(unique(yrmon), 1, 4))
	mon = month.abb[as.numeric(substr(unique(yrmon), 6, 8))]



	# average over all stations (for now, may want to reconsider this)
	if (method=='mean') long_dat = data.frame(yrs=yrs, mon=mon,  vals=apply(mthy, 1, mean, na.rm=T)) # now reshape 
	if (method=='max') long_dat = data.frame(yrs=yrs, mon=mon,  vals=apply(mthy, 1, max, na.rm=T)) # now reshape 
	#names(long_dat)[3] = varstr

	out = reshape(long_dat, timevar='mon', idvar='yrs', dir='wide')
	names(out) = gsub('vals\\.','', names(out))
	#out = data.frame(dts=unique(mon), val=apply(mly, 1, mean, na.rm=T))
	
	# associate July - December with the following ice season (shift down)
	out[-1,month.abb[7:12]] = out[-nrow(out),month.abb[7:12]]
	out = out[-1,]     # remove the 1972 met data (since we have no 72 ice data)
	out = out[-nrow(out),]     # remove the last year since it will likely be incomplete (no Jan - Jun)
	out = out[,c('yrs', month.abb[7:12], month.abb[1:6])] # re-order to be more intuitive per ice season

	return(out)

}



#tmax = aggregate_stn_data('WSU', 'AirTempMax')
#tmin = aggregate_stn_data('WSU', 'AirTempMin')
#fdd = aggregate_stn_data('WSU', 'AirTemp', method='FDD')

# need to update ice metrics and stns for 2025! for now exclude from 
tmax = aggregate_stn_data(sub_name, 'AirTempMax')
tmin = aggregate_stn_data(sub_name, 'AirTempMin')
pcp = aggregate_stn_data(sub_name, 'Precipitation')
wind = aggregate_stn_data(sub_name, 'WindSpeed', method='max' )
cld = aggregate_stn_data(sub_name, 'CloudCover')


wind[!is.finite(as.matrix(wind))] = NA

build_cor = function(x){
	if (!setequal(x$yrs, tmax$yrs)) stop('number of years dont match in ice and met var')

	cor_out = rbind(cor(x[,-1], tmax[,-1], use='pairwise.complete.obs'),
					cor(x[,-1], tmin[,-1], use='pairwise.complete.obs'),
					cor(x[,-1], wind[,-1], use='pairwise.complete.obs'),
					cor(x[,-1], cld[,-1], use='pairwise.complete.obs'),
					cor(x[,-1], pcp[,-1], use='pairwise.complete.obs'))
	#row.names(cor_out) = c('Tmax','Tmin','Tavg','Wind','Cloud','Tdew','Precip')
	#row.names(cor_out) = c('Tmax','Tmin','Tdew','Wind','Cloud','Precip')
	row.names(cor_out) = c('Tmax','Tmin','Wind','Cloud','Precip')
	return(cor_out)
}


#find_cor = function(x,y){

jfm = read.table('txt/jfm.txt', head=T)[c('yrs',sub_name)]
amic = read.table('txt/amic.txt', head=T)[c('yrs',sub_name)]
dur = read.table('txt/dur.txt', head=T)[c('yrs',sub_name)]



sub_yrs = function(x, selyrs) return(x[x['yrs']==selyrs,])
sel_yrs = intersect(jfm$yrs, tmax$yrs)
jfm = sub_yrs(jfm, sel_yrs)
amic = sub_yrs(amic, sel_yrs)
dur = sub_yrs(dur, sel_yrs)


cor_jfm  = build_cor(jfm)
cor_amic = build_cor(amic)
cor_dur  = build_cor(dur)

cor_jfm[is.na(cor_jfm)] = 0
cor_amic[is.na(cor_amic)] = 0
cor_dur[is.na(cor_dur)] = 0


add_cor = function(x,y) text(x, y, sprintf('%5.2f', t(cor_jfm[5:1,])[x,y]), cex=1.5, col='white')


png(sprintf('figures/cor/%s.png', sub_name), width=2400, height=980, pointsize=24)
corrplot(cor_jfm, 'square', cl.pos='n')
mtext(sprintf('JFM (%3.0f%%)', mean(as.matrix(jfm[,2]))), cex=1.5, side=2, line=1.5)
mtext(side=4, outer=T, fullname, line=-2, cex=2)


#SB
add_cor(7,4)
add_cor(5,2)

#NHU
#add_cor(7,4)
#add_cor(7,2)

dev.off()



# all 
#png(sprintf('corrplot_figs/%s.png', sub_name), width=975, height=1150)
#layout(cbind(1:3))
#corrplot(cor_dur, 'square', cl.pos='n')
#mtext(sprintf('duration (%3.0f days)', mean(as.matrix(dur))), 2)
#corrplot(cor_jfm, 'square', cl.pos='n')
#mtext(sprintf('JFM (%3.0f%%)', mean(as.matrix(jfm))), 2)
#corrplot(cor_amic, 'square', cl.pos='n')
#mtext(sprintf('AMIC (%3.0f%%)', mean(as.matrix(amic))), 2)
#mtext(side=4, outer=T, fullname, line=-2, cex=2)
#dev.off()




#miss_cld = sum(apply(cld, 1, function(x) any(is.nan(x))))
#miss_pcp = sum(apply(pcp, 1, function(x) any(is.nan(x))))
#miss_wnd = sum(apply(wind, 1, function(x) any(is.nan(x))))
#
#
#print('missing years for clds, precip and wind:')
#print(miss_cld)
#print(miss_pcp)
#print(miss_wnd)


# stepwise linear regression
#met_vars = data.frame(tmax=tmax, tmin=tmin, wind=wind, cloud=cld, precip=pcp)

#df  = data.frame(jfm = jfm[,], tmax=tmax, tmin=tmin, wind=wind, cloud=cld)
#df  = data.frame(jfm = jfm[,], tmax=tmax, tmin=tmin, wind=wind, precip=pcp)
#df  = data.frame(jfm = jfm[,]/100, tmax=tmax$Nov, tmin=tmin$Nov, wind=wind$Nov, precip=pcp$Nov, cloud=cld$Nov, tdew=dpt$Nov)
#df  = data.frame(jfm = jfm[,], tmax=tmax$Feb, tmin=tmin$Feb, wind=wind$Feb, precip=pcp$Feb, cloud=$cld$Feb, tdew=dpt$Feb)
#model = lm(jfm ~ ., df)
#step_mod = stepAIC(model, direction="both")
#C = step_mod$coefficients

#plot(1973:2024, as.matrix(df[,names(C)[-1]]) %*% C[-1] + C[1], 'l')
#points(1973:2024, t(jfm), col='blue')


#corrplot(rbind(cor_jfm, cor_amic, cor_dur), insig='blank')

##	cat(sprintf('%s \r',lk))
#	cat(sprintf('%s \t',lk))
#	ids = names(which(sel_stn[,i]==1))
#	all_stn_dat = read_var(varstr, ids, tf=tf, quiet=T)
#	if (i == 1) var_daily = data.frame(dts=all_stn_dat$dts)
#	var_daily[lk] = apply(all_stn_dat[,-1], 1, mean, na.rm=T)
#
#	# should save this instead of printing it
#	sel_dts = as.numeric(format(all_stn_dat$dts, '%m')) < 4 # JFM
#	val_stns = apply(all_stn_dat[sel_dts,-1], 1, function(x) sum(!is.na(x)))
#	cat(sprintf('numzer=%i\t %.0f\t %.0f\t %.0f\n', sum(val_stns==0), max(val_stns), median(val_stns), mean(val_stns)))
#}

# all monthly data


#should format to year mon WSU CSU, etc.

#write.table(file=sprintf('%s/%s.txt', dir_out, varstr), round(mly, digits=1), row.names=F, quote=F)



# access jan like mly[seq(1,nrow(mly),by=12),]


#mly = mly[11:nrow(mly),] # start Nov 1972

# jfm data
#mon = as.numeric(format(dts, '%m'))
#yrs = as.numeric(format(dts, '%Y'))


#jfm_var = aggregate(var_daily[mon<4,-1], 'mean', by=list(yrs=yrs[mon<4]))
#jfm_var = jfm_var[jfm_var$yrs>1972,]
#write.table(file=sprintf('%s/%s_jfm.txt', dir_out, varstr), round(jfm_var, digits=1), row.names=F, quote=F)

