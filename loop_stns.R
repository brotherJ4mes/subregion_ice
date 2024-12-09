#!/bin/Rscript
source('met_utils.R')
sel_stn_fn <- 'stn_meta/sel_stns_20km.txt'
met_dir <- 'stn_csv'
tf <- '2024-06-01'
dir_out <- '/home/kessler/work/subregion_ice/txt'

sel_stn <- as.matrix(read.table(sel_stn_fn))
lks <- colnames(sel_stn)

varstr <- 'AirTempMin' # focus on air min/max (convert to FDD in the future), windspeed, RH?, 
#varstr <- commandArgs(trail=T)
#print(paste('processsing ', varstr), sep='')
cat(sprintf('%s\n', varstr))


for (i in 1:ncol(sel_stn)){
	lk <- lks[i]
#	cat(sprintf('%s \r',lk))
	cat(sprintf('%s \t',lk))
	ids <- names(which(sel_stn[,i]==1))
	all_stn_dat <- read_var(varstr, ids, tf=tf, quiet=T)
	if (i == 1) var_daily <- data.frame(dts=all_stn_dat$dts)
	var_daily[lk] <- apply(all_stn_dat[,-1], 1, mean, na.rm=T)

	# should save this instead of printing it
	sel_dts <- as.numeric(format(all_stn_dat$dts, '%m')) < 4 # JFM
	val_stns <- apply(all_stn_dat[sel_dts,-1], 1, function(x) sum(!is.na(x)))
	cat(sprintf('numzer=%i\t %.0f\t %.0f\t %.0f\n', sum(val_stns==0), max(val_stns), median(val_stns), mean(val_stns)))
}

# all monthly data
dts <- var_daily$dts
mon <- format(dts, '%Y-%m')
mly <- aggregate(var_daily, by=list(mon=mon), mean) # omit dates column
mly <- mly[,names(mly) != 'dts']


#should format to year mon WSU CSU, etc.

#write.table(file=sprintf('%s/%s.txt', dir_out, varstr), round(mly, digits=1), row.names=F, quote=F)



# access jan like mly[seq(1,nrow(mly),by=12),]


#mly <- mly[11:nrow(mly),] # start Nov 1972

# jfm data
#mon <- as.numeric(format(dts, '%m'))
#yrs <- as.numeric(format(dts, '%Y'))


stop()
#jfm_var <- aggregate(var_daily[mon<4,-1], 'mean', by=list(yrs=yrs[mon<4]))
#jfm_var <- jfm_var[jfm_var$yrs>1972,]
#write.table(file=sprintf('%s/%s_jfm.txt', dir_out, varstr), round(jfm_var, digits=1), row.names=F, quote=F)

