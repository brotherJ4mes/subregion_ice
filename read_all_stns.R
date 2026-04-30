#!/usr/bin/Rscript
source('met_utils.R')

t0 = '1970-01-01'

dir_in = 'stn_csv'
save_nm  = 'all_stns_1970_onward.Rdata'

plot_val = function(dat){
	val_cnt = apply(dat, 1, function(x) sum(!is.na(x)))
	plot(dat$dts, val_cnt, 'l', ylab='# reporting stations', xlab=NA,)
}


AirTempMax    = read_all_stns('AirTempMax', dir=dir_in)
#AirTempMin    = read_all_stns('AirTempMix', t0=t0, dir=dir_in)
#AirTemp       = read_all_stns('AirTemp', t0=t0, dir=dir_in)
#Precipitation = read_all_stns('Precipitation', t0=t0, dir=dir_in)
#Dewpoint      = read_all_stns('Dewpoint', t0=t0, dir=dir_in)
## windspeed fails?
#WindSpeed 	  = read_all_stns('WindSpeed', t0=t0, dir=dir_in)
#CloudCover	  = read_all_stns('CloudCover', t0=t0, dir=dir_in)

#save(AirTempMax,AirTempMin,AirTemp,Precipitation,Dewpoint,WindSpeed,CloudCover,  file=save_nm)
