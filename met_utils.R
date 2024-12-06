create_pts <- function(stn_list_fn){
	hdr <- scan(stn_list_fn, skip=25, nlines=1, what='character', strip=T, sep=',')
	dat <- read.table(stn_list_fn, sep=',', skip=28, fill=NA, strip.white=T, quote="")
	names(dat) <- hdr
	pts <- st_as_sf(dat, coords=c('Longitude','Latitude'), crs='+proj=longlat +datum=WGS84')
	return(pts)
}

read_stn <- function(id, dir=met_dir){
	fin <- sprintf('%s/met_%s.csv', dir, id)
	print(fin)
	hdr <- scan(fin, skip=7, nlines=1, what='character', strip=T, sep=',')
	dat <- read.table(fin, sep=',', skip=9, fill=NA, strip.white=T, quote="")
	names(dat) <- hdr
	return(dat)
}

read_var <- function(varname, stn_ids, dir=met_dir, t0='1972-01-01', tf=Sys.Date(), quiet=F){
	#varname must  = one of {AirTempMin, AirTempMax, Precipitation, AirTemp, Dewpoint, WindSpeed, CloudCover}

    # preallocate dataframe
	df_out <- data.frame(dts=seq(as.Date(t0), as.Date(tf), by='days')) # preallocate dataframe
	
	# loop thru stns
    for (id in stn_ids){
		fin <- sprintf('%s/met_%s.csv', dir, id)
        if(!quiet) cat(sprintf('processing %s stn: %s \r', varname, id))
		hdr <- scan(fin, skip=7, nlines=1, what='character', strip=T, sep=',', quiet=T)
		dat <- read.table(fin, sep=',', skip=9, fill=NA, strip.white=T, quote="")
        names(dat) <- hdr
        dat <- dat[,c('Date',varname)]
        names(dat) <- c('dts',id)
        df_out <- merge(df_out, dat, by='dts', all.x=T)
    }
	if(!quiet) cat(sprintf('\n'))
    return(df_out)
}

read_all_stns <- function(varname, dir=met_dir, t0='1900-01-01', tf=Sys.Date()){
	#varname must  = one of {AirTempMin, AirTempMax, Precipitation, AirTemp, Dewpoint, WindSpeed, CloudCover}

	stn_files <- list.files(path=dir, pattern='met.*.csv', full.names=T)
 	stn_ids <- gsub('met_|.csv','',basename(stn_files))

    # preallocate dataframe
	df_out <- data.frame(dts=seq(as.Date(t0), as.Date(tf), by='days')) # preallocate dataframe
	
	# loop thru stns
    for (i in 1:length(stn_ids)){
		id <- stn_ids[i]
        cat(sprintf('processing %s stn: %s \r', varname, id))
        #cat(sprintf('processing %s stn: %s \n', varname, id))
		hdr <- scan(stn_files[i], skip=7, nlines=1, what='character', strip=T, sep=',', quiet=T)
		dat <- read.table(stn_files[i], sep=',', skip=9, fill=NA, strip.white=T, quote="")
        names(dat) <- hdr
        dat <- dat[,c('Date',varname)]
        names(dat) <- c('dts',id)
        df_out <- merge(df_out, dat, by='dts', all.x=T)
    }
    return(df_out)
}



