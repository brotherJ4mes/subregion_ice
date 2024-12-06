#!/usr/bin/Rscript
source('met_utils.R')


load('stns_by_var.Rdata')


val_tmp <- apply(tmp, 1, function(x) sum(!is.na(x)))
plot(tmp$dts, val_tmp, 'l', ylab='# reporting stations', xlab=NA)
grid(nx=NA, ny=NULL)


