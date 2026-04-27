#!/bin/bash

vars='AirTempMin AirTempMax Precipitation AirTemp Dewpoint WindSpeed CloudCover'
#xargs -n1 -P8 Rscript loop_stns.R  <<< $vars


xargs -n1 -P8 echo processing <<< $vars


# running serially
#declare -a vars=(AirTempMin AirTempMax Precipitation AirTemp Dewpoint WindSpeed CloudCover)
#for v in "${vars[@]}"; do
#	Rscript loop_stns.R $v
#done
