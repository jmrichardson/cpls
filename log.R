library(readr)

x <- read_fwf(
  file="store/system.log",   
  skip=0,
  fwf_positions( c(1,25,30),c(23,29,NA),c('Date','Priority','Message')))
