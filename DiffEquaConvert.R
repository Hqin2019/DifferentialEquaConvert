rm(list=ls(all=TRUE)) #Clear the environment.
#set up the working file.
setwd("C:/Users/hniqd/Google Drive/ClimateMath/DifferentialEquaConvert")

# library to read matlab data formats into R
library(R.matlab)
# read in our data
topo_025deg <- readMat("C:/Users/hniqd/Google Drive/ClimateMath/DifferentialEquaConvert//topo_025deg.mat")
str(topo_025deg)
lon<- as.vector(topo_025deg$lon)
length(lon) #1434
lat<- as.vector(topo_025deg$lat)
length(lat) #600

lon01<- t(replicate(600, lon))
lat01<- replicate(1434, lat)
dim(lon01)
#[1]  600 1434
dim(lat01)
#[1]  600 1434

hh<- 30

uv_data <- readMat("C:/Users/hniqd/Google Drive/ClimateMath/DifferentialEquaConvert//uv_remove_14_40day_600_3kkm_smooth_std_v6.mat")
str(uv_data)
ub<- uv_data$u #array
uv<- uv_data$v #array

poslon<- (lon>=175 & lon<= 275)
poslat<- abs(lat-0)<=11

lon1<- lon[poslon]#length is 400
lat1<- lat[poslat] #length is 88

#line 18-19 from Matlab code need to be converted

tim1<- as.vector(uv_data$tim)

matlab2POS = function(x, timez = "UTC") {
  days = x - 719529 	# 719529 = days from 1-1-0000 to 1-1-1970
  secs = days * 86400 # 86400 seconds in a day
  # This next string of functions is a complete disaster, but it works.
  # It tries to outsmart R by converting the secs value to a POSIXct value
  # in the UTC time zone, then converts that to a time/date string that 
  # should lose the time zone, and then it performs a second as.POSIXct()
  # conversion on the time/date string to get a POSIXct value in the user's 
  # specified timezone. Time zones are a goddamned nightmare.
  return(as.POSIXct(strftime(as.POSIXct(secs, origin = '1970-1-1', 
                                        tz = 'UTC'), format = '%Y-%m-%d %H:%M', 
                             tz = 'UTC', usetz = FALSE), tz = timez))
}
date1<- matlab2POS(tim1) #if not specify tz, it will use the current time zone.

ssh_data<- uv_data <- readMat("C:/Users/hniqd/Google Drive/ClimateMath/DifferentialEquaConvert//ssh_14_40day_600_3kkm_fft2_v6.mat")
str(ssh_data)
ssh<- ssh_data$ssh

