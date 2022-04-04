rm(list=ls(all=TRUE)) #Clear the environment.

# library to read matlab data formats into R
library(R.matlab)
# contains the meshgrid() function.
library(pracma)
# read in our data
topo_025deg <- readMat("topo_025deg.mat")
str(topo_025deg)
lon<- as.vector(topo_025deg$lon)
length(lon) #1434
lat<- as.vector(topo_025deg$lat)
length(lat) #600
lon01lat01<- meshgrid(lon, lat)

#line 7
hh<- 30

uv_data <- readMat("uv_remove_14_40day_600_3kkm_smooth_std_v6.mat")
str(uv_data)
ub<- uv_data$u #array
vb<- uv_data$v #array
lon<- as.vector(uv_data$lon)
lat<- as.vector(uv_data$lat)

poslon<- which(lon>=175 & lon<= 275)
poslat<- which(abs(lat-0)<=11)

lon1<- lon[poslon]#length is 100
lat1<- lat[poslat] #length is 44
ub<- ub[, poslat, poslon]
vb<- vb[, poslat, poslon]
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

#Line 25
ssh_data<- uv_data <- readMat("ssh_14_40day_600_3kkm_fft2_v6.mat")
str(ssh_data)
ssh<- ssh_data$ssh
sum(which(ssh==0))
#[1] 0
sst<- array(0L, dim(ssh))
sss<- array(0L, dim(sst))
lon_ssh<- as.vector(ssh_data$lon)
lat_ssh<- as.vector(ssh_data$lat)
poslon2<- (lon_ssh>=175 & lon_ssh<= 275)
poslat2<- abs(lat_ssh-0)<=11
lon2<- lon_ssh[poslon2] #length is 100
lat2<- lat_ssh[poslat2] #length is 44
ssh<- ssh[, poslat, poslon]
lon<-lon2
lat<- lat2

#Line 40
gg<- 9.8
fai<- gg*ssh
ssh1<- drop(fai[200, , ])
tim2<- as.vector(ssh_data$tim)
date2<- matlab2POS(tim2)

#Line 47
timup<- max(tim1[1], tim2[1]) #727931
timend<- min(tim1[length(tim1)], tim2[length(tim2)]) #737891
postim1<- (tim1>=timup & tim1 <= timend) #logic; length is 4908
postim2<- (tim2>=timup & tim2 <= timend) #logic; length is 3321
ub<- ub[postim1, , ]
vb<- vb[postim1, , ]
fai<- fai[postim2, , ]
tim<- tim1[postim1] #length is 3321
date<- matlab2POS(tim)
tim0<- tim
lon0<-lon
lat0<-lat

sst<- sst[postim2, poslat, poslon]
sss<- sss[postim2, poslat, poslon]

#Line 65
nx<- length(lon) #100
ny<- length(lat) #44
nt<- length(tim) #3321
a<- 6371393
deg<- 2*pi*a/360
omega<- 2*pi/24/3600
bb<- 2*omega/a
yy<- lat*deg
ff<- bb*yy



