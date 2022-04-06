rm(list=ls(all=TRUE)) #Clear the environment.

# library to read matlab data formats into R
library(R.matlab)
# contains the meshgrid() function.
library(pracma)
# read in our data
topo_025deg <- readMat("topo_025deg.mat")
str(topo_025deg)
topo<- topo_025deg$topo #matrix 600x1434
lon<- as.vector(topo_025deg$lon)
length(lon) #1434
lat<- as.vector(topo_025deg$lat)
length(lat) #600
lon01lat01<- meshgrid(lon, lat)
#lon01<- lon01lat01$X #matrix 600x1434
#lat01<- lon01lat01$Y #matrix 600x1434
lon01<- lon
lat01<- lat

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
endlon<- length(lon)
lonu<- c(lon[1]-0.5*diff(lon[1:2]), lon[1:(endlon-1)]+0.5*diff(lon),
         lon[endlon]+0.5*diff(lon[(endlon-1):endlon]))
endlat<- length(lat)
latv<- c(lat[1]-0.5*diff(lat[1:2]), lat[1:(endlat-1)]+0.5*diff(lat),
         lat[endlat]+0.5*diff(lat[(endlat-1):endlat]))
x0y0<-meshgrid(lon, lat)
xuyu<- meshgrid(lonu, lat)
xu<- xuyu$X #matrix 44x101
yu<- xuyu$Y #matrix 44x101
xvyv<- meshgrid(lon, latv)
xv<- xvyv$X #matrix 45x100
yv<- xvyv$Y #matrix 45x100

#Line 84
cc<- 2.8
rr<- sqrt(cc/2/bb)
aa<- bb*rr
dt<- diff(lon[1:2])*24*3600
dx<- diff(lon[1:2])*deg
dy<- diff(lat[1:2])*deg

nn_b<- ny*(nx+1)+nx*(ny+1) #8944
nn<- (ny-2)*(nx-1)+(nx-2)*(ny-1) #8372

#Line 98
taux<- array(0L, dim(ssh))
tauy<- array(0L, dim(ssh))
rou0<- 1024
#hh<- 30

#Line 114
ubdn<- array(0L, dim = c(nt, ny, nx+1)) #zero array with dimensions: 3321x44x101
vbdn<- array(0L, dim = c(nt, ny+1, nx)) #zero array with dimensions: 3321x45x100
#Missing line 122-123

#Line 125
ul<- ubdn[, , 1] #zero matrix 3321x44
ur<- ubdn[, , nx+1] #zero matrix 3321x44
uu<- ubdn[, ny, ] #zero matrix 3321x101
ud<- ubdn[, 1, ] #zero matrix 3321x101
vu<- vbdn[, ny+1, ] #zero matrix 3321x100
vd<- vbdn[, 1, ] #zero matrix 3321x100
vl<- vbdn[, , 1] #zero matrix 3321x45
vr<- vbdn[, , nx] #zero matrix 3321x45

u00<- drop(ubdn[1, , ]) #zero matrix 44x101
v00<- drop(vbdn[1, , ]) #zero matrix 45x100

u1<- u00[2:(nrow(u00)-1), 2:(ncol(u00)-1)] #zero matrix 42x99
v1<- v00[2:(nrow(v00)-1), 2:(ncol(v00)-1)] #zero matrix 43x98
U1<- c(matrix(t(u1), nrow=1, ncol = (ny-2)*(nx-1)), 
       matrix(t(v1), nrow=1, ncol = (nx-2)*(ny-1))) #zero vector with length 8372

u0<- array(0L, dim=c(nt, ny, nx+1)) #zero array 3321x44x101
v0<- array(0L, dim=c(nt, ny+1, nx)) #zero arary 3321x45x100

u0[1, ,]<- u00
u0[, , 1]<- ul
u0[, , nx+1]<- ur
u0[, 1, ]<- ud
u0[, ny, ]<- uu
v0[1, ,]<- v00
v0[, 1, ]<- vd
v0[, ny+1, ]<- vu
v0[, , 1]<- vl
v0[, , nx]<- vr
U0<- matrix(0L, nrow=nn_b, nt) #zero matrix 8944x3321
#Need to ask about the dimension.
for (i in 1:nt){
  U0[, i]<- c(matrix(drop(u0[i, , ]), nrow=ny*(nx+1), 1), 
              matrix(drop(v0[i, ,]),nrow = nx*(ny+1), 1))
}

u<- u0
v<- v0

#Missing line 163

lon<- lon0
lat<- lat0
tim<- tim0

#Line 172
#Need to check the meaning of "1."
ah<- 0
aat<- 3e-4 * 1
bbs<- 7.4e-4 * 1

bsst<- gg*hh/2*aat*sst #zero array 3321x44x100
bsss<- -gg*hh/2*bbs*sss #zero array 3321x44x100

jj<- 5
ii<- 10
au<- 1/ii/24/3600
av<- 1/jj/24/3600
aau<- 1/dt+au/2
ccu<- 1/dt-au/2
aav<- 1/dt+av/2
ccv<- 1/dt-av/2

#Line 190
topou<- interp2(lon01, lat01, topo, xu, yu, method = "linear") #vector length 4444




