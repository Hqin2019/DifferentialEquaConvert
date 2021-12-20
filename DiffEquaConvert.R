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

lon1<- lon[lon>=175 & lon<= 275] #length is 400
lat1<- lat[abs(lat-0)<=11] #length is 88
