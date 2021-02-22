library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(spData)
library(gstat)
library(tmap)
library(maptools)
library(readxl)
library(raster)
library(ggplot2)
library(rasterVis)
library(gridExtra)

# path1 = "C:/Users/WEM/OneDrive/AIT/papers/recharge paper/GIS"
# path2 = "C:/Users/ezhil/OneDrive/AIT/papers/recharge paper"
# 
usa <- readOGR("C:/Users/WEM/OneDrive/AIT/papers/recharge paper/GIS/usa_2.shp")
huc8 = readOGR ("C:/Users/WEM/OneDrive/AIT/papers/recharge paper/GIS/huc_2_ro_1.shp")


ncpath <- "C:/Users/WEM/OneDrive/AIT/papers/recharge paper/GRACE_2002-2019_Mainland_USA-20200709T030104Z-001/grace1/"
ncname <- "GRCTellus.JPL.200204_202007.GLO.RL06M.MSCNv02CRI"  
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "lwe_thickness"  # note: tmp means temperature (not temporary)


ncname1 = "CLM4.SCALE_FACTOR.JPL.MSCNv02CRI"
ncfname1 <- paste(ncpath, ncname1, ".nc", sep="")

# ncin <- nc_open(ncfname)
# 
# #get coord
# lon <- ncvar_get(ncin,"lon")
# nlon <- dim(lon)
# head(lon)
# 
# lat <- ncvar_get(ncin,"lat")
# nlat <- dim(lat)
# head(lat)
# 
# time <- ncvar_get(ncin,"time")
# time
# 
# tunits <- ncatt_get(ncin,"time","units")
# nt <- dim(time)
# nt
# 
# tunits
# 
# lwe_array <- ncvar_get(ncin,dname)

# fname <- "~/Documents/common_data/HadISST_sst.nc" #You will need to specify your location here
lwe1 <- brick(ncfname)  
fac = raster(ncfname1)
lwe1
plot(fac)

plot(lwe1[[58]])
#convert coord to-180
lwe1 = rotate(lwe1)
lwe1
fac = rotate(fac)

crs(lwe1)
crs(usa)
# newproj4 ="+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
# proj4string(lwe1) <- newproj4
lwe1.crop <- crop(lwe1,usa)
fac.crop <- crop(fac,usa)


plot(fac.crop)
plot(usa, add=T)

lwe2= lwe1.crop*fac.crop*10

plot(lwe1.crop[[1]])
plot(lwe2[[1]])


# lwe1.mask = mask(lwe1, usa)
# plot(lwe1.mask[[1]])

# # ne <- readOGR("C:/Users/WEM/OneDrive/AIT/papers/recharge paper/GIS/ne_bound.shp")
# huc8 = readOGR ("C:/Users/WEM/OneDrive/AIT/papers/recharge paper/GIS/huc_8_ro1.shp")


names = head(huc8)

r <- raster(ncol=148, nrow=71)
extent(r) <- extent(huc8)
crs(r) = crs(huc8)
col1 = huc8$X200001
col2 = data.frame(huc8)
col3 = col2[,35]
ro_zone = rasterize(huc8, r, c(06,13,08,12,11,17,02,10,09,03,15,14,18,16,07,05,01,04))
ro_zone1 =ro_zone
plot(ro_zone1)


ras_ro = list()
newproj3 <- "+proj=lcc +lat_1=50 +lat_2=50 +lat_0=50 +lon_0=-107 +x_0=5632642.22547 +y_0=4612545.65137 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
newproj4 ="+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

# ne <- readOGR("C:/Users/ezhil/OneDrive/AIT/papers/recharge paper/GIS/usa_2.shp")
# huc8 = readOGR ("C:/Users/ezhil/OneDrive/AIT/papers/recharge paper/GIS/huc_2_ro_1.shp")


xlim<-huc8@bbox[1,]
ylim<-huc8@bbox[2,]

# 
# library(tmap)
# library(maptools)
# library(readxl)
# library(raster)
# # # library(tmap)
re <- read_excel("C:/Users/WEM/OneDrive/AIT/papers/recharge paper/analysis/runoff_huc2_points.xlsx", sheet = "huc2")
re_test = re
cn = colnames(re[1,309:392])

# # # Create an empty grid where n is the total number of cells
# re_test = re
re_test$x = re_test$Lon
re_test$y = re_test$Lat

newproj4 ="+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

coordinates(re_test) = ~x + y
proj4string(re_test) <- newproj4
# 
# plot(ne)
ds = list()
for (i in 309:392){
  #length(re[1,])]
  #st = i
  #st = cn[i]
  #print(st)
  f = data.frame(re)
  # idx = which(is.na(f[,i]))
  # final <- f[-idx,]
  #[!(is.na(re[,i])),]
  final = f[,i]
  final1 = final
  
  #print(idx)
  x = f$Lon
  y = f$Lat
  #column = c("36557")
  
  z = final1
  xr <- round(range(xlim),1);xr
  yr <- round(range(ylim),1);yr
  b <- 0.5
  
  xyz = data.frame(x,y,z)
  #print(xyz)
  grd2 <- expand.grid(x=seq(from=xr[1],to=xr[2],by=b),
                      y=seq(from=yr[1],to=yr[2],by=b))
  
  coordinates(grd2) <- ~x+y
  gridded(grd2)   <- TRUE
  coordinates(xyz) <- ~x+y
  #coordinates(z) <- ~x+y
  idw2= idw(formula = z~1, locations=xyz, newdata = grd2)
  
  # image(idw2, col=terrain.colors(16))
  # contour(idw2, add=T)
  # points(x,y, col=4, pch=19)
  # plot(ne, add=TRUE)
  idw3 = raster(idw2)
  # idw4 = projectRaster(idw3, crs=newproj4)
  ds[[i-308]] = idw3
  #plot(ds)
  #plot(ne, add=TRUE)
  print(i)
  
}

#rp <- rasterize(huc8, r, col3)
ras_ro1 = stack(ds)
# ras_ro1 = ras_ro_all[[49:132]]


# ro = ras_ro[[10]]
plot(ras_ro1[[2]])


jan_ind1 = seq(1,84,by=12)
feb_ind1 = seq(2,84,by=12)
mar_ind1 = seq(3,84,by=12)
apl_ind1 = seq(4,84,by=12)
may_ind1 = seq(5,84,by=12)
jun_ind1 = seq(6,84,by=12)
jul_ind1 = seq(7,84,by=12)
aug_ind1 = seq(8,84,by=12)
sep_ind1 = seq(9,84,by=12)
oct_ind1 = seq(10,84,by=12)
nov_ind1 = seq(11,84,by=12)
dec_ind1 = seq(12,84,by=12)


# jan_ind1 = c(10,22,34,47,59,70,82,94,106,118,130,142,154)
# feb_ind1 = c(11,23,35,48,60,71,83,95,107,119,131,143,155)
# mar_ind1 = c(12,24,36,49,61,72,84,96,108,120,132,144,156)
# apl_ind1 = c(1,13,25,37,50,62,73,85,97,109,121,133,145,157)
# may_ind1 = c(2,14,26,38,51,63,74,86,98,110,122,134,146,158)
# jun_ind1 = c(3,15,27,39,52,64,75,87,99,111,123,135,147,159)
# jul_ind1 = c(4,16,28,40,53,65,76,88,100,112,124,136,148,160)
# aug_ind1 = c(5,17,29,41,54,66,77,89,101,113,125,137,149,161)
# sep_ind1 = c(6,18,30,42,55,67,78,90,102,114,126,138,150,162)
# oct_ind1 = c(7,19,31,43,56,68,79,91,103,115,127,139,151)
# nov_ind1 = c(8,20,32,44,57,69,80,92,104,116,128,140,152)
# dec_ind1 = c(9,21,33,45,46,58,81,93,105,117,129,141,153)


jan_ro_mod1 <- list()
feb_ro_mod1 <- list()
mar_ro_mod1 <- list()
apl_ro_mod1 <- list()
may_ro_mod1 <- list()
jun_ro_mod1 <- list()
jul_ro_mod1 <- list()
aug_ro_mod1 <- list()
sep_ro_mod1 <- list()
oct_ro_mod1 <- list()
nov_ro_mod1 <- list()
dec_ro_mod1 <- list()

# for (i in 1:length(jan_ind)){
#   
#   as <- raster(mod_ro_all[i])
#   jan_ro_mod <- addLayer(jan_ro_mod, as)
# }
# 
# plot(jan_ro_mod[[1]])

# for (i in 1:length(jan_ind1)) {
#   jan_ro_mod1[[i]] = stack(ras_ro1[jan_ind1[i]])
# }
# 
# for (i in 1:length(feb_ind1)) {
#   feb_ro_mod1[[i]] = stack(ras_ro1[feb_ind1[i]])
# }
# 
# for (i in 1:length(mar_ind1)) {
#   mar_ro_mod1[[i]] = stack(ras_ro1[mar_ind1[i]])
# }
# 
# for (i in 1:length(apl_ind1)) {
#   apl_ro_mod1[[i]] = stack(ras_ro[apl_ind1[i]])
# }
# 
# for (i in 1:length(may_ind1)) {
#   may_ro_mod1[[i]] = stack(ras_ro[may_ind1[i]])
# }
# for (i in 1:length(jun_ind1)) {
#   jun_ro_mod1[[i]] = stack(ras_ro[jun_ind1[i]])
# }
# for (i in 1:length(jul_ind1)) {
#   jul_ro_mod1[[i]] = stack(ras_ro[jul_ind1[i]])
# }
# for (i in 1:length(aug_ind1)) {
#   aug_ro_mod1[[i]] = stack(ras_ro[aug_ind1[i]])
# }
# for (i in 1:length(sep_ind1)) {
#   sep_ro_mod1[[i]] = stack(ras_ro[sep_ind1[i]])
# }
# for (i in 1:length(oct_ind1)) {
#   oct_ro_mod1[[i]] = stack(ras_ro[oct_ind1[i]])
# }
# for (i in 1:length(nov_ind1)) {
#   nov_ro_mod1[[i]] = stack(ras_ro[nov_ind1[i]])
# }
# for (i in 1:length(dec_ind1)) {
#   dec_ro_mod1[[i]] = stack(ras_ro[dec_ind1[i]])
# }

jan_stk1 = stack(ras_ro1[[jan_ind1]])
feb_stk1 = stack(ras_ro1[[feb_ind1]])
mar_stk1 = stack(ras_ro1[[mar_ind1]])
apl_stk1 = stack(ras_ro1[[apl_ind1]])
may_stk1 = stack(ras_ro1[[may_ind1]])
jun_stk1 = stack(ras_ro1[[jun_ind1]])
jul_stk1 = stack(ras_ro1[[jul_ind1]])
aug_stk1 = stack(ras_ro1[[aug_ind1]])
sep_stk1 = stack(ras_ro1[[sep_ind1]])
oct_stk1 = stack(ras_ro1[[oct_ind1]])
nov_stk1 = stack(ras_ro1[[nov_ind1]])
dec_stk1 = stack(ras_ro1[[dec_ind1]])

mean_jan1 <- calc(jan_stk1, fun = mean, na.rm = T)
crs(mean_jan1) = newproj4
mean_feb1 <- calc(feb_stk1, fun = mean, na.rm = T)
crs(mean_feb1) = newproj4
mean_mar1 <- calc(mar_stk1, fun = mean, na.rm = T)
crs(mean_mar1) = newproj4
mean_apl1 <- calc(apl_stk1, fun = mean, na.rm = T)
crs(mean_apl1) = newproj4
mean_may1 <- calc(may_stk1, fun = mean, na.rm = T)
crs(mean_may1) = newproj4
mean_jun1 <- calc(jun_stk1, fun = mean, na.rm = T)
crs(mean_jun1) = newproj4
mean_jul1 <- calc(jul_stk1, fun = mean, na.rm = T)
crs(mean_jul1) = newproj4
mean_aug1 <- calc(aug_stk1, fun = mean, na.rm = T)
crs(mean_aug1) = newproj4
mean_sep1 <- calc(sep_stk1, fun = mean, na.rm = T)
crs(mean_sep1) = newproj4
mean_oct1 <- calc(oct_stk1, fun = mean, na.rm = T)
crs(mean_oct1) = newproj4
mean_nov1 <- calc(nov_stk1, fun = mean, na.rm = T)
crs(mean_nov1) = newproj4
mean_dec1 <- calc(dec_stk1, fun = mean, na.rm = T)
crs(mean_dec1) = newproj4

ann_stk1 = stack(mean_jan1, mean_feb1,mean_mar1,mean_apl1,mean_may1, mean_jun1, mean_jul1, mean_aug1, mean_sep1, mean_oct1, mean_nov1, mean_dec1)
mean_ann1 <- calc(ann_stk1, fun = sum, na.rm = T)


plot(mean_ann1)
plot(huc8, add=TRUE)



#generate a list of input rasters ("grids")
#pattern = "*.tif$" - filters for main raster files only and skips any associated files (e.g. world files)

ppt_grids <- list.files("C:/Users/WEM/OneDrive/AIT/papers/recharge paper/narr_data/ppt1/rename", pattern = "*.tif$")
ppt_s <- raster::stack(paste0("C:/Users/WEM/OneDrive/AIT/papers/recharge paper/narr_data/ppt1/rename/", ppt_grids))

# ppt_grids <- list.files("C:/Users/WEM/OneDrive/AIT/papers/recharge paper/GIS/ppt", pattern = "*.tif$")
# ppt_s <- raster::stack(paste0("C:/Users/WEM/OneDrive/AIT/papers/recharge paper/GIS/ppt/", ppt_grids))



et_grids <- list.files("C:/Users/WEM/OneDrive/AIT/papers/recharge paper/GIS/CR_ET1/ET_new", pattern = "*.tif$")
et_s <- raster::stack(paste0("C:/Users/WEM/OneDrive/AIT/papers/recharge paper/GIS/CR_ET1/ET_new/", et_grids))

# et_grids <- list.files("C:/Users/WEM/OneDrive/AIT/papers/recharge paper/GIS/ET_new", pattern = "*.tif$")
# et_s <- raster::stack(paste0("C:/Users/WEM/OneDrive/AIT/papers/recharge paper/GIS/ET_new/", et_grids))

m1_ro1_grids <- list.files("C:/Users/WEM/OneDrive/AIT/papers/recharge paper/GLDAS/NOAH/surface_runoff", pattern = "*.tif$")
m1_ro1_s <- raster::stack(paste0("C:/Users/WEM/OneDrive/AIT/papers/recharge paper/GLDAS/NOAH/surface_runoff/", m1_ro1_grids))

m1_ro2_grids <- list.files("C:/Users/WEM/OneDrive/AIT/papers/recharge paper/GLDAS/NOAH/baseflow", pattern = "*.tif$")
m1_ro2_s <- raster::stack(paste0("C:/Users/WEM/OneDrive/AIT/papers/recharge paper/GLDAS/NOAH/baseflow/", m1_ro2_grids))

m2_ro1_grids <- list.files("C:/Users/WEM/OneDrive/AIT/papers/recharge paper/GLDAS/VIC/surface_ro", pattern = "*.tif$")
m2_ro1_s <- raster::stack(paste0("C:/Users/WEM/OneDrive/AIT/papers/recharge paper/GLDAS/VIC/surface_ro/", m2_ro1_grids))

m2_ro2_grids <- list.files("C:/Users/WEM/OneDrive/AIT/papers/recharge paper/GLDAS/VIC/baseflow", pattern = "*.tif$")
m2_ro2_s <- raster::stack(paste0("C:/Users/WEM/OneDrive/AIT/papers/recharge paper/GLDAS/VIC/baseflow/", m2_ro2_grids))



# ppt1 = ppt_s[[1]]
# et1 = et_s[[1]]
# # ro1 = ras_ro[[1]]
# 
# 
# ppt1_s = focal(ppt1, w = matrix(1, nrow = 11, ncol = 11), fun = mean)
# 
# 
# plot(ppt1[[1]])
# plot(ppt1_s)

# plot(et1[[1]])
# plot(et1_s)


# plot(et1)
# plot(ro1)
# ds = list()
# for (i in 1:190){
#   
#   r2 = et_s[[i]]
#   r1 = resample(ppt_s[[i]], et_s[[i]], "bilinear")
#   r3 = resample(ras_ro[[i]], et_s[[i]], "bilinear")
#   ds[[i]] <- overlay(r1, r2, r3, fun=function(r1, r2, r3){return(r1-r2-r3)})
#   
# }
# 
# plot(ds[[150]])
# gd = dfr[[5]]
# res(gd)
# gd

#grace read
path = "C:/Users/WEM/OneDrive/AIT/papers/recharge paper/GRACE_2002-2019_Mainland_USA-20200709T030104Z-001/grace"
#path = "C:/Users/WEM/OneDrive/AIT/papers/recharge paper/analysis/grace_data"
setwd(path)
temp = list()


temp <- list.files(pattern ='*.csv')


# mid <- as.numeric(substring(temp, first = 4, last = 5))
# dia=as.Date(temp[,1],"%y/%m/%d")
# id = substr(temp, 5,11)
#
#
#
# ndate1 <- as.Date(id, "%Y_%B");
# ndate1

dfr = list()
dfr1 = list()

newproj3 <- "+proj=lcc +lat_1=50 +lat_2=50 +lat_0=50 +lon_0=-107 +x_0=5632642.22547 +y_0=4612545.65137 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
newproj4 ="+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

for (i in 1:length(temp)){
  # length(temp)
  id = substr(temp[i], 1,11)
  tws <- read.csv(file = temp[i], header = TRUE)
  df = data.frame(tws)
  ras1 <- rasterFromXYZ(df)
  #conver to mm
  ras1 <- ras1*10
  crs(ras1) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  ras1 <- projectRaster(ras1, crs=newproj4)
  names(ras1) = id

  dfr[[i]] = ras1


}
# 
# #specifi file analsyis
# # 
# # files <- list.files(directory, pattern = paste0("^", filename, ".*\\.csv$"))
# # 
# # indices <- format(as.Date(names(ndvi.stack), format = "X%Y.%m.%d"), format = "%m")
# # indices <- as.numeric(indices)
# 
# 
# 
# 
# 
# plot(dfr[[1]])
# 
# # fun <- function(x) { (x[1]-x[2])/(x[1]+x[2])}
# # ndii <- calc(rast_stack, fun)
# 
# # newproj3 <- "+proj=lcc +lat_1=50 +lat_2=50 +lat_0=50 +lon_0=-107 +x_0=5632642.22547 +y_0=4612545.65137 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
# # 
# 
# # y = seq(1,84, by =12)
# # for (i in 2:length(y)){
# #   #length(dfr)
# #   id1 = substr(temp[i], 1,11)
# #   r1 = dfr[[y[i-1]]]
# #   r2 = resample(dfr[[y[i]]], dfr[[y[i-1]]], "bilinear")
# #   # r3 = resample(ras_ro[[i]], et_s[[i]], "bilinear")
# #   r3 <- overlay(r1, r2, fun=function(r1, r2){return(r2-r1)})
# #   pr3 <- projectRaster(r3, crs=newproj3)
# #   names(pr3) = id1
# #   
# #   dfr1[[i-1]] = pr3
# # }
# 
# 
newproj4 ="+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
for (i in 2:(length(dfr)-1)){
  #length(dfr)
  id1 = substr(temp[i], 1,11)
  r1 = dfr[[i-1]]
  r2 = resample(dfr[[i+1]], dfr[[i-1]], "bilinear")
  # r3 = resample(ras_ro[[i]], et_s[[i]], "bilinear")
  r3 <- overlay(r1, r2, fun=function(r1, r2){return(0.5*(r2-r1))})
  pr3 <- projectRaster(r3, crs=newproj4)
  names(pr3) = id1

  dfr1[[i]] = pr3
}


names(lwe1)
dfr2=list()

dfr2 = (lwe2[[19:102]])

# plot(dfr2[[2]])
twsc=list()
newproj4 ="+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

for (i in (2:83)){
  print(i)
  # id1 = substr(temp[i], 1,11)
  r1 = dfr2[[i-1]]
  r2 = resample(dfr2[[i+1]], dfr2[[i-1]], "bilinear")
  # r3 = resample(ras_ro[[i]], et_s[[i]], "bilinear")
  r3 <- overlay(r1, r2, fun=function(r1, r2){return(0.5*(r2-r1))})
  pr3 <- projectRaster(r3, crs=newproj4)
  # names(pr3) = id1

  twsc[[i]] = pr3
}



plot(twsc[[2]])


#annual
# ppt_ann = list()
# et_ann = list()

# ppt_s1 = ppt_s[[277:360]]
# et_s1 = et_s[[49:132]]

ppt_s1 = ppt_s[[301:384]]
et_s1 = et_s[[301:384]]

ppt_s2 = ppt_s1*86400*30


# plot(m1_ro2_s[[1]])
m1_ro = (m1_ro1_s+m1_ro2_s)*240

m1_bf = m1_ro2_s*240

m2_ro = (m2_ro1_s+m2_ro2_s)*240

m2_bf = m2_ro2_s*240

m2_bf


# plot(m1_bf[[5]])

# # ppt1_s = focal(ppt_s1[[2]], w = matrix(1, nrow = 61, ncol = 61), fun = mean)
# # et1_s = focal(et_s1[[2]], w = matrix(1, nrow = 61, ncol = 61), fun = mean)
# # 
# # 
# # plot(ppt1_s)
# # plot(et1_s)
# # plot(dfr1[[2]])
# # 
# # dfr2 = resample(dfr1[[2]], ppt1_s, "bilinear")
# # dfr3 = resample(dfr1[[2]], ppt_s1[[2]], "bilinear")
# # 
# # 
# # dif = ppt_s1[[2]] - et_s1[[2]] - dfr3
# # writeRaster(dif,'C:/Users/ezhil/OneDrive/AIT/papers/recharge paper/maps/t1.tif',options=c('TFW=YES'))
# 
# plot(dif)

# r11 = focal(r1, w = matrix(1, nrow = 11, ncol = 11), fun = mean)


# ras_ro_all = stack(ras_ro)
# ras_ro1 = ras_ro_all[[49:133]]
#calculate mod_ro

# plot(ppt_s1[[7]])
# 
# 
# y1 = seq(1,84, by =12)

# for (i in 2:length(y1)){
#   
#   print(i)
#   # id2 = substr(temp[i], 1,11)
#   # ppt
#   # 
#   rr1 = ppt_s1[[seq(y1[i-1],y1[i],by=1)]]
#   rr2 = et_s1[[seq(y1[i-1],y1[i],by=1)]]
#   # r21 = stack(r2)
#   r1 <- calc(stack(rr1), fun = sum, na.rm = T)
#   r2 <- calc(stack(rr2), fun = sum, na.rm = T)
#   r3 = dfr1[[i-1]]
#   # ppt_ann1 <- calc(stack(r1), fun = sum, na.rm = T)
#   r1 = resample(r1, r2, "bilinear")
#   r3 = resample(r3, r2, "bilinear")
#   mod_ro[[i-1]] <- overlay(r1, r2, r3, fun=function(r1, r2, r3){return(r1-r2-(r3))})
#   
# }
# plot(ppt_ann1)
# plot(et_ann1)
# plot(mod_ro[[1]])

mod_ro =list()

np_lcc ="+proj=lcc +lat_1=50 +lat_2=50 +lat_0=50 +lon_0=-107 +x_0=5632642.22547 +y_0=4612545.65137 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

plot(twsc[[2]])


for (i in 2:83){
  # (length(ppt_s1)-1)
  print(i)

  r1 = ppt_s2[[i]]
  
  # extent(r11)=extent(ro_zone1)
  r11 = focal(r1, w = matrix(1/49, nrow = 7, ncol = 7), na.rm= TRUE)
  r11=projectRaster(r11, crs=newproj4)
  
  # r11 <- focal(r1, w=matrix(1/100,nrow=10,ncol=10))
  r2 = et_s1[[i]]
  
  r21 = focal(r2, w = matrix(1/49, nrow = 7, ncol = 7), na.rm = TRUE)
  r21=projectRaster(r21, crs=newproj4)
  # r21 <- focal(r2, w=matrix(1/100,nrow=10,ncol=10))


  r3 = dfr1[[i]]
  # r31 = projectRaster(r3, crs=np_lcc)
  r21 = resample(r21, r3, "bilinear")
  r11 = resample(r11, r3, "bilinear")
  r4 = r11-r21-r3
  mod_ro[[i]] = r4
  
  # mod_ro[[i]] <- overlay(r11, r21, r31, fun=function(r11, r21, r31){return(r11-r21-(r31))})
  # mod_ro[[i]] = focal(r4, w = matrix(1, nrow = 11, ncol = 11), fun = mean)

}

plot(mod_ro[[2]])
# plot(ras_ro1[[2]])
# ras_ro_all = stack(ras_ro)
plot(ro_zone1, add=T)
# z1 = zonal(mod_ro[[2]], huc8, fun = "mean")
# writeRaster(mod_ro[[2]],'C:/Users/ezhil/OneDrive/AIT/papers/recharge paper/maps/mod_ro_3.tif',options=c('TFW=YES'))

# 
# mean_jan <- stackApply(STACK1, indices =  rep(1,nlayers(STACK1)), fun = "mean", na.rm = T)


jan_ind = seq(13,84,by=12)
feb_ind = seq(2,84,by=12)
mar_ind = seq(3,84,by=12)
apl_ind = seq(4,84,by=12)
may_ind = seq(5,84,by=12)
jun_ind = seq(6,84,by=12)
jul_ind = seq(7,84,by=12)
aug_ind = seq(8,84,by=12)
sep_ind = seq(9,84,by=12)
oct_ind = seq(10,84,by=12)
nov_ind = seq(11,84,by=12)
dec_ind = seq(12,83,by=12)

# jan_ind = c(8,19,31,43,55,67,79,91,112,122)
# feb_ind = c(9,20,32,44,56,68,80,92,103,113)
# mar_ind = c(10,21,33,45,57,69,81,93,104,114,132)
# apl_ind = c(1,11,22,34,46,58,70,82,94,105,124)
# may_ind = c(23,35,47,59,71,83,95,125)
# jun_ind = c(24,36,48,60,72,84,96,116,126)
# jul_ind = c(13,25,37,49,61,73,85,97,107,117)
# aug_ind = c(3,14,26,38,50,62,74,86,98,108,118)
# sep_ind = c(4,15,27,39,51,63,75,87,99,109)
# oct_ind = c(5,16,28,40,52,64,76,88,100,110,128)
# nov_ind = c(6,17,29,41,53,65,77,89,101,120,129)
# dec_ind = c(7,18,30,42,54,66,78,90,102,121,130)

# jan_ind = c(8,19,31,43,55,67,79,91)
# feb_ind = c(9,20,32,44,56,68,80,92)
# mar_ind = c(10,21,33,45,57,69,81,93)
# apl_ind = c(1,11,22,34,46,58,70,82,94)
# may_ind = c(23,35,47,59,71,83,95)
# jun_ind = c(24,36,48,60,72,84,96)
# jul_ind = c(13,25,37,49,61,73,85,97)
# aug_ind = c(3,14,26,38,50,62,74,86,98)
# sep_ind = c(4,15,27,39,51,63,75,87,99)
# oct_ind = c(5,16,28,40,52,64,76,88,100)
# nov_ind = c(6,17,29,41,53,65,77,89,101)
# dec_ind = c(7,18,30,42,54,66,78,90,102)


jan_ro_mod <- list()
feb_ro_mod <- list()
mar_ro_mod <- list()
apl_ro_mod <- list()
may_ro_mod <- list()
jun_ro_mod <- list()
jul_ro_mod <- list()
aug_ro_mod <- list()
sep_ro_mod <- list()
oct_ro_mod <- list()
nov_ro_mod <- list()
dec_ro_mod <- list()

# for (i in 1:length(jan_ind)){
#   
#   as <- raster(mod_ro_all[i])
#   jan_ro_mod <- addLayer(jan_ro_mod, as)
# }
# 
# plot(jan_ro_mod[[1]])
# 
# for (i in 2:length(jan_ind)) {
#   jan_ro_mod[[i-1]] = stack(m2_ro[jan_ind[i]])
# }
# 
# for (i in 1:length(feb_ind)) {
#   feb_ro_mod[[i]] = stack(m2_ro[feb_ind[i]])
# }
# 
# for (i in 1:length(mar_ind)) {
#   mar_ro_mod[[i]] = stack(m2_ro[mar_ind[i]])
# }
# 
# for (i in 1:length(apl_ind)) {
#   apl_ro_mod[[i]] = stack(m2_ro[apl_ind[i]])
# }
# 
# for (i in 1:length(may_ind)) {
#   may_ro_mod[[i]] = stack(m2_ro[may_ind[i]])
# }
# for (i in 1:length(jun_ind)) {
#   jun_ro_mod[[i]] = stack(m2_ro[jun_ind[i]])
# }
# for (i in 1:length(jul_ind)) {
#   jul_ro_mod[[i]] = stack(m2_ro[jul_ind[i]])
# }
# for (i in 1:length(aug_ind)) {
#   aug_ro_mod[[i]] = stack(m2_ro[aug_ind[i]])
# }
# for (i in 1:length(sep_ind)) {
#   sep_ro_mod[[i]] = stack(m2_ro[sep_ind[i]])
# }
# for (i in 1:length(oct_ind)) {
# oct_ro_mod[[i]] = stack(m2_ro[oct_ind[i]])
# }
# for (i in 1:length(nov_ind)) {
#   nov_ro_mod[[i]] = stack(m2_ro[nov_ind[i]])
# }
# for (i in 1:(length(dec_ind)-1)) {
#   dec_ro_mod[[i]] = stack(m2_ro[dec_ind[i]])
# }
plot(m1_bf[[1]])


jan_stk = stack(m1_bf[[jan_ind]])
feb_stk = stack(m1_bf[[feb_ind]])
mar_stk = stack(m1_bf[[mar_ind]])
apl_stk = stack(m1_bf[[apl_ind]])
may_stk = stack(m1_bf[[may_ind]])
jun_stk = stack(m1_bf[[jun_ind]])
jul_stk = stack(m1_bf[[jul_ind]])
aug_stk = stack(m1_bf[[aug_ind]])
sep_stk = stack(m1_bf[[sep_ind]])
oct_stk = stack(m1_bf[[oct_ind]])
nov_stk = stack(m1_bf[[nov_ind]])
dec_stk = stack(m1_bf[[dec_ind]])

mean_jan <- calc(jan_stk, fun = mean, na.rm = T)
# crs(mean_jan) = newproj4
mean_feb <- calc(feb_stk, fun = mean, na.rm = T)
# crs(mean_feb) = newproj4
mean_mar <- calc(mar_stk, fun = mean, na.rm = T)
# crs(mean_mar) = newproj4
mean_apl <- calc(apl_stk, fun = mean, na.rm = T)
# crs(mean_apl) = newproj4
mean_may <- calc(may_stk, fun = mean, na.rm = T)
# crs(mean_may) = newproj4
mean_jun <- calc(jun_stk, fun = mean, na.rm = T)
# crs(mean_jun) = newproj4
mean_jul <- calc(jul_stk, fun = mean, na.rm = T)
# crs(mean_jul) = newproj4
mean_aug <- calc(aug_stk, fun = mean, na.rm = T)
# crs(mean_aug) = newproj4
mean_sep <- calc(sep_stk, fun = mean, na.rm = T)
# crs(mean_sep) = newproj4
mean_oct <- calc(oct_stk, fun = mean, na.rm = T)
# crs(mean_oct) = newproj4
mean_nov <- calc(nov_stk, fun = mean, na.rm = T)
# crs(mean_nov) = newproj4
mean_dec <- calc(dec_stk, fun = mean, na.rm = T)
# crs(mean_dec) = newproj4


plot(mean_jun)


ann_stk = stack(mean_jan, mean_feb,mean_mar,mean_apl,mean_may, mean_jun, mean_jul, mean_aug, mean_sep, mean_oct, mean_nov, mean_dec)
mean_ann <- calc(ann_stk, fun = sum, na.rm = T)
plot(mean_ann)
plot(ro_zone1, add=T)
mean_ann_m = mask(mean_ann, usa)

stk1 = stack(mean_dec, mean_jan, mean_feb, mean_mar)
mean_s1 = calc(stk1, fun = sum, na.rm = T)

stk2 = stack(mean_apl, mean_may, mean_jun, mean_jul)
mean_s2 = calc(stk2, fun = sum, na.rm = T)

stk3 = stack(mean_aug, mean_sep, mean_oct, mean_nov)
mean_s3 = calc(stk3, fun = sum, na.rm = T)

stk1_1 = stack(mean_dec1, mean_jan1, mean_feb1, mean_mar1)
mean_s1_1 = calc(stk1_1, fun = sum, na.rm = T)

stk1_2 = stack(mean_apl1, mean_may1, mean_jun1, mean_jul1)
mean_s2_1 = calc(stk1_2, fun = sum, na.rm = T)

stk1_3 = stack(mean_aug1, mean_sep1, mean_oct1, mean_nov1)
mean_s3_1 = calc(stk1_3, fun = sum, na.rm = T)

# 

# mean_ann = focal(mean_ann, w = matrix(1, nrow = 61, ncol = 61), fun = mean, pad = TRUE, na.rm= TRUE)
# plot(mean_djbm)
# plot(mean_djbm1)
plot(mean_ann_m)
# mean_ann_mask = mask(mean_ann,usa)
# ro_zone_mask = mask(ro_zone,usa)
# # extent(ro_zone)=mean_ann
# plot(mean_ann_mask)
# plot(ro_zone1, add=T)
# 
# roz = projectRaster(ro_zone, crs = np_lcc)
# plot(roz, add=T)
# mean_s1_1 = resample(mean_s1_1,mean_ann,'bilinear')
# mean_s2_1 = resample(mean_s2_1,mean_ann,'bilinear')
# mean_s3_1 = resample(mean_s3_1,mean_ann,'bilinear')
# mean_ann1 = resample(mean_ann1,mean_ann,'bilinear')
# # mean_s1_1 = resample(mean_s1_1,mean_ann,'bilinear')
# # mean_s2_1 = resample(mean_s2_1,mean_ann,'bilinear')
# # mean_s3_1 = resample(mean_s3_1,mean_ann,'bilinear')
mean_ann1 = resample(mean_ann1,mean_ann,'bilinear')
mean_jan1 = resample(mean_jan1,mean_ann,'bilinear')
mean_aug1 = resample(mean_aug1,mean_ann,'bilinear')
ann_stk1 = resample(ann_stk1,mean_ann,'bilinear')
# 
# 
# 
# extent(ro_zone1)=extent(mean_ann_m)
rozone1 = resample(ro_zone1,mean_ann_m,'bilinear')
plot(mean_ann_m)
plot(rozone1, add=T)
# 
# # mean_ann = focal(mean_ann, w = matrix(1, nrow = 61, ncol = 61), fun = mean, pad = TRUE, na.rm= TRUE)
# s1_mod_ro  = zonal(mean_s1, rozone1, fun = "mean", na.rm=TRUE)
# s1_obs_ro = zonal(mean_s1_1, rozone1, fun = "mean", na.rm=TRUE)
# 
# s2_mod_ro  = zonal(mean_s2, rozone1, fun = "mean", na.rm=TRUE)
# s2_obs_ro = zonal(mean_s2_1, rozone1, fun = "mean", na.rm=TRUE)
# 
# s3_mod_ro  = zonal(mean_s3, rozone1, fun = "mean", na.rm=TRUE)
# s3_obs_ro = zonal(mean_s3_1, rozone1, fun = "mean", na.rm=TRUE)
# 
# 
# plot(mean_ann)
# plot(huc8, add=T)

mod_ro1  = zonal(mean_ann_m, rozone1, fun = "mean", na.rm=TRUE)
obs_ro1 = zonal(mean_ann1, rozone1, fun = "mean", na.rm=TRUE)
mod_ro1
obs_ro1
plot(mean_ann)
mean_ann_mod = mean_ann

# writeRaster(mean_ann1,'C:/Users/ezhil/OneDrive/AIT/papers/recharge paper/maps/obs_ro_new.tif',options=c('TFW=YES'))
# 
# bfi <- raster(x = "C:/Users/ezhil/OneDrive/AIT/papers/recharge paper/maps/bfi.tif")
# bfi1 <- projectRaster(bfi, res = 0.04166667, crs=newproj4)
# # extent(bfi1)=mean_ann_mod
# bfi2 = resample(bfi1, mean_ann, "bilinear")
# mean_ann_obs = resample(mean_ann_obs, mean_ann_mod, "bilinear")
# 
# plot(bfi2)
# plot(usa, add=T)
# mod_bf_ann = mean_ann
# plot(mod_bf_ann)
# 
# 
# obs_bf = mean_ann_obs * (bfi2*0.01)
# plot(obs_bf)
# 
# extent(ro_zone)=mod_bf_ann
# rozone1 = resample(ro_zone,mod_bf_ann,'bilinear')
# mod_bf1  = zonal(mod_bf_ann, rozone1, fun = "mean", na.rm=TRUE)
# obs_bf1  = zonal(obs_bf, rozone1, fun = "mean", na.rm=TRUE)
# mod_bf1
# obs_bf1


#plotting
mean_jan_mod = resample(mean_jan,mean_ann1,'bilinear')
mean_feb_mod = resample(mean_feb,mean_ann,'bilinear')
mean_mar_mod = resample(mean_mar,mean_ann,'bilinear')
mean_apl_mod = resample(mean_apl,mean_ann,'bilinear')
mean_may_mod = resample(mean_may,mean_ann,'bilinear')
mean_jun_mod = resample(mean_jun,mean_ann,'bilinear')
mean_jul_mod = resample(mean_jul,mean_ann,'bilinear')
mean_aug_mod = resample(mean_aug,mean_ann,'bilinear')
mean_sep_mod = resample(mean_sep,mean_ann,'bilinear')
mean_oct_mod = resample(mean_oct,mean_ann,'bilinear')
mean_nov_mod = resample(mean_nov,mean_ann,'bilinear')
mean_dec_mod = resample(mean_dec,mean_ann,'bilinear')


# mean_jan_mod = crop(mean_jan_mod,extent(usa))
mean_jan_mod = mask(mean_jan_mod,usa)
mean_feb_mod = mask(mean_feb_mod,usa)
mean_mar_mod = mask(mean_mar_mod,usa)
mean_apl_mod = mask(mean_apl_mod,usa)
mean_may_mod = mask(mean_may_mod,usa)
mean_jun_mod = mask(mean_jun_mod,usa)
mean_jul_mod = mask(mean_jul_mod,usa)
mean_aug_mod = mask(mean_aug_mod,usa)
mean_sep_mod = mask(mean_sep_mod,usa)
mean_oct_mod = mask(mean_oct_mod,usa)
mean_nov_mod = mask(mean_nov_mod,usa)
mean_dec_mod = mask(mean_dec_mod,usa)

mean_ann_mod =mean_ann
mean_ann_mod = mask(mean_ann_mod,usa)



#obs
mean_jan_obs = resample(mean_jan1,mean_jan_mod,'bilinear')
mean_feb_obs = resample(mean_feb1,mean_ann,'bilinear')
mean_mar_obs = resample(mean_mar1,mean_ann,'bilinear')
mean_apl_obs = resample(mean_apl1,mean_ann,'bilinear')
mean_may_obs = resample(mean_may1,mean_ann,'bilinear')
mean_jun_obs = resample(mean_jun1,mean_ann,'bilinear')
mean_jul_obs = resample(mean_jul1,mean_ann,'bilinear')
mean_aug_obs = resample(mean_aug1,mean_ann,'bilinear')
mean_sep_obs = resample(mean_sep1,mean_ann,'bilinear')
mean_oct_obs = resample(mean_oct1,mean_ann,'bilinear')
mean_nov_obs = resample(mean_nov1,mean_ann,'bilinear')
mean_dec_obs = resample(mean_dec1,mean_ann,'bilinear')

mean_ann_obs = resample(mean_ann1,mean_ann,'bilinear')


mean_jan_obs = mask(mean_jan_obs,usa)
mean_feb_obs = mask(mean_feb1,usa)
mean_mar_obs = mask(mean_mar1,usa)
mean_apl_obs = mask(mean_apl1,usa)
mean_may_obs = mask(mean_may1,usa)
mean_jun_obs = mask(mean_jun1,usa)
mean_jul_obs = mask(mean_jul1,usa)
mean_aug_obs = mask(mean_aug1,usa)
mean_sep_obs = mask(mean_sep1,usa)
mean_oct_obs = mask(mean_oct1,usa)
mean_nov_obs = mask(mean_nov1,usa)
mean_dec_obs = mask(mean_dec1,usa)

mean_ann_obs = mask(mean_ann1,usa)

extent(ro_zone)=mean_ann_mod
# mean_s1_1 = resample(mean_s1_1,mean_ann,'bilinear')
# mean_s2_1 = resample(mean_s2_1,mean_ann,'bilinear')
# mean_s3_1 = resample(mean_s3_1,mean_ann,'bilinear')
# mean_ann1 = resample(mean_ann1,mean_ann,'bilinear')


# mean_s1_1 = resample(mean_s1_1,mean_ann,'bilinear')
# mean_s2_1 = resample(mean_s2_1,mean_ann,'bilinear')
# mean_s3_1 = resample(mean_s3_1,mean_ann,'bilinear')
mean_ann_obs = resample(mean_ann_obs,mean_ann_mod,'bilinear')



rozone1 = resample(ro_zone,mean_ann_mod,'bilinear')

# mean_ann = focal(mean_ann, w = matrix(1, nrow = 61, ncol = 61), fun = mean, pad = TRUE, na.rm= TRUE)
# s1_mod_ro  = zonal(mean_s1, rozone1, fun = "mean", na.rm=TRUE)
# s1_obs_ro = zonal(mean_s1_1, rozone1, fun = "mean", na.rm=TRUE)
# 
# s2_mod_ro  = zonal(mean_s2, rozone1, fun = "mean", na.rm=TRUE)
# s2_obs_ro = zonal(mean_s2_1, rozone1, fun = "mean", na.rm=TRUE)
# 
# s3_mod_ro  = zonal(mean_s3, rozone1, fun = "mean", na.rm=TRUE)
# s3_obs_ro = zonal(mean_s3_1, rozone1, fun = "mean", na.rm=TRUE)


# mod_ro  = zonal(mean_ann_mod, rozone1, fun = "mean", na.rm=TRUE)
# obs_ro = zonal(mean_ann_obs, rozone1, fun = "mean", na.rm=TRUE)


# roc = data.frame(mod_ro[,2], obs_ro[,2])
# names(roc)=c('mod','obs')
# 
# 
# roc1 = ggplot(roc, aes(x = obs, y = mod)) +
#   geom_point(size=3)+
#   stat_smooth(method = "lm",
#               col = "#C42126",
#               se = FALSE,
#               size = 1) +
#   labs(
#     x = "Observed Runoff (mm/year)",
#     y = "Modelled Runoff (mm/year)"
#     # color = "Gear",
#     # title = "Relation between Mile per hours and drat",
#     # subtitle = "Relationship break down by gear class",
#     # caption = "Authors own computation"
#   ) +
#   xlim(0,1000)+
#   ylim(0,1000)+
#   
#   theme(axis.title.x = element_text(size=14),
#       axis.title.y = element_text(size=14),
#       axis.text.x = element_text(size=14),
#       axis.text.y = element_text(size=14),
#       # panel.grid.major = element_blank(),
#       # panel.grid.minor = element_blank(),
#       legend.position = "",
#       legend.key = element_blank()
# )+
#   geom_abline(intercept = 0, slope = 1)
# 
# roc1

mean_ann_mod =mean_ann_m
mean_ann_obs =mean_ann1

writeRaster(mean_ann1,'C:/Users/WEM/OneDrive/AIT/papers/recharge paper/maps/obs_ro_new.tif',options=c('TFW=YES'))

bfi <- raster(x = "C:/Users/WEM/OneDrive/AIT/papers/recharge paper/maps/bfi.tif")
bfi1 <- projectRaster(bfi, res = 0.04166667, crs=newproj4)
# extent(bfi1)=mean_ann_m
bfi2 = resample(bfi1, mean_ann_m, "bilinear")
mean_ann1_m = resample(mean_ann1, mean_ann_m, "bilinear")

plot(bfi2)
mod_bf = mean_ann_m * (bfi2*0.01)
plot(mod_bf)


obs_bf = mean_ann1_m * (bfi2*0.01)
plot(rozone1,add=T)

extent(ro_zone)=mod_bf
rozone1 = resample(ro_zone,mod_bf,'bilinear')
mod_bf1  = zonal(mean_ann_m, rozone1, fun = "mean", na.rm=TRUE)
obs_bf1  = zonal(obs_bf, rozone1, fun = "mean", na.rm=TRUE)
mod_bf1
obs_bf1




plot(mean_jan_mod)
path1 = "C:/Users/WEM/OneDrive/AIT/papers/recharge paper/maps/New folder"
setwd(path1)
# par(mfrow = c(4, 2))

# Create a box plot
# bp <- ggplot(df, aes(x=dose, y=len, color=dose)) +
#   geom_boxplot() + 
#   theme(legend.position = "none")
# # Create a dot plot
# # Add the mean point and the standard deviation
# dp <- ggplot(df, aes(x=dose, y=len, fill=dose)) +
#   geom_dotplot(binaxis='y', stackdir='center')+
#   stat_summary(fun.data=mean_sdl, mult=1, 
#                geom="pointrange", color="red")+
#   theme(legend.position = "none")
# # Create a violin plot
# vp <- ggplot(df, aes(x=dose, y=len)) +
#   geom_violin()+
#   geom_boxplot(width=0.1)
# # Create a stripchart
# sc <- ggplot(df, aes(x=dose, y=len, color=dose, shape=dose)) +
#   geom_jitter(position=position_jitter(0.2))+
#   theme(legend.position = "none") +
#   theme_gray()

#convert the raster to points for plotting

#jan
janm <- rasterToPoints(mean_jan_mod)
#Make the points a dataframe for ggplot
df <- data.frame(janm)

#Make appropriate column headings
colnames(df) <- c("Longitude", "Latitude", "MAP")
b.dem <- seq(min(df$MAP),max(df$MAP),length.out=100)
a1=ggplot(data=df, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=MAP)) +
  ggtitle("Modelled") +
  xlab("") +
  ylab("January") +
  labs(fill= 'Runoff (mm)')+
  scale_y_continuous(breaks = seq(25, 50, len = 3)) +

  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  scale_fill_gradientn(colours = rainbow(20), limits=c(-50,250))+
  # coord_equal() +
  # scale_fill_gradient("MAP (mm/yr)", limits=c(0,2500)) +
  # scale_fill_gradientn(name="Altitude",colours = rainbow(20),"MAP (mm/yr)", limits=c(-300,500)) +
  # geom_tile(aes(Longitude,Latitude,alpha=MAP), fill = "grey20") 
  
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "",
        legend.key = element_blank()
        ) + 
  coord_fixed()

  
  a1
  
  # ggsave(filename="a1.tiff", plot=a1, path=path1, dpi=300)

#convert the raster to points for plotting
jano <- rasterToPoints(mean_jan_obs)
#Make the points a dataframe for ggplot
df2 <- data.frame(jano)
#Make appropriate column headings
colnames(df2) <- c("Longitude", "Latitude", "MAP")
a2=ggplot(data=df2, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=MAP)) +
  ggtitle("Observed") +
  xlab("") +
  ylab("")+
  labs(fill= 'Runoff (mm)')+
  scale_y_continuous(breaks = seq(25, 50, len = 3)) +
  

  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  scale_fill_gradientn(colours = rainbow(20), limits=c(-50,250))+
  # coord_equal() +
  # scale_fill_gradientn(name="Altitude",colours = rainbow(20),
  #                      "MAP (mm/yr)", 
  #                      limits=c(-300,500)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "",
        legend.key = element_blank()
  )+ coord_fixed()


 a2

mean_jan_obs = resample(mean_jan_obs, mean_jan_mod, "bilinear")
jan_diff = mean_jan_mod - mean_jan_obs

#convert the raster to points for plotting
jand <- rasterToPoints(jan_diff)
#Make the points a dataframe for ggplot
df3 <- data.frame(jand)
#Make appropriate column headings
colnames(df3) <- c("Longitude", "Latitude", "MAP")
a3=ggplot(data=df3, aes(x=MAP)) +
  geom_histogram(color="black", fill="white") +
  # ggtitle("Plot of length \n by dose") +
  xlab("Error (mm)") + 
  ylab("Count") +
  
  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  xlim(-200, 200)+
  # coord_equal() +
  # scale_fill_gradientn(colours=c("blue","green","red"),
  #                      "MAP (mm/yr)", 
  #                      limits=c(-300,500)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=12),
          # "element_text(size=16, angle=90)nxt,"
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "",
        legend.key = element_blank()
  )+
  geom_vline(aes(xintercept = mean(MAP)),col='red',size=0.5)+
  
  scale_y_continuous(position = "right")
a3


# library(patchwork)
# combined = a1 + a2 + a3 & theme(legend.position = "right")
# combined1 = combined + plot_layout(ncol = 3, guides = "collect")
# combined1
# tiff('C:/Users/ezhil/OneDrive/AIT/papers/recharge paper/maps/New folder/t_1.tif', units="in", width=5, height=5, res=300)
# writeRaster(combined, 't_1.tif', res = 300)
# ggsave(combined, "t_1.tif", width = 44.45, height = 27.78, units = "cm", dpi=300)
# ggsave(filename="c1.tiff", plot=combined1, path=path1, dpi=300)


#feb

febm <- rasterToPoints(mean_feb_mod)
#Make the points a dataframe for ggplot
df <- data.frame(febm)

#Make appropriate column headings
colnames(df) <- c("Longitude", "Latitude", "MAP")
b.dem <- seq(min(df$MAP),max(df$MAP),length.out=100)
a4=ggplot(data=df, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=MAP)) +
  # ggtitle("Plot of length \n by dose") +
  xlab("") +
  ylab("February") +
  labs(fill= 'Runoff (mm)')+
  scale_y_continuous(breaks = seq(25, 50, len = 3)) +
  
  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  scale_fill_gradientn(colours = rainbow(20), limits=c(-50,250))+
  # coord_equal() +
  # scale_fill_gradientn(name="Altitude",colours = rainbow(20),
  #                      "MAP (mm/yr)", 
  #                      limits=c(-300,500)) +
  # geom_tile(aes(Longitude,Latitude,alpha=MAP), fill = "grey20") 
  
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "",
        legend.key = element_blank()
  )+ coord_fixed()

# + coord_equal() 

# a1

#convert the raster to points for plotting
febo <- rasterToPoints(mean_feb_obs)
#Make the points a dataframe for ggplot
df2 <- data.frame(febo)
#Make appropriate column headings
colnames(df2) <- c("Longitude", "Latitude", "MAP")
a5=ggplot(data=df2, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=MAP)) +
  # ggtitle("Plot of length \n by dose") +
  xlab("") +
  ylab("")+
  labs(fill= 'Runoff (mm)')+
  scale_y_continuous(breaks = seq(25, 50, len = 3)) +
  
  
  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  scale_fill_gradientn(colours = rainbow(20), limits=c(-50,250))+
  # coord_equal() +
  # scale_fill_gradientn(name="Altitude",colours = rainbow(20),
  #                      "MAP (mm/yr)", 
  #                      limits=c(-300,500)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "",
        legend.key = element_blank()
  )+ coord_fixed()


# a2

mean_feb_obs = resample(mean_feb_obs, mean_feb_mod, "bilinear")
feb_diff = mean_feb_mod - mean_feb_obs

#convert the raster to points for plotting
febd <- rasterToPoints(feb_diff)
#Make the points a dataframe for ggplot
df3 <- data.frame(febd)
#Make appropriate column headings
colnames(df3) <- c("Longitude", "Latitude", "MAP")
a6=ggplot(data=df3, aes(x=MAP)) +
  geom_histogram(color="black", fill="white") +
  # ggtitle("Plot of length \n by dose") +
  xlab("Error (mm)") + 
  ylab("Count") +
  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  xlim(-200, 200)+
  # coord_equal() +
  # scale_fill_gradientn(colours=c("blue","green","red"),
  #                      "MAP (mm/yr)", 
  #                      limits=c(-300,500)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=12),
        # "element_text(size=16, angle=90)nxt,"
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "",
        legend.key = element_blank()
  )+
  geom_vline(aes(xintercept = mean(MAP)),col='red',size=0.5)+
  
  scale_y_continuous(position = "right")
  

# library(patchwork)
# combined = a1 + a2 + a3 +a4 +a5 +a6 & theme(legend.position = "right")
# combined1 = combined + plot_layout(ncol = 3, guides = "collect")
# combined1
# dev_off()
#mar

marm <- rasterToPoints(mean_mar_mod)
#Make the points a dataframe for ggplot
df <- data.frame(marm)

#Make appropriate column headings
colnames(df) <- c("Longitude", "Latitude", "MAP")
b.dem <- seq(min(df$MAP),max(df$MAP),length.out=100)
a7=ggplot(data=df, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=MAP)) +
  # ggtitle("Plot of length \n by dose") +
  xlab("") +
  ylab("March") +
  labs(fill= 'Runoff (mm)')+
  
  scale_y_continuous(breaks = seq(25, 50, len = 3)) +
  
  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  scale_fill_gradientn(colours = rainbow(20), limits=c(-50,250))+
  # coord_equal() +
  # scale_fill_gradientn(name="Altitude",colours = rainbow(20),
  #                      "MAP (mm/yr)", 
  #                      limits=c(-300,500)) +
  # geom_tile(aes(Longitude,Latitude,alpha=MAP), fill = "grey20") 
  
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "",
        legend.key = element_blank()
  )+ coord_fixed()

# + coord_equal() 

# a1

#convert the raster to points for plotting
maro <- rasterToPoints(mean_mar_obs)
#Make the points a dataframe for ggplot
df2 <- data.frame(maro)
#Make appropriate column headings
colnames(df2) <- c("Longitude", "Latitude", "MAP")
a8=ggplot(data=df2, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=MAP)) +
  # ggtitle("Plot of length \n by dose") +
  xlab("") +
  ylab("")+
  labs(fill= 'Runoff (mm)')+
  
  scale_y_continuous(breaks = seq(25, 50, len = 3)) +
  
  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  scale_fill_gradientn(colours = rainbow(20), limits=c(-50,250))+
  # coord_equal() +
  # scale_fill_gradientn(name="Altitude",colours = rainbow(20),
  #                      "MAP (mm/yr)", 
  #                      limits=c(-300,500)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "",
        legend.key = element_blank()
  )+ coord_fixed()


# a2

mean_mar_obs = resample(mean_mar_obs, mean_mar_mod, "bilinear")
mar_diff = mean_mar_mod - mean_mar_obs

#convert the raster to points for plotting
mard <- rasterToPoints(mar_diff)
#Make the points a dataframe for ggplot
df3 <- data.frame(mard)
#Make appropriate column headings
colnames(df3) <- c("Longitude", "Latitude", "MAP")
a9=ggplot(data=df3, aes(x=MAP)) +
  geom_histogram(color="black", fill="white") +
  # ggtitle("Plot of length \n by dose") +
  xlab("Error(mm)") + 
  ylab("Count") +
  # scale_y_continuous(breaks = seq(25, 50, len = 3)) +
  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  xlim(-200, 200)+
  # coord_equal() +
  # scale_fill_gradientn(colours=c("blue","green","red"),
  #                      "MAP (mm/yr)", 
  #                      limits=c(-300,500)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=12),
        # "element_text(size=16, angle=90)nxt,"
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "",
        legend.key = element_blank()
  )+
  geom_vline(aes(xintercept = mean(MAP)),col='red',size=0.5)+
  
  scale_y_continuous(position = "right")


# library(patchwork)
# combined = a1 + a2 + a3 +a4 +a5 +a6 +a7+a8+a9& theme(legend.position = "right")
# combined1 = combined + plot_layout(ncol = 3, guides = "collect")
# combined1



#apl
aplm <- rasterToPoints(mean_apl_mod)
#Make the points a dataframe for ggplot
df <- data.frame(aplm)

#Make appropriate column headings
colnames(df) <- c("Longitude", "Latitude", "MAP")
b.dem <- seq(min(df$MAP),max(df$MAP),length.out=100)
a10=ggplot(data=df, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=MAP)) +
  # ggtitle("Plot of length \n by dose") +
  xlab("") +
  ylab("April") +
  labs(fill= 'Runoff (mm)')+
  
  scale_y_continuous(breaks = seq(25, 50, len = 3)) +
  
  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  scale_fill_gradientn(colours = rainbow(20), limits=c(-50,250))+
  # coord_equal() +
  # scale_fill_gradientn(name="Altitude",colours = rainbow(20),
  #                      "MAP (mm/yr)", 
  #                      limits=c(-300,500)) +
  # geom_tile(aes(Longitude,Latitude,alpha=MAP), fill = "grey20") 
  
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "",
        legend.key = element_blank()
  )+ coord_fixed()

# + coord_equal() 

# a1

#convert the raster to points for plotting
aplo <- rasterToPoints(mean_apl_obs)
#Make the points a dataframe for ggplot
df2 <- data.frame(aplo)
#Make appropriate column headings
colnames(df2) <- c("Longitude", "Latitude", "MAP")
a11=ggplot(data=df2, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=MAP)) +
  # ggtitle("Plot of length \n by dose") +
  xlab("") +
  ylab("")+
  labs(fill= 'Runoff (mm)')+
  
  scale_y_continuous(breaks = seq(25, 50, len = 3)) +
  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  scale_fill_gradientn(colours = rainbow(20), limits=c(-50,250))+
  # coord_equal() +
  # scale_fill_gradientn(name="Altitude",colours = rainbow(20),
  #                      "MAP (mm/yr)", 
  #                      limits=c(-300,500)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "",
        legend.key = element_blank()
  )+ coord_fixed()


# a2

mean_apl_obs = resample(mean_apl_obs, mean_apl_mod, "bilinear")
apl_diff = mean_apl_mod - mean_apl_obs

#convert the raster to points for plotting
apld <- rasterToPoints(apl_diff)
#Make the points a dataframe for ggplot
df3 <- data.frame(apld)
#Make appropriate column headings
colnames(df3) <- c("Longitude", "Latitude", "MAP")
a12=ggplot(data=df3, aes(x=MAP)) +
  geom_histogram(color="black", fill="white") +
  # ggtitle("Plot of length \n by dose") +
  xlab("Error (mm)") + 
  ylab("Count") +
  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  xlim(-200, 200)+
  # coord_equal() +
  # scale_fill_gradientn(colours=c("blue","green","red"),
  #                      "MAP (mm/yr)", 
  #                      limits=c(-300,500)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=12),
        # "element_text(size=16, angle=90)nxt,"
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "",
        legend.key = element_blank()
  )+
  geom_vline(aes(xintercept = mean(MAP)),col='red',size=0.5)+
  
  scale_y_continuous(position = "right")

# may
maym <- rasterToPoints(mean_may_mod)
#Make the points a dataframe for ggplot
df <- data.frame(maym)

#Make appropriate column headings
colnames(df) <- c("Longitude", "Latitude", "MAP")
b.dem <- seq(min(df$MAP),max(df$MAP),length.out=100)
a13=ggplot(data=df, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=MAP)) +
  # ggtitle("Plot of length \n by dose") +
  xlab("") +
  ylab("May") +
  labs(fill= 'Runoff (mm)')+
  
  scale_y_continuous(breaks = seq(25, 50, len = 3)) +
  
  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  scale_fill_gradientn(colours = rainbow(20), limits=c(-50,250))+
  # coord_equal() +
  # scale_fill_gradientn(name="Altitude",colours = rainbow(20),
  #                      "MAP (mm/yr)", 
  #                      limits=c(-300,500)) +
  # geom_tile(aes(Longitude,Latitude,alpha=MAP), fill = "grey20") 
  
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "",
        legend.key = element_blank()
  )+ coord_fixed()

# + coord_equal() 

# a1

#convert the raster to points for plotting
mayo <- rasterToPoints(mean_may_obs)
#Make the points a dataframe for ggplot
df2 <- data.frame(mayo)
#Make appropriate column headings
colnames(df2) <- c("Longitude", "Latitude", "MAP")
a14=ggplot(data=df2, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=MAP)) +
  # ggtitle("Plot of length \n by dose") +
  xlab("") +
  ylab("")+
  labs(fill= 'Runoff (mm)')+
  
  scale_y_continuous(breaks = seq(25, 50, len = 3)) +
  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  scale_fill_gradientn(colours = rainbow(20), limits=c(-50,250))+
  # coord_equal() +
  # scale_fill_gradientn(name="Altitude",colours = rainbow(20),
  #                      "MAP (mm/yr)", 
  #                      limits=c(-300,500)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "",
        legend.key = element_blank()
  )+ coord_fixed()


# a2

mean_may_obs = resample(mean_may_obs, mean_may_mod, "bilinear")
may_diff = mean_may_mod - mean_may_obs

#convert the raster to points for plotting
mayd <- rasterToPoints(may_diff)
#Make the points a dataframe for ggplot
df3 <- data.frame(mayd)
#Make appropriate column headings
colnames(df3) <- c("Longitude", "Latitude", "MAP")
a15=ggplot(data=df3, aes(x=MAP)) +
  geom_histogram(color="black", fill="white") +
  # ggtitle("Plot of length \n by dose") +
  xlab("Error (mm)") + 
  ylab("Count") +
  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  xlim(-200, 200)+
  # coord_equal() +
  # scale_fill_gradientn(colours=c("blue","green","red"),
  #                      "MAP (mm/yr)", 
  #                      limits=c(-300,500)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=12),
        # "element_text(size=16, angle=90)nxt,"
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "",
        legend.key = element_blank()
  )+
  geom_vline(aes(xintercept = mean(MAP)),col='red',size=0.5)+
  
  scale_y_continuous(position = "right")

#jun

junm <- rasterToPoints(mean_jun_mod)
#Make the points a dataframe for ggplot
df <- data.frame(junm)

#Make appropriate column headings
colnames(df) <- c("Longitude", "Latitude", "MAP")
b.dem <- seq(min(df$MAP),max(df$MAP),length.out=100)
a16=ggplot(data=df, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=MAP)) +
  # ggtitle("Plot of length \n by dose") +
  xlab("") +
  ylab("June") +
  labs(fill= 'Runoff (mm)')+
  
  scale_y_continuous(breaks = seq(25, 50, len = 3)) +
  
  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  scale_fill_gradientn(colours = rainbow(20), limits=c(-50,250))+
  # coord_equal() +
  # scale_fill_gradientn(name="Altitude",colours = rainbow(20),
  #                      "MAP (mm/yr)", 
  #                      limits=c(-300,500)) +
  # geom_tile(aes(Longitude,Latitude,alpha=MAP), fill = "grey20") 
  
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "B",
        legend.key = element_blank()
  )+ coord_fixed()

# + coord_equal() 

# a1

#convert the raster to points for plotting
juno <- rasterToPoints(mean_jun_obs)
#Make the points a dataframe for ggplot
df2 <- data.frame(juno)
#Make appropriate column headings
colnames(df2) <- c("Longitude", "Latitude", "MAP")
a17=ggplot(data=df2, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=MAP)) +
  # ggtitle("Plot of length \n by dose") +
  xlab("") +
  ylab("")+
  labs(fill= 'Runoff (mm)')+
  
  scale_y_continuous(breaks = seq(25, 50, len = 3)) +
  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  scale_fill_gradientn(colours = rainbow(20), limits=c(-50,250))+
  # coord_equal() +
  # scale_fill_gradientn(name="Altitude",colours = rainbow(20),
  #                      "MAP (mm/yr)", 
  #                      limits=c(-300,500)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "",
        legend.key = element_blank()
  )+ coord_fixed()


# a2

mean_jun_obs = resample(mean_jun_obs, mean_jun_mod, "bilinear")
jun_diff = mean_jun_mod - mean_jun_obs

#convert the raster to points for plotting
jund <- rasterToPoints(may_diff)
#Make the points a dataframe for ggplot
df3 <- data.frame(jund)
#Make appropriate column headings
colnames(df3) <- c("Longitude", "Latitude", "MAP")
a18=ggplot(data=df3, aes(x=MAP)) +
  geom_histogram(color="black", fill="white") +
  # ggtitle("Plot of length \n by dose") +
  xlab("Error (mm)") + 
  ylab("Count") +
  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  xlim(-200, 200)+
  # coord_equal() +
  # scale_fill_gradientn(colours=c("blue","green","red"),
  #                      "MAP (mm/yr)", 
  #                      limits=c(-300,500)) +
  theme(axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        # "element_text(size=16, angle=90)nxt,"
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "",
        legend.key = element_blank()
  )+
  geom_vline(aes(xintercept = mean(MAP)),col='red',size=0.5)+
  
  scale_y_continuous(position = "right")
  # g2_legend <- a17 + guides(fill = guide_legend(override.aes = list(size = 0.1)))
  # legend <- get_legend(a17)
#jul
julm <- rasterToPoints(mean_jul_mod)
#Make the points a dataframe for ggplot
df <- data.frame(julm)

#Make appropriate column headings
colnames(df) <- c("Longitude", "Latitude", "MAP")
b.dem <- seq(min(df$MAP),max(df$MAP),length.out=100)
a19=ggplot(data=df, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=MAP)) +
  ggtitle("Modelled") +
  xlab("") +
  ylab("July") +
  labs(fill= 'Runoff (mm)')+
  scale_y_continuous(breaks = seq(25, 50, len = 3)) +
  
  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  scale_fill_gradientn(colours = rainbow(100), limits=c(-50,250))+
  # coord_equal() +
  # scale_fill_gradient("MAP (mm/yr)", limits=c(0,2500)) +
  # scale_fill_gradientn(name="Altitude",colours = rainbow(20),"MAP (mm/yr)", limits=c(-300,500)) +
  # geom_tile(aes(Longitude,Latitude,alpha=MAP), fill = "grey20") 
  
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=10),
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "",
        legend.key = element_blank()
  )
# + coord_equal() 

a1

# ggsave(filename="a1.tiff", plot=a1, path=path1, dpi=300)

#convert the raster to points for plotting
julo <- rasterToPoints(mean_jul_obs)
#Make the points a dataframe for ggplot
df2 <- data.frame(julo)
#Make appropriate column headings
colnames(df2) <- c("Longitude", "Latitude", "MAP")
a20=ggplot(data=df2, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=MAP)) +
  ggtitle("Observed") +
  xlab("") +
  ylab("")+
  labs(fill= 'Runoff (mm)')+
  scale_y_continuous(breaks = seq(25, 50, len = 3)) +
  
  
  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  scale_fill_gradientn(colours = rainbow(100), limits=c(-50,250))+
  # coord_equal() +
  # scale_fill_gradientn(name="Altitude",colours = rainbow(20),
  #                      "MAP (mm/yr)", 
  #                      limits=c(-300,500)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "",
        legend.key = element_blank()
  )

a2

mean_jul_obs = resample(mean_jul_obs, mean_jul_mod, "bilinear")
jul_diff = mean_jul_mod - mean_jul_obs


#convert the raster to points for plotting
juld <- rasterToPoints(jul_diff)
#Make the points a dataframe for ggplot
df3 <- data.frame(juld)
#Make appropriate column headings
colnames(df3) <- c("Longitude", "Latitude", "MAP")
a21=ggplot(data=df3, aes(x=MAP)) +
  geom_histogram(color="black", fill="white") +
  # ggtitle("Plot of length \n by dose") +
  xlab("Error (mm)") + 
  ylab("Count") +
  
  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  xlim(-200, 200)+
  # coord_equal() +
  # scale_fill_gradientn(colours=c("blue","green","red"),
  #                      "MAP (mm/yr)", 
  #                      limits=c(-300,500)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=10),
        # "element_text(size=16, angle=90)nxt,"
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "",
        legend.key = element_blank()
  )+
  geom_vline(aes(xintercept = mean(MAP)),col='red',size=0.5)+
  
  scale_y_continuous(position = "right")
a3


# library(patchwork)
# combined = a1 + a2 + a3 & theme(legend.position = "right")
# combined1 = combined + plot_layout(ncol = 3, guides = "collect")
# combined1
# tiff('C:/Users/ezhil/OneDrive/AIT/papers/recharge paper/maps/New folder/t_1.tif', units="in", width=5, height=5, res=300)
# writeRaster(combined, 't_1.tif', res = 300)
# ggsave(combined, "t_1.tif", width = 44.45, height = 27.78, units = "cm", dpi=300)
# ggsave(filename="c1.tiff", plot=combined1, path=path1, dpi=300)


#aug

augm <- rasterToPoints(mean_aug_mod)
#Make the points a dataframe for ggplot
df <- data.frame(augm)

#Make appropriate column headings
colnames(df) <- c("Longitude", "Latitude", "MAP")
b.dem <- seq(min(df$MAP),max(df$MAP),length.out=100)
a22=ggplot(data=df, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=MAP)) +
  # ggtitle("Plot of length \n by dose") +
  xlab("") +
  ylab("August") +
  labs(fill= 'Runoff (mm)')+
  scale_y_continuous(breaks = seq(25, 50, len = 3)) +
  
  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  scale_fill_gradientn(colours = rainbow(100), limits=c(-50,250))+
  # coord_equal() +
  # scale_fill_gradientn(name="Altitude",colours = rainbow(20),
  #                      "MAP (mm/yr)", 
  #                      limits=c(-300,500)) +
  # geom_tile(aes(Longitude,Latitude,alpha=MAP), fill = "grey20") 
  
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=10),
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "",
        legend.key = element_blank()
  )
# + coord_equal() 

# a1

#convert the raster to points for plotting
augo <- rasterToPoints(mean_aug_obs)
#Make the points a dataframe for ggplot
df2 <- data.frame(augo)
#Make appropriate column headings
colnames(df2) <- c("Longitude", "Latitude", "MAP")
a23=ggplot(data=df2, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=MAP)) +
  # ggtitle("Plot of length \n by dose") +
  xlab("") +
  ylab("")+
  labs(fill= 'Runoff (mm)')+
  scale_y_continuous(breaks = seq(25, 50, len = 3)) +
  
  
  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  scale_fill_gradientn(colours = rainbow(100), limits=c(-50,250))+
  # coord_equal() +
  # scale_fill_gradientn(name="Altitude",colours = rainbow(20),
  #                      "MAP (mm/yr)", 
  #                      limits=c(-300,500)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "",
        legend.key = element_blank()
  )

# a2

mean_aug_obs = resample(mean_aug_obs, mean_aug_mod, "bilinear")
aug_diff = mean_aug_mod - mean_aug_obs

#convert the raster to points for plotting
augd <- rasterToPoints(aug_diff)
#Make the points a dataframe for ggplot
df3 <- data.frame(augd)
#Make appropriate column headings
colnames(df3) <- c("Longitude", "Latitude", "MAP")
a24=ggplot(data=df3, aes(x=MAP)) +
  geom_histogram(color="black", fill="white") +
  # ggtitle("Plot of length \n by dose") +
  xlab("Error (mm)") + 
  ylab("Count") +
  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  xlim(-200, 200)+
  # coord_equal() +
  # scale_fill_gradientn(colours=c("blue","green","red"),
  #                      "MAP (mm/yr)", 
  #                      limits=c(-300,500)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=10),
        # "element_text(size=16, angle=90)nxt,"
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "",
        legend.key = element_blank()
  )+
  geom_vline(aes(xintercept = mean(MAP)),col='red',size=0.5)+
  
  scale_y_continuous(position = "right")


# library(patchwork)
# combined = a1 + a2 + a3 +a4 +a5 +a6 & theme(legend.position = "right")
# combined1 = combined + plot_layout(ncol = 3, guides = "collect")
# combined1
# dev_off()
#mar
#sep
sepm <- rasterToPoints(mean_sep_mod)
#Make the points a dataframe for ggplot
df <- data.frame(sepm)

#Make appropriate column headings
colnames(df) <- c("Longitude", "Latitude", "MAP")
b.dem <- seq(min(df$MAP),max(df$MAP),length.out=100)
a25=ggplot(data=df, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=MAP)) +
  # ggtitle("Plot of length \n by dose") +
  xlab("") +
  ylab("September") +
  labs(fill= 'Runoff (mm)')+
  
  scale_y_continuous(breaks = seq(25, 50, len = 3)) +
  
  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  scale_fill_gradientn(colours = rainbow(100), limits=c(-50,250))+
  # coord_equal() +
  # scale_fill_gradientn(name="Altitude",colours = rainbow(20),
  #                      "MAP (mm/yr)", 
  #                      limits=c(-300,500)) +
  # geom_tile(aes(Longitude,Latitude,alpha=MAP), fill = "grey20") 
  
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=10),
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "",
        legend.key = element_blank()
  )
# + coord_equal() 

# a1

#convert the raster to points for plotting
sepo <- rasterToPoints(mean_sep_obs)
#Make the points a dataframe for ggplot
df2 <- data.frame(sepo)
#Make appropriate column headings
colnames(df2) <- c("Longitude", "Latitude", "MAP")
a26=ggplot(data=df2, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=MAP)) +
  # ggtitle("Plot of length \n by dose") +
  xlab("") +
  ylab("")+
  labs(fill= 'Runoff (mm)')+
  
  scale_y_continuous(breaks = seq(25, 50, len = 3)) +
  
  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  scale_fill_gradientn(colours = rainbow(100), limits=c(-50,250))+
  # coord_equal() +
  # scale_fill_gradientn(name="Altitude",colours = rainbow(20),
  #                      "MAP (mm/yr)", 
  #                      limits=c(-300,500)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "",
        legend.key = element_blank()
  )

# a2

mean_sep_obs = resample(mean_sep_obs, mean_sep_mod, "bilinear")
sep_diff = mean_sep_mod - mean_sep_obs

#convert the raster to points for plotting
sepd <- rasterToPoints(sep_diff)
#Make the points a dataframe for ggplot
df3 <- data.frame(sepd)
#Make appropriate column headings
colnames(df3) <- c("Longitude", "Latitude", "MAP")
a27=ggplot(data=df3, aes(x=MAP)) +
  geom_histogram(color="black", fill="white") +
  # ggtitle("Plot of length \n by dose") +
  xlab("Error(mm)") + 
  ylab("Count") +
  # scale_y_continuous(breaks = seq(25, 50, len = 3)) +
  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  xlim(-200, 200)+
  # coord_equal() +
  # scale_fill_gradientn(colours=c("blue","green","red"),
  #                      "MAP (mm/yr)", 
  #                      limits=c(-300,500)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=10),
        # "element_text(size=16, angle=90)nxt,"
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "",
        legend.key = element_blank()
  )+
  geom_vline(aes(xintercept = mean(MAP)),col='red',size=0.5)+
  
  scale_y_continuous(position = "right")


# library(patchwork)
# combined = a1 + a2 + a3 +a4 +a5 +a6 +a7+a8+a9& theme(legend.position = "right")
# combined1 = combined + plot_layout(ncol = 3, guides = "collect")
# combined1



#oct
octm <- rasterToPoints(mean_oct_mod)
#Make the points a dataframe for ggplot
df <- data.frame(octm)

#Make appropriate column headings
colnames(df) <- c("Longitude", "Latitude", "MAP")
b.dem <- seq(min(df$MAP),max(df$MAP),length.out=100)
a28=ggplot(data=df, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=MAP)) +
  # ggtitle("Plot of length \n by dose") +
  xlab("") +
  ylab("October") +
  labs(fill= 'Runoff (mm)')+
  
  scale_y_continuous(breaks = seq(25, 50, len = 3)) +
  
  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  scale_fill_gradientn(colours = rainbow(100), limits=c(-50,250))+
  # coord_equal() +
  # scale_fill_gradientn(name="Altitude",colours = rainbow(20),
  #                      "MAP (mm/yr)", 
  #                      limits=c(-300,500)) +
  # geom_tile(aes(Longitude,Latitude,alpha=MAP), fill = "grey20") 
  
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=10),
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "",
        legend.key = element_blank()
  )
# + coord_equal() 

# a1

#convert the raster to points for plotting
octo <- rasterToPoints(mean_oct_obs)
#Make the points a dataframe for ggplot
df2 <- data.frame(octo)
#Make appropriate column headings
colnames(df2) <- c("Longitude", "Latitude", "MAP")
a29=ggplot(data=df2, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=MAP)) +
  # ggtitle("Plot of length \n by dose") +
  xlab("") +
  ylab("")+
  labs(fill= 'Runoff (mm)')+
  
  scale_y_continuous(breaks = seq(25, 50, len = 3)) +
  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  scale_fill_gradientn(colours = rainbow(100), limits=c(-50,250))+
  # coord_equal() +
  # scale_fill_gradientn(name="Altitude",colours = rainbow(20),
  #                      "MAP (mm/yr)", 
  #                      limits=c(-300,500)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "",
        legend.key = element_blank()
  )

# a2

mean_oct_obs = resample(mean_oct_obs, mean_oct_mod, "bilinear")
oct_diff = mean_oct_mod - mean_oct_obs

#convert the raster to points for plotting
octd <- rasterToPoints(oct_diff)
#Make the points a dataframe for ggplot
df3 <- data.frame(octd)
#Make appropriate column headings
colnames(df3) <- c("Longitude", "Latitude", "MAP")
a30=ggplot(data=df3, aes(x=MAP)) +
  geom_histogram(color="black", fill="white") +
  # ggtitle("Plot of length \n by dose") +
  xlab("Error (mm)") + 
  ylab("Count") +
  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  xlim(-200, 200)+
  # coord_equal() +
  # scale_fill_gradientn(colours=c("blue","green","red"),
  #                      "MAP (mm/yr)", 
  #                      limits=c(-300,500)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=10),
        # "element_text(size=16, angle=90)nxt,"
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "",
        legend.key = element_blank()
  )+
  geom_vline(aes(xintercept = mean(MAP)),col='red',size=0.5)+
  
  scale_y_continuous(position = "right")

# nov
novm <- rasterToPoints(mean_nov_mod)
#Make the points a dataframe for ggplot
df <- data.frame(novm)

#Make appropriate column headings
colnames(df) <- c("Longitude", "Latitude", "MAP")
b.dem <- seq(min(df$MAP),max(df$MAP),length.out=100)
a31=ggplot(data=df, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=MAP)) +
  # ggtitle("Plot of length \n by dose") +
  xlab("") +
  ylab("November") +
  labs(fill= 'Runoff (mm)')+
  
  scale_y_continuous(breaks = seq(25, 50, len = 3)) +
  
  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  scale_fill_gradientn(colours = rainbow(100), limits=c(-50,250))+
  # coord_equal() +
  # scale_fill_gradientn(name="Altitude",colours = rainbow(20),
  #                      "MAP (mm/yr)", 
  #                      limits=c(-300,500)) +
  # geom_tile(aes(Longitude,Latitude,alpha=MAP), fill = "grey20") 
  
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=10),
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "",
        legend.key = element_blank()
  )
# + coord_equal() 

# a1

#convert the raster to points for plotting
novo <- rasterToPoints(mean_nov_obs)
#Make the points a dataframe for ggplot
df2 <- data.frame(novo)
#Make appropriate column headings
colnames(df2) <- c("Longitude", "Latitude", "MAP")
a32=ggplot(data=df2, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=MAP)) +
  # ggtitle("Plot of length \n by dose") +
  xlab("") +
  ylab("")+
  labs(fill= 'Runoff (mm)')+
  
  scale_y_continuous(breaks = seq(25, 50, len = 3)) +
  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  scale_fill_gradientn(colours = rainbow(100), limits=c(-50,250))+
  # coord_equal() +
  # scale_fill_gradientn(name="Altitude",colours = rainbow(20),
  #                      "MAP (mm/yr)", 
  #                      limits=c(-300,500)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "",
        legend.key = element_blank()
  )

# a2

mean_nov_obs = resample(mean_nov_obs, mean_nov_mod, "bilinear")
nov_diff = mean_nov_mod - mean_nov_obs

#convert the raster to points for plotting
novd <- rasterToPoints(nov_diff)
#Make the points a dataframe for ggplot
df3 <- data.frame(novd)
#Make appropriate column headings
colnames(df3) <- c("Longitude", "Latitude", "MAP")
a33=ggplot(data=df3, aes(x=MAP)) +
  geom_histogram(color="black", fill="white") +
  # ggtitle("Plot of length \n by dose") +
  xlab("Error (mm)") + 
  ylab("Count") +
  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  xlim(-200, 200)+
  # coord_equal() +
  # scale_fill_gradientn(colours=c("blue","green","red"),
  #                      "MAP (mm/yr)", 
  #                      limits=c(-300,500)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=10),
        # "element_text(size=16, angle=90)nxt,"
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "",
        legend.key = element_blank()
  )+
  geom_vline(aes(xintercept = mean(MAP)),col='red',size=0.5)+
  
  scale_y_continuous(position = "right")

#dec

decm <- rasterToPoints(mean_dec_mod)
#Make the points a dataframe for ggplot
df <- data.frame(decm)

#Make appropriate column headings
colnames(df) <- c("Longitude", "Latitude", "MAP")
b.dem <- seq(min(df$MAP),max(df$MAP),length.out=100)
a34=ggplot(data=df, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=MAP)) +
  # ggtitle("Plot of length \n by dose") +
  xlab("") +
  ylab("December") +
  labs(fill= 'Runoff (mm)')+
  
  scale_y_continuous(breaks = seq(25, 50, len = 3)) +
  
  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  scale_fill_gradientn(colours = rainbow(100), limits=c(-50,250))+
  # coord_equal() +
  # scale_fill_gradientn(name="Altitude",colours = rainbow(20),
  #                      "MAP (mm/yr)", 
  #                      limits=c(-300,500)) +
  # geom_tile(aes(Longitude,Latitude,alpha=MAP), fill = "grey20") 
  
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=10),
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "B",
        legend.key = element_blank()
  )
# + coord_equal() 

# a1

#convert the raster to points for plotting
deco <- rasterToPoints(mean_dec_obs)
#Make the points a dataframe for ggplot
df2 <- data.frame(deco)
#Make appropriate column headings
colnames(df2) <- c("Longitude", "Latitude", "MAP")
a35=ggplot(data=df2, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=MAP)) +
  # ggtitle("Plot of length \n by dose") +
  xlab("") +
  ylab("")+
  labs(fill= 'Runoff (mm)')+
  
  scale_y_continuous(breaks = seq(25, 50, len = 3)) +
  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  scale_fill_gradientn(colours = rainbow(100), limits=c(-50,250))+
  # coord_equal() +
  # scale_fill_gradientn(name="Altitude",colours = rainbow(20),
  #                      "MAP (mm/yr)", 
  #                      limits=c(-300,500)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "",
        legend.key = element_blank()
  )

# a2

mean_dec_obs = resample(mean_dec_obs, mean_dec_mod, "bilinear")
dec_diff = mean_dec_mod - mean_dec_obs

#convert the raster to points for plotting
decd <- rasterToPoints(dec_diff)
#Make the points a dataframe for ggplot
df3 <- data.frame(decd)
#Make appropriate column headings
colnames(df3) <- c("Longitude", "Latitude", "MAP")
a36=ggplot(data=df3, aes(x=MAP)) +
  geom_histogram(color="black", fill="white") +
  # ggtitle("Plot of length \n by dose") +
  xlab("Error (mm)") + 
  ylab("Count") +
  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  xlim(-200, 200)+
  # coord_equal() +
  # scale_fill_gradientn(colours=c("blue","green","red"),
  #                      "MAP (mm/yr)", 
  #                      limits=c(-300,500)) +
  theme(axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        # "element_text(size=16, angle=90)nxt,"
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "",
        legend.key = element_blank()
  )+
  geom_vline(aes(xintercept = mean(MAP)),col='red',size=0.5)+
  
  scale_y_continuous(position = "right")


  library(gridExtra)
  # get_legend<-function(myggplot){
  #   tmp <- ggplot_gtable(ggplot_build(myggplot))
  #   leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  #   legend <- tmp$grobs[[leg]]
  #   return(legend)
  # }
  
  # legend <- get_legend(a17)
  # 
  # grid.arrange(a1, a2, a3, a4, a5, a6,
  #              a7, a8, a9, a10, a11, a12,
  #              a13, a14, a15, a16, a17, a18,legend,
  #              ncol=3, nrow =7)

  # a1_lg = a13 
  # # extract_legend = function(my_ggp) {
  # #   step1 = ggplot_gtable(ggplot_build(my_ggp))
  # #   step2 = which(sapply(step1$grobs, function(x) x$name)== 'guide-box')
  # #   step3 = step1$grobs[[step2]]
  # #   return(step3)
  # # }
  # 
  # grid_arrange_shared_legend <- function(..., nrow = 1, ncol = length(list(...)), position = c("bottom", "right")) {
  #   
  #   plots <- list(...)
  #   position <- match.arg(position)
  #   g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  #   legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  #   lheight <- sum(legend$height)
  #   lwidth <- sum(legend$width)
  #   gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
  #   gl <- c(gl, nrow = nrow, ncol = ncol)
  #   
  #   combined <- switch(position,
  #                      "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
  #                                             legend,
  #                                             ncol = 1,
  #                                             heights = unit.c(unit(1, "npc") - lheight, lheight)),
  #                      "right" = arrangeGrob(do.call(arrangeGrob, gl),
  #                                            legend,
  #                                            ncol = 2,
  #                                            widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  #   grid.newpage()
  #   grid.draw(combined)
  #   
  # }
  # grid_arrange_shared_legend(a1, a2, a3, a4, nrow = 2, ncol = 2)
  # # memory.limit(size=56000)
  # shared_legend = extract_legend(a1_lg)
  # grid.arrange(arrangeGrob(a1, a2, a3, a4, a5, a6,
  #                          a7, a8, a9, a10, a11, a12,
  #                          a13, a14, a15, ncol=3), shared_legend, nrow =6, heights = c(5,1))
  # 

#ann_mod_ro
annm <- rasterToPoints(mean_ann_mod)
#Make the points a dataframe for ggplot
df <- data.frame(annm)

#Make appropriate column headings
colnames(df) <- c("Longitude", "Latitude", "MAP")
b.dem <- seq(min(df$MAP),max(df$MAP),length.out=100)
ann_mod_ro=ggplot(data=df, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=MAP)) +
  # ggtitle("Modelled") +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(fill= 'Runoff\n(mm/year)')+
  scale_y_continuous(breaks = seq(25, 50, len = 3)) +
  
  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  scale_fill_gradientn(colours = rainbow(50), limits=c(-50,1500))+
  # coord_equal() +
  # scale_fill_gradient("MAP (mm/yr)", limits=c(0,2500)) +
  # scale_fill_gradientn(name="Altitude",colours = rainbow(20),"MAP (mm/yr)", limits=c(-300,500)) +
  # geom_tile(aes(Longitude,Latitude,alpha=MAP), fill = "grey20") 
  
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        legend.key = element_blank(),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14)
  )
# + coord_equal() 

ann_mod_ro

#ann_ro
anno <- rasterToPoints(mean_ann_obs)
#Make the points a dataframe for ggplot
df <- data.frame(anno)

#Make appropriate column headings
colnames(df) <- c("Longitude", "Latitude", "MAP")
b.dem <- seq(min(df$MAP),max(df$MAP),length.out=100)
ann_obs_ro=ggplot(data=df, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=MAP)) +
  # ggtitle("Modelled") +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(fill= 'Runoff\n(mm/year)')+
  scale_y_continuous(breaks = seq(25, 50, len = 3)) +
  
  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  scale_fill_gradientn(colours = rainbow(50), limits=c(-50,1500))+
  # coord_equal() +
  # scale_fill_gradient("MAP (mm/yr)", limits=c(0,2500)) +
  # scale_fill_gradientn(name="Altitude",colours = rainbow(20),"MAP (mm/yr)", limits=c(-300,500)) +
  # geom_tile(aes(Longitude,Latitude,alpha=MAP), fill = "grey20") 
  
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        legend.key = element_blank(),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14)
  )
# + coord_equal() 

ann_obs_ro

#bf

bfi3 <- rasterToPoints(bfi2)
#Make the points a dataframe for ggplot
df <- data.frame(bfi3)

#Make appropriate column headings
colnames(df) <- c("Longitude", "Latitude", "MAP")
b.dem <- seq(min(df$MAP),max(df$MAP),length.out=100)
bfi4=ggplot(data=df, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=MAP)) +
  # ggtitle("Modelled") +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(fill= 'Baseflow\nIndex (%)')+
  scale_y_continuous(breaks = seq(25, 50, len = 3)) +
  
  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  scale_fill_gradientn(colours = rainbow(100), limits=c(0,100))+
  # coord_equal() +
  # scale_fill_gradient("MAP (mm/yr)", limits=c(0,2500)) +
  # scale_fill_gradientn(name="Altitude",colours = rainbow(20),"MAP (mm/yr)", limits=c(-300,500)) +
  # geom_tile(aes(Longitude,Latitude,alpha=MAP), fill = "grey20") 
  
  theme(axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        legend.key = element_blank(),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12)
  )+
  coord_fixed()

bfi4


#bf_mod

mod_bf <- rasterToPoints(mod_bf)
#Make the points a dataframe for ggplot
df <- data.frame(mod_bf)

#Make appropriate column headings
colnames(df) <- c("Longitude", "Latitude", "MAP")
b.dem <- seq(min(df$MAP),max(df$MAP),length.out=100)
mod_bfm=ggplot(data=df, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=MAP)) +
  # ggtitle("Modelled") +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(fill= 'Baseflow\n(mm/year)')+
  # scale_y_continuous(breaks = seq(25, 50, len = 3)) +
  # scale_x_continuous(breaks = seq(70, 120, len = 3)) +
  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  scale_fill_gradientn(colours = rainbow(100), limits=c(0,750))+
  # coord_equal() +
  # scale_fill_gradient("MAP (mm/yr)", limits=c(0,2500)) +
  # scale_fill_gradientn(name="Altitude",colours = rainbow(20),"MAP (mm/yr)", limits=c(-300,500)) +
  # geom_tile(aes(Longitude,Latitude,alpha=MAP), fill = "grey20") 
  
  theme(axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        legend.key = element_blank(),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14)
  )
mod_bfm

obs_bf <- rasterToPoints(obs_bf)
#Make the points a dataframe for ggplot
df <- data.frame(obs_bf)

#Make appropriate column headings
colnames(df) <- c("Longitude", "Latitude", "MAP")
b.dem <- seq(min(df$MAP),max(df$MAP),length.out=100)
obs_bfm=ggplot(data=df, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=MAP)) +
  # ggtitle("Modelled") +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(fill= 'Baseflow\n(mm/year)')+
  # scale_y_continuous(breaks = seq(25, 50, len = 5)) +
  # scale_x_continuous(breaks = seq(70, 120, len = 3)) +
  
  # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
  theme_bw() +
  scale_fill_gradientn(colours = rainbow(100), limits=c(0,750))+
  # coord_equal() +
  # scale_fill_gradient("MAP (mm/yr)", limits=c(0,2500)) +
  # scale_fill_gradientn(name="Altitude",colours = rainbow(20),"MAP (mm/yr)", limits=c(-300,500)) +
  # geom_tile(aes(Longitude,Latitude,alpha=MAP), fill = "grey20") 
  
  theme(axis.title.x = element_text(size=12),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        legend.key = element_blank(),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14)
  )
obs_bfm

roc = data.frame(mod_ro[,2], obs_ro[,2])
names(roc)=c('mod','obs')


roc1 = ggplot(roc, aes(x = obs, y = mod)) +
  geom_point(size=3)+
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1) +
  labs(
    x = "Observed Runoff (mm/year)",
    y = "Modelled Runoff (mm/year)"
    # color = "Gear",
    # title = "Relation between Mile per hours and drat",
    # subtitle = "Relationship break down by gear class",
    # caption = "Authors own computation"
  ) +
  xlim(0,1000)+
  ylim(0,1000)+
  
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        # panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank(),
        legend.position = "",
        legend.key = element_blank()
  )+
  geom_abline(intercept = 0, slope = 1)

roc1


#bfc

bfc = data.frame(mod_bf1[,2], obs_bf1[,2])
names(bfc)=c('mod','obs')


bfc1 = ggplot(bfc, aes(x = obs, y = mod)) +
  geom_point(size=3)+
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1) +
  labs(
    x = "Observed Baseflow (mm/year)",
    y = "Modelled Baseflow (mm/year)"
    # color = "Gear",
    # title = "Relation between Mile per hours and drat",
    # subtitle = "Relationship break down by gear class",
    # caption = "Authors own computation"
  ) +
  xlim(0,500)+
  ylim(0,500)+
  
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        # panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank(),
        legend.position = "",
        legend.key = element_blank()
  )+
  geom_abline(intercept = 0, slope = 1)

bfc1



  memory.limit(size=56000)
  library(patchwork)
  
  combined = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 +a9 +a10 +a11 +a12 +a13+a14+a15+a16+a17+a18 & theme(legend.position = "right")
  combined1 = combined + plot_layout(ncol = 3, guides = "collect")
  combined1
  
  combined2 = a19 + a20 + a21 + a22 + a23 + a24 + a25 + a26 +a27 +a28 +a29 +a30 +a31+a32+a33+a34+a35+a36 & theme(legend.position = "right")
  combined3 = combined2 + plot_layout(ncol = 3, guides = "collect")
  combined3
  
  combined4 = ann_mod_ro+ann_obs_ro & theme(legend.position = "right")
  combined5 = combined4 + plot_layout(ncol = 2, guides = "collect")
  combined5
  
  combined6 = mod_bfm+obs_bfm & theme(legend.position = "right")
  combined7 = combined6 + plot_layout(ncol = 2, guides = "collect")
  combined7
  
  # tiff('C:/Users/ezhil/OneDrive/AIT/papers/recharge paper/maps/New folder/t_1.tif', units="in", width=5, height=5, res=300)
  # writeRaster(combined, 't_1.tif', res = 300)
  # ggsave(combined, "t_1.tif", width = 44.45, height = 27.78, units = "cm", dpi=300)
  ggsave(filename="c6.tiff", plot=combined3, path=path1, dpi=300)
  
  ggsave(filename="ann_mod_obs_ro.tiff", plot=combined5, path=path1, dpi=300)
  ggsave(filename="roc1.tiff", plot=roc1, path=path1, dpi=300)
  ggsave(filename="bfi4.tiff", plot=bfi4, path=path1, dpi=300)
  ggsave(filename="mod_obs_bf.tiff", plot=combined7, path=path1, dpi=300)
  # ggsave(filename="obs_bf.tiff", plot=obs_bf, path=path1, dpi=300)
  ggsave(filename="bfc1.tiff", plot=bfc1, path=path1, dpi=300)
  # ggsave(filename="ann_obs_ro.tiff", plot=ann_obs_ro, path=path1, dpi=300)
  
  
  
  
writeRaster(mean_jan_mod,'C:/Users/ezhil/OneDrive/AIT/papers/recharge paper/maps/New folder/mean_jan_mod.tif',options=c('TFW=YES'), overwrite=T)
writeRaster(mean_feb_mod,'C:/Users/ezhil/OneDrive/AIT/papers/recharge paper/maps/New folder/mean_feb_mod.tif',options=c('TFW=YES'), overwrite=T)
writeRaster(mean_mar_mod,'C:/Users/ezhil/OneDrive/AIT/papers/recharge paper/maps/New folder/mean_mar_mod.tif',options=c('TFW=YES'), overwrite=T)
writeRaster(mean_apl_mod,'C:/Users/ezhil/OneDrive/AIT/papers/recharge paper/maps/New folder/mean_apl_mod.tif',options=c('TFW=YES'), overwrite=T)
writeRaster(mean_may_mod,'C:/Users/ezhil/OneDrive/AIT/papers/recharge paper/maps/New folder/mean_may_mod.tif',options=c('TFW=YES'), overwrite=T)
writeRaster(mean_jun_mod,'C:/Users/ezhil/OneDrive/AIT/papers/recharge paper/maps/New folder/mean_jun_mod.tif',options=c('TFW=YES'), overwrite=T)
writeRaster(mean_jul_mod,'C:/Users/ezhil/OneDrive/AIT/papers/recharge paper/maps/New folder/mean_jul_mod.tif',options=c('TFW=YES'), overwrite=T)
writeRaster(mean_aug_mod,'C:/Users/ezhil/OneDrive/AIT/papers/recharge paper/maps/New folder/mean_aug_mod.tif',options=c('TFW=YES'), overwrite=T)
writeRaster(mean_sep_mod,'C:/Users/ezhil/OneDrive/AIT/papers/recharge paper/maps/New folder/mean_sep_mod.tif',options=c('TFW=YES'), overwrite=T)
writeRaster(mean_oct_mod,'C:/Users/ezhil/OneDrive/AIT/papers/recharge paper/maps/New folder/mean_oct_mod.tif',options=c('TFW=YES'), overwrite=T)
writeRaster(mean_nov_mod,'C:/Users/ezhil/OneDrive/AIT/papers/recharge paper/maps/New folder/mean_nov_mod.tif',options=c('TFW=YES'), overwrite=T)
writeRaster(mean_dec_mod,'C:/Users/ezhil/OneDrive/AIT/papers/recharge paper/maps/New folder/mean_dec_mod.tif',options=c('TFW=YES'), overwrite=T)

usa_zone = rasterize(usa, r, c(01))
extent(usa_zone) = ppt_s1[[1]]
usa_zone1 = resample(usa_zone, ppt_s1[[1]],"bilinear")
# 

ann_ind = seq(0,84,by=12)
y1 = seq(1,12,1)
y2 = seq(13,24,1)
y3 = seq(25,36,1)
y4 = seq(37,48,1)
y5 = seq(49,60,1)
y6 = seq(61,72,1)
y7 = seq(73,84,1)

P=list()

# for (i in 2:2){
  # length(ann_ind)

p1 = stack(ppt_s1[[y1]])
c1 = calc(p1, fun = sum, na.rm = T)
p2 = stack(ppt_s1[[y2]])
c2 = calc(p2, fun = sum, na.rm = T)
p3 = stack(ppt_s1[[y3]])
c3 = calc(p3, fun = sum, na.rm = T)
p4 = stack(ppt_s1[[y4]])
c4 = calc(p4, fun = sum, na.rm = T)
p5 = stack(ppt_s1[[y5]])
c5 = calc(p5, fun = sum, na.rm = T)
p6 = stack(ppt_s1[[y6]])
c6 = calc(p6, fun = sum, na.rm = T)
p7 = stack(ppt_s1[[y7]])
c7 = calc(p7, fun = sum, na.rm = T)

P1 = zonal(c1, usa_zone1, fun = mean, na.rm=TRUE)
P2 = zonal(c2, usa_zone1, fun = mean, na.rm=TRUE)
P3 = zonal(c3, usa_zone1, fun = mean, na.rm=TRUE)
P4 = zonal(c4, usa_zone1, fun = mean, na.rm=TRUE)
P5 = zonal(c5, usa_zone1, fun = mean, na.rm=TRUE)
P6 = zonal(c6, usa_zone1, fun = mean, na.rm=TRUE)
P7 = zonal(c7, usa_zone1, fun = mean, na.rm=TRUE)


p1 = stack(et_s1[[y1]])
c1 = calc(p1, fun = sum, na.rm = T)
p2 = stack(et_s1[[y2]])
c2 = calc(p2, fun = sum, na.rm = T)
p3 = stack(et_s1[[y3]])
c3 = calc(p3, fun = sum, na.rm = T)
p4 = stack(et_s1[[y4]])
c4 = calc(p4, fun = sum, na.rm = T)
p5 = stack(et_s1[[y5]])
c5 = calc(p5, fun = sum, na.rm = T)
p6 = stack(et_s1[[y6]])
c6 = calc(p6, fun = sum, na.rm = T)
p7 = stack(et_s1[[y7]])
c7 = calc(p7, fun = sum, na.rm = T)

ET1 = zonal(c1, usa_zone1, fun = mean, na.rm=TRUE)
ET2 = zonal(c2, usa_zone1, fun = mean, na.rm=TRUE)
ET3 = zonal(c3, usa_zone1, fun = mean, na.rm=TRUE)
ET4 = zonal(c4, usa_zone1, fun = mean, na.rm=TRUE)
ET5 = zonal(c5, usa_zone1, fun = mean, na.rm=TRUE)
ET6 = zonal(c6, usa_zone1, fun = mean, na.rm=TRUE)
ET7 = zonal(c7, usa_zone1, fun = mean, na.rm=TRUE)

#dfr1
y1 = seq(2,12,1)
y7 = seq(73,83,1)

p1 = stack(dfr1[y1])
c1 = calc(p1, fun = sum, na.rm = T)
p2 = stack(dfr1[y2])
c2 = calc(p2, fun = sum, na.rm = T)
p3 = stack(dfr1[y3])
c3 = calc(p3, fun = sum, na.rm = T)
p4 = stack(dfr1[y4])
c4 = calc(p4, fun = sum, na.rm = T)
p5 = stack(dfr1[y5])
c5 = calc(p5, fun = sum, na.rm = T)
p6 = stack(dfr1[y6])
c6 = calc(p6, fun = sum, na.rm = T)
p7 = stack(dfr1[y7])
c7 = calc(p7, fun = sum, na.rm = T)

extent(usa_zone) = dfr1[[2]]
usa_zone1 = resample(usa_zone, dfr1[[2]],"bilinear")

DFR1 = zonal(c1, usa_zone1, fun = mean, na.rm=TRUE)
DFR2 = zonal(c2, usa_zone1, fun = mean, na.rm=TRUE)
DFR3 = zonal(c3, usa_zone1, fun = mean, na.rm=TRUE)
DFR4 = zonal(c4, usa_zone1, fun = mean, na.rm=TRUE)
DFR5 = zonal(c5, usa_zone1, fun = mean, na.rm=TRUE)
DFR6 = zonal(c6, usa_zone1, fun = mean, na.rm=TRUE)
DFR7 = zonal(c7, usa_zone1, fun = mean, na.rm=TRUE)

#mod_ro

p1 = stack(mod_ro[y1])
c1 = calc(p1, fun = sum, na.rm = T)
p2 = stack(mod_ro[y2])
c2 = calc(p2, fun = sum, na.rm = T)
p3 = stack(mod_ro[y3])
c3 = calc(p3, fun = sum, na.rm = T)
p4 = stack(mod_ro[y4])
c4 = calc(p4, fun = sum, na.rm = T)
p5 = stack(mod_ro[y5])
c5 = calc(p5, fun = sum, na.rm = T)
p6 = stack(mod_ro[y6])
c6 = calc(p6, fun = sum, na.rm = T)
p7 = stack(mod_ro[y7])
c7 = calc(p7, fun = sum, na.rm = T)


extent(usa_zone) = mod_ro[[2]]
usa_zone1 = resample(usa_zone, mod_ro[[2]],"bilinear")


MOD_RO1 = zonal(c1, usa_zone1, fun = mean, na.rm=TRUE)
MOD_RO2 = zonal(c2, usa_zone1, fun = mean, na.rm=TRUE)
MOD_RO3 = zonal(c3, usa_zone1, fun = mean, na.rm=TRUE)
MOD_RO4 = zonal(c4, usa_zone1, fun = mean, na.rm=TRUE)
MOD_RO5 = zonal(c5, usa_zone1, fun = mean, na.rm=TRUE)
MOD_RO6 = zonal(c6, usa_zone1, fun = mean, na.rm=TRUE)
MOD_RO7 = zonal(c7, usa_zone1, fun = mean, na.rm=TRUE)

#ras_ro1
y1 = seq(2,12,1)
y7 = seq(73,83,1)


p1 = stack(ras_ro1[[y1]])
c1 = calc(p1, fun = sum, na.rm = T)
p2 = stack(ras_ro1[[y2]])
c2 = calc(p2, fun = sum, na.rm = T)
p3 = stack(ras_ro1[[y3]])
c3 = calc(p3, fun = sum, na.rm = T)
p4 = stack(ras_ro1[[y4]])
c4 = calc(p4, fun = sum, na.rm = T)
p5 = stack(ras_ro1[[y5]])
c5 = calc(p5, fun = sum, na.rm = T)
p6 = stack(ras_ro1[[y6]])
c6 = calc(p6, fun = sum, na.rm = T)
p7 = stack(ras_ro1[[y7]])
c7 = calc(p7, fun = sum, na.rm = T)


extent(usa_zone) = ras_ro1[[2]]
usa_zone1 = resample(usa_zone, ras_ro1[[2]],"bilinear")


RAS_RO1 = zonal(c1, usa_zone1, fun = mean, na.rm=TRUE)
RAS_RO2 = zonal(c2, usa_zone1, fun = mean, na.rm=TRUE)
RAS_RO3 = zonal(c3, usa_zone1, fun = mean, na.rm=TRUE)
RAS_RO4 = zonal(c4, usa_zone1, fun = mean, na.rm=TRUE)
RAS_RO5 = zonal(c5, usa_zone1, fun = mean, na.rm=TRUE)
RAS_RO6 = zonal(c6, usa_zone1, fun = mean, na.rm=TRUE)
RAS_RO7 = zonal(c7, usa_zone1, fun = mean, na.rm=TRUE)



P = c(P1,P2,P3,P4,P5,P6,P7)
ET = c(ET1,ET2,ET3,ET4,ET5,ET6,ET7)
DFR = c(DFR1,DFR2,DFR3,DFR4,DFR5,DFR6,DFR7)
MOD_RO = c(MOD_RO1,MOD_RO2,MOD_RO3,MOD_RO4,MOD_RO5,MOD_RO6,MOD_RO7)
RAS_RO = c(RAS_RO1,RAS_RO2,RAS_RO3,RAS_RO4,RAS_RO5,RAS_RO6,RAS_RO7)


write.table(mean_jan_mod, "C:/Users/ezhil/OneDrive/AIT/papers/recharge paper/analysis/mean_jan_mod_t.txt", sep="\t")
write.table(mean_jan_obs, "C:/Users/ezhil/OneDrive/AIT/papers/recharge paper/analysis/mean_jan_obs_t.txt", sep="\t")
write.table(usa, "C:/Users/ezhil/OneDrive/AIT/papers/recharge paper/analysis/DFR.txt", sep="\t")
write.table(MOD_RO, "C:/Users/ezhil/OneDrive/AIT/papers/recharge paper/analysis/MOD_RO.txt", sep="\t")
write.table(RAS_RO, "C:/Users/ezhil/OneDrive/AIT/papers/recharge paper/analysis/RAS_RO.txt", sep="\t")

  
  
  
# }

# 
# plot(mean_ann)
# plot(huc8, add=TRUE)

# mrs = stack(mod_ro[[1]],mod_ro[[2]])
# 
# b1 <- stackApply(mod_ro_all, indices=c(1,1,1), fun=sum)
# b1
# b = raster(mod_ro_all, jan_ind)
# 
#  # for (i in 1:length(L)) {
# #   writeRaster(L[[i]], names(L[[i]]), format='GTiff')
# # }
# 
# names(drf1)
# 
# 
# md


# plot(mod_ro[[38]])
# plot(ne, add=TRUE)
# mod_ro[[38]]

# writeRaster(mod_ro[[38]],'C:/Users/ezhil/OneDrive/AIT/papers/recharge paper/maps/test.tif',options=c('TFW=YES'))
# 
# 
# mod_ro[[5]]
# 
# #get the date from the names of the layers and extract the month
# indices <- format(as.Date(names(ndvi.stack), format = "X%Y.%m.%d"), format = "%m")
# indices <- as.numeric(indices)
# 
# #sum layers
# MonthNDVI<- stackApply(ndvi.stack, indices, fun = mean)
# names(MonthNDVI) <- month.abb




# newproj1 = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# newproj2 = "+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"./;l,
# 
# # project grace data
# newproj3 <- "+proj=lcc +lat_1=50 +lat_2=50 +lat_0=50 +lon_0=-107 +x_0=5632642.22547 +y_0=4612545.65137 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
# # 
# 
# pr1 <- projectRaster(dfr1[[5]], crs=newproj3)
# # 
# pr1
# plot(dfr1[[5]])
# plot(pr1)
# plot(ppt_s[[5]])
# plot(et_s[[5]])
# pr1
# ppt_s[[5]]
# 
# resampleFactor <- 1/5  # reduce the cell size by 50% and double the number of rows and columns.
# inputRaster <- pr1
# inCols <- ncol(inputRaster)
# inRows <- nrow(inputRaster)
# resampledRaster <- raster(ncol=(inCols / resampleFactor), nrow=(inRows / resampleFactor))
# extent(resampledRaster) <- extent(inputRaster)
# 
# resampledRaster <- resample(inputRaster,resampledRaster,datatype="INT1U",method='bilinear')
# 
# plot(ppt1)
# 
# resampledRaster
# 
# ppt_s[[5]]
# 
# r2 = et_s[[5]]
# r1 = resample(ppt_s[[5]], et_s[[5]], "bilinear")
# r3 = resample(resampledRaster, et_s[[5]], "bilinear")
# ds1 <- overlay(r1, r2, r3, fun=function(r1, r2, r3){return(r1-r2-r3)})
# 
# plot(ds1)
# 

# 
# 
# 
# 
# r <- raster(ncol=148, nrow=71)
# extent(r) <- extent(huc8)
# col1 = huc8$X200001
# col2 = data.frame(huc8)
# col3 = col2[,35]
# ras_ro = list()
# for (i in 35:224){
#   ras_ro[i-34] <- rasterize(huc8, r, col2[,i])
# }



# res(ds[[150]])
#projecting
  # mean_ann = focal(mean_ann, w = matrix(1, nrow = 61, ncol = 61), fun = mean, pad = TRUE, na.rm= TRUE)
  # plot(mean_ann1)
  # extent(ro_zone)=mean_jan
  # rozone1 = resample(ro_zone,mean_ann,'bilinear')
  # 
  # # mean_ann = focal(mean_ann, w = matrix(1, nrow = 61, ncol = 61), fun = mean, pad = TRUE, na.rm= TRUE)
  # 
  # 
  # ex  = zonal(mean_jan, rozone1, fun = "mean", na.rm=TRUE)
  # ex
  # ext_ppt = extract(mean_ann, huc8)
  # 
  # # plot(mean_ann)
  # # plot(huc8, add=TRUE)
  # #
  # mean_ppt = lapply(ext_ppt, mean, na.rm = T)
  # # 
  # # plot(ne)
  # # 
  # # mean_ppt
  # 
  # #"C:/Users/ezhil/OneDrive/AIT/papers/recharge paper/analysis/test1.xlsx"
  # write.table(mean_ppt, "C:/Users/ezhil/OneDrive/AIT/papers/recharge paper/analysis/mod_ann_ro2.txt", sep="\t")
  # # write.xlsx(mean_ppt, "C:/Users/ezhil/OneDrive/AIT/papers/recharge paper/analysis/test1.xlsx")
  # 
  # # mean_ann2 <- projectRaster(mean_ann1, crs=newproj3)
  # # plot (mean_ann1)
  # ext_ppt1 = extract(mean_ann1, huc8)
  # #
  # mean_ppt1 = lapply(ext_ppt1, mean, na.rm = T)
  # # 
  # # plot(ne)
  # 
  # # mean_ppt1
  # 
  # #"C:/Users/ezhil/OneDrive/AIT/papers/recharge paper/analysis/test1.xlsx"
  # write.table(mean_ppt1, "C:/Users/ezhil/OneDrive/AIT/papers/recharge paper/analysis/obs_ann_ro.txt", sep="\t")
  # 
  # writeRaster(mean_ann1,'C:/Users/ezhil/OneDrive/AIT/papers/recharge paper/maps/obs2.tif',options=c('TFW=YES'))
writeRaster(mean_jan_mod,'C:/Users/ezhil/OneDrive/AIT/papers/recharge paper/maps/New folder/mean_jan_mod_1.tif',options=c('TFW=YES'), overwrite=T)
writeRaster(mean_jan_obs,'C:/Users/ezhil/OneDrive/AIT/papers/recharge paper/maps/New folder/mean_jan_obs_1.tif',options=c('TFW=YES'), overwrite=T)
# writeRaster(usa,'C:/Users/ezhil/OneDrive/AIT/papers/recharge paper/maps/New folder/usa.tif',options=c('TFW=YES'), overwrite=T)

