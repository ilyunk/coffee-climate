library(raster)
library(data.table)
library(rgdal)
#
udel_precipitation_dir <- '/projectnb/climpct/UDEL/precipitation'
centroid<-read.csv("/projectnb/climpct/ikoh/centroid coordinates and crop data/centroid_list2.csv", header=TRUE)
setwd(udel_precipitation_dir)

udel_files1990_2014 <-list.files(pattern="precip.197[4-9]|201[0-4]")
#udel_files1990_2014 <- udel_files1990_2014_1[ !grepl("air_temp_2014.tar", udel_files1990_2014_1) ]
#removed air_temp_2014.tar
# load all climate data

count <- 1

for(f in udel_files1990_2014) {
  x0 <- fread(f)
  
  # raw data do not identify year within the file, but year is in the filename
  # at each pass through the loop, grab filename and parse year out of the string, append as a separate column 
  
  x0$year <- as.numeric(gsub('.*\\.','',f)) 
  if (count==1) {
    d <- x0
  } else {
    d <- rbind(d,x0)
  }
  count <- count + 1
}

# data format: lon, lat, one column for each of 12 months, year

setnames(d,c('x','y',paste('m',sprintf('%02d',seq(1:12)),sep=''),'year'))

# crop to Brazil extent
# because data are 0.5 deg, remember to include a buffer of 0.5 deg to avoid NAs in border municipios 

d0 <- d[d$x>=-73.985527-0.5 & d$x<=-28.839041+0.5 & d$y>=-35-0.5 & d$y<=5.26486+0.5,]

# reshape long, create year.month layer id

d1 <- melt(d0,id=c('x','y','year'))
d1$layer.id <- paste(d1$year,d1$variable,sep='')
d1$layer.id <- gsub('m','\\.',d1$layer.id)
d1$year <- NULL
d1$variable <- NULL

# reshape wide, coordinates as rows, layer id as columns

d2 <- dcast(d1,x+y~layer.id)

# load Brazil shapefile, we'll need its projection and municipio id/name and polygon extents

shape <- readOGR(dsn = "/projectnb/climpct/ikoh/bra_shapefile/", layer = "municipios_2010_2")

# turn weather data into a spatial polygons data frame, apply shapefile projection

coordinates(d2) <- ~ x + y
gridded(d2) <- TRUE
proj4string(d2) <- proj4string(shape)

# stack columns of spatial polygons data frame into a multilayer raster 'brick'
#    NB this is why we transformed the data back into wide format, so that we could load it all into a raster in a single shot
b0 <- brick(d2)

# key step here!
# extract the stack of rasters into the municipio polygon shapefile, using bilinear interpolation
# by loading *all* the data into a raster brick we can efficiently perform this operation in one fell swoop

e0 <- extract(b0,shape,fun=mean,method='bilinear',na.rm=T)
e1 <- as.data.frame(e0)
setnames(e1,names(b0))
e1$id <- shape$codigo_ibg
e1$name <- shape$nome

e2 <- melt(e1,id=c('id','name'))
e2$year <- as.numeric(substr(e2$variable,2,5))
e2$month <- as.numeric(substr(e2$variable,7,8))
#e2$variable <- NULL
#    growing season calculation: assume Aug-Dec
#plant <- 1
#harvest <- 12
e3 <- data.table(e2)
e3$season <- e3$year
#e3[month<plant,]$season <- NA  # blank out non-growing season months 
#e3[month<=harvest,]$season <- e3[month<=harvest,]$year+1  # critical: Oct-Dec weather in any year affects next year's harvest
#e3[month>harvest,]$season <- NA 
#    note the beauty of the data.table syntax for taking statistics
#    this is why we use data.table, in conjunction with the split/apply/combine philosophy,
#    on "long" form data that can be subsetted according to the criteria you desire

#corn's growing season is aug to dec

seasonal_sum <- e3[!is.na(season),sum(value),by=c('id','name','season')]
names(seasonal_sum)[4]<-paste("pcp_sum")

require(data.table)
require(reshape2)
require(sandwich)

# Installation
install.packages('ggplot2')
# Loading
library(ggplot2)

weather_dir <- '/projectnb/climpct/ikoh/output_UDEL/seasonal0531/'
yield_dir <- '/projectnb/climpct/ikoh/output05312017/'
result_dir <- '/projectnb/climpct/ikoh/result_dir_UDEL/result_07062018'

#caf_yield.dt <- fread(paste(yield_dir,'table 106 coffee yield_centroid3.csv',sep=''),header=T,na.strings='-',stringsAsFactors=F)
caf_yield.dt <- fread(paste(yield_dir,'PAM_table1613_coffee_yield4.csv',sep=''),header=T,na.strings='-',stringsAsFactors=F)
caf_yield.dt <- melt(caf_yield.dt,id.vars=c('municipio','county'),measure.vars=6:46)
setnames(caf_yield.dt,c('variable','value'),c('year','yield'))
caf_yield.dt$year <- as.integer(as.character(caf_yield.dt$year))
caf_yield.dt$yield <- as.numeric(caf_yield.dt$yield)

#setwd(weather_dir)
#caf_pcp1.dt <- read.csv('/projectnb/climpct/ikoh/output_UDEL/seasonal0531/caf_precip_seasonal_allmonths_1974_2014.csv', header=T)
seasonal_sum$season <- as.integer(as.character(seasonal_sum$season))
seasonal_sum$municipio <- as.integer(as.character(seasonal_sum$municipio))

names(seasonal_sum)[1]<-paste("municipio")
names(seasonal_sum)[3]<-paste("year")

caf_tmp1.dt <- subset(caf_tmp1.dt, select=c('municipio', 'name', 'year', 'tmp_mean', 'tmp_max', 'tmp_min'))
caf_pcp1.dt <- subset(caf_pcp1.dt, select=c('municipio', 'name', 'year', 'pcp_mean', 'pcp_max', 'pcp_min'))

dt0 <- merge(caf_yield.dt,seasonal_sum,by=c('municipio','year'))
dt0$log_yield <- log(dt0$yield)
dt0$good <- 0
dt0[!is.na(dt0$yield) & dt0$yield>0,]$good <- 1
#dt1 <- subset(dt1, select=c('municipio', 'County', 'year', 'tmp_mean', 'tmp_max', 'tmp_min', 'pcp_mean', 'pcp_max', 'pcp_min', 'yield', 'log_yield', 'good'))


#add state code
dt0$state <- as.numeric(substr(dt0$municipio,1,2))

dt1 <- subset(dt0, dt0$state == 29 | dt0$state == 31 | dt0$state == 33 | dt0$state == 35 | dt0$state == 41 | dt0$state == 50 | dt0$state == 52 | dt0$state == 53)
dt1 <- dt1 <- subset(dt1, dt1$good == 1)
####
setwd(result_dir)
write.csv(dt1, '/projectnb/climpct/ikoh/output_UDEL/seasonal0531/caf_precip_seasonal_annual_1974_2014.csv',row.names=F) 