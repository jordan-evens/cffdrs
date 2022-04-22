library("terra")
library("devtools")
devtools::load_all()
test_fbpRaster <- rast(system.file("extdata", "test_fbpRaster.tif", package="cffdrs"))
input<-test_fbpRaster
# Rast doesn't hold the raster layer names, we have to assign
# them:
names(input)<-c("FuelType","LAT","LONG","ELV","FFMC","BUI", "WS","WD","GS","Dj","D0","hr","PC",
"PDF","GFL","cc","theta","Accel","Aspect","BUIEFF","CBH","CFL","ISI")
# Primary outputs:
# system.time(foo<-fbpRaster(input = input))
output = "ALL"
select=NULL
# Primary outputs:
system.time(foo1<-fbpRaster(input = input))
# Using the "select" option:
system.time(foo2<-fbpRaster(input = input,select=c("HFI","TFC", "ROS")))
# Secondary outputs:
system.time(foo3<-fbpRaster(input = input,output="S"))
# All outputs:
system.time(foo4<-fbpRaster(input = input,output="A"))

### Additional, longer running examples  ###
# Keep only the required input layers, the other layers would be
# assigned with default values:
# keep only the required inputs:
dat0<-input[[c("FuelType","LAT","LONG","FFMC","BUI","WS","GS", "Dj","Aspect")]]
system.time(foo5<-fbpRaster(input = dat0,output="A"))
