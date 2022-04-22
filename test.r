library("terra")
library("cffdrs")
test_fbpRaster <- rast(system.file("extdata", "test_fbpRaster.tif", package="cffdrs"))
input<-test_fbpRaster
# Rast doesn't hold the raster layer names, we have to assign
# them:
names(input)<-c("FuelType","LAT","LONG","ELV","FFMC","BUI", "WS","WD","GS","Dj","D0","hr","PC",
"PDF","GFL","cc","theta","Accel","Aspect","BUIEFF","CBH","CFL","ISI")
# Primary outputs:
# system.time(foo<-fbpRaster(input = input))
output = "Primary"
select=NULL

#  Quite often users will have a data frame called "input" already attached
#  to the workspace. To mitigate this, we remove that if it exists, and warn
#  the user of this case. This is also done in Fbp, but we require use
#  of this variable here before it gets to Fbp
if (!is.na(charmatch("input", search()))) {
warning("Attached dataset 'input' is being detached to use fbp() function.")
detach(input)
}
input_raster <- F
# This will detect if the user has input rasters and will return rasters even
# though the whole function will operate with terra. This will deprecate with
# raster.
if(class(input) %in% c("Raster","RasterStack","RasterBrick")){
input_raster <- T
input <- rast(input)
}
if(class(init) %in% c("Raster","RasterStack","RasterBrick")){
init <- rast(init)
}
names(input) <- tolower(names(input))

#Setup correct output names
allNames <- c("CFB","CFC","FD","HFI","RAZ","ROS","SFC","TFC","BE","SF","ISI",
			"FFMC", "FMC","D0", "RSO","CSI","FROS","BROS","HROSt","FROSt",
			"BROSt","FCFB", "BCFB","FFI","BFI", "FTFC","BTFC","TI","FTI",
			"BTI","LB","LBt","WSV", "DH","DB","DF","TROS","TROSt", "TCFB",
			"TFI","TTFC","TTI")
primaryNames <- allNames[1:8]
secondaryNames <- allNames[9:length(allNames)]
#If outputs are specified, then check if they exist and stop with an error
#  if not.
if (!is.null(select)){
select <- toupper(select)
select <- select[!duplicated(select)]
if(output == "SECONDARY" | output == "S"){
  if (!sort(select %in% secondaryNames)[1]){
	stop("Selected variables are not in the outputs")}
}
if (output == "PRIMARY" | output == "P"){
  if (!sort(select %in% primaryNames)[1]){
	stop("Selected variables are not in the outputs")} 
}
if (output == "ALL" | output == "A"){
  if (!sort(select %in% allNames)[1]){
	stop("Selected variables are not in the outputs")} 
}
}
names(input) <- toupper(names(input))
output <- toupper(output)
