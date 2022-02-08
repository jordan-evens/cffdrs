#' Raster-based Fire Weather Index System
#' 
#' \code{fwiRaster} is used to calculate the outputs of the Canadian Forest
#' Fire Weather Index (FWI) System for one day based on noon local standard
#' time (LST) weather observations of temperature, relative humidity, wind
#' speed, and 24-hour rainfall, as well as the previous day's fuel moisture
#' conditions. This function takes rasterized input and generates raster maps
#' as outputs.
#' 
#' The Canadian Forest Fire Weather Index (FWI) System is a major subsystem of
#' the Canadian Forest Fire Danger Rating System, which also includes Canadian
#' Forest Fire Behavior Prediction (FBP) System. The modern FWI System was
#' first issued in 1970 and is the result of work by numerous researchers from
#' across Canada. It evolved from field research which began in the 1930's and
#' regional fire hazard and fire danger tables developed from that early
#' research.
#' 
#' The modern System (Van Wagner 1987) provides six output indices which
#' represent fuel moisture and potential fire behavior in a standard pine
#' forest fuel type. Inputs are a daily noon observation of fire weather, which
#' consists of screen-level air temperature and relative humidity, 10 meter
#' open wind speed and 24 accumulated precipitation.
#' 
#' The first three outputs of the system (the Fire Fuel Moisture Code, the Duff
#' Moisture Code, and the Drought Code) track moisture in different layers of
#' the fuel making up the forest floor. Their calculation relies on the daily
#' fire weather observation and also, importantly, the code value from the
#' previous day as they are in essence bookkeeping systems tracking the amount
#' of moisture (water) in to and out of the layer.  It is therefore important
#' that when calculating FWI System outputs over an entire fire season, an
#' uninterrupted daily weather stream is provided; one day is the assumed time
#' step in the models and thus missing data must be filled in.
#' 
#' The next three outputs of the System are relative (unitless) indicators of
#' aspects of fire behavior potential: spread rate (the Initial Spread Index),
#' fuel consumption (the Build-up Index) and fire intensity per unit length of
#' fire front (the Fire Weather Index).  This final index, the fwi, is the
#' component of the System used to establish the daily fire danger level for a
#' region and communicated to the public.  This final index can be transformed
#' to the Daily Severity Rating (dsr) to provide a more reasonably-scaled
#' estimate of fire control difficulty.
#' 
#' Both the Duff Moisture Code (dmc) and Drought Code (dc) are influenced by
#' day length (see Van Wagner, 1987). Day length adjustments for different
#' ranges in latitude can be used (as described in Lawson and Armitage 2008
#' (\url{http://cfs.nrcan.gc.ca/pubwarehouse/pdfs/29152.pdf})) and are included
#' in this R function; latitude must be positive in the northern hemisphere and
#' negative in the southern hemisphere.
#' 
#' The default initial (i.e., "start-up") fuel moisture code values (FFMC=85,
#' DMC=6, DC=15) provide a reasonable set of conditions for most springtime
#' conditions in Canada, the Northern U.S., and Alaska. They are not suitable
#' for particularly dry winters and are presumably not appropriate for
#' different parts of the world.
#' 
#' @param input A c (terra), stack (raster), or brick (raster) containing rasterized daily weather
#' observations taken at noon LST. Variable names have to be the same as in the
#' following list, but they are case insensitive. The order in which the inputs
#' are entered is not important.
#' 
#' \tabular{lll}{ 
#' \var{lat} \tab (recommended) \tab Latitude (decimal degree,
#' __default=55__)\cr 
#' \var{temp} \tab (required) \tab Temperature (centigrade)\cr
#' \var{rh} \tab (required) \tab Relative humidity (\%)\cr 
#' \var{ws} \tab
#' (required) \tab 10-m height wind speed (km/h)\cr 
#' \var{prec} \tab (required)
#' \tab 24-hour rainfall (mm)\cr }
#' @param init A vector that contains the initial values for FFMC, DMC, and DC
#' or a c, stack or brick that contains raster maps of the three moisture codes calculated
#' for the previous day, which will be used for the current day's calculation.
#' Defaults are the standard initial values for FFMC, DMC, and DC defined as
#' the following: 
#' 
#' \tabular{lll}{ 
#' \bold{Variable} \tab \bold{Description} \tab \bold{Default} \cr
#' \var{ffmc} \tab Previous day Fine Fuel Moisture Code (FFMC; unitless) \tab 85 \cr
#' \var{dmc} \tab Previous day Duff Moisture Code (DMC; unitless)\tab 6 \cr
#' \var{dc} \tab Previous Day Drought Code (DC; unitless) \tab 15\cr
#' \var{lat} \tab Latitude of the weather station (\emph{Optional})\tab 55 \cr}
#' 
#' @param mon Month of the year (integer 1~12, default=7). Month is used in
#' latitude adjustment (\code{lat.adjust}), it is therefore recommended when
#' \code{lat.adjust=TRUE} was chosen.
#' @param out The function offers two output options, \code{out="all"} will
#' produce a raster c include both the input and the FWI System outputs;
#' \code{out="fwi"} will generate a c or brick with only the FWI system components.
#' @param lat.adjust The function offers options for whether latitude
#' adjustments to day lengths should be applied to the calculations. The
#' default value is "TRUE".
#' @param uppercase Output in upper cases or lower cases would be decided by
#' this argument. Default is TRUE.
#' @return By default, \code{fwi} returns a raster c which includes both
#' the input and the FWI System variables, as describe below: \item{Inputs
#' }{Including \code{temp}, \code{rh}, \code{ws}, and \code{prec} with
#' \code{lat} as optional.} \item{ffmc }{Fine Fuel Moisture Code} \item{dmc
#' }{Duff Moisture Code} \item{dc }{Drought Code} \item{isi }{Initial Spread
#' Index} \item{bui }{Buildup Index} \item{fwi }{Fire Weather Index} \item{dsr
#' }{Daily Severity Rating}
#' 
#' @author Xianli Wang, Alan Cantin, Marc-André Parisien, Mike Wotton, Kerry
#' Anderson, and Mike Flannigan
#' 
#' @seealso \code{\link{fbp}}, \code{\link{fbpRaster}}, \code{\link{fwi}},
#' \code{\link{hffmc}}, \code{\link{hffmcRaster}}
#' 
#' @references 1. Van Wagner, C.E. and T.L. Pickett. 1985. Equations and
#' FORTRAN program for the Canadian Forest Fire Weather Index System. Can. For.
#' Serv., Ottawa, Ont. For. Tech. Rep. 33. 18 p.
#' \url{http://cfs.nrcan.gc.ca/pubwarehouse/pdfs/19973.pdf}
#' 
#' 2. Van Wagner, C.E. 1987. Development and structure of the Canadian forest
#' fire weather index system. Forest Technology Report 35. (Canadian Forestry
#' Service: Ottawa). \url{http://cfs.nrcan.gc.ca/pubwarehouse/pdfs/19927.pdf}
#' 
#' 3.  Lawson, B.D. and O.B. Armitage. 2008. Weather guide for the Canadian
#' Forest Fire Danger Rating System. Nat. Resour. Can., Can. For. Serv., North.
#' For. Cent., Edmonton, AB.
#' \url{http://cfs.nrcan.gc.ca/pubwarehouse/pdfs/29152.pdf}
#' 
#' @keywords methods
#' 
#' @examples
#' 
#' library(cffdrs)
#' require(terra)
#' # The test data is a c with four input variables including 
#' # daily noon temp, rh, ws, and prec (we recommend tif format):
#' day01src <- system.file("extdata","test_rast_day01.tif",package="cffdrs")
#' day01 <- rast(day01src)
#' day01 <- crop(day01,c(250,255,47,51))
#' # assign variable names:
#' names(day01)<-c("temp","rh","ws","prec")
#' # (1) use the initial values
#' foo<-fwiRaster(day01)
#' plot(foo)
#' ### Additional, longer running examples ###
#' # (2) use initial values with larger raster
#' day01 <- c(day01src)
#' names(day01)<-c("temp","rh","ws","prec")
#' \donttest{foo<-fwiRaster(day01)}
#' plot(foo)
#' 
#' @export fwiRaster
fwiRaster <- function(input, init = c(ffmc = 85, dmc = 6, dc = 15), mon = 7,
                      out = "all", lat.adjust = TRUE, uppercase = TRUE) {
  
  #Quite often users will have a data frame called "input" already attached
  #  to the workspace. To mitigate this, we remove that if it exists, and warn
  #  the user of this case.
  if (!is.na(charmatch("input", search()))) {
    detach(input)
  }
  input_raster <- F
  if(class(input) %in% c("Raster","RasterStack","RasterBrick")){
    input_raster <- T
    input <- rast(input)
  }
  if(class(init) %in% c("Raster","RasterStack","RasterBrick")){
    init <- rast(init)
  }
  names(input) <- tolower(names(input))
  temp <- input$temp
  prec <- input$prec
  ws <- input$ws
  rh <- input$rh
  if ("lat" %in% names(input)) {
    lat <- input$lat
  }else {
    lat <- temp
    values(lat) <- 55
    names(lat) <- "lat"
  }
  
  if (!exists("temp") | is.null(temp))
    stop("temperature (temp) is missing!")
  if (!exists("prec") | is.null(prec))
    stop("precipitation (prec) is missing!")
  if (!!length(prec[prec < 0]))
    stop("precipiation (prec) cannot be negative!")
  if (!exists("ws") | is.null(ws))
    stop("wind speed (ws) is missing!")
  if (!!length(ws[ws < 0]))
    stop("wind speed (ws) cannot be negative!")
  if (!exists("rh") | is.null(rh))
    stop("relative humidity (rh) is missing!")
  if (!!length(rh[rh < 0]))
    stop("relative humidity (rh) cannot be negative!")
  
  names(init) <- tolower(names(init))
  
  #Assign values for initializing variables
  if (is.numeric(init)){
    if (is.null(names(init))){
      names(init)<-c('ffmc', 'dmc', 'dc')
    }
    ffmc_yda <- dmc_yda <- dc_yda <- temp
    values(ffmc_yda) <- init[['ffmc']]
    values(dmc_yda) <- init[['dmc']]
    values(dc_yda) <- init[['dc']]
  } else {
    ffmc_yda <- init$ffmc
    dmc_yda  <- init$dmc
    dc_yda   <- init$dc
  }
  names(ffmc_yda) <- "ffmc_yda"
  names(dmc_yda) <- "dmc_yda"
  names(dc_yda) <- "dc_yda"
  
  #constrain relative humidity
  rh[rh>=100]<- 99.9999
  ###########################################################################
  #                    Fine Fuel Moisture Code (FFMC)
  ###########################################################################
  
  ffmc = lapp(x = c( ffmc_yda, input[[c("temp","rh","ws","prec")]] ), 
              fun = Vectorize(FineFuelMoistureCode))
  
  ###########################################################################
  #                        Duff Moisture Code (DMC)
  ###########################################################################
  
  dmc = lapp(x = c( dmc_yda, input[[c("temp","rh","prec")]], lat),mon, 
             fun = Vectorize(DuffMoistureCode))
  
  ###########################################################################
  #                             Drought Code (DC)
  ###########################################################################
  
  dc = lapp(x = c( dc_yda, input[[c("temp","rh","prec")]], lat ),mon, 
            fun = Vectorize(DroughtCode))
  
  ###########################################################################
  #                    Initial Spread Index (ISI)
  ###########################################################################
  
  isi <- lapp(x = c(ffmc, input[["ws"]] ), 
              fun = Vectorize(InitialSpreadIndex))
  
  ###########################################################################
  #                       Buildup Index (BUI)
  ###########################################################################
  
  bui <- lapp(x = c( dmc, dc ),
              fun = Vectorize(BuildupEffect))
  
  ###########################################################################
  #                     Fire Weather Index (FWI)
  ###########################################################################
  
  fwi <- lapp(x = c( isi, bui ), 
              fun = Vectorize(FireWeatherIndex))
  
  ###########################################################################
  #                   Daily Severity Rating (DSR)
  ###########################################################################
  #Eq. 31
  dsr <- 0.0272 * (fwi^1.77)
  
  #If output specified is "fwi", then return only the FWI variables
  if (out == "fwi") {
    #Creating a raster c of FWI variables to return
    new_FWI <- c(ffmc, dmc, dc, isi, bui, fwi, dsr)
    names(new_FWI) <- c("ffmc", "dmc", "dc", "isi", "bui", "fwi", "dsr")
    if (uppercase){
      names(new_FWI) <- toupper(names(new_FWI))
    }
    #If output specified is "all", then return both FWI and input weather vars
  } else {
    if (out == "all") {
      #Create a raster c of input and FWI variables
      new_FWI <- c(input, ffmc, dmc, dc, isi, bui, fwi, dsr)
      names(new_FWI) <- c(names(input),"ffmc", "dmc", "dc", "isi", "bui", "fwi", "dsr")
      if (uppercase){
        names(new_FWI) <- toupper(names(new_FWI))
      }
    }
  }
  return(if(input_raster){raster::brick(new_FWI)}else{new_FWI})
}

