#' Sheltered Duff Moisture Code
#' 
#' \code{sdmc} is used to calculate sheltered DMC (sDMC, Wotton et al., 2005)
#' based on daily noon weather observations of temperature, relative humidity,
#' wind speed, 24-hour rainfall, and a previous day's calculated or estimated
#' value of sDMC. This function calculates sDMC for either one weather station
#' or for multiple weather stations over the duration of the daily weather data
#' set, typically over a fire season.
#' 
#' The Duff Moisture Code (DMC) component of the Canadian Forest Fire Weather
#' Index (FWI) System tracks moisture content of the forest floor away from the
#' sheltering influences of overstory trees.  This sheltered Duff Moisture Code
#' (sDMC) was developed to track moisture in the upper 5 cm of the organic
#' layer in the rain sheltered areas near (<0.5 m) the boles of overstory trees
#' (Wotton et al. 2005), an area where lightning strikes usually ignite the
#' forest floor when they run to ground. The sDMC is very similar in structure
#' (and identical in data requirements) to the DMC.  The sDMC, like all the FWI
#' System moisture codes, is a bookkeeping system that tracks gain and loss of
#' moisture from day-to-day; thus an estimate of the previous day's sDMC value
#' is needed to provide a starting point for each day's moisture calculation.
#' Like the other moisture codes in the FWI System the sDMC is converted from a
#' moisture content value to an outputted CODE value which increases in value
#' with decreasing moisture content.
#' 
#' @param input A data.frame containing input variables of daily noon weather
#' observations. Variable names have to be the same as in the following list,
#' but they are case insensitive. The order in which the input variables are
#' entered is not important either.
#' 
#' \tabular{lll}{ 
#' \var{temp} \tab (required) \tab Temperature (centigrade)\cr
#' \var{rh} \tab (required) \tab Relative humidity (\%)\cr 
#' \var{ws} \tab (required) \tab 10-m height wind speed (km/h)\cr 
#' \var{prec} \tab (required) \tab 1-hour rainfall (mm)\cr
#'\var{mon} \tab (recommended) \tab Month of the observations (integer 1-12)\cr 
#' \var{day} \tab (optional) \tab Day of the observations (integer)\cr }
#' @param sdmc_old Previous day's value of SDMC. At the start of calculations,
#' when there is no calculated previous day's SDMC value to use, the user must
#' specify an estimate of this value.  Where \code{sdmc_old=NULL}, the function
#' will calculate the initial SDMC values based on the initial DMC. The
#' \code{sdmc_old} argument can accept a single initial value for multiple
#' weather stations, and also accept a vector of initial values for multiple
#' weather stations.
#' @param batch Whether the computation is iterative or single step, default is
#' TRUE. When \code{batch=TRUE}, the function will calculate daily SDMC for one
#' weather station over a period of time iteratively. If multiple weather
#' stations are processed, an additional "id" column is required in the input
#' to label different stations, and the data needs to be sorted by date/time
#' and "id".  If \code{batch=FALSE}, the function calculates only one time step
#' base on either the previous day's SDMC or the initial start value.
#' @return \code{sdmc} returns either a single value or a vector of SDMC
#' values.
#' @author Xianli Wang, Mike Wotton, Alan Cantin, and Mike Flannigan
#' @seealso \code{\link{fwi}}
#' @references Wotton, B.M., B.J. Stocks, and D.L. Martell. 2005. An index for
#' tracking sheltered forest floor moisture within the Canadian Forest Fire
#' Weather Index System. International Journal of Wildland Fire, 14, 169-182.
#' @keywords methods
#' @examples
#' 
#' library(cffdrs)
#' data("test_sdmc")
#' #order the data:
#' test_sdmc<-test_sdmc[with(test_sdmc,order(yr,mon,day)),]
#' # (1)Default of sdmc, calculate sdmc for a chronical period
#' # of time.
#' # Because sdmc_old is better to be calculated, we normally
#' # ignore this option:
#' test_sdmc$SDMC<-sdmc(test_sdmc)
#' # (2) multiple weather stations:
#' # Batch process with multiple stations (2 stations) assuming
#' # they are from the same month:
#' test_sdmc$mon<-7
#' test_sdmc$day<-rep(1:24,2)
#' test_sdmc$id<-rep(1:2,each=24)
#' # Sort the data by date and weather station id:
#' test_sdmc<-test_sdmc[with(test_sdmc,order(yr,mon,day,id)),]
#' # Apply the function
#' test_sdmc$SDMC_mult_stn<-sdmc(test_sdmc,batch=TRUE)
#' # Assuming each record is from a different weather station, and
#' # calculate only one time step:
#'   foo<-sdmc(test_sdmc,batch=FALSE)
#' 
#' @export sdmc
sdmc <- function(input, sdmc_old = NULL, batch = TRUE){
  #############################################################################
  # Description: Calculate the sheltered Duff Moisture Code (sDMC) based on
  #              noon weather observations of temp, rh, 24-hour rain and
  #              previous day's sDMC. Equations being referenced are from
  #              Wotton et. al. (2005) or Van Wagner & Pickett (1985).
  #
  #              Wotton, B.M., B.J. Stocks, and D.L. Martell. 2005. An index for
  #              tracking sheltered forest floor moisture within the Canadian 
  #              Forest Fire Weather Index System. International Journal of 
  #              Wildland Fire, 14, 169-182.
  #
  #              Equations and FORTRAN program for the Canadian Forest Fire 
  #              Weather Index System. 1985. Van Wagner, C.E.; Pickett, T.L. 
  #              Canadian Forestry Service, Petawawa National Forestry 
  #              Institute, Chalk River, Ontario. Forestry Technical Report 33. 
  #              18 p.
  #     
  #  
  # Args:  
  #       input:  View Documentation (sdmc.Rd) for full description of input
  #               data frame
  #    sdmc_old:  previous day's calculated sDMC value
  #       batch:  Function can be run in a batch mode, where multiple 
  #               weather stations or points can be calculated at once. 
  #               (TRUE/FALSE, default TRUE)
  #       
  #
  # Returns: sdmc single or vector of SDMC value(s)
  #
  #############################################################################
  #Quite often users will have a data frame called "input" already attached
  #  to the workspace. To mitigate this, we remove that if it exists, and warn
  #  the user of this case.
  if (!is.na(charmatch("input", search()))) {
    detach(input)
  }
  names(input) <- tolower(names(input))
  #order dataset
  input<-input[with(input,order(mon,day)),]
  #enable batch mode
  if (batch){
    #if id is set, then multiple stations is correct
    if ("id" %in% names(input)) {
      input <- input[with(input, order(mon, day, id)), ]
      n <- length(unique(input$id))
      if(length(unique(input[1:n, "id"])) != n){
        stop("Multiple stations have to start and end at the same dates,and 
             input data must be sorted by date/time and id")
      }
    } else {
      n <- 1
    }
    #not batch mode
  } else {
    n <- nrow(input)
  }
  #set local scope variables
  temp <- input$temp
  prec <- input$prec
  rh <- input$rh
  mon<-input$mon
  dmc<-input$dmc
  #set some warnings if data not setup appropriately
  if (!exists("dmc") | is.null(dmc)) 
    warning("dmc is missing!")
  if (!exists("temp") | is.null(temp)) 
    warning("temperature (temp) is missing!")
  if (!exists("prec") | is.null(prec)) 
    warning("precipitation (prec) is missing!")
  if (!exists("rh") | is.null(rh)) 
    warning("relative humidity (rh) is missing!")
  #   if (!exists("mon") | is.null(mon)) 
  #     warning("month (mon) is missing!")
  if (length(temp)%%n != 0)
    warning("Input data do not match with number of weather stations")
  
  #Length of weather run
  n0 <- length(temp) %/% n
  SDMC <- NULL
  #loop through all elements
  for (i in 1:n0){
    #k is the data for all stations by day
    k <- (n * (i - 1) + 1):(n * i)
    SDMC0 <- ShelteredDuffMoistureCode(temp[k], prec[k], rh[k], dmc[k], mon[k], sdmc_old)
    SDMC <- c(SDMC, SDMC0)
    sdmc_old <- SDMC0
  }
  return(SDMC)
}
