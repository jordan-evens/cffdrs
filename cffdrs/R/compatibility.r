# provides backwards compatibility with old version of package that had all
# the code that is now in cffdrs.core
library(cffdrs.core)
#' @noRd
.BEcalc <- cffdrs.core::BuildupEffect

#' @noRd
.BROScalc <- cffdrs.core::BackRateOfSpread

#' @noRd
.buiCalc <- cffdrs.core::BuildupIndex

#' C-6 Conifer Plantation Fire Spread Calculator
#'
#' Calculate c6 (Conifer plantation) Fire Spread. C6 is a special case, and thus
#'  has it's own function. To calculate C6 fire spread, this function 
#'  also calculates and can return ROS, CFB, RSC, or RSI by specifying in 
#'  the option parameter.
#' All variables names are laid out in the same manner as Forestry Canada Fire 
#' Danger Group (FCFDG) (1992). Development and Structure of the Canadian Forest
#'  Fire Behavior Prediction System." Technical Report ST-X-3, Forestry Canada, 
#'  Ottawa, Ontario.
#' 
#' @references \url{https://cfs.nrcan.gc.ca/publications/download-pdf/10068} 
#' Development and Structure of the Canadian Forest Fire Behavior Prediction 
#' System." Technical Report ST-X-3, Forestry Canada, Ottawa, Ontario. 
#'
#' @param FUELTYPE    The Fire Behaviour Prediction FuelType
#' @param ISI         Initial Spread Index
#' @param BUI         Buildup Index
#' @param FMC         Foliar Moisture Content
#' @param SFC         Surface Fuel Consumption
#' @param CBH         Crown Base Height
#' @param ROS         Rate of Spread
#' @param CFB         Crown Fraction Burned
#' @param RSC         Crown Fire Spread Rate (m/min)
#' @param option      Which variable to calculate(ROS, CFB, RSC, or RSI) _Default:_ "CFB"
#' 
#' @return ROS, CFB, RSC or RSI depending on which option was selected
#' @noRd
.C6calc <- function(FUELTYPE, ISI, BUI, FMC, SFC, CBH, ROS, CFB, RSC, option="CFB")
{ 
  RSI <- cffdrs.core::IntermediateSurfaceRateOfSpreadC6(ISI, FMC)
  #Return at this point, if specified by caller
  if (option == "RSI") {
    return(RSI)
  }
  RSS <- cffdrs.core::SurfaceRateOfSpreadC6(FUELTYPE, RSI, BUI)
  RSC <- cffdrs.core::CrownRateOfSpreadC6(ISI)
  #Return at this point, if specified by caller
  if (option == "RSC") {
    return(RSC)
  }
  #Crown Fraction Burned
  CFB <- 0
  if (RSC > RSS)
  {
    CSI <- cffdrs.core::CriticalSurfaceIntensity(FMC, CBH)
    RSO <- cffdrs.core::CriticalSurfaceRateOfSpread(CSI, SFC)
    CFB <- cffdrs.core::CrownFractionBurned(RSS, RSO)
  }
  #Return at this point, if specified by caller
  if (option == "CFB") {
    return(CFB)
  }
  ROS <- cffdrs.core::RateOfSpreadC6(RSC, RSS, CFB)
  return(ROS)
}

#' @noRd
.CFBcalc <- function(FUELTYPE, FMC, SFC, ROS, CBH, option="CFB")
{
  CSI <- cffdrs.core::CriticalSurfaceIntensity(FUELTYPE, FMC, SFC, ROS, CBH)
  #Return at this point, if specified by caller
  if(option=="CSI"){
    return(CSI)
  }
  RSO <- cffdrs.core::CriticalSurfaceRateOfSpread(CSI, SFC)
  #Return at this point, if specified by caller
  if(option=="RSO"){
    return(RSO)
  }
  CFB <- cffdrs.core::CrownFractionBurned(ROS, RSO)
  return(CFB)
}


#' @noRd
.dcCalc <- cffdrs.core::DroughtCode

#' @noRd
.DISTtcalc <- cffdrs.core::DistanceAtTime

#' @noRd
.dmcCalc <- cffdrs.core::DuffMoistureCode

#' @noRd
.FBPcalc <- cffdrs.core::FireBehaviourPrediction

#' @noRd
.ffmcCalc <- cffdrs.core::FineFuelMoistureCode

#' @noRd
.FIcalc <- cffdrs.core::FireIntensity

#' @noRd
.FMCcalc <- cffdrs.core::FoliarMoistureContent

#' @noRd
.FROScalc <- cffdrs.core::FlankRateOfSpread

#' @noRd
.fwiCalc <- cffdrs.core::FireWeatherIndex

#' @noRd
.ISIcalc <- cffdrs.core::InitialSpreadIndex

#' @noRd
.LBcalc <- cffdrs.core::LengthToBreadthRatio

#' @noRd
.LBtcalc <- cffdrs.core::LengthToBreadthRatioAtTime

#' @noRd
.ROScalc <- cffdrs.core::RateOfSpread

#' @noRd
.ROStcalc <- cffdrs.core::RateOfSpreadAtTime

#' @noRd
.ROSthetacalc <- cffdrs.core::RateOfSpreadAtTheta

#' @noRd
.SFCcalc <- cffdrs.core::SurfaceFuelConsumption

#' Slope Adjusted wind speed or slope direction of spread calculation
#' 
#'   Calculate the net effective windspeed (WSV), the net effective wind 
#'   direction (RAZ) or the wind azimuth (WAZ).
#'
#'   All variables names are laid out in the same manner as FCFDG (1992) and
#'   Wotton (2009).
#'
#'   
#'   Forestry Canada Fire Danger Group (FCFDG) (1992). "Development and 
#'   Structure of the Canadian Forest Fire Behavior Prediction System." 
#'   Technical Report ST-X-3, Forestry Canada, Ottawa, Ontario.
#'
#'   Wotton, B.M., Alexander, M.E., Taylor, S.W. 2009. Updates and revisions to
#'   the 1992 Canadian forest fire behavior prediction system. Nat. Resour. 
#'   Can., Can. For. Serv., Great Lakes For. Cent., Sault Ste. Marie, Ontario, 
#'   Canada. Information Report GLC-X-10, 45p.
#'
#' @param FUELTYPE  The Fire Behaviour Prediction FuelType
#' @param FMC       Fine Fuel Moisture Code
#' @param BUI       The Buildup Index value
#' @param WS        Windspeed (km/h)
#' @param WAZ       Wind Azimuth
#' @param GS        Ground Slope (%)
#' @param SAZ       Slope Azimuth
#' @param FMC       Foliar Moisture Content
#' @param SFC       Surface Fuel Consumption (kg/m^2)
#' @param PC        Percent Conifer (%)
#' @param PDF       Percent Dead Balsam Fir (%)
#' @param CC        Constant
#' @param CBH       Crown Base Height (m)
#' @param ISI       Initial Spread Index
#' @param output    Type of variable to output (RAZ/WSV, default=RAZ)
#' 
#' @returns  RAZ or WSV - Rate of spread azimuth (degrees) or Wind Slope speed (km/hr)
#' @noRd
.Slopecalc <- function(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
                        CC, CBH, ISI, output = "RAZ")
{
  # output options include: RAZ and WSV
  
  #check for valid output types
  validOutTypes = c("RAZ", "WAZ", "WSV")
  if(!(output %in% validOutTypes)){
    stop(paste("In 'SlopeAdjust()', '",output, "' is an invalid 'output' type.", 
               sep=""))
  }
  result <- cffdrs.core::SlopeAdjust(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC,
                                     PC, PDF, CC, CBH, ISI)
  if (output=="WSV")
  {
    return(result$WSV)
  }
  return(result$RAZ)
  
}

#' Total Fuel Consumption calculation
#' 
#'   Computes the Total (Surface + Crown) Fuel Consumption by Fuel Type.
#'   All variables names are laid out in the same manner as FCFDG (1992) or
#'   Wotton et. al (2009) 
#'   
#'   Forestry Canada Fire Danger Group (FCFDG) (1992). "Development and 
#'   Structure of the Canadian Forest Fire Behavior Prediction System." 
#'   Technical Report ST-X-3, Forestry Canada, Ottawa, Ontario.
#'
#'   Wotton, B.M., Alexander, M.E., Taylor, S.W. 2009. Updates and revisions to
#'   the 1992 Canadian forest fire behavior prediction system. Nat. Resour. 
#'   Can., Can. For. Serv., Great Lakes For. Cent., Sault Ste. Marie, Ontario, 
#'   Canada. Information Report GLC-X-10, 45p.
#'
#' @param FUELTYPE The Fire Behaviour Prediction FuelType
#' @param CFL      Crown Fuel Load (kg/m^2)
#' @param CFB      Crown Fraction Burned (0-1)
#' @param SFC      Surface Fuel Consumption (kg/m^2)
#' @param  PC      Percent Conifer (%)
#' @param PDF      Percent Dead Balsam Fir (%)
#' @param option   Type of output (TFC, CFC, default=TFC)
#' 
#' @returns TFC Total (Surface + Crown) Fuel Consumption (kg/m^2) OR
#' CFC Crown Fuel Consumption (kg/m^2)
#' 
#' @noRd
.TFCcalc <- function(FUELTYPE, CFL, CFB, SFC, PC, PDF, option = "TFC")
{
  CFC <- cffdrs.core::CrownFuelConsumption(FUELTYPE, CFL, CFB, PC, PDF)
  #Return CFC if requested
  if (option=="CFC")
    return(CFC)
  TFC <- cffdrs.core::TotalFuelConsumption(CFC, SFC)
  return(TFC)
}





#' Fire Behaviour Prediction Sample Data Set
#' 
#' This data set is a set of input data for each of the test cases in the
#' publication supplied below.
#' 
#' 
#' @name test_fbp
#' @docType data
#' @format A data frame containing 24 columns, 21 rows, including 1 header line
#' @references 1. Wotton, B.M., Alexander, M.E., Taylor, S.W. 2009. Updates and
#' revisions to the 1992 Canadian forest fire behavior prediction system. Nat.
#' Resour. Can., Can. For. Serv., Great Lakes For. Cent., Sault Ste. Marie,
#' Ontario, Canada. Information Report GLC-X-10, 45p.
#' @source \url{http://cfs.nrcan.gc.ca/pubwarehouse/pdfs/31414.pdf}
#' @keywords datasets
#' @export
test_fbp <- cffdrs.core::test_fbp





#' Fire Weather Index Sample Input Data Set
#' 
#' This data set is the sample input data that was used in original FWI program
#' calibration.
#' 
#' 
#' @name test_fwi
#' @docType data
#' @format A data frame containing 9 columns and 49 rows, with 1 header line
#' @references 1. Van Wagner, CE. and T.L. Pickett. 1985. Equations and FORTRAN
#' program for the Canadian Forest Fire Weather Index System. Can. For. Serv.,
#' Ottawa, Ont. For. Tech. Rep. 33. 18 p.
#' @source \url{http://cfs.nrcan.gc.ca/pubwarehouse/pdfs/19973.pdf}
#' @keywords datasets
#' @export
test_fwi <- cffdrs.core::test_fwi

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
sdmc <- cffdrs.core:::sdmc