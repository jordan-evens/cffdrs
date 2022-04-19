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
  RSS <- cffdrs.core::SurfaceRateOfSpreadC6(RSI, BUI)
  RSC <- cffdrs.core::CrownRateOfSpreadC6(ISI, FMC)
  #Return at this point, if specified by caller
  if (option == "RSC") {
    return(RSC)
  }
  #Crown Fraction Burned
  CFB <- 0
  if (RSC > RSS)
  {
    CSI <- cffdrs.core::CriticalSurfaceIntensity(FUELTYPE, FMC, CBH)
    RSO <- cffdrs.core::CriticalSurfaceRateOfSpread(CSI, SFC)
    CFB <- cffdrs.core::CrownFractionBurned(FUELTYPE, RSS, RSO)
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
  CSI <- cffdrs.core::CriticalSurfaceIntensity(FUELTYPE, FMC, CBH)
  #Return at this point, if specified by caller
  if(option=="CSI"){
    return(CSI)
  }
  RSO <- cffdrs.core::CriticalSurfaceRateOfSpread(CSI, SFC)
  #Return at this point, if specified by caller
  if(option=="RSO"){
    return(RSO)
  }
  CFB <- cffdrs.core::CrownFractionBurned(FUELTYPE, ROS, RSO)
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

#' Hourly Fine Fuel Moisture Code
#' 
#' \code{hffmc} is used to calculate hourly Fine Fuel Moisture Code (FFMC) and
#' is based on a calculation routine first described in detail by Van Wagner
#' (1977) and which has been updated in minor ways by the Canadian Forest
#' Service to have it agree with the calculation methodology for the daily FFMC
#' (see \code{\link{fwi}}).  In its simplest typical use this current routine
#' calculates a value of FFMC based on a series of uninterrupted hourly weather
#' observations of screen level (~1.4 m) temperature, relative humidity, 10 m
#' wind speed, and 1-hour rainfall. This implementation of the function
#' includes an optional time.step input which is defaulted to one hour, but can
#' be reduced if sub-hourly calculation of the code is needed.  The FFMC is in
#' essence a bookkeeping system for moisture content and thus it needs to use
#' the last time.step's value of FFMC in its calculation as well.  This
#' function could be used for either one weather station or for multiple
#' weather stations.
#' 
#' The hourly FFMC is very similar in its structure and calculation to the
#' Canadian Forest Fire Weather Index System's daily FFMC (\code{\link{fwi}})
#' but has an altered drying and wetting rate which more realistically reflects
#' the drying and wetting of a pine needle litter layer sitting on a decaying
#' organic layer.  This particular implementation of the Canadian Forest Fire
#' Danger Rating System's hourly FFMC provides for a flexible time step; that
#' is, the data need not necessarily be in time increments of one hour.  This
#' flexibility has been added for some users who use this method with data
#' sampled more frequently that one hour.  We do not recommend using a time
#' step much greater than one hour. An important and implicit assumption in
#' this calculation is that the input weather is constant over the time step of
#' each calculation (e.g., typically over the previous hour).  This is a
#' reasonable assumption for an hour; however it can become problematic for
#' longer periods.  For brevity we have referred to this routine throughout
#' this description as the hourly FFMC.
#' 
#' Because of the shortened time step, which can lead to more frequent
#' calculations and conversion between moisture content and the code value
#' itself, we have increased the precision of one of the constants in the
#' simple formula that converts litter moisture content to the 'Code' value.
#' This is necessary to avoid a potential bias that gets introduced during
#' extremely dry conditions.  This is simply a change in the precision at which
#' this constant is used in the equation and is not a change to the standard
#' FFMC conversion between moisture and code value (which is referred to as the
#' FF-scale).
#' 
#' The calculation requires the previous hour's FFMC as an input to the
#' calculation of the current hour's FFMC; this is because the routine can be
#' thought of as a bookkeeping system and needs to know the amount of moisture
#' being held in the fuel prior to any drying or wetting in the current period.
#' After each hour's calculation that newly calculated FFMC simply becomes the
#' starting FFMC in the next hour's calculation.  At the beginning of the
#' calculations at a station this previous hours FFMC must be estimated. It is
#' typical to use a value of 85 when this value cannot be estimated more
#' accurately; this code value corresponds to a moisture content of about 16\%
#' in typical pine litter fuels.
#' 
#' @param weatherstream A dataframe containing input variables of hourly
#' weather observations. It is important that variable names have to be the
#' same as in the following list, but they are case insensitive. The order in
#' which the input variables are entered is not important.
#' 
#' \tabular{lll}{ 
#' \var{temp} \tab (required) \tab Temperature (centigrade)\cr
#' \var{rh} \tab (required) \tab Relative humidity (\%)\cr 
#' \var{ws} \tab (required) \tab 10-m height wind speed (km/h)\cr 
#' \var{prec} \tab (required) \tab 1-hour rainfall (mm)\cr 
#' \var{hr} \tab (optional) \tab Hourly value to calculate sub-hourly ffmc \cr 
#' \var{bui} \tab (optional) \tab Daily BUI value for the computation of hourly 
#' FWI. It is required when \code{hourlyFWI=TRUE}.\cr } 
#' Typically this dataframe also contains date and
#' hour fields so outputs can be associated with a specific day and time,
#' however these fields are not used in the calculations.  If multiple weather
#' stations are being used, a weather station ID field is typically included as
#' well, though this is simply for bookkeeping purposes and does not affect the
#' calculation.
#' @param ffmc_old Initial FFMC. At the start of calculations at a particular
#' station there is a need to provide an estimate of the FFMC in the previous
#' timestep; this is because the FFMC is, in essence, a bookkeeping system for
#' moisture.  If no estimate of previous hour's FFMC is available the function
#' will use default value, \code{ffmc_old=85}. When using the routine to
#' calculate hourly FFMC at multiple stations the \code{ffmc_old} argument can
#' also accept a vector with the same number of weather stations.
#' @param time.step Is the time (in hours) between the previous value of FFMC
#' and the current time at which we want to calculate a new value of the FFMC.
#' When not specified it will take on a default value of \code{time.step=1}.
#' @param calc.step Optional for whether time step between two observations is
#' calculated. Default is FALSE, no calculations. This is used when time
#' intervals are not uniform in the input.
#' @param batch Whether the computation is iterative or single step, default is
#' TRUE. When \code{batch=TRUE}, the function will calculate hourly or
#' sub-hourly FFMC for one weather station over a period of time iteratively.
#' If multiple weather stations are processed, an additional "id" column is
#' required in the input weatherstream to label different stations, and the
#' data needs to be sorted by date/time and "id".  If \code{batch=FALSE}, the
#' function calculates only one time step base on either the previous hourly
#' FFMC or the initial start value.
#' @param hourlyFWI Optional for the computation of hourly ISI, FWI, and DSR.
#' Default is FALSE. While \code{hourlyFWI=TRUE}, daily BUI is required for the
#' computation of FWI.
#' @return \code{hffmc} returns a vector of hourly or sub-hourly FFMC values,
#' which may contain 1 or multiple elements. Optionally when
#' \code{hourlyFWI=TRUE}, the function also output a data.frame contains input
#' weatherstream as well as the hourly or sub-hourly FFMC, ISI, FWI, and DSR.
#' @author Xianli Wang, Mike Wotton, Alan Cantin, Brett Moore, and Mike
#' Flannigan
#' @seealso \code{\link{fbp}}, \code{\link{fwi}}, \code{\link{hffmcRaster}}
#' @references Van Wagner, C.E. 1977. A method of computing fine fuel moisture
#' content throughout the diurnal cycle. Environment Canada, Canadian Forestry
#' Service, Petawawa Forest Experiment Station, Chalk River, Ontario.
#' Information Report PS-X-69.
#' \url{http://cfs.nrcan.gc.ca/pubwarehouse/pdfs/25591.pdf}
#' @keywords methods
#' @examples
#' 
#' library(cffdrs)
#' data("test_hffmc")
#' # show the data format:
#' head(test_hffmc)
#' # (1)hffmc default: 
#' # Re-order the data by year, month, day, and hour:
#' test_hffmc<-test_hffmc[with(test_hffmc, order(yr,mon,day,hr)),]
#' # Because the test data has 24 hours input variables 
#' # it is possible to calculate the hourly FFMC chronically 
#' # through multiple days(with the default initial ffmc_old=85):
#' test_hffmc$ffmc_default<-hffmc(test_hffmc) 
#' # (2) Calculate FFMC for multiple stations:
#' # Calculate hourly FFMC with only one initial 
#' # value (ffmc_old=85), but multiple weather stations. 
#' # Sort the input by date/time and the station id:
#' test_hffmc<-test_hffmc[with(test_hffmc,order(yr,mon,hr)),]
#' # Add weather station id:
#' test_hffmc$id<-rep(1:10,nrow(test_hffmc)/10)
#' #check the data:
#' head(test_hffmc)
#' test_hffmc$ffmc01<-hffmc(test_hffmc,batch=TRUE)
#' # With multiple initial FFMC (ffmc_old) as a vector: 
#' test_hffmc$ffmc02<- hffmc(test_hffmc,ffmc_old = sample(70:100,10, replace=TRUE),batch=TRUE)
#' # One time step assuming all records are from different 
#' # weather stations: 
#'      foo<-hffmc(test_hffmc,batch=FALSE)
#' # (3) output all hourly FWI System variables:
#' test_hffmc$id<-NULL
#' test_hffmc<-test_hffmc[with(test_hffmc,    order(yr,mon,day,hr)),]
#' foo<-hffmc(test_hffmc,hourlyFWI=TRUE)
#' # this will not run: warning message requesting for daily BUI
#' test_hffmc$bui<-100
#' foo<-hffmc(test_hffmc,hourlyFWI=TRUE)
#' # (4) Calculate time steps in case the time intervals are 
#' # not uniform:
#' dat0<-test_hffmc[sample(1:30,20),]
#' dat0<-dat0[with(dat0,order(yr,mon,day,hr)),]
#' # with or without calc.step, hffmc is going to generate
#' # different FFMC values.
#' # without calculating time step (default):
#' hffmc(dat0,time.step=1)
#' # with calc.step=TRUE, time.step=1 is applied to 
#' # only the first record, the rests would be calculated:
#' hffmc(dat0,time.step=1,calc.step=TRUE)
#' 
#' @export hffmc
hffmc <- cffdrs.core:::hffmc

#' Overwintering Drought Code
#'
#' \code{wDC} calculates an initial or season starting Drought Code (DC) value
#' based on a standard method of overwintering the Drought Code (Lawson and
#' Armitage 2008).  This method uses the final DC value from previous year,
#' over winter precipitation and estimates of how much over-winter
#' precipitation 'refills' the moisture in this fuel layer. This function could
#' be used for either one weather station or for multiple weather stations.
#'
#' Of the three fuel moisture codes (i.e. FFMC, DMC and DC) making up the FWI
#' System, only the DC needs to be considered in terms of its values carrying
#' over from one fire season to the next.  In Canada both the FFMC and the DMC
#' are assumed to reach moisture saturation from overwinter precipitation at or
#' before spring melt; this is a reasonable assumption and any error in these
#' assumed starting conditions quickly disappears.  If snowfall (or other
#' overwinter precipitation) is not large enough however, the fuel layer
#' tracked by the Drought Code may not fully reach saturation after spring snow
#' melt; because of the long response time in this fuel layer (53 days in
#' standard conditions) a large error in this spring starting condition can
#' affect the DC for a significant portion of the fire season.  In areas where
#' overwinter precipitation is 200 mm or more, full moisture recharge occurs
#' and DC overwintering is usually unnecessary.  More discussion of
#' overwintering and fuel drying time lag can be found in Lawson and Armitage
#' (2008) and Van Wagner (1985).
#'
#' @param DCf Final fall DC value from previous year
#' @param rw Winter precipitation (mm)
#' @param a User selected values accounting for carry-over fraction (view table
#' below)
#' @param b User selected values accountain for wetting efficiency fraction
#' (view table below)
#' @return \code{wDC} returns either a single value or a vector of wDC values.
#' @author Xianli Wang, Mike Wotton, Alan Cantin, and Mike Flannigan
#' @seealso \code{\link{fwi}}, \code{\link{fireSeason}}
#' @references Lawson B.D. and Armitage O.B. 2008. Weather Guide for the
#' Canadian Forest Fire Danger Rating System. Natural Resources Canada,
#' Canadian Forest Service, Northern Forestry Centre, Edmonton, Alberta. 84 p.
#' \url{http://cfs.nrcan.gc.ca/pubwarehouse/pdfs/29152.pdf}
#'
#' Van Wagner, C.E. 1985. Drought, timelag and fire danger rating. Pages
#' 178-185 in L.R. Donoghue and R.E. Martin, eds. Proc. 8th Conf. Fire For.
#' Meteorol., 29 Apr.-3 May 1985, Detroit, MI. Soc. Am. For., Bethesda, MD.
#' \url{http://cfs.nrcan.gc.ca/pubwarehouse/pdfs/23550.pdf}
#' @keywords methods
#' @examples
#'
#' library(cffdrs)
#' # The standard test data:
#' data("test_wDC")
#' # (1) Simple case previous fall's DC was 300, overwinter
#' # rain 110mm
#' winter_DC <- wDC(DCf=300,rw=110)
#' winter_DC
#' #(2) modified a and b parameters. Find table values in listed
#' # reference for Lawson and Armitage, 2008.
#' winter_DC <- wDC(DCf=300,rw=110,a=1.0,b=0.9)
#' winter_DC
#' #(3)with multiple inputs:
#' winter_DC <- wDC(DCf=c(400,300,250), rw=c(99,110,200),
#'                    a=c(0.75,1.0,0.75), b=c(0.75,0.9,0.75))
#' winter_DC
#' #(4) A realistic example:
#' #precipitation accumulation and date boundaries
#' input <- test_wDC
#' #order data by ID and date
#' input <- with(input,input[order(id,yr,mon,day),])
#' input$date <- as.Date(as.POSIXlt(paste(input$yr,"-",input$mon,"-",input$day,sep="")))
#' #select id value 1
#' input.2 <- input[input$id==2,]
#' #Explicitly defined fire start and end dates.
#' data("test_wDC_fs")
#' print(test_wDC_fs)
#' #Set date field
#' test_wDC_fs$date <- as.Date(as.POSIXlt(paste(test_wDC_fs$yr,"-",test_wDC_fs$mon,"-",
#'                                              test_wDC_fs$day,sep="")))
#' #match to current id value
#' input.2.fs <- test_wDC_fs[test_wDC_fs$id==2,]
#' #assign start of winter date (or end of fire season date)
#' winterStartDate <- input.2.fs[2,"date"]
#' #assign end of winter date (or start of new fire season date)
#' winterEndDate <-  input.2.fs[3,"date"]
#' #Accumulate overwinter precip based on chosen dates
#' curYr.prec <- sum(input.2[(input.2$date>winterStartDate & input.2$date < winterEndDate),]$prec)
#' #Assign a fall DC value
#' fallDC <- 500
#' #calculate winter DC
#' winter_DC <- wDC(DCf=fallDC,rw=curYr.prec)
#' winter_DC
#' #Assign a different fall DC value
#' fallDC <- 250
#' #calculate winter DC
#' winter_DC <- wDC(DCf=fallDC,rw=curYr.prec,a=1.0)
#' winter_DC
#'
#' @export wDC
wDC <- cffdrs.core:::OverwinterDroughtCode
data("test_wDC", package="cffdrs.core")
data("test_wDC_fs", package="cffdrs.core")

#' Grass Fuel Moisture Code
#' 
#' \code{gfmc} calculates both the moisture content of the surface of a fully
#' cured matted grass layer and also an equivalent Grass Fuel Moisture Code
#' (gfmc) (Wotton, 2009) to create a parallel with the hourly ffmc (see the
#' \code{\link{fwi}} and \code{\link{hffmc}}functions). The calculation is
#' based on hourly (or sub-hourly) weather observations of temperature,
#' relative humidity, wind speed, rainfall, and solar radiation. The user must
#' also estimate an initial value of the gfmc for the layer. This function
#' could be used for either one weather station or multiple weather stations.
#' 
#' The Canadian Forest Fire Danger Rating System (CFFDRS) is used throughout
#' Canada, and in a number of countries throughout the world, for estimating
#' fire potential in wildland fuels. This new Grass Fuel Moisture Code (GFMC)
#' is an addition (Wotton 2009) to the CFFDRS and retains the structure of that
#' System's hourly Fine Fuel Moisture Code (HFFMC) (Van Wagner 1977). It tracks
#' moisture content in the top 5 cm of a fully-cured and fully-matted layer of
#' grass and thus is representative of typical after winter conditions in areas
#' that receive snowfall.  This new moisture calculation method outputs both
#' the actual moisture content of the layer and also the transformed moisture
#' Code value using the FFMC's FF-scale.  In the CFFDRS the moisture codes are
#' in fact relatively simple transformations of actual moisture content such
#' that decreasing moisture content (increasing dryness) is indicated by an
#' increasing Code value. This moisture calculation uses the same input weather
#' observations as the hourly FFMC, but also requires an estimate of solar
#' radiation incident on the fuel.
#' 
#' @param input A dataframe containing input variables of daily noon weather
#' observations. Variable names have to be the same as in the following list,
#' but they are case insensitive. The order in which the input variables are
#' entered is not important.
#' 
#' \tabular{lll}{ 
#' \var{temp} \tab (required) \tab Temperature (centigrade)\cr
#' \var{rh} \tab (required) \tab Relative humidity (\%)\cr 
#' \var{ws} \tab (required) \tab 10-m height wind speed (km/h)\cr 
#' \var{prec} \tab (required) \tab 1-hour rainfall (mm)\cr
#' \var{isol} \tab (required) \tab Solar radiation (kW/m^2)\cr 
#' \var{mon} \tab (recommended) \tab Month of the year (integer' 1-12)\cr 
#' \var{day} \tab (optional) \tab Day of the month (integer)\cr }
#' @param GFMCold Previous value of GFMC (i.e. value calculated at the previous
#' time step)[default is 85 (which corresponds to a moisture content of about
#' 16\%)]. On the first calculation this is the estimate of the GFMC value at
#' the start of the time step. The \code{GFMCold} argument can accept a single
#' initial value for multiple weather stations, and also accept a vector of
#' initial values for multiple weather stations.  NOTE: this input represents
#' the CODE value, not a direct moisture content value. The CODE values in the
#' Canadian FWI System increase within decreasing moisture content. To roughly
#' convert a moisture content value to a CODE value on the FF-scale (used in
#' the FWI Systems FFMC) use \code{GFMCold} =101-gmc (where gmc is moisture
#' content in \%)
#' 
#' @param time.step Time step (hour) [default 1 hour]
#' @param roFL The nominal fuel load of the fine fuel layer, default is 0.3
#' kg/m^2
#' @param batch Whether the computation is iterative or single step, default is
#' TRUE. When \code{batch=TRUE}, the function will calculate hourly or
#' sub-hourly GFMC for one weather station over a period of time iteratively.
#' If multiple weather stations are processed, an additional "id" column is
#' required in the input to label different stations, and the data needs to be
#' sorted by time sequence and "id".  If \code{batch=FALSE}, the function
#' calculates only one time step (1 hour) base on either the previous hourly
#' GFMC or the initial start value.
#' @param out Output format, default is "GFMCandMC", which contains both GFMC
#' and moisture content (MC) in a data.frame format. Other choices include:
#' "GFMC", "MC", and "ALL", which include both the input and GFMC and MC.
#' @return \code{gfmc} returns GFMC and moisture content (MC) values
#' collectively (default) or separately.
#' @author Xianli Wang, Mike Wotton, Alan Cantin, and Mike Flannigan
#' @seealso \code{\link{fwi}}, \code{\link{hffmc}}
#' @references Wotton, B.M. 2009. A grass moisture model for the Canadian
#' Forest Fire Danger Rating System. In: Proceedings 8th Fire and Forest
#' Meteorology Symposium, Kalispell, MT Oct 13-15, 2009. Paper 3-2.
#' \url{https://ams.confex.com/ams/pdfpapers/155930.pdf}
#' 
#' Van Wagner, C.E. 1977. A method of computing fine fuel moisture content
#' throughout the diurnal cycle. Environment Canada, Canadian Forestry Service,
#' Petawawa Forest Experiment Station, Chalk River, Ontario. Information Report
#' PS-X-69. \url{http://cfs.nrcan.gc.ca/pubwarehouse/pdfs/25591.pdf}
#' @keywords methods
#' @examples
#' 
#' library(cffdrs)
#' #load the test data
#' data("test_gfmc")
#' # show the data format:
#' head(test_gfmc)
#' #     yr mon day hr temp   rh   ws prec  isol
#' # 1 2006   5  17 10 15.8 54.6  5.0    0 0.340
#' # 2 2006   5  17 11 16.3 52.9  5.0    0 0.380
#' # 3 2006   5  17 12 18.8 45.1  5.0    0 0.626
#' # 4 2006   5  17 13 20.4 40.8  9.5    0 0.656
#' # 5 2006   5  17 14 20.1 41.7  8.7    0 0.657
#' # 6 2006   5  17 15 18.6 45.8 13.5    0 0.629
#' # (1) gfmc default: 
#' # Re-order the data by year, month, day, and hour:
#' dat<-test_gfmc[with(test_gfmc,order(yr,mon,day,hr)),]
#' # Because the test data has 24 hours input variables 
#' # it is possible to calculate the hourly GFMC continuously 
#' # through multiple days(with the default initial GFMCold=85):
#' dat$gfmc_default<-gfmc(dat) 
#' # two variables will be added to the input, GFMC and MC
#' head(dat)
#' # (2) For multiple weather stations:
#' # One time step (1 hour) with default initial value:
#'   foo<-gfmc(dat,batch=FALSE)
#' # Chronical hourly GFMC with only one initial 
#' # value (GFMCold=85), but multiple weather stations. 
#' # Note: data is ordered by date/time and the station id. Subset 
#' # the data by keeping only the first 10 hours of observations 
#' # each day:
#' dat1<-subset(dat,hr%in%c(0:9))
#' #assuming observations were from the same day but with 
#' #9 different weather stations:
#' dat1$day<-NULL
#' dat1<-dat1[with(dat1,order(yr,mon,hr)),]
#' dat1$id<-rep(1:8,nrow(dat1)/8)
#' #check the data:
#' head(dat1)
#' # Calculate GFMC for multiple stations:
#' dat1$gfmc01<-gfmc(dat1,batch=TRUE)
#' # We can provide multiple initial GFMC (GFMCold) as a vector:   
#' dat1$gfmc02<- gfmc(dat1,GFMCold = sample(70:100,8, replace=TRUE),batch=TRUE)
#' # (3)output argument
#' ## include all inputs and outputs:
#' dat0<-dat[with(dat,order(yr,mon,day,hr)),]
#' foo<-gfmc(dat,out="ALL")
#' ## subhourly time step:
#' gfmc(dat0,time.step=1.5)
#' 
#' @export gfmc
gfmc <- cffdrs.core::gfmc

#' Fire Season Start and End
#' 
#' \code{\link{fireSeason}} calculates the start and end fire season dates for
#' a given weather station. The current method used in the function is based on
#' three consecutive daily maximum temperature thresholds (Wotton and Flannigan
#' 1993, Lawson and Armitage 2008). This function process input from a single
#' weather station.
#' 
#' An important aspect to consider when calculating Fire Weather Index (FWI)
#' System variables is a definition of the fire season start and end dates
#' (Lawson and Armitage 2008). If a user starts calculations on a fire season
#' too late in the year, the FWI System variables may take too long to reach
#' equilibrium, thus throwing off the resulting indices. This function presents
#' two method of calculating these start and end dates, adapted from Wotton and
#' Flannigan (1993), and Lawson and Armitage (2008). The approach taken in this
#' function starts the fire season after three days of maximum temperature
#' greater than 12 degrees Celsius. The end of the fire season is determined
#' after three consecutive days of maximum temperature less than 5 degrees
#' Celsius.  The two temperature thresholds can be adjusted as parameters in
#' the function call. In regions where temperature thresholds will not end a
#' fire season, it is possible for the fire season to span multiple years, in
#' this case setting the multi.year parameter to TRUE will allow these
#' calculations to proceed.
#' 
#' This fire season length definition can also feed in to the overwinter DC
#' calculations (\link{wDC}). View the cffdrs package help files for an example
#' of using the \code{fireSeason}, \link{wDC}, and \link{fwi} functions in
#' conjunction.
#' 
#' @param input A data.frame containing input variables of including the
#' date/time and daily maximum temperature. Variable names have to be the same
#' as in the following list, but they are case insensitive. The order in which
#' the input variables are entered is not important either.
#' 
#' \tabular{lll}{ 
#' \var{yr} \tab (required) \tab Year of the observations\cr
#' \var{mon} \tab (required) \tab Month of the observations\cr 
#' \var{day} \tab (required) \tab Day of the observations\cr 
#' \var{tmax} \tab (required) \tab Maximum Daily Temperature (degrees C)\cr 
#' \var{snow_depth} \tab (optional) \tab Is consistent snow data in the input?\cr }.
#' 
#' @param fs.start Temperature threshold (degrees C) to start the fire season
#' (default=12)
#' @param fs.end Temperature threshold (degrees C) to end the fire season
#' (default=5)
#' @param method Method of fire season calculation. Options are "wf93"" or
#' "la08" (default=WF93)
#' @param consistent.snow Is consistent snow data in the input? (default=FALSE)
#' @param multi.year Should the fire season span multiple years?
#' (default=FALSE)
#' @return \link{fireSeason} returns a data frame of season and start and end
#' dates. Columns in data frame are described below.
#' 
#' Primary FBP output includes the following 8 variables: 
#' \item{yr }{Year of the fire season start/end date} 
#' \item{mon }{Month of the fire season start/end date} 
#' \item{day }{Day of the fire season start/end date}
#' \item{fsdatetype }{Fire season date type (values are either "start" or "end")} 
#' \item{date}{Full date value}
#' 
#' @author Alan Cantin, Xianli Wang, Mike Wotton, and Mike Flannigan
#' 
#' @seealso \code{\link{fwi}, \link{wDC}}
#' 
#' @references Wotton, B.M. and Flannigan, M.D. (1993). Length of the fire
#' season in a changing climate. Forestry Chronicle, 69, 187-192.
#' 
#' \url{http://www.ualberta.ca/~flanniga/publications/1993_Wotton_Flannigan.pdf}
#' 
#' Lawson, B.D. and O.B. Armitage. 2008. Weather guide for the Canadian Forest
#' Fire Danger Rating System. Nat. Resour. Can., Can. For. Serv., North. For.
#' Cent., Edmonton, AB \url{http://cfs.nrcan.gc.ca/pubwarehouse/pdfs/29152.pdf}
#' @keywords methods
#' @examples
#' 
#' library(cffdrs)
#' #The standard test data:
#' data("test_wDC")
#' print(head(test_wDC))
#' ## Sort the data:
#' input <- with(test_wDC, test_wDC[order(id,yr,mon,day),])
#' 
#' #Using the default fire season start and end temperature 
#' #thresholds:
#' a_fs <- fireSeason(input[input$id==1,])
#' 
#' #Check the result:
#' a_fs
#' 
#' #    yr mon day fsdatetype
#' #1 1999   5   4      start
#' #2 1999   5  12        end
#' #3 1999   5  18      start
#' #4 1999   5  25        end
#' #5 1999   5  30      start
#' #6 1999  10   6        end
#' #7 2000   6  27      start
#' #8 2000  10   7        end
#' 
#' #In the resulting data frame, the fire season starts 
#' #and ends multiple times in the first year. It is up to the user #for how to interpret this.
#' 
#' #modified fire season start and end temperature thresholds
#' a_fs <- fireSeason (input[input$id==1,],fs.start=10, fs.end=3)
#' a_fs
#' #    yr mon day fsdatetype
#' #1 1999   5   2      start
#' #2 1999  10  20        end
#' #3 2000   6  16      start
#' #4 2000  10   7        end
#' #select another id value, specify method explicitly
#' b_fs <- fireSeason(input[input$id==2,],method="WF93")
#' #print the calculated fireseason
#' b_fs
#' #   yr mon day fsdatetype
#' #1 1980   4  21      start
#' #2 1980   9  19        end
#' #3 1980  10   6      start
#' #4 1980  10  16        end
#' #5 1981   5  21      start
#' #6 1981  10  13        end
#' 
#' @export fireSeason
fireSeason <- cffdrs.core:::FireSeason


#' Point-based input for Simard Rate of Spread and Direction
#' 
#' \code{pros} is used to calculate the rate of spread and direction given one
#' set of three point-based observations of fire arrival time. The function
#' requires that the user specify the time that the fire crossed each point,
#' along with the latitude and longitude of each observational point. This
#' function allows quick input of a dataframe specifying one or many triangles.
#' 
#' \code{pros} allows R users to calculate the rate of spread and direction of
#' a fire across a triangle, given three time measurements and details about
#' the orientation and distance between observational points. The algorithm is
#' based on the description from Simard et al. (1984).
#' 
#' Rate of spread and direction of spread are primary variables of interest
#' when observing wildfire growth over time. Observations might be recorded
#' during normal fire management operations (e.g., by a Fire Behaviour
#' Analyst), during prescribed fire treatments, and during experimental
#' research burns. Rate of spread is especially important for estimating
#' Byram's fireline intensity, fireline intensity = heat constant of fuel ×
#' weight of fuel consumed × forward rate of spread (Byram 1959).
#' 
#' Rate of spread is difficult to measure and highly variable in the field.
#' Many techniques were proposed over the years, but most were based on
#' observations collected from a pre-placed reference grid and stopwatch (Curry
#' and Fons 1938; Simard et al. 1982). Early approaches required that observers
#' be in visual contact with the reference grid, but later, thermocouples and
#' dataloggers were employed to measure the onset of the heat pulse at each
#' point.
#' 
#' Simard et al. (1982) proposed calculations for spread based on an
#' equilateral triangle layout. Simard et al. (1984) proposed calculations for
#' spread based on any type of triangle. Both articles also discussed field
#' sampling design and layout, with special attention to the size of the
#' triangles (large enough that the fire traverses the triangle in one to two
#' minutes) and even using triangles of varying size within one field plot (but
#' no triangle larger than one fourth of the site's total area).
#' 
#' The underlying algorithms use trigonometry to solve for rate of spread and
#' direction of spread. One important assumption is that the spread rate and
#' direction is uniform across one triangular plot, and that the fire front is
#' spreading as a straight line; Simard et al. (1982, 1984) acknowledge that
#' these assumption are likely broken to some degree during fire spread events.
#' 
#' The functions require the user to arrange the input dataframe so that each
#' triangle of interest is identified based on a new row in the dataframe. The
#' input format forces the user to identify the triangles, one triangle per row
#' of input dataframe. Very complex arrangements of field plot layouts are
#' possible, and the current version of these functions do not attempt to
#' determine each triangle of interest automatically.
#' 
#' @param input A dataframe containing input variables of Time fire front
#' crossed points 1, 2, 3, and latitude/longitude for those same points.
#' Variable names have to be the same as in the following list, but they are
#' case insensitive. The order in which the input variables are entered is not
#' important.
#' 
#' \tabular{lll}{ 
#' \var{T1} \tab (required) \tab Time that the fire front
#' crossed point 1. Time entered in fractional \cr\tab\tab format. Output ROS
#' will depend on the level of precision entered \cr\tab\tab (minute, second,
#' decisecond)\cr 
#' \var{T2} \tab (required) \tab Time that the fire front
#' crossed point 2. Time entered in fractional \cr\tab\tab format. Output ROS
#' will depend on the level of precision entered \cr\tab\tab (minute, second,
#' decisecond)\cr 
#' \var{T3} \tab (required) \tab Time that the fire front crossed point 3. Time 
#' entered in fractional \cr\tab\tab format. Output ROS will depend on the level
#'  of precision entered \cr\tab\tab (minute, second,
#' decisecond)\cr 
#' \var{Long1}\tab (required) \tab Longitude for datalogger 1. (decimal degrees). \cr 
#' \var{Long2}\tab (required) \tab Longitude for datalogger 2. (decimal degrees). \cr 
#' \var{Long3}\tab (required) \tab Longitude for datalogger 3. (decimal degrees). \cr 
#' \var{Lat1} \tab (required) \tab Latitude for datalogger 1. (decimal degrees). \cr 
#' \var{Lat2} \tab (required) \tab Latitude for datalogger 2. (decimal degrees). \cr
#' \var{Lat3} \tab (required) \tab Latitude for datalogger 3. (decimal
#' degrees). \cr }
#' @return \code{pros} returns a dataframe which includes the rate of spread
#' and spread direction. Output units depend on the user’s inputs for
#' distance (typically meters) and time (seconds or minutes).
#' @author Tom Schiks, Xianli Wang, Alan Cantin
#' @seealso \code{\link{lros}},
#' @references 1. Simard, A.J., Eenigenburg, J.E., Adams, K.B., Nissen, R.L.,
#' Deacon, and Deacon, A.G. 1984. A general procedure for sampling and
#' analyzing wildland fire spread.
#' 
#' 2. Byram, G.M. 1959. Combustion of forest fuels. In: Davis, K.P. Forest Fire
#' Control and Use. McGraw-Hill, New York.
#' 
#' 3. Curry, J.R., and Fons, W.L. 1938. Rate of spread of surface fires in the
#' Ponderosa Pine Type of California. Journal of Agricultural Research 57(4):
#' 239-267.
#' 
#' 4. Simard, A.J., Deacon, A.G., and Adams, K.B. 1982. Nondirectional sampling
#' wildland fire spread. Fire Technology: 221-228.
#' @keywords ros
#' @examples
#' 
#' library(cffdrs)
#' # manual single entry
#' pros.in1 <- data.frame(t(c(2, -79.701027, 43.808872, 50, -79.699650, 43.808833
#'                             , 120, -79.700387, 43.809816)))
#' colnames(pros.in1)<-c("T1", "LONG1", "LAT1", "T2", "LONG2", "LAT2", "T3", 
#'                       "LONG3", "LAT3")
#' pros.out1 <- pros(pros.in1)
#' # multiple entries using a dataframe
#' # load the test dataframe for pros
#' data("test_pros")
#' pros(test_pros)
#' 
#' @export pros
pros <- cffdrs.core:::SimardRateOfSpreadPoint
data("test_pros", package="cffdrs.core")


#' Line-based input for Simard Rate of Spread and Direction
#' 
#' \code{lros} is used to calculate the rate of spread and direction given one
#' set of three point-based observations of fire arrival time. The function
#' requires that the user specify the time that the fire crossed each point,
#' along with the measured lengths between each pair of observational points,
#' and a reference bearing (one specified side of the triangle). This function
#' allows quick input of a dataframe specifying one or many triangles.
#' 
#' \code{lros} Allows R users to calculate the rate of spread and direction of
#' a fire across a triangle, given three time measurements and details about
#' the orientation and distance between observational points. The algorithm is
#' based on the description from Simard et al. (1984). See \code{pros} for more
#' information.
#' 
#' The functions require the user to arrange the input dataframe so that each
#' triangle of interest is identified based on a new row in the dataframe. The
#' input format forces the user to identify the triangles, one triangle per row
#' of input dataframe. Very complex arrangements of field plot layouts are
#' possible, and the current version of these functions do not attempt to
#' determine each triangle of interest automatically.
#' 
#' @param input A dataframe containing input variables of time fire front
#' crossed points 1, 2, 3, and latitude/longitude for those same points.
#' Variable names have to be the same as in the following list, but they are
#' case insensitive. The order in which the input variables are entered is not
#' important.
#' 
#' \tabular{lll}{ 
#' \var{T1} 
#' \tab (required) 
#' \tab Time that the fire front crossed point 1.\cr
#' \tab\tab Time entered in fractional format. \cr
#' \tab\tab Output ROS will depend on the level \cr
#' \tab\tab of precision entered \cr
#' \tab\tab (minute, second, decisecond)\cr 
#' \var{T2} 
#' \tab (required) 
#' \tab Time that the fire front crossed point 2.\cr
#' \tab\tab Time entered in fractional format. \cr
#' \tab\tab Output ROS will depend on the level \cr
#' \tab\tab of precision entered \cr
#' \tab\tab (minute, second, decisecond)\cr 
#' \var{T3} 
#' \tab (required) 
#' \tab Time that the fire front crossed point 3. \cr 
#' \tab\tab Time entered in fractional format. \cr
#' \tab\tab Output ROS will depend on the level \cr
#' \tab\tab of precision entered \cr
#' \tab\tab (minute, second, decisecond)\cr 
#' \var{LengthT1T2}
#' \tab (required) \tab Length between each pair of\cr
#' \tab\tab observation points T1 and T2 (subscripts \cr
#' \tab\tab denote time-ordered pairs). (meters)\cr 
#' \var{LengthT2T3}
#' \tab (required) 
#' \tab Length between each pair of\cr
#' \tab\tab observation points T2 and T3 (subscripts \cr
#' \tab\tab denote time-ordered pairs). (meters)\cr 
#' \var{LengthT1T3}
#' \tab (required) 
#' \tab Length between each pair of\cr
#' \tab\tab observation points T1 and T3 (subscripts \cr
#' \tab\tab denote time-ordered pairs). (meters)\cr 
#' \var{BearingT1T2} 
#' \tab (required) 
#' \tab Reference bearing. For reference,\cr
#' \tab\tab North = 0, West = -90, East = 90 (degrees)\cr 
#' \var{BearingT1T3} 
#' \tab (required) 
#' \tab Reference bearing. For reference,\cr 
#' \tab\tab North = 0, West = -90, East = 90 (degrees)\cr 
#' }
#' @return \code{lros} returns a dataframe which includes the rate of spread
#' and spread direction. Output units depend on the user’s inputs for
#' distance (typically meters) and time (seconds or minutes).
#' @author Tom Schiks, Xianli Wang, Alan Cantin
#' @seealso \code{\link{pros}},
#' @references 1. Simard, A.J., Eenigenburg, J.E., Adams, K.B., Nissen, R.L.,
#' Deacon, and Deacon, A.G. 1984. A general procedure for sampling and
#' analyzing wildland fire spread.
#' 
#' 2. Byram, G.M. 1959. Combustion of forest fuels. In: Davis, K.P. Forest Fire
#' Control and Use. McGraw-Hill, New York.
#' 
#' 3. Curry, J.R., and Fons, W.L. 1938. Rate of spread of surface fires in the
#' Ponderosa Pine Type of California. Journal of Agricultural Research 57(4):
#' 239-267.
#' 
#' 4. Simard, A.J., Deacon, A.G., and Adams, K.B. 1982. Nondirectional sampling
#' wildland fire spread. Fire Technology: 221-228.
#' @keywords ros
#' @examples
#' 
#' library(cffdrs)
#' # manual single entry, but converted to a data frame
#' lros.in1 <- data.frame(t(c(0, 24.5, 50, 22.6, 120, 20.0, 90, 35)))
#' colnames(lros.in1)<-c("T1","LengthT1T2", "T2", "LengthT1T3", "T3", 
#'                       "LengthT2T3", "bearingT1T2", "bearingT1T3")
#' lros.out1 <- lros(lros.in1)
#' lros.out1
#' 
#' # multiple entries using a dataframe
#' # load the test dataframe for lros
#' data("test_lros")
#' lros(test_lros)
#' 
#' 
#' 
#' @export lros
lros <- cffdrs.core:::SimardRateOfSpreadLine
data("test_lros", package="cffdrs.core")


#' Fire Behavior Prediction System function
#' 
#' \code{fbp} calculates the outputs from the Canadian Forest Fire Behavior
#' Prediction (FBP) System (Forestry Canada Fire Danger Group 1992) based on
#' given fire weather and fuel moisture conditions (from the Canadian Forest
#' Fire Weather Index (FWI) System (Van Wagner 1987)), fuel type, date, and
#' slope. Fire weather, for the purpose of FBP System calculation, comprises
#' observations of 10 m wind speed and direction at the time of the fire, and
#' two associated outputs from the Fire Weather Index System, the Fine Fuel
#' Moisture Content (FFMC) and Buildup Index (BUI). FWI System components can
#' be calculated with the sister function \code{\link{fwi}}.
#' 
#' The Canadian Forest Fire Behavior Prediction (FBP) System (Forestry Canada
#' Fire Danger Group 1992) is a subsystem of the Canadian Forest Fire Danger
#' Rating System, which also includes the Canadian Forest Fire Weather Index
#' (FWI) System. The FBP System provides quantitative estimates of head fire
#' spread rate, fuel consumption, fire intensity, and a basic fire description
#' (e.g., surface, crown) for 16 different important forest and rangeland types
#' across Canada. Using a simple conceptual model of the growth of a point
#' ignition as an ellipse through uniform fuels and under uniform weather
#' conditions, the system gives, as a set of secondary outputs, estimates of
#' flank and back fire behavior and consequently fire area perimeter length and
#' growth rate.
#' 
#' The FBP System evolved since the mid-1970s from a series of regionally
#' developed burning indexes to an interim edition of the nationally develop
#' FBP system issued in 1984. Fire behavior models for spread rate and fuel
#' consumption were derived from a database of over 400 experimental, wild and
#' prescribed fire observations. The FBP System, while providing quantitative
#' predictions of expected fire behavior is intended to supplement the
#' experience and judgment of operational fire managers (Hirsch 1996).
#' 
#' The FBP System was updated with some minor corrections and revisions in 2009
#' (Wotton et al. 2009) with several additional equations that were initially
#' not included in the system. This fbp function included these updates and
#' corrections to the original equations and provides a complete suite of fire
#' behavior prediction variables. Default values of optional input variables
#' provide a reasonable mid-range setting. Latitude, longitude, elevation, and
#' the date are used to calculate foliar moisture content, using a set of
#' models defined in the FBP System; note that this latitude/longitude-based
#' function is only valid for Canada. If the Foliar Moisture Content (FMC) is
#' specified directly as an input, the fbp function will use this value
#' directly rather than calculate it. This is also true of other input
#' variables.
#' 
#' Note that Wind Direction (WD) is the compass direction from which wind is
#' coming. Wind azimuth (not an input) is the direction the wind is blowing to
#' and is 180 degrees from wind direction; in the absence of slope, the wind
#' azimuth is coincident with the direction the head fire will travel (the
#' spread direction azimuth, RAZ). Slope aspect is the main compass direction
#' the slope is facing. Slope azimuth (not an input) is the direction a head
#' fire will spread up slope (in the absence of wind effects) and is 180
#' degrees from slope aspect (Aspect).  Wind direction and slope aspect are the
#' commonly used directional identifiers when specifying wind and slope
#' orientation respectively.  The input theta specifies an angle (given as a
#' compass bearing) at which a user is interested in fire behavior predictions;
#' it is typically some angle off of the final spread rate direction since if
#' for instance theta=RAZ (the final spread azimuth of the fire) then the rate
#' of spread at angle theta (TROS) will be equivalent to ROS.
#' 
#' @param input The input data, a data.frame containing fuel types, fire
#' weather component, and slope (see below). Each vector of inputs defines a
#' single FBP System prediction for a single fuel type and set of weather
#' conditions. The data.frame can be used to evaluate the FBP System for a
#' single fuel type and instant in time, or multiple records for a single point
#' (e.g., one weather station, either hourly or daily for instance) or multiple
#' points (multiple weather stations or a gridded surface). All input variables
#' have to be named as listed below, but they are case insensitive, and do not
#' have to be in any particular order. Fuel type is of type character; other
#' arguments are numeric. Missing values in numeric variables could either be
#' assigned as NA or leave as blank.\cr\cr
#' 
#' 
#' \tabular{lll}{ 
#' \bold{Required Inputs:}\tab\tab\cr 
#' \bold{Input} \tab \bold{Description/Full name} \tab \bold{Defaults}\cr 
#' 
#' \var{FuelType} 
#' \tab FBP System Fuel Type including "C-1",\cr
#' \tab"C-2", "C-3", "C-4","C-5", "C-6", "C-7",\cr
#' \tab "D-1", "M-1", "M-2", "M-3", "M-4", "NF",\cr
#' \tab "D-1", "S-2", "S-3", "O-1a", "O-1b", and\cr
#' \tab  "WA", where "WA" and "NF" stand for \cr
#' \tab "water" and "non-fuel", respectively.\cr\cr
#'  
#' \var{LAT} \tab Latitude [decimal degrees] \tab 55\cr 
#' \var{LONG} \tab Longitude [decimal degrees] \tab -120\cr 
#' \var{FFMC} \tab Fine fuel moisture code [FWI System component] \tab 90\cr 
#' \var{BUI} \tab Buildup index [FWI System component] \tab 60\cr 
#' \var{WS} \tab Wind speed [km/h] \tab 10\cr
#' \var{GS} \tab Ground Slope [percent] \tab 0\cr 
#' \var{Dj} \tab Julian day \tab 180\cr 
#' \var{Aspect} \tab Aspect of the slope [decimal degrees] \tab 0\cr\cr 
#' 
#' \bold{Optional Inputs (1):}
#' \tab Variables associated with certain fuel \cr
#' \tab types. These could be skipped if relevant \cr
#' \tab fuel types do not appear in the input data.\cr\cr
#' 
#' \bold{Input} \tab \bold{Full names of inputs} \tab \bold{Defaults}\cr 
#' 
#' \var{PC} \tab Percent Conifer for M1/M2 [percent] \tab 50\cr 
#' \var{PDF} \tab Percent Dead Fir for M3/M4 [percent] \tab 35\cr
#' \var{cc} \tab Percent Cured for O1a/O1b [percent] \tab 80\cr 
#' \var{GFL} \tab Grass Fuel Load [kg/m^2] \tab 0.35\cr\cr 
#' 
#' \bold{Optional Inputs (2):} 
#' \tab Variables that could be ignored without \cr
#' \tab causing major impacts to the primary outputs\cr\cr
#' 
#' \bold{Input} \tab \bold{Full names of inputs} \tab \bold{Defaults}\cr 
#' \var{CBH}   \tab Crown to Base Height [m] \tab 3\cr 
#' \var{WD}    \tab Wind direction [decimal degrees] \tab 0\cr 
#' \var{Accel} \tab Acceleration: 1 = point, 0 = line \tab 0\cr 
#' \var{ELV*}  \tab Elevation [meters above sea level] \tab NA\cr 
#' \var{BUIEff}\tab Buildup Index effect: 1=yes, 0=no \tab 1\cr 
#' \var{D0}    \tab Julian day of minimum Foliar Moisture Content \tab 0\cr 
#' \var{hr}    \tab Hours since ignition \tab 1\cr 
#' \var{ISI}   \tab Initial spread index \tab 0\cr 
#' \var{CFL}   \tab Crown Fuel Load [kg/m^2]\tab 1.0\cr 
#' \var{FMC}   \tab Foliar Moisture Content if known [percent] \tab 0\cr 
#' \var{SH}    \tab C-6 Fuel Type Stand Height [m] \tab 0\cr 
#' \var{SD}    \tab C-6 Fuel Type Stand Density [stems/ha] \tab 0\cr 
#' \var{theta} \tab Elliptical direction of calculation [degrees] \tab 0\cr\cr }
#' 
#' @param output FBP output offers 3 options (see details in \bold{Values}
#' section):
#' 
#' \tabular{lc}{ \bold{Outputs} \tab \bold{Number of outputs}\cr 
#' \var{Primary(\bold{default})} \tab 8\cr 
#' \var{Secondary} \tab 34\cr 
#' \var{All} \tab 42\cr\cr}
#' 
#' @param m Optimal number of pixels at each iteration of computation when
#' \code{nrow(input) >= 1000}. Default \code{m = NULL}, where the function will
#' assign \code{m = 1000} when \code{nrow(input)} is between 1000 and 500,000,
#' and \code{m = 3000} otherwise. By including this option, the function is
#' able to process large dataset more efficiently. The optimal value may vary
#' with different computers.
#' 
#' @param cores Number of CPU cores (integer) used in the computation, default
#' is 1.  By signing \code{cores > 1}, the function will apply parallel
#' computation technique provided by the \code{foreach} package, which
#' significantly reduces the computation time for large input data (over a
#' million records). For small dataset, \code{cores=1} is actually faster.
#' 
#' @return \code{fbp} returns a dataframe with primary, secondary, or all
#' output variables, a combination of the primary and secondary outputs.
#' 
#' \bold{Primary} FBP output includes the following 8 variables: 
#' 
#' \item{CFB}{Crown Fraction Burned by the head fire} 
#' \item{CFC}{Crown Fuel Consumption [kg/m^2]} 
#' \item{FD}{Fire description (1=Surface, 2=Intermittent, 3=Crown)} 
#' \item{HFI}{Head Fire Intensity [kW/m]}
#' \item{RAZ}{Spread direction azimuth [degrees]} 
#' \item{ROS}{Equilibrium Head Fire Rate of Spread [m/min]} 
#' \item{SFC}{Surface Fuel Consumption [kg/m^2]} 
#' \item{TFC}{Total Fuel Consumption [kg/m^2]}
#' 
#' \bold{Secondary} FBP System outputs include the following 34 raster layers. In order
#' to calculate the reliable secondary outputs, depending on the outputs, 
#' optional inputs may have to be provided.  
#' 
#' \item{BE}{BUI effect on spread rate} 
#' \item{SF}{Slope Factor (multiplier for ROS increase upslope)} 
#' \item{ISI}{Initial Spread Index} 
#' \item{FFMC}{Fine fuel moisture code [FWI System component]} 
#' \item{FMC}{Foliar Moisture Content [\%]} 
#' \item{Do}{Julian Date of minimum FMC} 
#' \item{RSO}{Critical spread rate for crowning [m/min]}
#' \item{CSI}{Critical Surface Intensity for crowning [kW/m]}
#' \item{FROS}{Equilibrium Flank Fire Rate of Spread [m/min]}
#' \item{BROS}{Equilibrium Back Fire Rate of Spread [m/min]}
#' \item{HROSt}{Head Fire Rate of Spread at time hr [m/min]}
#' \item{FROSt}{Flank Fire Rate of Spread at time hr [m/min]}
#' \item{BROSt}{Back Fire Rate of Spread at time hr [m/min]}
#' \item{FCFB}{Flank Fire Crown Fraction Burned} 
#' \item{BCFB}{Back Fire Crown Fraction Burned} 
#' \item{FFI}{Equilibrium Spread Flank Fire Intensity [kW/m]} 
#' \item{BFI}{Equilibrium Spread Back Fire Intensity [kW/m]} 
#' \item{FTFC}{Flank Fire Total Fuel Consumption [kg/m^2] } 
#' \item{BTFC}{Back Fire Total Fuel Consumption [kg/m^2] } 
#' \item{DH}{Head Fire Spread Distance after time hr [m] }
#' \item{DB}{Back Fire Spread Distance after time hr [m] }
#' \item{DF}{Flank Fire Spread Distance after time hr [m] }
#' \item{TI}{Time to Crown Fire Initiation [hrs since ignition] }
#' \item{FTI}{Time to Flank Fire Crown initiation [hrs since ignition]} 
#' \item{BTI}{Time to Back Fire Crown initiation [hrs since ignition]} 
#' \item{LB}{Length to Breadth ratio} 
#' \item{LBt}{Length to Breadth ratio after elapsed time hr } 
#' \item{WSV}{Net vectored wind speed [km/hr]} 
#' \item{TROS*}{Equilibrium Rate of Spread at bearing theta [m/min] } 
#' \item{TROSt*}{Rate of Spread at bearing theta at time t [m/min] } 
#' \item{TCFB*}{Crown Fraction Burned at bearing theta } 
#' \item{TFI*}{Fire Intensity at bearing theta [kW/m] } 
#' \item{TTFC*}{Total Fuel Consumption at bearing theta [kg/m^2] } 
#' \item{TTI*}{Time to Crown Fire initiation at bearing theta [hrs since ignition] }
#' 
#' *These outputs represent fire behaviour at a point on the perimeter of an
#' elliptical fire defined by a user input angle theta. theta represents the
#' bearing of a line running between the fire ignition point and a point on the
#' perimeter of the fire. It is important to note that in this formulation the
#' theta is a bearing and does not represent the angle from the semi-major axis
#' (spread direction) of the ellipse. This formulation is similar but not
#' identical to methods presented in Wotton et al (2009) and Tymstra et al
#' (2009).
#' @author Xianli Wang, Alan Cantin, Marc-André Parisien, Mike Wotton, Kerry
#' Anderson, and Mike Flannigan
#' @seealso \code{\link{fwi}, \link{fbpRaster}}
#' @references 1.  Hirsch K.G. 1996. Canadian Forest Fire Behavior Prediction
#' (FBP) System: user's guide. Nat. Resour. Can., Can. For. Serv., Northwest
#' Reg., North. For. Cent., Edmonton, Alberta. Spec. Rep. 7. 122p.
#' 
#' 2.  Forestry Canada Fire Danger Group. 1992. Development and structure of
#' the Canadian Forest Fire Behavior Prediction System. Forestry Canada,
#' Ottawa, Ontario Information Report ST-X-3. 63 p.
#' \url{http://cfs.nrcan.gc.ca/pubwarehouse/pdfs/10068.pdf}
#' 
#' 3.  Wotton, B.M., Alexander, M.E., Taylor, S.W. 2009. Updates and revisions
#' to the 1992 Canadian forest fire behavior prediction system. Nat. Resour.
#' Can., Can. For. Serv., Great Lakes For. Cent., Sault Ste. Marie, Ontario,
#' Canada. Information Report GLC-X-10, 45p.
#' \url{http://publications.gc.ca/collections/collection_2010/nrcan/Fo123-2-10-2009-eng.pdf}
#' 
#' 4.  Tymstra, C., Bryce, R.W., Wotton, B.M., Armitage, O.B. 2009. Development
#' and structure of Prometheus: the Canadian wildland fire growth simulation
#' Model. Nat. Resour. Can., Can. For. Serv., North. For. Cent., Edmonton, AB.
#' Inf. Rep. NOR-X-417.\url{https://d1ied5g1xfgpx8.cloudfront.net/pdfs/31775.pdf}
#' @keywords methods
#' @examples
#' 
#' library(cffdrs)
#' # The dataset is the standard test data for FPB system
#' # provided by Wotton et al (2009)
#' data("test_fbp")
#' head(test_fbp)
#' #  id FuelType LAT LONG ELV FFMC BUI   WS WD GS  Dj  D0         hr PC PDF GFL cc theta Accel Aspect BUIEff CBH CFL ISI
#' #1  1      C-1  55  110  NA   90 130 20.0  0 15 182  NA 0.33333333 NA  NA  NA  NA     0     1    270      1  NA  NA   0
#' #2  2       C2  50   90  NA   97 119 20.4  0 75 121  NA 0.33333333 NA  NA  NA  NA     0     1    315      1  NA  NA   0
#' #3  3      C-3  55  110  NA   95  30 50.0  0  0 182  NA 0.08333333 NA  NA  NA  NA     0     1    180      1  NA  NA   0
#' #4  4      C-4  55  105 200   85  82  0.0 NA 75 182  NA 0.50000000 NA  NA  NA  NA     0     1    315      1  NA  NA   0
#' #5  5       c5  55  105  NA   88  56  3.4  0 23 152 145 0.50000000 NA  NA  NA  NA     0     1    180      1  NA  NA   0
#' 
#' #Primary output (default)
#' fbp(test_fbp)
#' #or
#' fbp(test_fbp,output="Primary") 
#' #or 
#' fbp(test_fbp,"P")
#' #Secondary output          
#' fbp(test_fbp,"Secondary")
#' #or
#' fbp(test_fbp,"S")
#' #All output          
#' fbp(test_fbp,"All")
#' #or
#' fbp(test_fbp,"A")
#' #For a single record:
#' fbp(test_fbp[7,])  	
#' #For a section of the records:
#' fbp(test_fbp[8:13,])	
#' #fbp function produces the default values if no data is fed to
#' #the function:
#' fbp()
#' 
#' @export fbp
fbp  <- function(input = NULL, output = "Primary", m = NULL, cores = 1){  
  
  #hack to avoid Note about no visible binding for global variable ID
  #http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
  #look at the globalvariables() option or others in place of this issue
  # do not remove this comment until resolved
  if (!is.na(charmatch("input", search()))) {
    detach(input)
  }
  fullList <- cffdrs.core::FireBehaviourPrediction(input, output = output)
  
  return(fullList)
}


#' Fire Weather Index System
#' 
#' \code{fwi} is used to calculate the outputs of the Canadian Forest Fire
#' Weather Index (FWI) System for one day or one fire season based on noon
#' local standard time (LST) weather observations of temperature, relative
#' humidity, wind speed, and 24-hour rainfall, as well as the previous day's
#' fuel moisture conditions. This function could be used for either one weather
#' station or for multiple weather stations.
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
#' The first three outputs of the system (the Fire Fuel Moisture Code (ffmc),
#' the Duff Moisture Code (dmc), and the Drought Code (dc)) track moisture in
#' different layers of the fuel making up the forest floor. Their calculation
#' relies on the daily fire weather observation and also, importantly, the
#' moisture code value from the previous day as they are in essence bookkeeping
#' systems tracking the amount of moisture (water) in to and out of the layer.
#' It is therefore important that when calculating FWI System outputs over an
#' entire fire season, an uninterrupted daily weather stream is provided; one
#' day is the assumed time step in the models and thus missing data must be
#' filled in.
#' 
#' The next three outputs of the System are relative (unitless) indicators of
#' aspects of fire behavior potential: spread rate (the Initial Spread Index,
#' isi), fuel consumption (the Build-up Index, bui) and fire intensity per unit
#' length of fire front (the Fire Weather Index, fwi).  This final index, the
#' fwi, is the component of the System used to establish the daily fire danger
#' level for a region and communicated to the public.  This final index can be
#' transformed to the Daily Severity Rating (dsr) to provide a more
#' reasonably-scaled estimate of fire control difficulty.
#' 
#' Both the Duff Moisture Code (dmc) and Drought Code (dc) are influenced by
#' day length (see Van Wagner 1987). Day length adjustments for different
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
#' @param input A dataframe containing input variables of daily weather
#' observations taken at noon LST. Variable names have to be the same as in the
#' following list, but they are case insensitive. The order in which the input
#' variables are entered is not important.
#' 
#' \tabular{lll}{ 
#' \var{id} \tab (optional) 
#' \tab Unique identifier of a weather\cr
#' \tab\tab station or spatial point (no restriction on\cr
#' \tab\tab data type); required when \code{batch=TRUE}\cr 
#' \var{lat} \tab (recommended) \tab Latitude (decimal degree, default=55)\cr 
#' \var{long} \tab (optional) \tab Longitude (decimal degree)\cr 
#' \var{yr} \tab (optional) \tab Year of observation;
#' required when \code{batch=TRUE}\cr 
#' \var{mon} \tab (recommended) \tab Month of the year (integer 1-12, default=7)\cr 
#' \var{day} \tab (optional) \tab Dayof the month (integer); required when \code{batch=TRUE}\cr 
#' \var{temp} \tab (required) \tab Temperature (centigrade)\cr 
#' \var{rh} \tab (required) \tab Relative humidity (\%)\cr 
#' \var{ws} \tab (required) \tab 10-m height wind speed (km/h)\cr 
#' \var{prec} \tab (required) \tab 24-hour rainfall (mm)\cr }
#' 
#' @param init A data.frame or vector contains either the initial values for
#' FFMC, DMC, and DC or the same variables that were calculated for the
#' previous day and will be used for the current day's calculation. The
#' function also accepts a vector if the initial or previous day FWI values is
#' for only one weather station (a warning message comes up if a single set of
#' initial values is used for multiple weather stations). Defaults are the
#' standard initial values for FFMC, DMC, and DC defined as the following:
#' \tabular{lll}{ 
#' \bold{Variable} \tab \bold{Description} \tab \bold{Default} \cr
#' \var{ffmc} \tab Previous day Fine Fuel Moisture Code (FFMC; unitless) \tab 85 \cr
#' \var{dmc} \tab Previous day Duff Moisture Code (DMC; unitless)\tab 6 \cr
#' \var{dc} \tab Previous Day Drought Code (DC; unitless) \tab 15\cr
#' \var{lat} \tab Latitude of the weather station (\emph{Optional}) \tab 55 \cr}
#' 
#' @param batch Whether the computation is iterative or single step, default is
#' TRUE. When \code{batch=TRUE}, the function will calculate daily FWI System
#' outputs for one weather station over a period of time chronologically with
#' the initial conditions given (\code{init}) applied only to the first day of
#' calculation. If multiple weather stations are processed, an additional "id"
#' column is required in the input to label different stations, and the data
#' needs to be sorted by date/time and "id".  If \code{batch=FALSE}, the
#' function calculates only one time step (1 day) base on either the initial
#' start values or the previous day's FWI System variables, which should also
#' be assigned to \code{init} argument.
#' 
#' @param out The function offers two output options, \code{out="all"} will
#' produce a data frame that includes both the input and the FWI System
#' outputs; \code{out="fwi"} will generate a data frame with only the FWI
#' system components.
#' 
#' @param lat.adjust The function offers options for whether day length
#' adjustments should be applied to the calculations.  The default value is
#' "TRUE".
#' 
#' @param uppercase Output in upper cases or lower cases would be decided by
#' this argument. Default is TRUE.
#' 
#' @return \code{fwi} returns a dataframe which includes both the input and the
#' FWI System variables as described below: 
#' \item{Input Variables }{Including temp, rh, ws, and prec with id, long, lat, yr, mon, or day as optional.}
#' \item{ffmc }{Fine Fuel Moisture Code} 
#' \item{dmc }{Duff Moisture Code}
#' \item{dc }{Drought Code} 
#' \item{isi }{Initial Spread Index} 
#' \item{bui }{Buildup Index} 
#' \item{fwi }{Fire Weather Index} 
#' \item{dsr }{Daily Severity Rating}
#' 
#' @author Xianli Wang, Alan Cantin, Marc-André Parisien, Mike Wotton, Kerry
#' Anderson, and Mike Flannigan
#' 
#' @seealso \code{\link{fbp}}, \code{\link{fwiRaster}}, \code{\link{gfmc}},
#' \code{\link{hffmc}}, \code{\link{hffmcRaster}}, \code{\link{sdmc}},
#' \code{\link{wDC}}, \code{\link{fireSeason}}
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
#' # The test data is a standard test
#' # dataset for FWI system (Van Wagner and Pickett 1985) 
#' data("test_fwi")
#' # Show the data, which is already sorted by time:
#' # head(test_fwi)
#' # long  lat	yr	mon	day	temp	rh	ws	prec
#' # -100	40	1985	4	  13	17	  42	25	0
#' # -100	40	1985	4	  14	20	  21	25	2.4
#' # -100	40	1985	4	  15	8.5	  40	17	0
#' # -100	40	1985	4	  16	6.5	  25	6	0
#' # -100	40	1985	4	  17	13	  34	24	0
#' 
#' ## (1) FWI System variables for a single weather station:
#' # Using the default initial values and batch argument, 
#' # the function calculate FWI variables chronically:
#' fwi.out1<-fwi(test_fwi) 				
#' # Using a different set of initial values:
#' fwi.out2<-fwi(test_fwi,init=data.frame(ffmc=80, dmc=10,dc=16, lat=50))
#' # This could also be done as the following:
#' fwi.out2<-fwi(test_fwi,init=data.frame(80,10,6,50))
#' # Or:
#' fwi.out2<-fwi(test_fwi,init=c(80,10,6,50))
#' # Latitude could be ignored, and the default value (55) will 
#' # be used:
#' fwi.out2<-fwi(test_fwi,init=data.frame(80,10,6))
#' 
#' ## (2) FWI for one or multiple stations in a single day:
#' # Change batch argument to FALSE, fwi calculates FWI 
#' # components based on previous day's fwi outputs:
#' 
#' fwi.out3<-fwi(test_fwi,init=fwi.out1,batch=FALSE)                 
#' # Using a suite of initials, assuming variables from fwi.out1
#' # are the initial values for different records. 
#' init_suite<-fwi.out1[,c("FFMC","DMC","DC","LAT")]
#' # Calculating FWI variables for one day but with multiple
#' # stations. Because the calculations is for one time step, 
#' # batch=FALSE:
#' fwi.out4<-fwi(test_fwi,init=init_suite,batch=FALSE)
#' 
#' ## (3) FWI for multiple weather stations over a period of time: 
#' #Assuming there are 4 weather stations in the test dataset, and they are 
#' # ordered by day:
#' test_fwi$day<-rep(1:(nrow(test_fwi)/4),each=4)
#' test_fwi$id<-rep(1:4,length(unique(test_fwi$day)))
#' # Running the function with the same default initial inputs, will receive a 
#' # warning message, but that is fine: 
#' fwi(test_fwi)
#' 
#' ## (4) Daylength adjustment:
#' # Change latitude values where the monthly daylength adjustments
#' # are different from the standard ones
#' test_fwi$lat<-22
#' # With daylength adjustment
#' fwi(test_fwi)[1:3,]
#' # Without daylength adjustment
#' fwi(test_fwi,lat.adjust=FALSE)[1:3,]
#' 
#' @export fwi
#' 
fwi <- function(input, init = data.frame(ffmc = 85, dmc = 6, dc = 15, lat = 55),
                batch = TRUE, out = "all", lat.adjust = TRUE, uppercase = TRUE) {
  #############################################################################
  # Description: Canadian Forest Fire Weather Index Calculations. All code
  #              is based on a C code library that was written by Canadian
  #              Forest Service Employees, which was originally based on
  #              the Fortran code listed in the reference below. All equations
  #              in this code refer to that document, unless otherwise noted.
  #
  #              Equations and FORTRAN program for the Canadian Forest Fire 
  #              Weather Index System. 1985. Van Wagner, C.E.; Pickett, T.L. 
  #              Canadian Forestry Service, Petawawa National Forestry 
  #              Institute, Chalk River, Ontario. Forestry Technical Report 33. 
  #              18 p.
  #
  #              Additional reference on FWI system
  #
  #              Development and structure of the Canadian Forest Fire Weather 
  #              Index System. 1987. Van Wagner, C.E. Canadian Forestry Service,
  #              Headquarters, Ottawa. Forestry Technical Report 35. 35 p.
  #  
  #Args:  input:    View Documentation (fwi.Rd) for full description
  #                 of input data frame
  #       init:     Initializing moisture values
  #                 ffmc:     Fine Fuel Moisture Code (default 85)
  #                 dmc:      Duff Moisture Code (default 6)
  #                 dc:       Drought Code (default 15)
  #                 lat:      Latitude (decimal degrees, default 55)
  #       batch:    Function can be run in a batch mode, where multiple 
  #                 weather stations or points can be calculated at once. 
  #                 (TRUE/FALSE, default TRUE)
  #       out:      Display the calculated FWI values, with or without the 
  #                 inputs. (all/fwi, default all)
  #       lat.adjust: Option to adjust day length in the calculations 
  #                   (TRUE/FALSE, default TRUE)
  #       uppercase:  Output names in upper or lower case - a commonly 
  #                   asked for feature, as dataset naming conventions vary 
  #                   considerably. (TRUE/FALSE, default TRUE)
  #       
  #
  # Returns: A data.frame of the calculated FWI values with or without
  #          the input data attached to it.
  #
  #############################################################################
  
  #Quite often users will have a data frame called "input" already attached
  #  to the workspace. To mitigate this, we remove that if it exists, and warn
  #  the user of this case.
  if (!is.na(charmatch("input", search()))) {
    detach(input)
  }
  names(input) <- tolower(names(input))
  
  #convert vector to data.frame to ensure consitency
  if (is.vector(init)){
    init <- as.data.frame(t(init))
  }
  names(init) <- tolower(names(init))
  #resolve missing names of the initializing variables if necessary
  if(substr(names(init), 1, 1)[1] == "x" | substr(names(init), 1, 1)[1] == "v"){
    if (ncol(init) == 3){
      names(init) <- c("ffmc", "dmc", "dc")
      init$lat <- 55
    }else if(ncol(init) == 4){
      names(init) <- c("ffmc", "dmc", "dc", "lat")
    }
  }
  
  #############################################################################
  #                                 
  # Set local variables and display warnings to user if default is being used
  #############################################################################
  ffmc_yda <- init$ffmc
  dmc_yda  <- init$dmc
  dc_yda   <- init$dc
  
  if ("lat" %in% names(input)) {
    lat <- input$lat
  }
  else {
    warning("latitude was not provided, assign default value 55")
    lat <- rep(55, nrow(input))
  }
  if ("long" %in% names(input)) {
    long <- input$long
  }
  else {
    warning("long was not provided, assign a default number -120")
    long <- rep(-120, nrow(input))
  }
  if ("yr" %in% names(input)) {
    yr <- as.numeric(as.character(input$yr))
  }
  else {
    warning("Year was not provided, assigned default number 5000")
    yr <- rep(5000, nrow(input))
  }
  if ("mon" %in% names(input)) {
    mon <- as.numeric(as.character(input$mon))
  }
  else {
    warning("Month was not provided, assigned the default value, July")
    mon <- rep(7, nrow(input))
  }
  if ("day" %in% names(input)) {
    day <- as.numeric(as.character(input$day))
  }
  else {
    warning("Day was not provided, assigned default number -99")
    day <- rep(-99, nrow(input))
  }
  
  #If batch selected, then sort the data by Date and id and determine the 
  # length of each run.
  # Currently when running multiple stations, the stations much have the same
  # amount of data and same start/end dates
  #Function stops running if these requirements are not met
  if (batch){
    if ("id" %in% names(input)) {
      input <- input[with(input,order(yr,mon,day,id)),]
      #number of stations
      n <- length(unique(input$id))
      if(length(unique(input[1:n, "id"])) != n){
        stop("Multiple stations have to start and end at the same dates, and 
             input data must be sorted by date/time and id")
      }
    } else {
      n <- 1
    }
  }else{
    n <- nrow(input)
  }
  
  temp <- input$temp
  prec <- input$prec
  ws <- input$ws
  rh <- input$rh
  if (!exists("temp") | is.null(temp)) 
    stop("temperature (temp) is missing!")
  if (!exists("prec") | is.null(prec)) 
    stop("precipitation (prec) is missing!")
  if (length(prec[prec < 0]) > 0)
    stop("precipiation (prec) cannot be negative!")
  if (!exists("ws") | is.null(ws)) 
    stop("wind speed (ws) is missing!")
  if (length(ws[ws < 0]) > 0)
    stop("wind speed (ws) cannot be negative!")
  if (!exists("rh") | is.null(rh)) 
    stop("relative humidity (rh) is missing!")
  if (length(rh[rh < 0]) > 0)
    stop("relative humidity (rh) cannot be negative!")
  #############################################################################
  #                                 END
  # Set local variables and display warnings to user if default is being used
  #############################################################################
  
  if (length(temp) %% n != 0)
    warning("Missing records may generate wrong outputs")
  if (nrow(init) == 1 & n > 1){
    warning("Same initial data were used for multiple weather stations")
    ffmc_yda <- rep(ffmc_yda, n)
    dmc_yda <- rep(dmc_yda, n)
    dc_yda <- rep(dc_yda, n)
  }
  #if the number of rows in the init file does not equal that of the number of
  # stations, then stop execution as we do not have a complete input set
  if(nrow(init) > 1 & nrow(init) != n) {
    stop("Number of initial values do not match with number of weather 
         stations")
  }
  
  #Length of weather run
  n0 <- length(temp) / n
  #Initialize variables
  ffmc <- dmc <- dc <- isi <- bui <- fwi <- dsr <- NULL
  #For each day in the run
  for (i in 1:n0){
    #k is the data for all stations by day
    k  <- ((i - 1) * n + 1):(i * n)
    #constrain relative humidity
    rh[k] <- ifelse(rh[k] >= 100, 99.9999, rh[k])
    ###########################################################################
    # Fine Fuel Moisture Code (FFMC)
    ###########################################################################
    ffmc1 = cffdrs.core::FineFuelMoistureCode(ffmc_yda, temp[k], rh[k], ws[k], prec[k])
    
    ###########################################################################
    # Duff Moisture Code (DMC)
    ###########################################################################
    dmc1 = cffdrs.core::DuffMoistureCode(dmc_yda, temp[k], rh[k], prec[k], lat[k], mon[k], 
                                         lat.adjust)
    
    ###########################################################################
    # Drought Code (DC)
    ###########################################################################
    dc1 <- cffdrs.core::DroughtCode(dc_yda, temp[k], rh[k], prec[k], lat[k], mon[k],
                                    lat.adjust)
    
    ###########################################################################
    # Initial Spread Index (ISI)
    ###########################################################################
    isi1 <- cffdrs.core::InitialSpreadIndex(ffmc1, ws[k], FALSE)
    
    ###########################################################################
    # Buildup Index (BUI)
    ###########################################################################
    bui1 <- cffdrs.core::BuildupIndex(dmc1, dc1)
    
    ###########################################################################
    # Fire Weather Index (FWI)
    ###########################################################################
    fwi1 <- cffdrs.core::FireWeatherIndex(isi1, bui1)
    ###########################################################################
    #                   Daily Severity Rating (DSR)
    ###########################################################################
    #Eq. 31
    dsr1 <- 0.0272 * (fwi1^1.77)
    
    #Concatenate values
    ffmc<-c(ffmc,ffmc1)
    dmc<-c(dmc,dmc1)
    dc<-c(dc,dc1)
    isi<-c(isi,isi1)
    bui<-c(bui,bui1)
    fwi<-c(fwi,fwi1)
    dsr<-c(dsr,dsr1)
    ffmc_yda<-ffmc1
    dmc_yda<-dmc1
    dc_yda<-dc1
  } 
  
  #If output specified is "fwi", then return only the FWI variables
  if (out == "fwi") {
    new_FWI <- data.frame(ffmc = ffmc, dmc = dmc, dc = dc, isi = isi, 
                          bui = bui, fwi = fwi, dsr = dsr)
    if (uppercase){
      names(new_FWI) <- toupper(names(new_FWI))
    }
  }
  #If output specified is "all", then return both FWI and input weather vars
  else {
    if (out == "all") {
      new_FWI <- cbind(input, ffmc, dmc, dc, isi, bui, fwi, dsr)
      if (uppercase){
        names(new_FWI) <- toupper(names(new_FWI))
      }
    }
  }
  return(new_FWI)
}
