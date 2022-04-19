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
