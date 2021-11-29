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
