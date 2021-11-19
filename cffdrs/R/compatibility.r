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

#' @noRd
.Slopecalc <- cffdrs.core::SlopeAdjust

#' @noRd
.TFCcalc <- cffdrs.core::TotalFuelConsumption
