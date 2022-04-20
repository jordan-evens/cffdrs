# provides backwards compatibility with old version of package

#' @noRd
.BEcalc <- function(FUELTYPE, BUI)
{
  .Deprecated("BuildupEffect")
  return(BuildupEffect(FUELTYPE, BUI))
}

#' @noRd
.BROScalc <- function(FUELTYPE, FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH)
{
  .Deprecated("BackRateOfSpread")
  return(BackRateOfSpread(FUELTYPE, FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH))
}
#' @noRd
.buiCalc <- function(dmc, dc)
{
  .Deprecated("BuildupIndex")
  return(BuildupIndex(dmc, dc))
}

#' @noRd
.C6calc <- function(FUELTYPE, ISI, BUI, FMC, SFC, CBH, ROS, CFB, RSC, option="CFB")
{
  RSI <- IntermediateSurfaceRateOfSpreadC6(ISI, FMC)
  #Return at this point, if specified by caller
  if (option == "RSI") {
    .Deprecated("IntermediateSurfaceRateOfSpreadC6")
    return(RSI)
  }
  RSS <- SurfaceRateOfSpreadC6(RSI, BUI)
  RSC <- CrownRateOfSpreadC6(ISI, FMC)
  #Return at this point, if specified by caller
  if (option == "RSC") {
    .Deprecated("CrownRateOfSpreadC6")
    return(RSC)
  }
  #Crown Fraction Burned
  CFB <- 0
  if (RSC > RSS)
  {
    CSI <- CriticalSurfaceIntensity(FUELTYPE, FMC, CBH)
    RSO <- CriticalSurfaceRateOfSpread(CSI, SFC)
    CFB <- CrownFractionBurned(FUELTYPE, RSS, RSO)
  }
  #Return at this point, if specified by caller
  if (option == "CFB") {
    .Deprecated("CrownFractionBurned")
    return(CFB)
  }
  ROS <- RateOfSpreadC6(RSC, RSS, CFB)
  .Deprecated("RateOfSpreadC6")
  return(ROS)
}

#' @noRd
.CFBcalc <- function(FUELTYPE, FMC, SFC, ROS, CBH, option="CFB")
{
  CSI <- CriticalSurfaceIntensity(FUELTYPE, FMC, CBH)
  #Return at this point, if specified by caller
  if(option=="CSI"){
    .Deprecated("CriticalSurfaceIntensity")
    return(CSI)
  }
  RSO <- CriticalSurfaceRateOfSpread(CSI, SFC)
  #Return at this point, if specified by caller
  if(option=="RSO"){
    .Deprecated("CriticalSurfaceRateOfSpread")
    return(RSO)
  }
  CFB <- CrownFractionBurned(FUELTYPE, ROS, RSO)
  .Deprecated("CrownFractionBurned")
  return(CFB)
}


#' @noRd
.dcCalc <- function(dc_yda, temp, rh, prec, lat, mon, lat.adjust=TRUE)
{
  .Deprecated("DroughtCode")
  return(DroughtCode(dc_yda, temp, rh, prec, lat, mon, lat.adjust))
}

#' @noRd
.DISTtcalc <- function(FUELTYPE, ROSeq, HR, CFB)
{
  .Deprecated("DistanceAtTime")
  return(DistanceAtTime(FUELTYPE, ROSeq, HR, CFB))
}

#' @noRd
.dmcCalc <- function(dmc_yda, temp, rh, prec, lat, mon, lat.adjust=TRUE)
{
  .Deprecated("DuffMoistureCode")
  return(DuffMoistureCode(dmc_yda, temp, rh, prec, lat, mon, lat.adjust))
}

#' @noRd
.FBPcalc <- function(FUELTYPE, output, ID, HR, LAT, LONG, CBH, SD, SH, CFL, FMC,
                     D0, ELV, DJ, WS, WAZ, SAZ, FFMC, ISI, BUI, PC, PDF, GFL,
                     BUIEFF, GS, CC, ACCEL, THETA)
{
  .Deprecated("FireBehaviourPrediction")
  return(FireBehaviourPrediction(FUELTYPE, output, ID, HR, LAT, LONG, CBH, SD,
                                 SH, CFL, FMC, D0, ELV, DJ, WS, WAZ, SAZ, FFMC,
                                 ISI, BUI, PC, PDF, GFL, BUIEFF, GS, CC, ACCEL,
                                 THETA))
}

#' @noRd
.ffmcCalc <- function(ffmc_yda, temp, rh, ws, prec)
{
  .Deprecated("FineFuelMoistureCode")
  return(FineFuelMoistureCode(ffmc_yda, temp, rh, ws, prec))
}

#' @noRd
.FIcalc <- function(FC, ROS)
{
  .Deprecated("FireIntensity")
  return(FireIntensity(FC, ROS))
}

#' @noRd
.FMCcalc <- function(LAT, LONG, ELV, DJ, D0)
{
  .Deprecated("FoliarMoistureContent")
  return(FoliarMoistureContent(LAT, LONG, ELV, DJ, D0))
}

#' @noRd
.FROScalc <- function(ROS, BROS, LB)
{
  .Deprecated("FlankRateOfSpread")
  return(FlankRateOfSpread(ROS, BROS, LB))
}

#' @noRd
.fwiCalc <- function(isi, bui)
{
  .Deprecated("FireWeatherIndex")
  return(FireWeatherIndex(isi, bui))
}

#' @noRd
.ISIcalc <- function(ffmc, ws, fbpMod=FALSE)
{
  .Deprecated("InitialSpreadIndex")
  return(InitialSpreadIndex(ffmc, ws, fbpMod))
}

#' @noRd
.LBcalc <- function(FUELTYPE, WSV)
{
  .Deprecated("LengthToBreadthRatio")
  return(LengthToBreadthRatio(FUELTYPE, WSV))
}

#' @noRd
.LBtcalc <- function(FUELTYPE, LB, HR, CFB)
{
  .Deprecated("LengthToBreadthRatioAtTime")
  return(LengthToBreadthRatioAtTime(FUELTYPE, LB, HR, CFB))
}

#' @noRd
.ROScalc <- function(FUELTYPE, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
{
  .Deprecated("RateOfSpread")
  return(RateOfSpread(FUELTYPE, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH))
}

#' @noRd
.ROStcalc <- function(FUELTYPE, ROSeq, HR, CFB)
{
  .Deprecated("RateOfSpreadAtTime")
  return(RateOfSpreadAtTime(FUELTYPE, ROSeq, HR, CFB))
}

#' @noRd
.ROSthetacalc <- function(ROS, FROS, BROS, THETA)
{
  .Deprecated("RateOfSpreadAtTheta")
  return(RateOfSpreadAtTheta(ROS, FROS, BROS, THETA))
}

#' @noRd
.SFCcalc <- function(FUELTYPE, FFMC, BUI, PC, GFL)
{
  .Deprecated("SurfaceFuelConsumption")
  return(SurfaceFuelConsumption(FUELTYPE, FFMC, BUI, PC, GFL))
}

#' @noRd
.Slopecalc <- function(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
                        CC, CBH, ISI, output = "RAZ")
{
  .Deprecated("SlopeAdjust")
  # output options include: RAZ and WSV
  
  #check for valid output types
  validOutTypes = c("RAZ", "WAZ", "WSV")
  if(!(output %in% validOutTypes)){
    stop(paste("In 'SlopeAdjust()', '",output, "' is an invalid 'output' type.", 
               sep=""))
  }
  result <- SlopeAdjust(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC,
                                     PC, PDF, CC, CBH, ISI)
  if (output=="WSV")
  {
    return(result$WSV)
  }
  return(result$RAZ)
  
}

#' @noRd
.TFCcalc <- function(FUELTYPE, CFL, CFB, SFC, PC, PDF, option = "TFC")
{
  CFC <- CrownFuelConsumption(FUELTYPE, CFL, CFB, PC, PDF)
  #Return CFC if requested
  if (option=="CFC")
  {
    .Deprecated("CrownFuelConsumption")
    return(CFC)
  }
  .Deprecated("TotalFuelConsumption")
  TFC <- TotalFuelConsumption(CFC, SFC)
  return(TFC)
}

#' @noRd
wDC <- function(DCf = 100, rw = 200, a = 0.75, b = 0.75)
{
  .Deprecated("OverwinterDroughtCode")
  return(OverwinterDroughtCode(DCf, rw, a, b))
}

#' @noRd
fireSeason <- function(input, fs.start = 12, fs.end = 5, method = "WF93", 
                       consistent.snow = FALSE, multi.year = FALSE)
{
  .Deprecated("FireSeason")
  return(FireSeason(input, fs.start, fs.end, method, consistent.snow, multi.year))
}

#' @noRd
pros <- function(input)
{
  .Deprecated("SimardRateOfSpreadPoint")
  return(SimardRateOfSpreadPoint(input))
}

#' @noRd
lros <- function(input)
{
  .Deprecated("SimardRateOfSpreadLine")
  return(SimardRateOfSpreadLine(input))
}

#' @noRd
fbp  <- function(input = NULL, output = "Primary", m = NULL, cores = 1)
{
  .Deprecated("FireBehaviourPrediction")
  #hack to avoid Note about no visible binding for global variable ID
  #http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
  #look at the globalvariables() option or others in place of this issue
  # do not remove this comment until resolved
  if (!is.na(charmatch("input", search()))) {
    detach(input)
  }
  fullList <- FireBehaviourPrediction(input, output = output)
  
  return(fullList)
}


#' @noRd
fwi <- function(input, init = data.frame(ffmc = 85, dmc = 6, dc = 15, lat = 55),
                batch = TRUE, out = "all", lat.adjust = TRUE, uppercase = TRUE)
{
  .Deprecated("FireWeatherIndices")
  return(FireWeatherIndices(input, init, batch, out, lat.adjust, uppercase))
}

#' @noRd
gfmc <- function(input, GFMCold = 85, batch = TRUE, time.step = 1, roFL = 0.3,
                 out = "GFMCandMC")
{
  .Deprecated("GrassFuelMoistureCode")
  return(GrassFuelMoistureCode(input, GFMCold, batch, time.step, roFL, out))
}

#' @noRd
hffmc <- function(weatherstream, ffmc_old = 85, time.step = 1,
                  calc.step = FALSE, batch = TRUE, hourlyFWI = FALSE)
{
  .Deprecated("HourlyFineFuelMoistureCode")
  return(HourlyFineFuelMoistureCode(weatherstream, ffmc_old, time.step,
                                    calc.step, batch, hourlyFWI))
}
