.FuelBase <- structure(.Data=c(name=".FuelBase"), class=(".FuelBase"))
.Alpha <- function(this, ...) UseMethod(".Alpha", this)
.BackRateOfSpread <- function(this, FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH) UseMethod(".BackRateOfSpread", this)
.BuildupEffect <- function(this, BUI) UseMethod(".BuildupEffect", this)
.CriticalSurfaceIntensity <- function(this, FMC, CBH) UseMethod(".CriticalSurfaceIntensity", this)
.CrownBaseHeight <- function(this, CBH, SD, SH) UseMethod(".CrownBaseHeight", this)
.CrownFuelConsumption <- function(this, CFL, CFB, PC, PDF) UseMethod(".CrownFuelConsumption", this)
.DistanceAtTime <- function(this, ROSeq, HR, CFB) UseMethod(".DistanceAtTime", this)
.FireBehaviourPrediction <- function(this, output, ID, HR, LAT, LONG, CBH, SD, SH, CFL, FMC, D0, ELV, DJ, WS, WAZ, SAZ, FFMC, ISI, BUI, PC, PDF, GFL, BUIEFF, GS, CC, ACCEL, THETA) UseMethod(".FireBehaviourPrediction", this)
.FoliarMoistureContent <- function(this, LAT, LONG, ELV, DJ, D0) UseMethod(".FoliarMoistureContent", this)
.LengthToBreadthRatio <- function(this, WSV) UseMethod(".LengthToBreadthRatio", this)
.LengthToBreadthRatioAtTime <- function(this, LB, HR, CFB) UseMethod(".LengthToBreadthRatioAtTime", this)
.RateOfSpread <- function(this, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH) UseMethod(".RateOfSpread", this)
.RateOfSpreadAtTime <- function(this, ROSeq, HR, CFB) UseMethod(".RateOfSpreadAtTime", this)
.SlopeAdjust <- function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI) UseMethod(".SlopeAdjust", this)
.SurfaceFuelConsumption <- function(this, FFMC, BUI, PC, GFL) UseMethod(".SurfaceFuelConsumption", this)
Fuel <- structure(.Data=list(
                   name="Fuel",
                   a=as.numeric(NA),
                   c0=as.numeric(NA),
                   BUIo=as.numeric(NA),
                   Q=as.numeric(NA),
                   sfcA=as.numeric(NA),
                   sfcB=as.numeric(NA),
                   sfcC=as.numeric(NA),
                   sfcD=as.numeric(NA),
                   CBH=as.numeric(NA),
                   CFL=as.numeric(NA)
                 ),
                 class=c("Fuel", ".FuelBase")
)
.CrownBaseHeight.Fuel <- function(this, CBH, SD, SH)
{
  CBH <- ifelse(CBH <= 0 | CBH > 50 | is.na(CBH),
                ifelse((this[["name"]] == "C6") & SD > 0 & SH > 0,
                       -11.2 + 1.06 * SH + 0.0017 * SD,
                       this[["CBH"]]),
                CBH)
  CBH <- ifelse(CBH < 0, 1e-07, CBH)
  return(CBH)
}
.FoliarMoistureContent.Fuel <- function(this, LAT, LONG, ELV, DJ, D0)
{
  return(FoliarMoistureContent(LAT, LONG, ELV, DJ, D0))
}
.FuelClosed <- structure(.Data=c(name=".FuelClosed"), class=c(".FuelClosed", "Fuel", ".FuelBase"))
.FuelOpen <- structure(.Data=c(name=".FuelOpen"), class=c(".FuelOpen", "Fuel", ".FuelBase"))

.Alpha..FuelClosed <- function(this, CBH)
{
  #Eq. 72 (FCFDG 1992)
  #Calculate the alpha constant for the DISTt calculation
  alpha <- 0.115 - 18.8 * (CBH**2.5) * exp(-8* CBH)
  return (alpha)
}
.Alpha..FuelOpen <- function(this, CBH)
{
  #Eq. 72 (FCFDG 1992)
  #Calculate the alpha constant for the DISTt calculation
  alpha <- 0.115
  return (alpha)
}
.FoliarMoistureContent.FuelOpen <- function(this, LAT, LONG, ELV, DJ, D0)
{
  return(0)
}

.FuelGrass <- structure(.Data=c(name=".FuelGrass"), class=c(".FuelGrass", ".FuelOpen", "Fuel", ".FuelBase"))
.FuelSlash <- structure(.Data=c(name=".FuelSlash"), class=c(".FuelSlash", ".FuelOpen", "Fuel", ".FuelBase"))
.FuelMixedwood <- structure(.Data=c(name=".FuelMixedwood"), class=c(".FuelMixedwood", ".FuelClosed", "Fuel", ".FuelBase"))
.FuelMixedDead <- structure(.Data=c(name=".FuelMixedDead"), class=c(".FuelMixedDead", ".FuelClosed", "Fuel", ".FuelBase"))
