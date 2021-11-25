.FuelWA <- structure(.Data=list(name="WA",
                                a=as.numeric(NA),
                                b=as.numeric(NA),
                                c0=as.numeric(NA),
                                BUIo=as.numeric(NA),
                                Q=as.numeric(NA),
                                sfcA=as.numeric(NA),
                                sfcB=as.numeric(NA),
                                sfcC=as.numeric(NA),
                                sfcD=as.numeric(NA),
                                CBH=as.numeric(NA),
                                CFL=as.numeric(NA)),
                     class=c(".FuelNF", ".FuelBase")
)
.FuelNF <- structure(.Data=list(name="NF",
                                a=as.numeric(NA),
                                b=as.numeric(NA),
                                c0=as.numeric(NA),
                                BUIo=as.numeric(NA),
                                Q=as.numeric(NA),
                                sfcA=as.numeric(NA),
                                sfcB=as.numeric(NA),
                                sfcC=as.numeric(NA),
                                sfcD=as.numeric(NA),
                                CBH=as.numeric(NA),
                                CFL=as.numeric(NA)),
                     class=c(".FuelNF", ".FuelBase")
)
.Alpha..FuelNF <- function(this, CBH) { return(0) }
.BackRateOfSpread..FuelNF <- function(this, FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH) { return(0) }
.BuildupEffect..FuelNF <- function(this, BUI) { return(0) }
.CriticalSurfaceIntensity..FuelNF <- function(this, FMC, CBH) { return(0) }
.CrownBaseHeight..FuelNF <- function(this, CBH, SD, SH) { return(0) }
.CrownFuelConsumption..FuelNF <- function(this, CFL, CFB, PC, PDF) { return(0) }
.DistanceAtTime..FuelNF <- function(this, ROSeq, HR, CFB) { return(0) }
.FireBehaviourPrediction..FuelNF <- function(this, output, ID, HR, LAT, LONG, CBH, SD, SH, CFL, FMC, D0, ELV, DJ, WS, WAZ, SAZ, FFMC, ISI, BUI, PC, PDF, GFL, BUIEFF, GS, CC, ACCEL, THETA) { return(0) }
.FoliarMoistureContent..FuelNF <- function(this, LAT, LONG, ELV, DJ, D0) { return(0) }
.LengthToBreadthRatio..FuelNF <- function(this, WSV) { return(0) }
.LengthToBreadthRatioAtTime..FuelNF <- function(this, LB, HR, CFB) { return(0) }
.RateOfSpread..FuelNF <- function(this, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH) { return(0) }
.RateOfSpreadAtTime..FuelNF <- function(this, ROSeq, HR, CFB) { return(0) }
.SlopeAdjust..FuelNF <- function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI) { return(0) }
.SurfaceFuelConsumption..FuelNF <- function(this, FFMC, BUI, PC, GFL) { return(0) }
.FireBehaviourPrediction..FuelNF <- function(this, output, ID, HR, LAT, LONG, CBH, SD, SH, CFL, FMC, D0, ELV, DJ, WS, WAZ, SAZ, FFMC, ISI, BUI, PC, PDF, GFL, BUIEFF, GS, CC, ACCEL, THETA)
{
  ID <- CFB <- CFC <- FD <- HFI <- RAZ <- ROS <- SFC <- 
    TFC <- BE <- SF <- ISI <- FFMC <- FMC <- D0 <- RSO <- CSI <- FROS <- 
    BROS <- HROSt <- FROSt <- BROSt <- FCFB <- BCFB <- FFI <- BFI <- 
    FTFC <- BTFC <- TI <- FTI <- BTI <- LB <- LBt <- WSV <- DH <- DB <- DF <- 
    TROS <- TROSt <- TCFB <- TFI <- TTFC <- TTI <- 0
  #if Primary is selected, wrap the primary outputs into a data frame and
  #  return them
  if (output == "PRIMARY" | output == "P") {
    FBP <- data.frame(ID, CFB, CFC, FD, HFI, RAZ, ROS, SFC, 
                      TFC)
    FBP[, "FD"] <- "NA"
  }
  #If Secondary is selected, wrap the secondary outputs into a data frame
  #  and return them.
  else if (output == "SECONDARY" | output == "S") {
    FBP <- data.frame(ID, BE, SF, ISI, FFMC, FMC, D0, RSO, 
                      CSI, FROS, BROS, HROSt, FROSt, BROSt, FCFB, BCFB, 
                      FFI, BFI, FTFC, BTFC, TI, FTI, BTI, LB, LBt, WSV, 
                      DH, DB, DF, TROS, TROSt, TCFB, TFI, TTFC, TTI)
  }
  #If all outputs are selected, then wrap all outputs into a data frame and
  #  return it.
  else if (output == "ALL" | output == "A") {
    FBP <- data.frame(ID, CFB, CFC, FD, HFI, RAZ, ROS, SFC, 
                      TFC, BE, SF, ISI, FFMC, FMC, D0, RSO, CSI, FROS, 
                      BROS, HROSt, FROSt, BROSt, FCFB, BCFB, FFI, BFI, 
                      FTFC, BTFC, TI, FTI, BTI, LB, LBt, WSV, DH, DB, DF, 
                      TROS, TROSt, TCFB, TFI, TTFC, TTI)
    FBP[, "FD"] <- "NA"
  }
  return(list(FBP))
}
