.FuelNF <- structure(.Data=list(),
                     class=c(".FuelNF", ".FuelBase")
)
.WA <- structure(.Data=list(name="WA",
                            CFL=0),
                     class=c(".WA", ".FuelNF", ".FuelBase")
)
.NF <- structure(.Data=list(name="NF",
                            CFL=0),
                     class=c(".NF", ".FuelNF", ".FuelBase")
)
#.Alpha..FuelNF <- function(this, CBH) { return(0) }
#.BackRateOfSpread..FuelNF <- function(this, FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH) { return(0) }
.BaseRateOfSpread..FuelNF <- function(this, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH) { return(0) }
.BuildupEffect..FuelNF <- function(this, BUI) {
  # keep old behaviour
  if (BUI <= 0)
  {
    return(1)
  }
  return(as.numeric(NA))
}
.CrownBaseHeight..FuelNF <- function(this, CBH, SD, SH)
{
  CBH <- ifelse(CBH <= 0 | CBH > 50 | is.na(CBH), NA, CBH)
  return(CBH)
}
# .CrownFractionBurned..FuelNF <- function(this, ROS, RSO)
# {
#   #Eq. 58 (FCFDG 1992) Crown fraction burned 
#   CFB <- ifelse(ROS > RSO, 1 - exp(-0.23 * (ROS - RSO)), 0)
#   return(CFB)
# }
#.DistanceAtTime..FuelNF <- function(this, ROSeq, HR, CFB) { return(0) }
.FireBehaviourPrediction..FuelNF <- function(this, output, ID, HR, LAT, LONG, CBH, SD, SH, CFL, FMC, D0, ELV, DJ, WS, WAZ, SAZ, FFMC, ISI, BUI, PC, PDF, GFL, BUIEFF, GS, CC, ACCEL, THETA) { return(0) }
#.LengthToBreadthRatio..FuelNF <- function(this, WSV) { return(0) }
#.LengthToBreadthRatioAtTime..FuelNF <- function(this, LB, HR, CFB) { return(1.0) }
#.RateOfSpread..FuelNF <- function(this, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH) { return(0) }
# HACK: return old value instead of logical one
#.RateOfSpreadAtTime..FuelNF <- function(this, ROSeq, HR, CFB) { return(0) }
#.SlopeAdjust..FuelNF <- function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI) { return(list(WSV=as.numeric(NA), RAZ=as.numeric(NA))) }
.SlopeEquivalentInitialSpreadIndex..FuelNF <- function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
{
  stopifnot(this$name %in% c('NF', 'WA'))
  # NOTE: keep old behaviour
  return(-99.0)
}
# .SlopeEquivalentWindSpeed..FuelNF <- function()
# {
#   ifelse(ROS <= 0,0.000001,ROS)
# }
.SurfaceFuelConsumptionBase..FuelNF <- function(this, FFMC, BUI, PC, GFL) { return(0) }
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
    FBP <- list(ID=ID, CFB=CFB, CFC=CFC, FD="NA", HFI=HFI, RAZ=RAZ, ROS=ROS, SFC=SFC, 
                TFC=SFC)
  }
  #If Secondary is selected, wrap the secondary outputs into a data frame
  #  and return them.
  else if (output == "SECONDARY" | output == "S") {
    FBP <- list(ID=ID, BE=BE, SF=SF, ISI=ISI, FFMC=FFMC, FMC=FMC, D0=D0, RSO=RSO,
                CSI=CSI, FROS=FROS, BROS=BROS, HROSt=HROSt, FROSt=FROSt, BROSt=BROSt, FCFB=FCFB, BCFB=BCFB,
                FFI=FFI, BFI=BFI, FTFC=FTFC, BTFC=BTFC, TI=TI, FTI=FTI, BTI=BTI, LB=LB, LBt=LBt, WSV=WSV,
                DH=DH, DB=DB, DF=DF, TROS=TROS, TROSt=TROSt, TCFB=TCFB, TFI=TFI, TTFC=TTFC, TTI=TTI)
  }
  #If all outputs are selected, then wrap all outputs into a data frame and
  #  return it.
  else if (output == "ALL" | output == "A") {
    FBP <- list(ID=ID, CFB=CFB, CFC=CFC, FD="NA", HFI=HFI, RAZ=RAZ, ROS=ROS, SFC=SFC,
                TFC=TFC, BE=BE, SF=SF, ISI=ISI, FFMC=FFMC, FMC=FMC, D0=D0, RSO=RSO, CSI=CSI, FROS=FROS,
                BROS=BROS, HROSt=HROSt, FROSt=FROSt, BROSt=BROSt, FCFB=FCFB, BCFB=BCFB, FFI=FFI, BFI=BFI,
                FTFC=FTFC, BTFC=BTFC, TI=TI, FTI=FTI, BTI=BTI, LB=LB, LBt=LBt, WSV=WSV, DH=DH, DB=DB, DF=DF,
                TROS=TROS, TROSt=TROSt, TCFB=TCFB, TFI=TFI, TTFC=TTFC, TTI=TTI)
  }
  return(list(FBP))
}
.FuelNF$.BaseRateOfSpread <- .BaseRateOfSpread..FuelNF
.FuelNF$.BuildupEffect <- .BuildupEffect..FuelNF
.FuelNF$.CrownBaseHeight <- .CrownBaseHeight..FuelNF
.FuelNF$.FireBehaviourPrediction <- .FireBehaviourPrediction..FuelNF
.FuelNF$.SlopeEquivalentInitialSpreadIndex <- .SlopeEquivalentInitialSpreadIndex..FuelNF
.FuelNF$.SurfaceFuelConsumptionBase <- .SurfaceFuelConsumptionBase..FuelNF
.FuelNF$.FireBehaviourPrediction <- .FireBehaviourPrediction..FuelNF
