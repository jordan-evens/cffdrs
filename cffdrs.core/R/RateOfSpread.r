.FUELS <- list(C1=list(a=90, b=0.0649, c0=4.5),
               C2=list(a=110, b=0.0282, c0=1.5),
               C3=list(a=110, b=0.0444, c0=3.0),
               C4=list(a=110, b=0.0293, c0=1.5),
               C5=list(a=30, b=0.0697, c0=4.0),
               C6=list(a=30, b=0.0800, c0=3.0),
               C7=list(a=45, b=0.0305, c0=2.0),
               D1=list(a=30, b=0.0232, c0=1.6),
               M1=list(a=0, b=0, c0=0),
               M2=list(a=0, b=0, c0=0),
               M3=list(a=120, b=0.0572, c0=1.4),
               M4=list(a=100, b=0.0404, c0=1.48),
               S1=list(a=75, b=0.0297, c0=1.3),
               S2=list(a=40, b=0.0438, c0=1.7),
               S3=list(a=55, b=0.0829, c0=3.2),
               O1A=list(a=190, b=0.0310, c0=1.4),
               O1B=list(a=250, b=0.0350, c0=1.7))

.RateOfSpreadFunctions <- (function()
{
  rsiBase <- function(fuel, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
  {
    #Eq. 26 (FCFDG 1992) - Initial Rate of Spread for Conifer and Slash types
    return(as.numeric(fuel$a * (1 - exp(-fuel$b * ISI))**fuel$c0))
  }
  rsiM1 <- function(fuel, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
  {
    NoBUI <- rep(-1, length(ISI))
    #Eq. 27 (FCFDG 1992) - Initial Rate of Spread for M1 Mixedwood type
    RSI <- PC / 100 * .RateOfSpreadFunctions[["C2"]](ISI, NoBUI, FMC, SFC, PC, PDF, CC, CBH)
            + (100 - PC) / 100 * .RateOfSpreadFunctions[["D1"]](ISI, NoBUI, FMC, SFC, PC, PDF, CC, CBH)
    return(RSI)
  }
  rsiM2 <- function(fuel, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
  {
    NoBUI <- rep(-1, length(ISI))
    #Eq. 27 (FCFDG 1992) - Initial Rate of Spread for M2 Mixedwood type
    RSI <- PC / 100 * .RateOfSpreadFunctions[["C2"]](ISI, NoBUI, FMC, SFC, PC, PDF, CC, CBH)
      + 0.2 * ((100 - PC) / 100) * .RateOfSpreadFunctions[["D1"]](ISI, NoBUI, FMC, SFC, PC, PDF, CC, CBH)
    return(RSI)
  }
  rsiM3 <- function(fuel, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
  {
    NoBUI <- rep(-1, length(ISI))
    #Initial Rate of Spread for M3 Mixedwood
    #Eq. 30 (Wotton et. al 2009)
    RSI_m3 <- as.numeric(fuel$a * ((1 - exp(-fuel$b * ISI)) ** fuel$c0))
    #Eq. 29 (Wotton et. al 2009)
    RSI <- PDF / 100 * RSI_m3
      + (1 - PDF / 100) * .RateOfSpreadFunctions[["D1"]](ISI, NoBUI, FMC, SFC, PC, PDF, CC, CBH)
    return(RSI)
  }
  rsiM4 <- function(fuel, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
  {
    NoBUI <- rep(-1, length(ISI))
    #Initial Rate of Spread for M4 Mixedwood
    #Eq. 30 (Wotton et. al 2009)
    RSI_m4 <- as.numeric(fuel$a * ((1 - exp(-fuel$b * ISI))**fuel$c0))
    #Eq. 33 (Wotton et. al 2009)
    RSI <- PDF / 100* RSI_m4
      + 0.2 * (1 - PDF / 100)* .RateOfSpreadFunctions[["D1"]](ISI, NoBUI, FMC, SFC, PC, PDF, CC,CBH)
    return(RSI)
  }
  rsiO1 <- function(fuel, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
  {
    #Eq. 35b (Wotton et. al. 2009) - Calculate Curing function for grass
    CF <- ifelse(CC < 58.8,
                 0.005 * (exp(0.061 * CC) - 1),
                 0.176 + 0.02 * (CC - 58.8))
    #Eq. 36 (FCFDG 1992) - Calculate Initial Rate of Spread for Grass
    RSI <- fuel$a * ((1 - exp(-fuel$b * ISI))**fuel$c0) * CF
    return(RSI)
  }
  rosC6 <- function(ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
  {
    #Calculate C6 separately
    RSI <- IntermediateSurfaceRateOfSpreadC6(ISI, FMC)
    RSS <- SurfaceRateOfSpreadC6(RSI, BUI)
    RSC <- CrownRateOfSpreadC6(ISI, FMC)
    CSI <- CriticalSurfaceIntensity(FMC, CBH)
    RSO <- CriticalSurfaceRateOfSpread(CSI, SFC)
    CFB <- CrownFractionBurned(RSS, RSO)
    ROS <- RateOfSpreadC6(RSC, RSS, CFB)
    return(ROS)
  }
  makeFuelFct <- function(fuel, fct)
  {
    return(function(ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
    {
      RSI <- fct(.FUELS[[fuel]], ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
      ROS <- BuildupEffect(fuel, BUI) * RSI
      return(ROS)
    })
  }
  return(list(C1=makeFuelFct("C1", rsiBase),
              C2=makeFuelFct("C2", rsiBase),
              C3=makeFuelFct("C3", rsiBase),
              C4=makeFuelFct("C4", rsiBase),
              C5=makeFuelFct("C5", rsiBase),
              C6=rosC6,
              C7=makeFuelFct("C7", rsiBase),
              D1=makeFuelFct("D1", rsiBase),
              M1=makeFuelFct("M1", rsiM1),
              M2=makeFuelFct("M2", rsiM2),
              M3=makeFuelFct("M3", rsiM3),
              M4=makeFuelFct("M4", rsiM4),
              S1=makeFuelFct("S1", rsiBase),
              S2=makeFuelFct("S2", rsiBase),
              S3=makeFuelFct("S3", rsiBase),
              O1A=makeFuelFct("O1A", rsiO1),
              O1B=makeFuelFct("O1B", rsiO1)))
})()
#' Rate of Spread Calculation
#' 
#' 
#' Computes the Rate of Spread prediction based on fuel type and FWI
#' conditions. Equations are from listed FCFDG (1992) and Wotton et. al. 
#' (2009), and are marked as such.
#' 
#' All variables names are laid out in the same manner as Forestry Canada 
#' Fire Danger Group (FCFDG) (1992). Development and Structure of the 
#' Canadian Forest Fire Behavior Prediction System." Technical Report 
#' ST-X-3, Forestry Canada, Ottawa, Ontario.
#' 
#' Wotton, B.M., Alexander, M.E., Taylor, S.W. 2009. Updates and revisions to
#' the 1992 Canadian forest fire behavior prediction system. Nat. Resour. 
#' Can., Can. For. Serv., Great Lakes For. Cent., Sault Ste. Marie, Ontario, 
#' Canada. Information Report GLC-X-10, 45p.
#' 
#' @param FUELTYPE    The Fire Behaviour Prediction FuelType
#' @param ISI         Intiial Spread Index 
#' @param BUI         Buildup Index 
#' @param FMC         Foliar Moisture Content 
#' @param SFC         Surface Fuel Consumption (kg/m^2) 
#' @param PC          Percent Conifer (%)
#' @param PDF         Percent Dead Balsam Fir (%) 
#' @param CC          Constant
#' @param CBH         Crown to base height(m) 
#' 
#' @returns ROS - Rate of Spread (m/min) value
#' 
#' @export RateOfSpread
RateOfSpread <- function(FUELTYPE, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH){
  fct <- function(v)
  {
    return(.RateOfSpreadFunctions[[v["FUELTYPE"]]](
      as.numeric(v["ISI"]),
      as.numeric(v["BUI"]),
      as.numeric(v["FMC"]),
      as.numeric(v["SFC"]),
      as.numeric(v["PC"]),
      as.numeric(v["PDF"]),
      as.numeric(v["CC"]),
      as.numeric(v["CBH"])))
  }
  ROS <- apply(data.frame(FUELTYPE, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH), MARGIN=1, FUN=fct)
  stopifnot(length(ROS) == length(ISI))
  #add a constraint
  ROS <- ifelse(ROS <= 0,0.000001,ROS)
  return(ROS)
}
