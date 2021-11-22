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

  #Set up some data vectors
  NoBUI <- rep(-1,length(ISI))
  FUELS <- list(C1=list(a=90, b=0.0649, c0=4.5),
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
  fuel <- FUELS[FUELTYPE][[1]]

  #Calculate RSI (set up data vectors first)
  #Eq. 26 (FCFDG 1992) - Initial Rate of Spread for Conifer and Slash types
  RSI <- rep(-1,length(ISI))
  RSI <- ifelse(FUELTYPE %in% c("C1", "C2", "C3", "C4", "C5", "C7", "D1", "S1", 
                                "S2", "S3"),
          as.numeric(fuel$a * (1 - exp(-fuel$b * ISI))**fuel$c0),
          RSI)
  #Eq. 27 (FCFDG 1992) - Initial Rate of Spread for M1 Mixedwood type
  RSI <- ifelse(FUELTYPE %in% c("M1"), 
            PC/100 * 
              RateOfSpread(rep("C2", length(ISI)),ISI,NoBUI,FMC,SFC,PC,PDF,CC,CBH)
              + (100 - PC) / 100 *
              RateOfSpread(rep("D1", length(ISI)),ISI,NoBUI,FMC,SFC,PC,PDF,CC, CBH),
            RSI)
  #Eq. 27 (FCFDG 1992) - Initial Rate of Spread for M2 Mixedwood type
  RSI <- ifelse(FUELTYPE %in% c("M2"), 
            PC/100 * 
              RateOfSpread(rep("C2", length(ISI)),ISI,NoBUI,FMC,SFC,PC,PDF,CC,CBH)
              + 0.2*(100-PC)/100 *
              RateOfSpread(rep("D1", length(ISI)),ISI,NoBUI,FMC,SFC,PC,PDF,CC, CBH),
            RSI)
  #Initial Rate of Spread for M3 Mixedwood
  RSI_m3 <- rep(-99,length(ISI))
  #Eq. 30 (Wotton et. al 2009)
  RSI_m3 <- 
    ifelse(FUELTYPE %in% c("M3"), 
      as.numeric(fuel$a * ((1 - exp(-fuel$b * ISI))**fuel$c0)), RSI_m3)
  #Eq. 29 (Wotton et. al 2009)
  RSI <- 
    ifelse(FUELTYPE %in% c("M3"),
      PDF/100* RSI_m3 + (1-PDF/100) * 
        RateOfSpread(rep("D1", length(ISI)), ISI, NoBUI, FMC, SFC, PC, PDF, CC,CBH),
      RSI)
  #Initial Rate of Spread for M4 Mixedwood
  RSI_m4 <- rep(-99,length(ISI))
  #Eq. 30 (Wotton et. al 2009)  
  RSI_m4 <- 
    ifelse(FUELTYPE %in% c("M4"), 
      as.numeric(fuel$a * ((1 - exp(-fuel$b * ISI))**fuel$c0)), RSI_m4)
  #Eq. 33 (Wotton et. al 2009)
  RSI <- 
    ifelse(FUELTYPE %in% c("M4"),
      PDF / 100* RSI_m4 + 0.2 * (1 - PDF / 100)* 
        RateOfSpread(rep("D1", length(ISI)), ISI, NoBUI, FMC, SFC, PC, PDF, CC,CBH),
      RSI)
  #Eq. 35b (Wotton et. al. 2009) - Calculate Curing function for grass
  CF <- rep(-99,length(ISI))
  CF <- 
    ifelse(FUELTYPE %in% c("O1A", "O1B"),
           ifelse(CC < 58.8, 
                  0.005 * (exp(0.061 * CC) - 1), 
                  0.176 + 0.02 * (CC - 58.8)), 
           CF)
  #Eq. 36 (FCFDG 1992) - Calculate Initial Rate of Spread for Grass
  RSI <- 
    ifelse(FUELTYPE %in% c("O1A", "O1B"),
           fuel$a * ((1 - exp(-fuel$b * ISI))**fuel$c0) * CF,
    RSI)
  #Calculate C6 separately
  isC6 <- FUELTYPE %in% c("C6")
  RSI <- ifelse(isC6, IntermediateSurfaceRateOfSpreadC6(ISI, FMC), rep(0, length(ISI)))
  RSS <- ifelse(isC6, SurfaceRateOfSpreadC6(RSI, BUI), rep(0, length(ISI)))
  RSC <- ifelse(isC6, CrownRateOfSpreadC6(ISI, FMC), rep(0, length(ISI)))
  CSI <- ifelse(isC6, CriticalSurfaceIntensity(FMC, CBH), rep(0, length(ISI)))
  RSO <- ifelse(isC6, CriticalSurfaceRateOfSpread(CSI, SFC), rep(0, length(ISI)))
  CFB <- ifelse(isC6, CrownFractionBurned(RSS, RSO), rep(0, length(ISI)))
  ROS <- ifelse(isC6,
                RateOfSpreadC6(RSC, RSS, CFB),
                BuildupEffect(FUELTYPE, BUI) * RSI)
  #add a constraint
  ROS <- ifelse(ROS <= 0,0.000001,ROS)
  return(ROS)
}
