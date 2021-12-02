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
#' 
#' @noRd
#'


.ROScalc <- function(FUELTYPE, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH){
  
  #Set up some data vectors
  NoBUI <- rep(-1,length(ISI))
  d <- c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "D1", "M1", "M2", "M3", "M4",
         "S1", "S2", "S3", "O1A", "O1B")
  a <- c(90, 110, 110, 110, 30, 30, 45, 30, 0, 0, 120, 100, 75, 40, 55, 190, 
         250)
  b <- c(0.0649, 0.0282, 0.0444, 0.0293, 0.0697, 0.0800, 0.0305, 0.0232, 0, 0, 
         0.0572, 0.0404, 0.0297, 0.0438, 0.0829, 0.0310, 0.0350)
  c0 <- c(4.5, 1.5, 3.0, 1.5, 4.0, 3.0, 2.0, 1.6, 0, 0, 1.4, 1.48, 1.3, 1.7, 
          3.2, 1.4, 1.7)
  names(a) <- names(b) <- names(c0) <- d
  
  #Calculate RSI (set up data vectors first)
  #Eq. 26 (FCFDG 1992) - Initial Rate of Spread for Conifer and Slash types
  RSI <- rep(-1,length(ISI))
  RSI <- ifelse(FUELTYPE %in% c("C1", "C2", "C3", "C4", "C5", "C7", "D1", "S1", 
                                "S2", "S3"),
                as.numeric(a[FUELTYPE] * (1 - exp(-b[FUELTYPE] * ISI))**c0[FUELTYPE]),
                RSI)
  #Eq. 27 (FCFDG 1992) - Initial Rate of Spread for M1 Mixedwood type
  RSI <- ifelse(FUELTYPE %in% c("M1"), 
                PC/100 * 
                  .ROScalc(rep("C2", length(ISI)),ISI,NoBUI,FMC,SFC,PC,PDF,CC,CBH)
                + (100 - PC) / 100 *
                  .ROScalc(rep("D1", length(ISI)),ISI,NoBUI,FMC,SFC,PC,PDF,CC, CBH),
                RSI)
  #Eq. 27 (FCFDG 1992) - Initial Rate of Spread for M2 Mixedwood type
  RSI <- ifelse(FUELTYPE %in% c("M2"), 
                PC/100 * 
                  .ROScalc(rep("C2", length(ISI)),ISI,NoBUI,FMC,SFC,PC,PDF,CC,CBH)
                + 0.2*(100-PC)/100 *
                  .ROScalc(rep("D1", length(ISI)),ISI,NoBUI,FMC,SFC,PC,PDF,CC, CBH),
                RSI)
  #Initial Rate of Spread for M3 Mixedwood
  RSI_m3 <- rep(-99,length(ISI))
  #Eq. 30 (Wotton et. al 2009)
  RSI_m3 <- 
    ifelse(FUELTYPE %in% c("M3"), 
           as.numeric(a[["M3"]] * ((1 - exp(-b[["M3"]] * ISI))**c0[["M3"]])), RSI_m3)
  #Eq. 29 (Wotton et. al 2009)
  RSI <- 
    ifelse(FUELTYPE %in% c("M3"),
           PDF/100* RSI_m3 + (1-PDF/100) * 
             .ROScalc(rep("D1", length(ISI)), ISI, NoBUI, FMC, SFC, PC, PDF, CC,CBH),
           RSI)
  #Initial Rate of Spread for M4 Mixedwood
  RSI_m4 <- rep(-99,length(ISI))
  #Eq. 30 (Wotton et. al 2009)  
  RSI_m4 <- 
    ifelse(FUELTYPE %in% c("M4"), 
           as.numeric(a[["M4"]] * ((1 - exp(-b[["M4"]] * ISI))**c0[["M4"]])), RSI_m4)
  #Eq. 33 (Wotton et. al 2009)
  RSI <- 
    ifelse(FUELTYPE %in% c("M4"),
           PDF / 100* RSI_m4 + 0.2 * (1 - PDF / 100)* 
             .ROScalc(rep("D1", length(ISI)), ISI, NoBUI, FMC, SFC, PC, PDF, CC,CBH),
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
           a[FUELTYPE] * ((1 - exp(-b[FUELTYPE] * ISI))**c0[FUELTYPE]) * CF,
           RSI)
  #Calculate C6 separately
  ROS <- 
    ifelse(FUELTYPE %in% c("C6"),
           .C6calc(FUELTYPE, ISI, BUI, FMC, SFC, CBH, option = "ROS"),
           .BEcalc(FUELTYPE, BUI) * RSI)
  #add a constraint
  ROS <- ifelse(ROS <= 0,0.000001,ROS)
  return(ROS)
}
.ISIcalc <- function(ffmc, ws, fbpMod=FALSE){
  
  #Eq. 10 - Moisture content
  fm <- 147.2 * (101 - ffmc)/(59.5 + ffmc)
  #Eq. 24 - Wind Effect
  #the ifelse, also takes care of the ISI modification for the fbp functions
  # This modification is Equation 53a in FCFDG (1992)
  fW   <- ifelse(ws >= 40 & fbpMod==TRUE,
                 12 * (1 - exp(-0.0818 * (ws - 28))), 
                 exp(0.05039 * ws))
  #Eq. 25 - Fine Fuel Moisture
  fF <- 91.9 * exp(-0.1386 * fm) * (1 + (fm^5.31) / 49300000)
  #Eq. 26 - Spread Index Equation
  isi <- 0.208 * fW * fF
  return(isi)
}

.Slopecalc <- function(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
                       CC, CBH, ISI, output = "RAZ") {
  # output options include: RAZ and WSV
  
  #check for valid output types
  validOutTypes = c("RAZ", "WAZ", "WSV")
  if(!(output %in% validOutTypes)){
    stop(paste("In 'slopecalc()', '",output, "' is an invalid 'output' type.", 
               sep=""))
  }
  
  NoBUI <- rep(-1,length(FFMC))
  #Eq. 39 (FCFDG 1992) - Calculate Spread Factor
  SF <- ifelse (GS >= 70, 10, exp(3.533 * (GS / 100)^1.2))
  #ISI with 0 wind on level grounds
  ISZ <- .ISIcalc(FFMC, 0)
  #Surface spread rate with 0 wind on level ground
  RSZ <- .ROScalc(FUELTYPE, ISZ, BUI = NoBUI, FMC, SFC, PC, PDF, CC, CBH)
  #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope
  RSF <- RSZ * SF
  #setup some reference vectors
  d <- c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "D1", "M1", "M2", "M3", "M4",
         "S1", "S2", "S3", "O1A", "O1B")
  a <- c(90, 110, 110, 110, 30, 30, 45, 30, 0, 0, 120, 100, 75, 40, 55, 190, 
         250)
  b <- c(0.0649, 0.0282, 0.0444, 0.0293, 0.0697, 0.0800, 0.0305, 0.0232, 0, 0, 
         0.0572, 0.0404, 0.0297, 0.0438, 0.0829, 0.0310, 0.0350)
  c0 <- c(4.5, 1.5, 3.0, 1.5, 4.0, 3.0, 2.0, 1.6, 0, 0, 1.4, 1.48, 1.3, 1.7, 
          3.2, 1.4, 1.7)
  names(a) <- names(b) <- names(c0) <- d
  
  #initialize some local vars
  RSZ <- rep(-99,length(FFMC))
  RSF_C2 <- rep(-99,length(FFMC))
  RSF_D1 <- rep(-99,length(FFMC))
  RSF_M3 <- rep(-99,length(FFMC))
  RSF_M4 <- rep(-99,length(FFMC))
  CF <- rep(-99,length(FFMC))
  ISF <- rep(-99,length(FFMC))
  ISF_C2 <- rep(-99,length(FFMC))
  ISF_D1 <- rep(-99,length(FFMC))
  ISF_M3 <- rep(-99,length(FFMC))
  ISF_M4 <- rep(-99,length(FFMC))
  
  #Eqs. 41a, 41b (Wotton 2009) - Calculate the slope equivalend ISI
  ISF <- ifelse(FUELTYPE %in% c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "D1", 
                                "S1", "S2", "S3"),
                ifelse((1 - (RSF / a[FUELTYPE])**(1 / c0[FUELTYPE])) >= 0.01,
                       log(1 - (RSF / a[FUELTYPE])**(1 / c0[FUELTYPE])) / (-b[FUELTYPE]),
                       log(0.01)/(-b[FUELTYPE])),
                ISF)
  
  #When calculating the M1/M2 types, we are going to calculate for both C2
  # and D1 types, and combine
  #Surface spread rate with 0 wind on level ground
  RSZ <- ifelse(FUELTYPE %in% c("M1", "M2"),
                .ROScalc(rep("C2", length(ISZ)), ISZ, BUI = NoBUI, FMC, SFC, PC, PDF, 
                         CC, CBH),
                RSZ)
  #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for C2
  RSF_C2 <- ifelse(FUELTYPE %in% c("M1", "M2"), RSZ * SF, RSF_C2)
  RSZ <- ifelse(FUELTYPE %in% c("M1", "M2"),
                .ROScalc(rep("D1", length(ISZ)), ISZ, BUI = NoBUI, FMC, SFC, PC, 
                         PDF, CC, CBH),RSZ)
  #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for D1
  RSF_D1 <- ifelse(FUELTYPE %in% c("M1", "M2"), RSZ * SF, RSF_D1)
  RSF0 <- 1 - (RSF_C2 / a[["C2"]])^(1 / c0[["C2"]])
  #Eq. 41a (Wotton 2009) - Calculate the slope equivalent ISI
  ISF_C2 <- ifelse(FUELTYPE %in% c("M1", "M2") & RSF0 >= 0.01,
                   log(1 - (RSF_C2 / a[["C2"]])**(1 / c0[["C2"]])) / (-b[["C2"]]), 
                   ISF_C2)
  #Eq. 41b (Wotton 2009) - Calculate the slope equivalent ISI
  ISF_C2 <- ifelse(FUELTYPE %in% c("M1", "M2") & RSF0 < 0.01,
                   log(0.01) / (-b[["C2"]]),
                   ISF_C2)
  RSF0 <- 1 - (RSF_D1 / a[["D1"]])^(1 / c0[["D1"]])
  #Eq. 41a (Wotton 2009) - Calculate the slope equivalent ISI
  ISF_D1 <- ifelse(FUELTYPE %in% c("M1", "M2") & RSF0 >= 0.01,
                   log(1 - (RSF_D1 / a[["D1"]])**(1 / c0[["D1"]])) / (-b[["D1"]]),
                   ISF_D1)
  #Eq. 41b (Wotton 2009) - Calculate the slope equivalent ISI
  ISF_D1 <- ifelse(FUELTYPE %in% c("M1", "M2") & RSF0 < 0.01,
                   log(0.01) / (-b[["D1"]]),
                   ISF_D1)
  #Eq. 42a (Wotton 2009) - Calculate weighted average for the M1/M2 types
  ISF <- ifelse(FUELTYPE %in% c("M1", "M2"), PC / 100 * ISF_C2 + 
                  (1 - PC / 100) * ISF_D1, 
                ISF)
  
  #Set % Dead Balsam Fir to 100%
  PDF100 <- rep(100, length(ISI))
  #Surface spread rate with 0 wind on level ground
  RSZ <- ifelse(FUELTYPE %in% c("M3"), 
                .ROScalc(rep("M3", length(FMC)), ISI = ISZ, BUI = NoBUI, FMC, SFC, 
                         PC, PDF100, CC, CBH), 
                RSZ)
  #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for M3
  RSF_M3 <- ifelse(FUELTYPE %in% c("M3"), RSZ * SF, RSF_M3)
  #Surface spread rate with 0 wind on level ground, using D1
  RSZ <- ifelse(FUELTYPE %in% c("M3"), 
                .ROScalc(rep("D1", length(ISZ)), ISZ, BUI = NoBUI, FMC, SFC, PC, 
                         PDF100, CC, CBH), 
                RSZ)
  #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for M3
  RSF_D1 <- ifelse(FUELTYPE %in% c("M3"), RSZ * SF, RSF_D1)
  RSF0 <- 1 - (RSF_M3 / a[["M3"]])^(1 / c0[["M3"]])
  #Eq. 41a (Wotton 2009) - Calculate the slope equivalent ISI
  ISF_M3 <- ifelse(FUELTYPE %in% c("M3") & RSF0 >= 0.01,
                   log(1 - (RSF_M3/a[["M3"]])**(1/c0[["M3"]]))/(-b[["M3"]]),ISF_M3)
  #Eq. 41b (Wotton 2009) - Calculate the slope equivalent ISI
  ISF_M3 <- ifelse(FUELTYPE %in% c("M3") & RSF0 < 0.01,
                   log(0.01) / (-b[["M3"]]),
                   ISF_M3)
  #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for D1
  RSF0 <- 1 - (RSF_D1 / a[["D1"]])^(1 / c0[["D1"]])
  #Eq. 41a (Wotton 2009) - Calculate the slope equivalent ISI
  ISF_D1 <- ifelse(FUELTYPE %in% c("M3") & RSF0 >= 0.01,
                   log(1 - (RSF_D1 / a[["D1"]])**(1 / c0[["D1"]])) / (-b[["D1"]]),
                   ISF_D1)
  #Eq. 41b (Wotton 2009) - Calculate the slope equivalent ISI
  ISF_D1 <- ifelse(FUELTYPE %in% c("M3") & RSF0 < 0.01,
                   log(0.01) / (-b[["D1"]]),
                   ISF_D1)
  #Eq. 42b (Wotton 2009) - Calculate weighted average for the M3 type
  ISF <- ifelse(FUELTYPE %in% c("M3"), 
                PDF / 100 * ISF_M3 + (1 - PDF / 100) * ISF_D1, 
                ISF)
  #Surface spread rate with 0 wind on level ground, using M4
  RSZ <- ifelse(FUELTYPE %in% c("M4"), 
                .ROScalc(rep("M4", length(FMC)), ISI = ISZ, BUI = NoBUI, FMC, SFC, 
                         PC, PDF100, CC, CBH), 
                RSZ)
  #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for M4
  RSF_M4 <- ifelse(FUELTYPE %in% c("M4"), RSZ * SF, RSF_M4)
  #Surface spread rate with 0 wind on level ground, using M4
  RSZ <- ifelse(FUELTYPE %in% c("M4"), 
                .ROScalc(rep("D1", length(ISZ)), ISZ, BUI = NoBUI, FMC, SFC, PC, 
                         PDF100, CC, CBH), 
                RSZ)
  #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for D1
  RSF_D1 <- ifelse(FUELTYPE %in% c("M4"), RSZ * SF,RSF_D1)
  #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for D1
  RSF0 <- 1 - (RSF_M4 / a[["M4"]])^(1 / c0[["M4"]])
  #Eq. 41a (Wotton 2009) - Calculate the slope equivalent ISI
  ISF_M4 <- ifelse(FUELTYPE %in% c("M4") & RSF0 >= 0.01,
                   log(1 - (RSF_M4 / a[["M4"]])**(1 / c0[["M4"]])) / (-b[["M4"]]),
                   ISF_M4)
  #Eq. 41b (Wotton 2009) - Calculate the slope equivalent ISI
  ISF_M4 <- ifelse(FUELTYPE %in% c("M4") & RSF0 < 0.01,
                   log(0.01) / (-b[["M4"]]),
                   ISF_M4)
  #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for D1
  RSF0 <- 1 - (RSF_D1 / a[["D1"]])^(1 / c0[["D1"]])
  #Eq. 41a (Wotton 2009) - Calculate the slope equivalent ISI (D1)
  ISF_D1 <- ifelse(FUELTYPE %in% c("M4") & RSF0 >= 0.01,
                   log(1 - (RSF_D1 / a[["D1"]])**(1 / c0[["D1"]])) / (-b[["D1"]]),
                   ISF_D1)
  #Eq. 41b (Wotton 2009) - Calculate the slope equivalent ISI (D1)
  ISF_D1 <- ifelse(FUELTYPE %in% c("M4") & RSF0 < 0.01,
                   log(0.01) / (-b[["D1"]]),
                   ISF_D1)
  #Eq. 42c (Wotton 2009) - Calculate weighted average for the M4 type
  ISF <- ifelse(FUELTYPE %in% c("M4"), PDF / 100 * ISF_M4 + (1 - PDF / 100.) * 
                  ISF_D1, 
                ISF)
  #Eqs. 35a, 35b (Wotton 2009) - Curing Factor pivoting around % 58.8
  CF <- ifelse(FUELTYPE %in% c("O1A", "O1B"), 
               ifelse(CC < 58.8, 0.005 * (exp(0.061 * CC) - 1), 
                      0.176 + 0.02 * (CC-58.8)),
               CF)
  #Eqs. 43a, 43b (Wotton 2009) - slope equivilent ISI for Grass
  ISF <- ifelse(FUELTYPE %in% c("O1A", "O1B"),
                ifelse((1 - (RSF / (CF * a[FUELTYPE]))**(1 / c0[FUELTYPE])) >= 0.01,
                       log(1 - (RSF / (CF * a[FUELTYPE]))**(1 / c0[FUELTYPE])) / 
                         (-b[FUELTYPE]),
                       log(0.01) / (-b[FUELTYPE])),
                ISF)
  #Eq. 46 (FCFDG 1992)
  m <- 147.2 * (101 - FFMC) / (59.5 + FFMC)
  #Eq. 45 (FCFDG 1992) - FFMC function from the ISI equation
  fF <- 91.9 * exp(-.1386 * m) * (1 + (m**5.31) / 4.93e7)
  #Eqs. 44a, 44d (Wotton 2009) - Slope equivalent wind speed
  WSE <- 1 / 0.05039 * log(ISF / (0.208 * fF))
  #Eqs. 44b, 44e (Wotton 2009) - Slope equivalent wind speed
  WSE <- ifelse(WSE > 40 & ISF < (0.999 * 2.496 * fF),
                28 - (1 / 0.0818 * log(1 - ISF/ ( 2.496 * fF))),
                WSE)
  #Eqs. 44c (Wotton 2009) - Slope equivalent wind speed
  WSE <- ifelse(WSE > 40 & ISF >= (0.999 * 2.496 * fF), 112.45, WSE)
  #Eq. 47 (FCFDG 1992) - resultant vector magnitude in the x-direction
  WSX <- WS * sin(WAZ) + WSE * sin(SAZ)
  #Eq. 48 (FCFDG 1992) - resultant vector magnitude in the y-direction
  WSY <- WS * cos(WAZ) + WSE * cos(SAZ)
  #Eq. 49 (FCFDG 1992) - the net effective wind speed
  WSV <- sqrt(WSX * WSX + WSY * WSY)
  #stop execution here and return WSV if requested
  if (output=="WSV")
    return(WSV)
  #Eq. 50 (FCFDG 1992) - the net effective wind direction (radians)
  RAZ <- acos(WSY / WSV)
  #Eq. 51 (FCFDG 1992) - convert possible negative RAZ into more understandable
  # directions
  RAZ <- ifelse(WSX < 0, 2 * pi - RAZ, RAZ)
  return(RAZ)
}

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
#' @returns  list(RAZ, WSV) - Rate of spread azimuth (degrees) and Wind Slope speed (km/hr)
#' 
#' @export SlopeAdjust
SlopeAdjust <- function(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ,
                        FMC, SFC, PC, PDF, CC, CBH, ISI)
{
  return(.SlopeAdjust(FUELS[[FUELTYPE]], FFMC, BUI, WS, WAZ, GS, SAZ,
                      FMC, SFC, PC, PDF, CC, CBH, ISI))
}
# 
# oldISF <- function(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
#                    CC, CBH, ISI, output = "RAZ")
# {
#   NoBUI <- rep(-1,length(FFMC))
#   #Eq. 39 (FCFDG 1992) - Calculate Spread Factor
#   SF <- ifelse (GS >= 70, 10, exp(3.533 * (GS / 100)^1.2))
#   #ISI with 0 wind on level grounds
#   ISZ <- InitialSpreadIndex(FFMC, 0)
#   #Surface spread rate with 0 wind on level ground
#   RSZ <- RateOfSpread(FUELTYPE, ISZ, BUI = NoBUI, FMC, SFC, PC, PDF, CC, CBH)
#   #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope
#   RSF <- RSZ * SF
#   #setup some reference vectors
#   d <- c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "D1", "M1", "M2", "M3", "M4",
#          "S1", "S2", "S3", "O1A", "O1B")
#   a <- c(90, 110, 110, 110, 30, 30, 45, 30, 0, 0, 120, 100, 75, 40, 55, 190,
#          250)
#   b <- c(0.0649, 0.0282, 0.0444, 0.0293, 0.0697, 0.0800, 0.0305, 0.0232, 0, 0,
#          0.0572, 0.0404, 0.0297, 0.0438, 0.0829, 0.0310, 0.0350)
#   c0 <- c(4.5, 1.5, 3.0, 1.5, 4.0, 3.0, 2.0, 1.6, 0, 0, 1.4, 1.48, 1.3, 1.7,
#           3.2, 1.4, 1.7)
#   names(a) <- names(b) <- names(c0) <- d
#   
#   #initialize some local vars
#   RSZ <- rep(-99,length(FFMC))
#   RSF_C2 <- rep(-99,length(FFMC))
#   RSF_D1 <- rep(-99,length(FFMC))
#   RSF_M3 <- rep(-99,length(FFMC))
#   RSF_M4 <- rep(-99,length(FFMC))
#   CF <- rep(-99,length(FFMC))
#   ISF <- rep(-99,length(FFMC))
#   ISF_C2 <- rep(-99,length(FFMC))
#   ISF_D1 <- rep(-99,length(FFMC))
#   ISF_M3 <- rep(-99,length(FFMC))
#   ISF_M4 <- rep(-99,length(FFMC))
#   
#   #Eqs. 41a, 41b (Wotton 2009) - Calculate the slope equivalend ISI
#   ISF <- ifelse(FUELTYPE %in% c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "D1",
#                                 "S1", "S2", "S3"),
#                 ifelse((1 - (RSF / a[FUELTYPE])**(1 / c0[FUELTYPE])) >= 0.01,
#                        log(1 - (RSF / a[FUELTYPE])**(1 / c0[FUELTYPE])) / (-b[FUELTYPE]),
#                        log(0.01)/(-b[FUELTYPE])),
#                 ISF)
#   
#   #When calculating the M1/M2 types, we are going to calculate for both C2
#   # and D1 types, and combine
#   #Surface spread rate with 0 wind on level ground
#   RSZ <- ifelse(FUELTYPE %in% c("M1", "M2"),
#                 RateOfSpread(rep("C2", length(ISZ)), ISZ, BUI = NoBUI, FMC, SFC, PC, PDF,
#                              CC, CBH),
#                 RSZ)
#   #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for C2
#   RSF_C2 <- ifelse(FUELTYPE %in% c("M1", "M2"), RSZ * SF, RSF_C2)
#   RSZ <- ifelse(FUELTYPE %in% c("M1", "M2"),
#                 RateOfSpread(rep("D1", length(ISZ)), ISZ, BUI = NoBUI, FMC, SFC, PC,
#                              PDF, CC, CBH),RSZ)
#   #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for D1
#   RSF_D1 <- ifelse(FUELTYPE %in% c("M1", "M2"), RSZ * SF, RSF_D1)
#   RSF0 <- 1 - (RSF_C2 / a[["C2"]])^(1 / c0[["C2"]])
#   #Eq. 41a (Wotton 2009) - Calculate the slope equivalent ISI
#   ISF_C2 <- ifelse(FUELTYPE %in% c("M1", "M2") & RSF0 >= 0.01,
#                    log(1 - (RSF_C2 / a[["C2"]])**(1 / c0[["C2"]])) / (-b[["C2"]]),
#                    ISF_C2)
#   #Eq. 41b (Wotton 2009) - Calculate the slope equivalent ISI
#   ISF_C2 <- ifelse(FUELTYPE %in% c("M1", "M2") & RSF0 < 0.01,
#                    log(0.01) / (-b[["C2"]]),
#                    ISF_C2)
#   RSF0 <- 1 - (RSF_D1 / a[["D1"]])^(1 / c0[["D1"]])
#   #Eq. 41a (Wotton 2009) - Calculate the slope equivalent ISI
#   ISF_D1 <- ifelse(FUELTYPE %in% c("M1", "M2") & RSF0 >= 0.01,
#                    log(1 - (RSF_D1 / a[["D1"]])**(1 / c0[["D1"]])) / (-b[["D1"]]),
#                    ISF_D1)
#   #Eq. 41b (Wotton 2009) - Calculate the slope equivalent ISI
#   ISF_D1 <- ifelse(FUELTYPE %in% c("M1", "M2") & RSF0 < 0.01,
#                    log(0.01) / (-b[["D1"]]),
#                    ISF_D1)
#   #Eq. 42a (Wotton 2009) - Calculate weighted average for the M1/M2 types
#   ISF <- ifelse(FUELTYPE %in% c("M1", "M2"), PC / 100 * ISF_C2 +
#                   (1 - PC / 100) * ISF_D1,
#                 ISF)
# 
#   #Set % Dead Balsam Fir to 100%
#   PDF100 <- rep(100, length(ISI))
#   #Surface spread rate with 0 wind on level ground
#   RSZ <- ifelse(FUELTYPE %in% c("M3"),
#                 RateOfSpread(rep("M3", length(FMC)), ISI = ISZ, BUI = NoBUI, FMC, SFC,
#                          PC, PDF100, CC, CBH),
#                 RSZ)
#   #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for M3
#   RSF_M3 <- ifelse(FUELTYPE %in% c("M3"), RSZ * SF, RSF_M3)
#   #Surface spread rate with 0 wind on level ground, using D1
#   RSZ <- ifelse(FUELTYPE %in% c("M3"),
#                 RateOfSpread(rep("D1", length(ISZ)), ISZ, BUI = NoBUI, FMC, SFC, PC,
#                          PDF100, CC, CBH),
#                 RSZ)
#   #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for M3
#   RSF_D1 <- ifelse(FUELTYPE %in% c("M3"), RSZ * SF, RSF_D1)
#   RSF0 <- 1 - (RSF_M3 / a[["M3"]])^(1 / c0[["M3"]])
#   #Eq. 41a (Wotton 2009) - Calculate the slope equivalent ISI
#   ISF_M3 <- ifelse(FUELTYPE %in% c("M3") & RSF0 >= 0.01,
#                    log(1 - (RSF_M3/a[["M3"]])**(1/c0[["M3"]]))/(-b[["M3"]]),ISF_M3)
#   #Eq. 41b (Wotton 2009) - Calculate the slope equivalent ISI
#   ISF_M3 <- ifelse(FUELTYPE %in% c("M3") & RSF0 < 0.01,
#                    log(0.01) / (-b[["M3"]]),
#                    ISF_M3)
#   #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for D1
#   RSF0 <- 1 - (RSF_D1 / a[["D1"]])^(1 / c0[["D1"]])
#   #Eq. 41a (Wotton 2009) - Calculate the slope equivalent ISI
#   ISF_D1 <- ifelse(FUELTYPE %in% c("M3") & RSF0 >= 0.01,
#                    log(1 - (RSF_D1 / a[["D1"]])**(1 / c0[["D1"]])) / (-b[["D1"]]),
#                    ISF_D1)
#   #Eq. 41b (Wotton 2009) - Calculate the slope equivalent ISI
#   ISF_D1 <- ifelse(FUELTYPE %in% c("M3") & RSF0 < 0.01,
#                    log(0.01) / (-b[["D1"]]),
#                    ISF_D1)
#   #Eq. 42b (Wotton 2009) - Calculate weighted average for the M3 type
#   ISF <- ifelse(FUELTYPE %in% c("M3"),
#                 PDF / 100 * ISF_M3 + (1 - PDF / 100) * ISF_D1,
#                 ISF)
#   #Surface spread rate with 0 wind on level ground, using M4
#   RSZ <- ifelse(FUELTYPE %in% c("M4"),
#                 RateOfSpread(rep("M4", length(FMC)), ISI = ISZ, BUI = NoBUI, FMC, SFC,
#                          PC, PDF100, CC, CBH),
#                 RSZ)
#   #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for M4
#   RSF_M4 <- ifelse(FUELTYPE %in% c("M4"), RSZ * SF, RSF_M4)
#   #Surface spread rate with 0 wind on level ground, using M4
#   RSZ <- ifelse(FUELTYPE %in% c("M4"),
#                 RateOfSpread(rep("D1", length(ISZ)), ISZ, BUI = NoBUI, FMC, SFC, PC,
#                          PDF100, CC, CBH),
#                 RSZ)
#   #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for D1
#   RSF_D1 <- ifelse(FUELTYPE %in% c("M4"), RSZ * SF,RSF_D1)
#   #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for D1
#   RSF0 <- 1 - (RSF_M4 / a[["M4"]])^(1 / c0[["M4"]])
#   #Eq. 41a (Wotton 2009) - Calculate the slope equivalent ISI
#   ISF_M4 <- ifelse(FUELTYPE %in% c("M4") & RSF0 >= 0.01,
#                    log(1 - (RSF_M4 / a[["M4"]])**(1 / c0[["M4"]])) / (-b[["M4"]]),
#                    ISF_M4)
#   #Eq. 41b (Wotton 2009) - Calculate the slope equivalent ISI
#   ISF_M4 <- ifelse(FUELTYPE %in% c("M4") & RSF0 < 0.01,
#                    log(0.01) / (-b[["M4"]]),
#                    ISF_M4)
#   #Eq. 40 (FCFDG 1992) - Surface spread rate with 0 wind upslope for D1
#   RSF0 <- 1 - (RSF_D1 / a[["D1"]])^(1 / c0[["D1"]])
#   #Eq. 41a (Wotton 2009) - Calculate the slope equivalent ISI (D1)
#   ISF_D1 <- ifelse(FUELTYPE %in% c("M4") & RSF0 >= 0.01,
#                    log(1 - (RSF_D1 / a[["D1"]])**(1 / c0[["D1"]])) / (-b[["D1"]]),
#                    ISF_D1)
#   #Eq. 41b (Wotton 2009) - Calculate the slope equivalent ISI (D1)
#   ISF_D1 <- ifelse(FUELTYPE %in% c("M4") & RSF0 < 0.01,
#                    log(0.01) / (-b[["D1"]]),
#                    ISF_D1)
#   #Eq. 42c (Wotton 2009) - Calculate weighted average for the M4 type
#   ISF <- ifelse(FUELTYPE %in% c("M4"), PDF / 100 * ISF_M4 + (1 - PDF / 100.) *
#                   ISF_D1,
#                 ISF)
#   #Eqs. 35a, 35b (Wotton 2009) - Curing Factor pivoting around % 58.8
#   CF <- ifelse(FUELTYPE %in% c("O1A", "O1B"),
#                ifelse(CC < 58.8, 0.005 * (exp(0.061 * CC) - 1),
#                       0.176 + 0.02 * (CC-58.8)),
#                CF)
#   #Eqs. 43a, 43b (Wotton 2009) - slope equivilent ISI for Grass
#   ISF <- ifelse(FUELTYPE %in% c("O1A", "O1B"),
#                 ifelse((1 - (RSF / (CF * a[FUELTYPE]))**(1 / c0[FUELTYPE])) >= 0.01,
#                        log(1 - (RSF / (CF * a[FUELTYPE]))**(1 / c0[FUELTYPE])) /
#                          (-b[FUELTYPE]),
#                        log(0.01) / (-b[FUELTYPE])),
#                 ISF)
#   return(ISF)
#   #return(NaN)
# }
.SlopeAdjust..FuelBase <- function(this, FFMC, BUI, WS, WAZ, GS, SAZ,
                                   FMC, SFC, PC, PDF, CC, CBH, ISI)
{
  ISF <- .SlopeEquivalentInitialSpreadIndex(this, FFMC, BUI, WS, WAZ, GS, SAZ,
                                            FMC, SFC, PC, PDF, CC, CBH, ISI)
  if (!(this$name %in% c('NF', 'WA')) && !is.na(ISF))
  {
    stopifnot(ISF != -99)
  }
  stopifnot(1 == length(ISF))
  # old <- oldISF(this$name, FFMC, BUI, WS, WAZ, GS, SAZ,
  #               FMC, SFC, PC, PDF, CC, CBH, ISI)
  # if (!(ISF == old || (is.na(ISF) && is.na(old))))
  # {
  #   print(this$name)
  # }
  # stopifnot(ISF == old || (is.na(ISF) && is.na(old)))
  if (is.na(ISF) || -99.0 == ISF)
  {
    return(list(WSV=NA, RAZ=NA))
  }
  #Eq. 46 (FCFDG 1992)
  m <- 147.2 * (101 - FFMC) / (59.5 + FFMC)
  #Eq. 45 (FCFDG 1992) - FFMC function from the ISI equation
  fF <- 91.9 * exp(-.1386 * m) * (1 + (m**5.31) / 4.93e7)
  #Eqs. 44a, 44d (Wotton 2009) - Slope equivalent wind speed
  WSE <- 1 / 0.05039 * log(ISF / (0.208 * fF))
  #Eqs. 44b, 44e (Wotton 2009) - Slope equivalent wind speed
  WSE <- ifelse(WSE > 40 & ISF < (0.999 * 2.496 * fF),
                28 - (1 / 0.0818 * log(1 - ISF/ ( 2.496 * fF))),
                WSE)
  #Eqs. 44c (Wotton 2009) - Slope equivalent wind speed
  WSE <- ifelse(WSE > 40 & ISF >= (0.999 * 2.496 * fF), 112.45, WSE)
  #Eq. 47 (FCFDG 1992) - resultant vector magnitude in the x-direction
  WSX <- WS * sin(WAZ) + WSE * sin(SAZ)
  #Eq. 48 (FCFDG 1992) - resultant vector magnitude in the y-direction
  WSY <- WS * cos(WAZ) + WSE * cos(SAZ)
  #Eq. 49 (FCFDG 1992) - the net effective wind speed
  WSV <- sqrt(WSX * WSX + WSY * WSY)
  #Eq. 50 (FCFDG 1992) - the net effective wind direction (radians)
  RAZ <- acos(WSY / WSV)
  #Eq. 51 (FCFDG 1992) - convert possible negative RAZ into more understandable
  # directions
  RAZ <- ifelse(WSX < 0, 2 * pi - RAZ, RAZ)
  WSV <- ifelse(is.na(WSV), NaN, WSV)
  RAZ <- ifelse(is.na(RAZ), NaN, RAZ)
  # result <- c(WSV=WSV, RAZ=RAZ)
  result <- list(WSV=WSV, RAZ=RAZ)
  # library(testthat)
  # expect_equal(WSV, WSV)
  # expect_equal(RAZ, RAZ)
  # expect_equal(result$WSV, WSV)
  # expect_equal(result$RAZ, RAZ)
  # expect_equal(result[["RAZ"]], RAZ)
  # expect_equal(result[["WSV"]], WSV)
  oldWSV <- .Slopecalc(this$name, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
                    CC, CBH, ISI, "WSV")
  stopifnot(oldWSV == WSV)
  oldRAZ <- .Slopecalc(this$name, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
                       CC, CBH, ISI, "RAZ")
  if (!((is.na(oldRAZ) && is.na(RAZ)) || oldRAZ == RAZ))
  {
    print(c(this$name, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF,
            CC, CBH, ISI))
    stopifnot(oldRAZ == RAZ)
  }
  return(result)
}
