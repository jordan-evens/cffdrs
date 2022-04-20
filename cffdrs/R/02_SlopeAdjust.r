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
#' @param GS        Ground Slope (\%)
#' @param SAZ       Slope Azimuth
#' @param FMC       Foliar Moisture Content
#' @param SFC       Surface Fuel Consumption (kg/m^2)
#' @param PC        Percent Conifer (\%)
#' @param PDF       Percent Dead Balsam Fir (\%)
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
.SlopeAdjust..FuelBase <- function(this, FFMC, BUI, WS, WAZ, GS, SAZ,
                                   FMC, SFC, PC, PDF, CC, CBH, ISI)
{
  ISF <- .SlopeEquivalentInitialSpreadIndex(this, FFMC, BUI, WS, WAZ, GS, SAZ,
                                            FMC, SFC, PC, PDF, CC, CBH, ISI)
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
  return(result)
}
