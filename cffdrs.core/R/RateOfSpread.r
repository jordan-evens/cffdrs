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
RateOfSpread <- Vectorize(function(FUELTYPE, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
{
  ROS <- .RateOfSpread(FUELS[[FUELTYPE]], ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
  #add a constraint
  ROS <- ifelse(ROS <= 0,0.000001,ROS)
  return(ROS)
})
.RateOfSpread.Fuel <- function(this, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
{
  #Eq. 26 (FCFDG 1992) - Initial Rate of Spread for Conifer and Slash types
  return(this[["a"]] * (1 - exp(-this[["b"]] * ISI))**this[["c0"]])
}
.RateOfSpread..FuelGrass <- function(this, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
{
  #Eq. 35b (Wotton et. al. 2009) - Calculate Curing function for grass
  CF <- ifelse(CC < 58.8,
               0.005 * (exp(0.061 * CC) - 1),
               0.176 + 0.02 * (CC - 58.8))
  #Eq. 36 (FCFDG 1992) - Calculate Initial Rate of Spread for Grass
  RSI <- this[["a"]] * ((1 - exp(-this[["b"]] * ISI))**this[["c0"]]) * CF
  return(RSI)
}
Fuel$RateOfSpread <- .RateOfSpread.Fuel
.FuelGrass$RateOfSpread <- .RateOfSpread..FuelGrass
