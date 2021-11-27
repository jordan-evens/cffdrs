#' Crown Fraction Burned Calculator
#' 
#' Calculate Calculate Crown Fraction Burned.
#' 
#' All variables names are laid out in the same manner as Forestry Canada Fire 
#' Danger Group (FCFDG) (1992). Development and Structure of the Canadian Forest
#'  Fire Behavior Prediction System." Technical Report ST-X-3, Forestry Canada,
#'   Ottawa, Ontario.
#' 
#' @references \url{https://cfs.nrcan.gc.ca/publications/download-pdf/10068} 
#' Development and Structure of the Canadian Forest Fire Behavior Prediction 
#' System." Technical Report ST-X-3, Forestry Canada, Ottawa, Ontario.
#' 
#' @param ROS      Rate of Spread
#' @param RSO      Critical Surface Rate of Spread
#' 
#' @return CFB
#' @export CrownFractionBurned
CrownFractionBurned <- function(FUELTYPE, ROS, RSO)
{
  return(.CrownFractionBurned(FUELS[[FUELTYPE]], ROS, RSO))
}
.CrownFractionBurned.Fuel <- function(this, ROS, RSO)
{
  #Eq. 58 (FCFDG 1992) Crown fraction burned 
  CFB <- ifelse(ROS > RSO, 1 - exp(-0.23 * (ROS - RSO)), 0)
  return(CFB)
}
