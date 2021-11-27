#' C-6 Conifer Plantation Fire Spread Calculator
#'
#' Calculate surface fire rate of spread (RSS).
#' 
#' All variables names are laid out in the same manner as Forestry Canada Fire 
#' Danger Group (FCFDG) (1992). Development and Structure of the Canadian Forest
#'  Fire Behavior Prediction System." Technical Report ST-X-3, Forestry Canada, 
#'  Ottawa, Ontario.
#' 
#' @references \url{https://cfs.nrcan.gc.ca/publications/download-pdf/10068} 
#' Development and Structure of the Canadian Forest Fire Behavior Prediction 
#' System." Technical Report ST-X-3, Forestry Canada, Ottawa, Ontario. 
#'
#' @param FUELTYPE    The Fire Behaviour Prediction FuelType
#' @param RSI         Intermediate Surface Fire Rate of Spread
#' @param BUI         Buildup Index
#' 
#' @return RSS
#' @export SurfaceRateOfSpreadC6
SurfaceRateOfSpreadC6 <- function(RSI, BUI)
{
  #Eq. 63 (FCFDG 1992) Surface fire spread rate (m/min)
  RSS <- RSI * .BuildupEffect(.C6, BUI)
  return (RSS)
}
