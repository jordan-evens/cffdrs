#' Critical Surface Rate of Spread Calculator
#' 
#' Calculate Critical Surface fire rate of spread (RSO). The value of each of these equations can be returned to the calling 
#' function without unecessary additional calculations.
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
#' @param CSI      Critical Surface Intensity
#' @param SFC      Surface Fuel Consumption
#' 
#' @return RSO
#' @export CriticalSurfaceRateOfSpread
CriticalSurfaceRateOfSpread <- function(CSI, SFC)
{
  #Eq. 57 (FCFDG 1992) Surface fire rate of spread (m/min)
  return (CSI / (300 * SFC))
}
