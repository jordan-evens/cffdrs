#' Critical Surface Intensity Calculator
#' 
#' Calculate Critical surface intensity (CSI).
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
#' @param FMC      Foliar Moisture Content 
#' @param CBH      Crown Base Height
#' 
#' @return CSI
#' @export CriticalSurfaceIntensity
CriticalSurfaceIntensity <- function(FMC, CBH)
{
  #Eq. 56 (FCFDG 1992) Critical surface intensity
  CSI <- 0.001 * (CBH**1.5) * (460 + 25.9 * FMC)**1.5
  return (CSI)
}
