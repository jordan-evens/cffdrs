#' C-6 Conifer Plantation Intermediate Surface Fire Spread Rate Calculator
#'
#' Calculate intermediate surface fire rate of spread, before BUI
#' effect is applied (RSI).
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
#' @param ISI         Initial Spread Index
#' @param FMC         Foliar Moisture Content
#' 
#' @return RSI
#' @export IntermediateSurfaceRateOfSpreadC6
IntermediateSurfaceRateOfSpreadC6 <- function(ISI, FMC)
{ 
  #Eq. 62 (FCFDG 1992) Intermediate surface fire spread rate
  RSI <- 30 * (1 - exp(-0.08 * ISI))**3.0
  return (RSI)
}
