#' C-6 Crown Fire Spread Calculator
#'
#' Calculate crown rate of spread (RSC).
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
#' 
#' @return RSC
#' @export CrownRateOfSpreadC6
CrownRateOfSpreadC6 <- function(ISI)
{
  #Average foliar moisture effect
  FMEavg <- 0.778                                                                                                                   
  #Eq. 59 (FCFDG 1992) Crown flame temperature (degrees K)
  tt <- 1500 - 2.75 * FMC
  #Eq. 60 (FCFDG 1992) Head of ignition (kJ/kg)
  H <- 460 + 25.9 * FMC
  #Eq. 61 (FCFDG 1992) Average foliar moisture effect
  FME <- ((1.5 - 0.00275 * FMC)**4.)/(460 + 25.9 * FMC) * 1000
  #Eq. 64 (FCFDG 1992) Crown fire spread rate (m/min)
  RSC <- 60 * (1 - exp(-0.0497 * ISI)) * FME / FMEavg
  return (RSC)
}
