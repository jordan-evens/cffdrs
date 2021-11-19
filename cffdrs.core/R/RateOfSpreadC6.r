#' C-6 Conifer Plantation Fire Spread Calculator
#'
#' Calculate rate of spread (ROS).
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
#' @param RSC         Crown Fire Spread Rate (m/min)
#' @param RSS         Surface Fire Spread Rate (m/min)
#' @param CFB         Crown Fraction Burned
#' 
#' @return ROS
#' @export RateOfSpreadC6
RateOfSpreadC6 <- function(RSC, RSS, CFB)
{
  #Eq. 65 (FCFDG 1992) Calculate Rate of spread (m/min)
  ROS <- ifelse(RSC > RSS, RSS + (CFB)*(RSC - RSS), RSS)
  return (ROS)
}
