#' Total Fuel Consumption calculation
#' 
#'   Computes the Total (Surface + Crown) Fuel Consumption by Fuel Type.
#'   All variables names are laid out in the same manner as FCFDG (1992) or
#'   Wotton et. al (2009) 
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
#' @param CFC      Crown Fuel Consumption (kg/m^2)
#' @param SFC      Surface Fuel Consumption (kg/m^2)
#' 
#' @returns TFC Total (Surface + Crown) Fuel Consumption (kg/m^2)
#' 
#' @export TotalFuelConsumption
TotalFuelConsumption <- function(CFC, SFC)
{
  #Eq. 67 (FCFDG 1992) - Total Fuel Consumption
  TFC <- SFC + CFC
  return(TFC)
}
