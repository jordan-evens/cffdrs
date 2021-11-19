.CrownFuelConsumptionFunctions <- (function()
{
  cfc <- function(CFL, CFB, PC, PDF)
  {
    #Eq. 66a (Wotton 2009) - Crown Fuel Consumption (CFC)
    CFC <- CFL * CFB
    return (CFC)
  }
  cfcM1M2 <- function(CFL, CFB, PC, PDF)
  {
    #Eq. 66a (Wotton 2009) - Crown Fuel Consumption (CFC)
    CFC <- CFL * CFB
    #Eq. 66b (Wotton 2009) - CFC for M1/M2 types
    CFC <- PC / 100 * CFC
    return (CFC)
  }
  cfcM3M4 <- function(CFL, CFB, PC, PDF)
  {
    #Eq. 66a (Wotton 2009) - Crown Fuel Consumption (CFC)
    CFC <- CFL * CFB
    #Eq. 66c (Wotton 2009) - CFC for M3/M4 types
    CFC <- PDF / 100 * CFC
    return (CFC)
  }
  return (list(C1=cfc,
               C2=cfc,
               C3=cfc,
               C4=cfc,
               C5=cfc,
               C6=cfc,
               C7=cfc,
               D1=cfc,
               M1=cfcM1M2,
               M2=cfcM1M2,
               M3=cfcM3M4,
               M4=cfcM3M4,
               O1A=cfc,
               O1B=cfc,
               S1=cfc,
               S2=cfc,
               S3=cfc))
})()

#' Crown Fuel Consumption calculation
#' 
#'   Computes the Crown Fuel Consumption by Fuel Type.
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
#' @param FUELTYPE The Fire Behaviour Prediction FuelType
#' @param CFL      Crown Fuel Load (kg/m^2)
#' @param CFB      Crown Fraction Burned (0-1)
#' @param  PC      Percent Conifer (%)
#' @param PDF      Percent Dead Balsam Fir (%)
#' 
#' @returns CFC Crown Fuel Consumption (kg/m^2)
#' 
#' @export TotalFuelConsumption
CrownFuelConsumption <- function(FUELTYPE, CFL, CFB, PC, PDF)
{
  CFC <- .CrownFuelConsumptionFunctions[FUELTYPE][[1]](CFL, CFB, PC, PDF)
  return (CFC)
}
