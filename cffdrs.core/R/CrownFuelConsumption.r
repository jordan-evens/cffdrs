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
#' @export CrownFuelConsumption
CrownFuelConsumption <- Vectorize(function(FUELTYPE, CFL, CFB, PC, PDF)
{
  return(.CrownFuelConsumption(FUELS[[FUELTYPE]], CFL, CFB, PC, PDF))
})

.CrownFuelConsumption.Fuel <- function(this, CFL, CFB, PC, PDF)
{
  #Eq. 66a (Wotton 2009) - Crown Fuel Consumption (CFC)
  CFC <- CFL * CFB
  return (CFC)
}
.CrownFuelConsumption..FuelMixedwood <- function(this, CFL, CFB, PC, PDF)
{
  #Eq. 66a (Wotton 2009) - Crown Fuel Consumption (CFC)
  CFC <- CFL * CFB
  #Eq. 66b (Wotton 2009) - CFC for M1/M2 types
  CFC <- PC / 100 * CFC
  return (CFC)
}
.CrownFuelConsumption..FuelMixedDead <- function(this, CFL, CFB, PC, PDF)
{
  #Eq. 66a (Wotton 2009) - Crown Fuel Consumption (CFC)
  CFC <- CFL * CFB
  #Eq. 66c (Wotton 2009) - CFC for M3/M4 types
  CFC <- PDF / 100 * CFC
  return (CFC)
}
