#' Surface Fuel Consumption Calculator
#' 
#'   Computes the Surface Fuel Consumption by Fuel Type.
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
#' @param BUI      Buildup Index
#' @param FFMC     Fine Fuel Moisture Code
#' @param PC       Percent Conifer (%)
#' @param GFL      Grass Fuel Load (kg/m^2)
#'        
#' @returns   SFC Surface Fuel Consumption (kg/m^2)
#' 
#' @export SurfaceFuelConsumption
SurfaceFuelConsumption <- Vectorize(function(FUELTYPE, FFMC, BUI, PC, GFL)
{
  SFC <- .SurfaceFuelConsumption(FUELS[[FUELTYPE]], FFMC, BUI, PC, GFL)
  SFC <- ifelse(SFC <= 0, 0.000001, SFC)
  return(SFC)
})
.SurfaceFuelConsumption.Fuel <- function(this, FFMC, BUI, PC, GFL)
{
  #Eq. 10, 11, 12, 16 (FCFDG 1992)
  #C2/M3/M4, C3/C4, C5/C6, and D1 Fuel Types
  return (this[["sfcA"]] * (1 - exp(this[["sfcB"]] * BUI))**this[["sfcC"]])
}
.SurfaceFuelConsumption..FuelMixedwood <- function(this, FFMC, BUI, PC, GFL)
{
  #Eq. 17 (FCFDG 1992) - M1 and M2 Fuel Types
  SFC_C2 <- (PC / 100 * .SurfaceFuelConsumption(.C2, FFMC, BUI, PC, GFL))
  SFC_D1 <- ((100 - PC) / 100 * .SurfaceFuelConsumption(.D1, FFMC, BUI, PC, GFL))
  SFC_C2_check <- PC / 100 * (5.0 * (1 - exp(-0.0115 * BUI)))
  SFC_D1_check <- ((100 - PC) / 100 * (1.5 * (1 - exp(-0.0183 * BUI))))
  stopifnot(SFC_C2 == SFC_C2_check)
  stopifnot(SFC_D1 == SFC_D1_check)
  SFC <- SFC_C2 + SFC_D1
  SFC_check <- SFC_C2_check + SFC_D1_check
  stopifnot(SFC == SFC_check)
  return(SFC)
}
.SurfaceFuelConsumption..FuelGrass <- function(this, FFMC, BUI, PC, GFL)
{
  #Eq. 18 (FCFDG 1992) - Grass Fuel Types
  return (GFL)
}
.SurfaceFuelConsumption..FuelSlash <- function(this, FFMC, BUI, PC, GFL)
{
  #Eq. 19, 20, 25 (FCFDG 1992) - S1 Fuel Type
  #Eq. 21, 22, 25 (FCFDG 1992) - S2 Fuel Type
  #Eq. 23, 24, 25 (FCFDG 1992) - S3 Fuel Type
  return (this[["sfcA"]] * (1 - exp(this[["sfcB"]] * BUI))
          + this[["sfcC"]] * (1 - exp(this[["sfcD"]] * BUI)))
}
