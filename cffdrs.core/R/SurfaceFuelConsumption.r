.SurfaceFuelConsumptionFunctions <- (function()
{
  sfcC1 <- function(FFMC, BUI, PC, GFL)
  {
    #Eqs. 9a, 9b (Wotton et. al. 2009) - Solving the lower bound of FFMC value
    # for the C1 fuel type SFC calculation
    return (ifelse(FFMC > 84,
                   0.75 + 0.75 * (1 - exp(-0.23 * (FFMC - 84)))**0.5,
                   0.75 - 0.75 * (1 - exp(-0.23 * (84 - FFMC)))**0.5))
  }
  sfcC2M3M4 <- function(FFMC, BUI, PC, GFL)
  {
    #Eq. 10 (FCFDG 1992) - C2, M3, and M4 Fuel Types
    return (5.0 * (1 - exp(-0.0115 * BUI)))
  }
  sfcC3C4 <- function(FFMC, BUI, PC, GFL)
  {
    #Eq. 11 (FCFDG 1992) - C3, C4 Fuel Types
    return (5.0 * (1 - exp(-0.0164 * BUI))**2.24)
  }
  sfcC5C6 <- function(FFMC, BUI, PC, GFL)
  {
    #Eq. 12 (FCFDG 1992) - C5, C6 Fuel Types
    return (5.0 * (1 - exp(-0.0149 * BUI))**2.48)
  }
  sfcC7 <- function(FFMC, BUI, PC, GFL)
  {
    #Eqs. 13, 14, 15 (FCFDG 1992) - C7 Fuel Types
    return (ifelse(FFMC > 70,
                   2 * (1 - exp(-0.104 * (FFMC - 70))),
                   0) + 1.5 * (1 - exp(-0.0201 * BUI)))
    
  }
  sfcD1 <- function(FFMC, BUI, PC, GFL)
  {
    #Eq. 16 (FCFDG 1992) - D1 Fuel Type
    return (1.5 * (1 - exp(-0.0183 * BUI)))
  }
  sfcM1M2 <- function(FFMC, BUI, PC, GFL)
  {
    #Eq. 17 (FCFDG 1992) - M1 and M2 Fuel Types
    return (PC / 100 * (5.0 * (1 - exp(-0.0115 * BUI))) + 
              ((100 - PC) / 100 * (1.5 * (1 - exp(-0.0183 * BUI)))))
  }
  sfcO1 <- function(FFMC, BUI, PC, GFL)
  {
    #Eq. 18 (FCFDG 1992) - Grass Fuel Types
    return (GFL)
  }
  sfcS1 <- function(FFMC, BUI, PC, GFL)
  {
    #Eq. 19, 20, 25 (FCFDG 1992) - S1 Fuel Type
    return (4.0 * (1 - exp(-0.025 * BUI)) + 4.0 * (1 - exp(-0.034 * BUI)))
  }
  sfcS2 <- function(FFMC, BUI, PC, GFL)
  {
    #Eq. 21, 22, 25 (FCFDG 1992) - S2 Fuel Type
    return (10.0 * (1 - exp(-0.013 * BUI)) + 6.0 * (1 - exp(-0.060 * BUI)))
  }
  sfcS3 <- function(FFMC, BUI, PC, GFL)
  {
    #Eq. 23, 24, 25 (FCFDG 1992) - S3 Fuel Type
    return (12.0 * (1 - exp(-0.0166 * BUI)) + 20.0 * (1-exp(-0.0210 * BUI)))
  }
  return(list(C1=sfcC1,
              C2=sfcC2M3M4,
              C3=sfcC3C4,
              C4=sfcC3C4,
              C5=sfcC5C6,
              C6=sfcC5C6,
              C7=sfcC7,
              D1=sfcD1,
              M1=sfcM1M2,
              M2=sfcM1M2,
              M3=sfcC2M3M4,
              M4=sfcC2M3M4,
              O1A=sfcO1,
              O1B=sfcO1,
              S1=sfcS1,
              S2=sfcS2,
              S3=sfcS3))
})()

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
SurfaceFuelConsumption <- function(FUELTYPE, FFMC, BUI, PC, GFL) {
  SFC <- rep(-999,length(FFMC))
  # NOTE: need to index the result because it's still a list
  SFC <- .SurfaceFuelConsumptionFunctions[FUELTYPE][[1]](FFMC, BUI, PC, GFL)
  #Constrain SFC value
  SFC <- ifelse(SFC <= 0, 0.000001, SFC)
  
  return(SFC)
}
