.BuildupEffectCoefficients <- list(C1=list(BUIo=72, Q=0.9),
                                   C2=list(BUIo=64, Q=0.7),
                                   C3=list(BUIo=62, Q=0.75),
                                   C4=list(BUIo=66, Q=0.8),
                                   C5=list(BUIo=56, Q=0.8),
                                   C6=list(BUIo=62, Q=0.8),
                                   C7=list(BUIo=106, Q=0.85),
                                   D1=list(BUIo=32, Q=0.9),
                                   M1=list(BUIo=50, Q=0.8),
                                   M2=list(BUIo=50, Q=0.8),
                                   M3=list(BUIo=50, Q=0.8),
                                   M4=list(BUIo=50, Q=0.8),
                                   S1=list(BUIo=38, Q=0.75),
                                   S2=list(BUIo=63, Q=0.75),
                                   S3=list(BUIo=31, Q=0.75),
                                   O1A=list(BUIo=01, Q=1.0),
                                   O1B=list(BUIo=01, Q=1.0))
#' Build Up Effect Calculator
#' 
#' Computes the Buildup Effect on Fire Spread Rate. All variables names are 
#' laid out in the same manner as Forestry Canada Fire Danger Group (FCFDG) 
#' (1992). 
#' 
#' @references \url{https://cfs.nrcan.gc.ca/publications/download-pdf/10068} 
#' Development and Structure of the Canadian Forest Fire Behavior Prediction 
#' System." Technical Report ST-X-3, Forestry Canada, Ottawa, Ontario. 
#' 
#' @param FUELTYPE The Fire Behaviour Prediction FuelType
#' @param BUI The Buildup Index value
#' 
#' @return BE: Build up effect
#' @export BuildupEffect
BuildupEffect <- Vectorize(function(FUELTYPE, BUI)
{
  f <- .BuildupEffectCoefficients[[FUELTYPE]]
  #Eq. 54 (FCFDG 1992) The Buildup Effect
  BE<- ifelse(BUI > 0 & f$BUIo > 0,
              exp(50 * log(f$Q) * (1 / BUI - 1 / f$BUIo)),
              1)
  return(BE)
})
