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
  return(.BuildupEffect(FUELS[[FUELTYPE]], BUI))
})

setMethod(".BuildupEffect",
          "Fuel",
          function(this, BUI)
          {
            #Eq. 54 (FCFDG 1992) The Buildup Effect
            BE<- ifelse(BUI > 0 & this@BUIo > 0,
                        exp(50 * log(this@Q) * (1 / BUI - 1 / this@BUIo)),
                        1)
            return(BE)
          }
)
