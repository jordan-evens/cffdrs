#' Distance at time t calculator
#' 
#' Calculate the Head fire spread distance at time t. In the documentation this 
#' variable is just "D".
#' 
#' All variables names are laid out in the same manner as Forestry Canada Fire 
#' Danger Group (FCFDG) (1992). Development and Structure of the  Canadian 
#' Forest Fire Behavior Prediction System." Technical Report ST-X-3, 
#' Forestry Canada, Ottawa, Ontario.
#' 
#' @param FUELTYPE The Fire Behaviour Prediction FuelType
#' @param ROSeq    The predicted equilibrium rate of spread (m/min)
#' @param HR       The elapsed time (min)
#' @param CFB      Crown Fraction Burned
#' 
#' @return DISTt - Head fire spread distance at time t
#' @export DistanceAtTime
DistanceAtTime <- Vectorize(function(FUELTYPE, ROSeq, HR, CFB)
{
  return(.DistanceAtTime(FUELS[[FUELTYPE]], ROSeq, HR, CFB))
})
.DistanceAtTime..FuelBase <- function(this, ROSeq, HR, CFB)
{
  #Eq. 72 (FCFDG 1992)
  #Calculate the alpha constant for the DISTt calculation
  alpha <- this$.Alpha(this, CFB)
  #Eq. 71 (FCFDG 1992) Calculate Head fire spread distance
  DISTt  <- ROSeq *
    (HR + exp(-alpha * HR) / alpha - 1 / alpha)
  return(DISTt)
}
.FuelBase$.DistanceAtTime <- .DistanceAtTime..FuelBase
