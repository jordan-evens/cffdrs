.DistanceAtTimeFunctions <- (function()
{
  a1 <- function(CFB)
  {
    #Eq. 72 (FCFDG 1992)
    #Calculate the alpha constant for the DISTt calculation
    alpha <- 0.115
    return (alpha)
  }
  a2 <- function(CFB)
  {
    #Eq. 72 (FCFDG 1992)
    #Calculate the alpha constant for the DISTt calculation
    alpha <- 0.115 - 18.8 * (CFB**2.5) * exp(-8* CFB)
    return (alpha)
  }
  result <- list(a1, a2, a2, a2, a2, a2, a2,
                 a1, a2, a2, a2, a2, a1, a1,
                 a1, a1, a1)
  names(result) <- c("C1", "C2", "C3", "C4", "C5", "C6", "C7",
                     "D1", "M1", "M2", "M3", "M4", "O1A", "O1B",
                     "S1", "S2", "S3")
  return (result)
})()
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
DistanceAtTime <- function(FUELTYPE, ROSeq, HR, CFB) {

  #Eq. 72 (FCFDG 1992)
  #Calculate the alpha constant for the DISTt calculation
  alpha <- .DistanceAtTimeFunctions[FUELTYPE][[1]](CFB)
  #Eq. 71 (FCFDG 1992) Calculate Head fire spread distance
  DISTt  <- ROSeq * (HR + exp(-alpha * HR) / alpha - 1 / alpha)
  
  return(DISTt)
}
