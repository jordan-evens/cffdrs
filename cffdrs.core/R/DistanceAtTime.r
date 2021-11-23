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
  return (list(C1=a1,
               C2=a2,
               C3=a2,
               C4=a2,
               C5=a2,
               C6=a2,
               C7=a2,
               D1=a1,
               M1=a2,
               M2=a2,
               M3=a2,
               M4=a2,
               O1A=a1,
               O1B=a1,
               S1=a1,
               S2=a1,
               S3=a1))
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
DistanceAtTime <- Vectorize(function(FUELTYPE, ROSeq, HR, CFB)
{
  #Eq. 72 (FCFDG 1992)
  #Calculate the alpha constant for the DISTt calculation
  alpha <- .DistanceAtTimeFunctions[[FUELTYPE]](CFB)
  #Eq. 71 (FCFDG 1992) Calculate Head fire spread distance
  DISTt  <- ROSeq *
    (HR + exp(-alpha * HR) / alpha - 1 / alpha)
  return(DISTt)
})
