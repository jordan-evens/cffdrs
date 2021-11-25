#' Length-to-Breadth ratio at time t
#' 
#' Computes the Length to Breadth ratio of an elliptically shaped fire at
#' elapsed time since ignition. Equations are from listed FCFDG (1992) and
#' Wotton et. al. (2009), and are marked as such.
#' 
#' All variables names are laid out in the same manner as Forestry Canada 
#' Fire Danger Group (FCFDG) (1992). Development and Structure of the 
#' Canadian Forest Fire Behavior Prediction System." Technical Report 
#' ST-X-3, Forestry Canada, Ottawa, Ontario.
#' 
#' Wotton, B.M., Alexander, M.E., Taylor, S.W. 2009. Updates and revisions to
#' the 1992 Canadian forest fire behavior prediction system. Nat. Resour. 
#' Can., Can. For. Serv., Great Lakes For. Cent., Sault Ste. Marie, Ontario, 
#' Canada. Information Report GLC-X-10, 45p.
#' 
#' @param FUELTYPE The Fire Behaviour Prediction FuelType
#' @param  LB: Length to Breadth ratio
#' @param HR: Time since ignition (hours)
#' @param CFB: Crown Fraction Burned
#'  
#' @returns Length to Breadth ratio at time since ignition
#' 
#' @export LengthToBreadthRatioAtTime
LengthToBreadthRatioAtTime <- Vectorize(function(FUELTYPE, LB, HR, CFB)
{
  return(.LengthToBreadthRatioAtTime(FUELS[[FUELTYPE]], LB, HR, CFB))
})
setMethod(".LengthToBreadthRatioAtTime",
          "Fuel",
          function(this, LB, HR, CFB)
          {
            #Eq. 72 (FCFDG 1992) - alpha constant value, dependent on fuel type
            alpha <- .Alpha(this, CFB)
            #Eq. 81 (Wotton et.al. 2009) - LB at time since ignition
            LBt <- (LB - 1) * (1 - exp(-alpha * HR)) + 1
            return(LBt)
          }
)
