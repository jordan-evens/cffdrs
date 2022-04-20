fctFBP <- Vectorize(function(FUELTYPE, output, ID, HR, LAT, LONG, CBH, SD, SH, CFL, FMC, D0, ELV, DJ, WS, WAZ, SAZ, FFMC, ISI, BUI, PC, PDF, GFL, BUIEFF, GS, CC, ACCEL, THETA)
{
  return(.FireBehaviourPrediction(FUELS[[FUELTYPE]], output, ID, HR, LAT, LONG, CBH, SD, SH, CFL, FMC, D0, ELV, DJ, WS, WAZ, SAZ, FFMC, ISI, BUI, PC, PDF, GFL, BUIEFF, GS, CC, ACCEL, THETA))
}
)
.FireBehaviourPrediction.Fuel <- function(this, output, ID, HR, LAT, LONG, CBH, SD, SH, CFL, FMC, D0, ELV, DJ, WS, WAZ, SAZ, FFMC, ISI, BUI, PC, PDF, GFL, BUIEFF, GS, CC, ACCEL, THETA)
{
  ############################################################################
  #                         START
  # Initializing variables
  ############################################################################
  SFC <- TFC <- HFI <- CFB <- ROS <- 0
  RAZ <- -999
  if (output == "SECONDARY" | output == "ALL" | output == "S" | 
      output == "A") {
    FROS <- BROS <- TROS <- HROSt <- FROSt <- BROSt <- TROSt <- FCFB <- 
      BCFB <- TCFB <- FFI <- BFI <- TFI <- FTFC <- BTFC <- TTFC <- 0
    TI <- FTI <- BTI <- TTI <- LB <- WSV <- -999
  }
  CBH <- .CrownBaseHeight(this, CBH, SD, SH)
  CFL <- ifelse(CFL <= 0 | CFL > 2 | is.na(CFL), this$CFL, CFL)
  FMC <- ifelse(FMC <= 0 | FMC > 120 | is.na(FMC),
                .FoliarMoistureContent(this, LAT, LONG, ELV, DJ, D0),
                FMC)
  ############################################################################
  #                         END
  ############################################################################
  #Calculate Surface fuel consumption (SFC)
  SFC <- .SurfaceFuelConsumption(this, FFMC, BUI, PC, GFL)
  #Disable BUI Effect if necessary
  BUI <- ifelse(BUIEFF != 1, 0, BUI)
  SLOPE_ADJUST <- .SlopeAdjust(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
  #Calculate the net effective windspeed (WSV)
  # WSV0 <- SLOPE_ADJUST[["WSV"]]
  WSV0 <- SLOPE_ADJUST$WSV
  WSV <- ifelse(GS > 0 & FFMC > 0, WSV0, WS)
  #Calculate the net effective wind direction (RAZ)
  # RAZ0 <- SLOPE_ADJUST[["RAZ"]]
  RAZ0 <- SLOPE_ADJUST$RAZ
  RAZ <- ifelse(GS > 0 & FFMC > 0, RAZ0, WAZ)
  #Calculate or keep Initial Spread Index (ISI)
  ISI <- ifelse(ISI > 0, ISI, InitialSpreadIndex(FFMC, WSV, TRUE))
  #Calculate Critical Surface Intensity
  CSI <- .CriticalSurfaceIntensity(this, FMC, CBH)
  #Calculate Surface fire rate of spread (m/min)
  RSO <- CriticalSurfaceRateOfSpread(CSI, SFC)
  # Calculate the Rate of Spread (ROS) and Crown Fraction Burned (CFB)
  ROS <- .RateOfSpread(this, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
  CFB <- ifelse(CFL > 0,
                .CrownFractionBurned(this, ROS, RSO),
                0)
  #Calculate Crown Fuel Consumption(CFC)
  CFC <- .CrownFuelConsumption(this, CFL, CFB, PC, PDF)
  #Calculate Total Fuel Consumption (TFC)
  TFC <- TotalFuelConsumption(CFC, SFC)
  #Calculate Head Fire Intensity(HFI)
  HFI <- FireIntensity(TFC, ROS)
  #Adjust Crown Fraction Burned
  CFB <- ifelse(HR < 0, -CFB, CFB)
  #Adjust RAZ
  RAZ <- RAZ * 180/pi
  RAZ <- ifelse(RAZ == 360, 0, RAZ)
  #Calculate Fire Type (S = Surface, C = Crowning, I = Intermittent Crowning)
  FD <- ifelse(CFB < 0.1, "S", ifelse(CFB >= 0.9, "C", "I"))
  #Calculate the Secondary Outputs
  if (output == "SECONDARY" | output == "ALL" | output == "S" | 
      output == "A") {
    #Eq. 39 (FCFDG 1992) Calculate Spread Factor (GS is group slope)
    SF <- ifelse(GS >= 70, 10, exp(3.533 * (GS/100)^1.2))
    #Calculate The Buildup Effect
    BE <- .BuildupEffect(this, BUI)
    #Calculate length to breadth ratio
    LB <- .LengthToBreadthRatio(this, WSV)
    LBt <- ifelse(ACCEL == 0, LB, .LengthToBreadthRatioAtTime(this, LB, HR, CFB))
    #Calculate Back fire rate of spread (BROS)
    BROS <- .BackRateOfSpread(this, FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH)
    #Calculate Flank fire rate of spread (FROS) 
    FROS <- FlankRateOfSpread(ROS, BROS, LB)
    #Calculate the eccentricity  
    E <- sqrt(1 - 1/LB/LB)
    #Calculate the rate of spread towards angle theta (TROS)
    TROS <- ROS * (1 - E)/(1 - E * cos(THETA - RAZ))
    #Calculate rate of spread at time t for Flank, Back of fire and at angle 
    #  theta.
    ROSt <- ifelse(ACCEL == 0, ROS, .RateOfSpreadAtTime(this, ROS, HR, CFB))
    BROSt <- ifelse(ACCEL == 0, BROS, .RateOfSpreadAtTime(this, BROS, HR, CFB))
    FROSt <- ifelse(ACCEL == 0, FROS, FlankRateOfSpread(ROSt, BROSt, LBt))
    #Calculate rate of spread towards angle theta at time t (TROSt)
    TROSt <- ifelse(ACCEL == 0, TROS, 
                    ROSt * (1 - sqrt(1 - 1 / LBt / LBt)) / 
                      (1 - sqrt(1 - 1 / LBt / LBt) * cos(THETA - RAZ)))
    #Calculate Crown Fraction Burned for Flank, Back of fire and at angle theta.
    FCFB <- 0
    BCFB <- 0
    TCFB <- 0
    if (CFL != 0)
    {
      FCFB <- .CrownFractionBurned(this, FROS, RSO)
      BCFB <- .CrownFractionBurned(this, BROS, RSO)
      TCFB <- .CrownFractionBurned(this, TROS, RSO)
    }
    #Calculate Total fuel consumption for the Flank fire, Back fire and at
    #  angle theta
    FTFC <- TotalFuelConsumption(.CrownFuelConsumption(this, CFL, FCFB, PC, PDF), SFC)
    BTFC <- TotalFuelConsumption(.CrownFuelConsumption(this, CFL, BCFB, PC, PDF), SFC)
    TTFC <- TotalFuelConsumption(.CrownFuelConsumption(this, CFL, TCFB, PC, PDF), SFC)
    #Calculate the Fire Intensity at the Flank, Back and at angle theta fire
    FFI <- FireIntensity(FTFC, FROS)
    BFI <- FireIntensity(BTFC, BROS)
    TFI <- FireIntensity(TTFC, TROS)
    #Calculate Rate of spread at time t for the Head, Flank, Back of fire and
    #  at angle theta.
    HROSt <- ifelse(HR < 0, -ROSt, ROSt)
    FROSt <- ifelse(HR < 0, -FROSt, FROSt)
    BROSt <- ifelse(HR < 0, -BROSt, BROSt)
    TROSt <- ifelse(HR < 0, -TROSt, TROSt)
    
    #Calculate the elapsed time to crown fire initiation for Head, Flank, Back
    # fire and at angle theta. The (a# variable is a constant for Head, Flank, 
    # Back and at angle theta used in the *TI equations)
    # HACK: old version used non-constant equation for every FUELTYPE
    TI <- log(ifelse(1 - RSO/ROS > 0, 1 - RSO/ROS, 1))/(-.Alpha..FuelBase(this, CFB))
    FTI <- log(ifelse(1 - RSO/FROS > 0, 1 - RSO/FROS, 1))/(-.Alpha..FuelBase(this, FCFB))
    BTI <- log(ifelse(1 - RSO/BROS > 0, 1 - RSO/BROS, 1))/(-.Alpha..FuelBase(this, BCFB))
    TTI <- log(ifelse(1 - RSO/TROS > 0, 1 - RSO/TROS, 1))/(-.Alpha..FuelBase(this, TCFB))
    
    # FIX: shouldn't it be this?
    # TI <- log(ifelse(1 - RSO/ROS > 0, 1 - RSO/ROS, 1))/(-.Alpha(this, CFB))
    # FTI <- log(ifelse(1 - RSO/FROS > 0, 1 - RSO/FROS, 1))/(-.Alpha(this, FCFB))
    # BTI <- log(ifelse(1 - RSO/BROS > 0, 1 - RSO/BROS, 1))/(-.Alpha(this, BCFB))
    # TTI <- log(ifelse(1 - RSO/TROS > 0, 1 - RSO/TROS, 1))/(-.Alpha(this, TCFB))
    
    #Fire spread distance for Head, Back, and Flank of fire
    DH <- ifelse(ACCEL == 1, .DistanceAtTime(this, ROS, HR, CFB), ROS * HR)
    DB <- ifelse(ACCEL == 1, .DistanceAtTime(this, BROS, HR, CFB), BROS * HR)
    DF <- ifelse(ACCEL == 1, (DH + DB)/(LBt * 2), (DH + DB)/(LB * 2))
  }
  #if Primary is selected, wrap the primary outputs into a data frame and
  #  return them
  if (output == "PRIMARY" | output == "P") {
    FBP <- list(ID=ID, CFB=CFB, CFC=CFC, FD=as.character(FD), HFI=HFI, RAZ=RAZ, ROS=ROS, SFC=SFC, 
                TFC=TFC)
  }
  #If Secondary is selected, wrap the secondary outputs into a data frame
  #  and return them.
  else if (output == "SECONDARY" | output == "S") {
    FBP <- list(ID=ID, BE=BE, SF=SF, ISI=ISI, FFMC=FFMC, FMC=FMC, D0=D0, RSO=RSO,
                CSI=CSI, FROS=FROS, BROS=BROS, HROSt=HROSt, FROSt=FROSt, BROSt=BROSt, FCFB=FCFB, BCFB=BCFB,
                FFI=FFI, BFI=BFI, FTFC=FTFC, BTFC=BTFC, TI=TI, FTI=FTI, BTI=BTI, LB=LB, LBt=LBt, WSV=WSV,
                DH=DH, DB=DB, DF=DF, TROS=TROS, TROSt=TROSt, TCFB=TCFB, TFI=TFI, TTFC=TTFC, TTI=TTI)
  }
  #If all outputs are selected, then wrap all outputs into a data frame and
  #  return it.
  else if (output == "ALL" | output == "A") {
    FBP <- list(ID=ID, CFB=CFB, CFC=CFC, FD=as.character(FD), HFI=HFI, RAZ=RAZ, ROS=ROS, SFC=SFC,
                TFC=TFC, BE=BE, SF=SF, ISI=ISI, FFMC=FFMC, FMC=FMC, D0=D0, RSO=RSO, CSI=CSI, FROS=FROS,
                BROS=BROS, HROSt=HROSt, FROSt=FROSt, BROSt=BROSt, FCFB=FCFB, BCFB=BCFB, FFI=FFI, BFI=BFI,
                FTFC=FTFC, BTFC=BTFC, TI=TI, FTI=FTI, BTI=BTI, LB=LB, LBt=LBt, WSV=WSV, DH=DH, DB=DB, DF=DF,
                TROS=TROS, TROSt=TROSt, TCFB=TCFB, TFI=TFI, TTFC=TTFC, TTI=TTI)
  }
  return(list(FBP))
}


#' Fire Behavior Prediction System function
#' 
#' \code{FireBehaviourPrediction} calculates the outputs from the Canadian Forest Fire Behavior
#' Prediction (FBP) System (Forestry Canada Fire Danger Group 1992) based on
#' given fire weather and fuel moisture conditions (from the Canadian Forest
#' Fire Weather Index (FWI) System (Van Wagner 1987)), fuel type, date, and
#' slope. Fire weather, for the purpose of FBP System calculation, comprises
#' observations of 10 m wind speed and direction at the time of the fire, and
#' two associated outputs from the Fire Weather Index System, the Fine Fuel
#' Moisture Content (FFMC) and Buildup Index (BUI). FWI System components can
#' be calculated with the sister function \code{\link{fwi}}.
#' 
#' The Canadian Forest Fire Behavior Prediction (FBP) System (Forestry Canada
#' Fire Danger Group 1992) is a subsystem of the Canadian Forest Fire Danger
#' Rating System, which also includes the Canadian Forest Fire Weather Index
#' (FWI) System. The FBP System provides quantitative estimates of head fire
#' spread rate, fuel consumption, fire intensity, and a basic fire description
#' (e.g., surface, crown) for 16 different important forest and rangeland types
#' across Canada. Using a simple conceptual model of the growth of a point
#' ignition as an ellipse through uniform fuels and under uniform weather
#' conditions, the system gives, as a set of secondary outputs, estimates of
#' flank and back fire behavior and consequently fire area perimeter length and
#' growth rate.
#' 
#' The FBP System evolved since the mid-1970s from a series of regionally
#' developed burning indexes to an interim edition of the nationally develop
#' FBP system issued in 1984. Fire behavior models for spread rate and fuel
#' consumption were derived from a database of over 400 experimental, wild and
#' prescribed fire observations. The FBP System, while providing quantitative
#' predictions of expected fire behavior is intended to supplement the
#' experience and judgment of operational fire managers (Hirsch 1996).
#' 
#' The FBP System was updated with some minor corrections and revisions in 2009
#' (Wotton et al. 2009) with several additional equations that were initially
#' not included in the system. This \code{FireBehaviourPrediction} function included these updates and
#' corrections to the original equations and provides a complete suite of fire
#' behavior prediction variables. Default values of optional input variables
#' provide a reasonable mid-range setting. Latitude, longitude, elevation, and
#' the date are used to calculate foliar moisture content, using a set of
#' models defined in the FBP System; note that this latitude/longitude-based
#' function is only valid for Canada. If the Foliar Moisture Content (FMC) is
#' specified directly as an input, the \code{FireBehaviourPrediction} function will use this value
#' directly rather than calculate it. This is also true of other input
#' variables.
#' 
#' Note that Wind Direction (WD) is the compass direction from which wind is
#' coming. Wind azimuth (not an input) is the direction the wind is blowing to
#' and is 180 degrees from wind direction; in the absence of slope, the wind
#' azimuth is coincident with the direction the head fire will travel (the
#' spread direction azimuth, RAZ). Slope aspect is the main compass direction
#' the slope is facing. Slope azimuth (not an input) is the direction a head
#' fire will spread up slope (in the absence of wind effects) and is 180
#' degrees from slope aspect (Aspect).  Wind direction and slope aspect are the
#' commonly used directional identifiers when specifying wind and slope
#' orientation respectively.  The input theta specifies an angle (given as a
#' compass bearing) at which a user is interested in fire behavior predictions;
#' it is typically some angle off of the final spread rate direction since if
#' for instance theta=RAZ (the final spread azimuth of the fire) then the rate
#' of spread at angle theta (TROS) will be equivalent to ROS.
#' 
#' @param input The input data, a data.frame containing fuel types, fire
#' weather component, and slope (see below). Each vector of inputs defines a
#' single FBP System prediction for a single fuel type and set of weather
#' conditions. The data.frame can be used to evaluate the FBP System for a
#' single fuel type and instant in time, or multiple records for a single point
#' (e.g., one weather station, either hourly or daily for instance) or multiple
#' points (multiple weather stations or a gridded surface). All input variables
#' have to be named as listed below, but they are case insensitive, and do not
#' have to be in any particular order. Fuel type is of type character; other
#' arguments are numeric. Missing values in numeric variables could either be
#' assigned as NA or leave as blank.\cr\cr
#' 
#' 
#' \tabular{lll}{ 
#' \bold{Required Inputs:}\tab\tab\cr 
#' \bold{Input} \tab \bold{Description/Full name} \tab \bold{Defaults}\cr 
#' 
#' \var{FuelType} 
#' \tab FBP System Fuel Type including "C-1",\cr
#' \tab"C-2", "C-3", "C-4","C-5", "C-6", "C-7",\cr
#' \tab "D-1", "M-1", "M-2", "M-3", "M-4", "NF",\cr
#' \tab "D-1", "S-2", "S-3", "O-1a", "O-1b", and\cr
#' \tab  "WA", where "WA" and "NF" stand for \cr
#' \tab "water" and "non-fuel", respectively.\cr\cr
#'  
#' \var{LAT} \tab Latitude [decimal degrees] \tab 55\cr 
#' \var{LONG} \tab Longitude [decimal degrees] \tab -120\cr 
#' \var{FFMC} \tab Fine fuel moisture code [FWI System component] \tab 90\cr 
#' \var{BUI} \tab Buildup index [FWI System component] \tab 60\cr 
#' \var{WS} \tab Wind speed [km/h] \tab 10\cr
#' \var{GS} \tab Ground Slope [percent] \tab 0\cr 
#' \var{Dj} \tab Julian day \tab 180\cr 
#' \var{Aspect} \tab Aspect of the slope [decimal degrees] \tab 0\cr\cr 
#' 
#' \bold{Optional Inputs (1):}
#' \tab Variables associated with certain fuel \cr
#' \tab types. These could be skipped if relevant \cr
#' \tab fuel types do not appear in the input data.\cr\cr
#' 
#' \bold{Input} \tab \bold{Full names of inputs} \tab \bold{Defaults}\cr 
#' 
#' \var{PC} \tab Percent Conifer for M1/M2 [percent] \tab 50\cr 
#' \var{PDF} \tab Percent Dead Fir for M3/M4 [percent] \tab 35\cr
#' \var{cc} \tab Percent Cured for O1a/O1b [percent] \tab 80\cr 
#' \var{GFL} \tab Grass Fuel Load [kg/m^2] \tab 0.35\cr\cr 
#' 
#' \bold{Optional Inputs (2):} 
#' \tab Variables that could be ignored without \cr
#' \tab causing major impacts to the primary outputs\cr\cr
#' 
#' \bold{Input} \tab \bold{Full names of inputs} \tab \bold{Defaults}\cr 
#' \var{CBH}   \tab Crown to Base Height [m] \tab 3\cr 
#' \var{WD}    \tab Wind direction [decimal degrees] \tab 0\cr 
#' \var{Accel} \tab Acceleration: 1 = point, 0 = line \tab 0\cr 
#' \var{ELV*}  \tab Elevation [meters above sea level] \tab NA\cr 
#' \var{BUIEff}\tab Buildup Index effect: 1=yes, 0=no \tab 1\cr 
#' \var{D0}    \tab Julian day of minimum Foliar Moisture Content \tab 0\cr 
#' \var{hr}    \tab Hours since ignition \tab 1\cr 
#' \var{ISI}   \tab Initial spread index \tab 0\cr 
#' \var{CFL}   \tab Crown Fuel Load [kg/m^2]\tab 1.0\cr 
#' \var{FMC}   \tab Foliar Moisture Content if known [percent] \tab 0\cr 
#' \var{SH}    \tab C-6 Fuel Type Stand Height [m] \tab 0\cr 
#' \var{SD}    \tab C-6 Fuel Type Stand Density [stems/ha] \tab 0\cr 
#' \var{theta} \tab Elliptical direction of calculation [degrees] \tab 0\cr\cr }
#' 
#' @param output FBP output offers 3 options (see details in \bold{Values}
#' section):
#' 
#' \tabular{lc}{ \bold{Outputs} \tab \bold{Number of outputs}\cr 
#' \var{Primary(\bold{default})} \tab 8\cr 
#' \var{Secondary} \tab 34\cr 
#' \var{All} \tab 42\cr\cr}
#' 
#' @param m Optimal number of pixels at each iteration of computation when
#' \code{nrow(input) >= 1000}. Default \code{m = NULL}, where the function will
#' assign \code{m = 1000} when \code{nrow(input)} is between 1000 and 500,000,
#' and \code{m = 3000} otherwise. By including this option, the function is
#' able to process large dataset more efficiently. The optimal value may vary
#' with different computers.
#' 
#' @param cores Number of CPU cores (integer) used in the computation, default
#' is 1.  By signing \code{cores > 1}, the function will apply parallel
#' computation technique provided by the \code{foreach} package, which
#' significantly reduces the computation time for large input data (over a
#' million records). For small dataset, \code{cores=1} is actually faster.
#' 
#' @return \code{FireBehaviourPrediction} returns a dataframe with primary, secondary, or all
#' output variables, a combination of the primary and secondary outputs.
#' 
#' \bold{Primary} FBP output includes the following 8 variables: 
#' 
#' \item{CFB}{Crown Fraction Burned by the head fire} 
#' \item{CFC}{Crown Fuel Consumption [kg/m^2]} 
#' \item{FD}{Fire description (1=Surface, 2=Intermittent, 3=Crown)} 
#' \item{HFI}{Head Fire Intensity [kW/m]}
#' \item{RAZ}{Spread direction azimuth [degrees]} 
#' \item{ROS}{Equilibrium Head Fire Rate of Spread [m/min]} 
#' \item{SFC}{Surface Fuel Consumption [kg/m^2]} 
#' \item{TFC}{Total Fuel Consumption [kg/m^2]}
#' 
#' \bold{Secondary} FBP System outputs include the following 34 raster layers. In order
#' to calculate the reliable secondary outputs, depending on the outputs, 
#' optional inputs may have to be provided.  
#' 
#' \item{BE}{BUI effect on spread rate} 
#' \item{SF}{Slope Factor (multiplier for ROS increase upslope)} 
#' \item{ISI}{Initial Spread Index} 
#' \item{FFMC}{Fine fuel moisture code [FWI System component]} 
#' \item{FMC}{Foliar Moisture Content [\%]} 
#' \item{Do}{Julian Date of minimum FMC} 
#' \item{RSO}{Critical spread rate for crowning [m/min]}
#' \item{CSI}{Critical Surface Intensity for crowning [kW/m]}
#' \item{FROS}{Equilibrium Flank Fire Rate of Spread [m/min]}
#' \item{BROS}{Equilibrium Back Fire Rate of Spread [m/min]}
#' \item{HROSt}{Head Fire Rate of Spread at time hr [m/min]}
#' \item{FROSt}{Flank Fire Rate of Spread at time hr [m/min]}
#' \item{BROSt}{Back Fire Rate of Spread at time hr [m/min]}
#' \item{FCFB}{Flank Fire Crown Fraction Burned} 
#' \item{BCFB}{Back Fire Crown Fraction Burned} 
#' \item{FFI}{Equilibrium Spread Flank Fire Intensity [kW/m]} 
#' \item{BFI}{Equilibrium Spread Back Fire Intensity [kW/m]} 
#' \item{FTFC}{Flank Fire Total Fuel Consumption [kg/m^2] } 
#' \item{BTFC}{Back Fire Total Fuel Consumption [kg/m^2] } 
#' \item{DH}{Head Fire Spread Distance after time hr [m] }
#' \item{DB}{Back Fire Spread Distance after time hr [m] }
#' \item{DF}{Flank Fire Spread Distance after time hr [m] }
#' \item{TI}{Time to Crown Fire Initiation [hrs since ignition] }
#' \item{FTI}{Time to Flank Fire Crown initiation [hrs since ignition]} 
#' \item{BTI}{Time to Back Fire Crown initiation [hrs since ignition]} 
#' \item{LB}{Length to Breadth ratio} 
#' \item{LBt}{Length to Breadth ratio after elapsed time hr } 
#' \item{WSV}{Net vectored wind speed [km/hr]} 
#' \item{TROS*}{Equilibrium Rate of Spread at bearing theta [m/min] } 
#' \item{TROSt*}{Rate of Spread at bearing theta at time t [m/min] } 
#' \item{TCFB*}{Crown Fraction Burned at bearing theta } 
#' \item{TFI*}{Fire Intensity at bearing theta [kW/m] } 
#' \item{TTFC*}{Total Fuel Consumption at bearing theta [kg/m^2] } 
#' \item{TTI*}{Time to Crown Fire initiation at bearing theta [hrs since ignition] }
#' 
#' *These outputs represent fire behaviour at a point on the perimeter of an
#' elliptical fire defined by a user input angle theta. theta represents the
#' bearing of a line running between the fire ignition point and a point on the
#' perimeter of the fire. It is important to note that in this formulation the
#' theta is a bearing and does not represent the angle from the semi-major axis
#' (spread direction) of the ellipse. This formulation is similar but not
#' identical to methods presented in Wotton et al (2009) and Tymstra et al
#' (2009).
#' @author Xianli Wang, Alan Cantin, Marc-André Parisien, Mike Wotton, Kerry
#' Anderson, and Mike Flannigan
#' @seealso \code{\link{fwi}, \link{fbpRaster}}
#' @references 1.  Hirsch K.G. 1996. Canadian Forest Fire Behavior Prediction
#' (FBP) System: user's guide. Nat. Resour. Can., Can. For. Serv., Northwest
#' Reg., North. For. Cent., Edmonton, Alberta. Spec. Rep. 7. 122p.
#' 
#' 2.  Forestry Canada Fire Danger Group. 1992. Development and structure of
#' the Canadian Forest Fire Behavior Prediction System. Forestry Canada,
#' Ottawa, Ontario Information Report ST-X-3. 63 p.
#' \url{http://cfs.nrcan.gc.ca/pubwarehouse/pdfs/10068.pdf}
#' 
#' 3.  Wotton, B.M., Alexander, M.E., Taylor, S.W. 2009. Updates and revisions
#' to the 1992 Canadian forest fire behavior prediction system. Nat. Resour.
#' Can., Can. For. Serv., Great Lakes For. Cent., Sault Ste. Marie, Ontario,
#' Canada. Information Report GLC-X-10, 45p.
#' \url{http://publications.gc.ca/collections/collection_2010/nrcan/Fo123-2-10-2009-eng.pdf}
#' 
#' 4.  Tymstra, C., Bryce, R.W., Wotton, B.M., Armitage, O.B. 2009. Development
#' and structure of Prometheus: the Canadian wildland fire growth simulation
#' Model. Nat. Resour. Can., Can. For. Serv., North. For. Cent., Edmonton, AB.
#' Inf. Rep. NOR-X-417.\url{https://d1ied5g1xfgpx8.cloudfront.net/pdfs/31775.pdf}
#' @keywords methods
#' @examples
#' 
#' library(cffdrs)
#' # The dataset is the standard test data for FPB system
#' # provided by Wotton et al (2009)
#' data("test_fbp")
#' head(test_fbp)
#' #  id FuelType LAT LONG ELV FFMC BUI   WS WD GS  Dj  D0         hr PC PDF GFL cc theta Accel Aspect BUIEff CBH CFL ISI
#' #1  1      C-1  55  110  NA   90 130 20.0  0 15 182  NA 0.33333333 NA  NA  NA  NA     0     1    270      1  NA  NA   0
#' #2  2       C2  50   90  NA   97 119 20.4  0 75 121  NA 0.33333333 NA  NA  NA  NA     0     1    315      1  NA  NA   0
#' #3  3      C-3  55  110  NA   95  30 50.0  0  0 182  NA 0.08333333 NA  NA  NA  NA     0     1    180      1  NA  NA   0
#' #4  4      C-4  55  105 200   85  82  0.0 NA 75 182  NA 0.50000000 NA  NA  NA  NA     0     1    315      1  NA  NA   0
#' #5  5       c5  55  105  NA   88  56  3.4  0 23 152 145 0.50000000 NA  NA  NA  NA     0     1    180      1  NA  NA   0
#' 
#' #Primary output (default)
#' FireBehaviourPrediction(test_fbp)
#' #or
#' FireBehaviourPrediction(test_fbp,output="Primary") 
#' #or 
#' FireBehaviourPrediction(test_fbp,"P")
#' #Secondary output          
#' FireBehaviourPrediction(test_fbp,"Secondary")
#' #or
#' FireBehaviourPrediction(test_fbp,"S")
#' #All output          
#' FireBehaviourPrediction(test_fbp,"All")
#' #or
#' FireBehaviourPrediction(test_fbp,"A")
#' #For a single record:
#' FireBehaviourPrediction(test_fbp[7,])  	
#' #For a section of the records:
#' FireBehaviourPrediction(test_fbp[8:13,])	
#' #\code{FireBehaviourPrediction} function produces the default values if no data is fed to
#' #the function:
#' FireBehaviourPrediction()
#' 
#' @export FireBehaviourPrediction
FireBehaviourPrediction  <- function(input=NULL, output="Primary") {                                                                                           
  
  #  Quite often users will have a data frame called "input" already attached
  #  to the workspace. To mitigate this, we remove that if it exists, and warn
  #  the user of this case.
  if (!is.na(charmatch("input", search()))) {
    warning("Attached dataset 'input' is being detached to use FireBehaviourPrediction() function.")
    detach(input)
  }
  output <- toupper(output)
  #if input does not exist, then set defaults
  if (is.null(input)) {
    input<-data.frame(FUELTYPE="C2",ACCEL=0,DJ=180,D0=0,ELV=0,BUIEFF=1,HR=1,
                      FFMC=90,ISI=0,BUI=60,WS=10,WD=0,GS=0,ASPECT=0,PC=50,
                      PDF=35,CC=80,GFL=0.35,CBH=3,CFL=1,LAT=55,LONG=-120,
                      FMC=0,THETA=0)
    input[, "FUELTYPE"] <- as.character(input[, "FUELTYPE"])
  }
  #set local scope variables from the parameters for simpler to referencing
  names(input) <- toupper(names(input))
  ID <- input$ID
  FUELTYPE <- toupper(input$FUELTYPE)
  FFMC <- input$FFMC
  BUI <- input$BUI
  WS <- input$WS
  WD <- input$WD
  FMC <- input$FMC
  GS <- input$GS
  LAT <- input$LAT
  LONG <- input$LONG
  ELV <- input$ELV
  DJ <- input$DJ
  D0 <- input$D0
  SD <- input$SD
  SH <- input$SH
  HR <- input$HR
  PC <- input$PC
  PDF <- input$PDF
  GFL <- input$GFL
  CC <- input$CC
  THETA <- input$THETA
  ACCEL <- input$ACCEL
  ASPECT <- input$ASPECT
  BUIEFF <- input$BUIEFF
  CBH <- input$CBH
  CFL <- input$CFL
  ISI <- input$ISI
  n0 <- nrow(input)
  ############################################################################
  #                         BEGIN
  # Set warnings for missing and required input variables.
  # Set defaults for inputs that are not already set.
  ############################################################################
  if (!exists("FUELTYPE") | is.null(FUELTYPE)){ 
    warning("FuelType is a required input, default FuelType = C2 is used in the 
            calculation")
    FUELTYPE <- rep("C2", n0)}
  if (!exists("FFMC") | is.null(FFMC)){ 
    warning("FFMC is a required input, default FFMC = 90 is used in the 
            calculation")
    FFMC <- rep(90, n0)}
  if (!exists("BUI") | is.null(BUI)){ 
    warning("BUI is a required input, default BUI = 60 is used in the 
            calculation")
    BUI <- rep(60, n0)}
  if (!exists("WS") | is.null(WS)){ 
    warning("WS is a required input, WS = 10 km/hr is used in the calculation")
    WS <- rep(10, n0)}
  if (!exists("GS") | is.null(GS)){ 
    warning("GS is a required input,GS = 0 is used in the calculation")
    GS <- rep(0, n0)}
  if (!exists("LAT") | is.null(LAT)){ 
    warning("LAT is a required input, default LAT=55 is used in the 
            calculation")
    LAT <- rep(55, n0)}
  if (!exists("LONG") | is.null(LONG)){ 
    warning("LONG is a required input, LONG = -120 is used in the calculation")
    LONG <- rep(-120, n0)}
  if (!exists("DJ") | is.null(DJ)){ 
    warning("Dj is a required input, Dj = 180 is used in the calculation")
    DJ <- rep(180, n0)}
  if (!exists("ASPECT") | is.null(ASPECT)){ 
    warning("Aspect is a required input, Aspect = 0 is used in the calculation")
    ASPECT <- rep(0, n0)}
  if (!exists("WD") | is.null(WD)) 
    WD <- rep(0, n0)
  if (!exists("FMC") | is.null(FMC)) 
    FMC <- rep(0, n0)
  if (!exists("ELV") | is.null(ELV)) 
    ELV <- rep(0, n0)
  if (!exists("SD") | is.null(SD)) 
    SD <- rep(0, n0)
  if (!exists("SH") | is.null(SH)) 
    SH <- rep(0, n0)
  if (!exists("D0") | is.null(D0)) 
    D0 <- rep(0, n0)
  if (!exists("HR") | is.null(HR)) 
    HR <- rep(1, n0)
  if (!exists("PC") | is.null(PC)) 
    PC <- rep(50, n0)
  if (!exists("PDF") | is.null(PDF)) 
    PDF <- rep(35, n0)
  if (!exists("GFL") | is.null(GFL)) 
    GFL <- rep(0.35, n0)
  if (!exists("CC") | is.null(CC)) 
    CC <- rep(80, n0)
  if (!exists("THETA") | is.null(THETA)) 
    THETA <- rep(0, n0)
  if (!exists("ACCEL") | is.null(ACCEL)) 
    ACCEL <- rep(0, n0)
  if (!exists("BUIEFF") | is.null(BUIEFF)) 
    BUIEFF <- rep(1, n0)
  if (!exists("CBH") | is.null(CBH)) 
    CBH <- rep(0, n0)
  if (!exists("CFL") | is.null(CFL)) 
    CFL <- rep(0, n0)
  if (!exists("ISI") | is.null(ISI)) 
    ISI <- rep(0, n0)
  #Convert Wind Direction from degress to radians
  WD <- WD * pi/180
  #Convert Theta from degress to radians
  THETA <- THETA * pi/180
  ASPECT <- ifelse(is.na(ASPECT), 0, ASPECT)
  ASPECT <- ifelse(ASPECT < 0, ASPECT + 360, ASPECT)
  #Convert Aspect from degress to radians
  ASPECT <- ASPECT * pi/180
  ACCEL <- ifelse(is.na(ACCEL) | ACCEL < 0, 0, ACCEL)
  if (length(ACCEL[!ACCEL %in% c(0, 1)]) > 0) 
    warning("Input variable Accel is out of range, will be assigned to 1")
  ACCEL <- ifelse(!ACCEL %in% c(0, 1), 1, ACCEL)
  DJ <- ifelse(DJ < 0 | DJ > 366, 0, DJ)
  DJ <- ifelse(is.na(DJ), 180, DJ)
  D0 <- ifelse(is.na(D0) | D0 < 0 | D0 > 366, 0, D0)
  ELV <- ifelse(ELV < 0 | ELV > 10000, 0, ELV)
  ELV <- ifelse(is.na(ELV), 0, ELV)
  BUIEFF <- ifelse(BUIEFF <= 0, 0, 1)
  BUIEFF <- ifelse(is.na(BUIEFF), 1, BUIEFF)
  HR <- ifelse(HR < 0, -HR, HR)
  HR <- ifelse(HR > 366 * 24, 24, HR)
  HR <- ifelse(is.na(HR), 0, HR)
  FFMC <- ifelse(FFMC < 0 | FFMC > 101, 0, FFMC)
  FFMC <- ifelse(is.na(FFMC), 90, FFMC)
  ISI <- ifelse(is.na(ISI) | ISI < 0 | ISI > 300, 0, ISI)
  BUI <- ifelse(BUI < 0 | BUI > 1000, 0, BUI)
  BUI <- ifelse(is.na(BUI), 60, BUI)
  WS <- ifelse(WS < 0 | WS > 300, 0, WS)
  WS <- ifelse(is.na(WS), 10, WS)
  WD <- ifelse(is.na(WD) | WD < -2 * pi | WD > 2 * pi, 
               0, WD)
  GS <- ifelse(is.na(GS) | GS < 0 | GS > 200, 0, GS)
  GS <- ifelse(ASPECT < -2 * pi | ASPECT > 2 * pi, 0, GS)
  PC <- ifelse(is.na(PC) | PC < 0 | PC > 100, 50, PC)
  PDF <- ifelse(is.na(PDF) | PDF < 0 | PDF > 100, 35, PDF)
  CC <- ifelse(CC <= 0 | CC > 100, 95, CC)
  CC <- ifelse(is.na(CC), 80, CC)
  GFL <- ifelse(is.na(GFL) | GFL <= 0 | GFL > 100, 0.35, 
                GFL)
  LAT <- ifelse(LAT < -90 | LAT > 90, 0, LAT)
  LAT <- ifelse(is.na(LAT), 55, LAT)
  LONG <- ifelse(LONG < -180 | LONG > 360, 0, LONG)
  LONG <- ifelse(is.na(LONG), -120, LONG)
  THETA <- ifelse(is.na(THETA) | THETA < -2 * pi | THETA > 
                    2 * pi, 0, THETA)
  SD <- ifelse(SD < 0 | SD > 1e+05, -999, SD)
  SD <- ifelse(is.na(SD), 0, SD)
  SH <- ifelse(SH < 0 | SH > 100, -999, SH)
  SH <- ifelse(is.na(SH), 0, SH)
  
  FUELTYPE <- sub("-", "", FUELTYPE)
  FUELTYPE <- sub(" ", "", FUELTYPE)
  if(length(FUELTYPE[is.na(FUELTYPE)])>0){
    warning("FuelType contains NA, using C2 (default) in the calculation")
    FUELTYPE<-ifelse(is.na(FUELTYPE),"C2",FUELTYPE)}
  ############################################################################
  #                         END
  ############################################################################
  ############################################################################
  #                         START
  # Corrections
  ############################################################################
  #Convert hours to minutes
  HR <- HR * 60
  #Corrections to reorient Wind Azimuth(WAZ) and Uphill slode azimuth(SAZ)
  WAZ <- WD + pi
  WAZ <- ifelse(WAZ > 2 * pi, WAZ - 2 * pi, WAZ)
  SAZ <- ASPECT + pi
  SAZ <- ifelse(SAZ > 2 * pi, SAZ - 2 * pi, SAZ)
  #Any negative longitudes (western hemisphere) are translated to positive 
  #  longitudes
  LONG <- ifelse(LONG < 0, -LONG, LONG)
  #Create an id field if it does not exist
  if (exists("ID") && !is.null(ID)) ID<-ID else ID <- row.names(input)  
  ############################################################################
  #                         END
  ############################################################################
  FBP <- data.frame(rbindlist(fctFBP(FUELTYPE, output, ID, HR, LAT, LONG, CBH, SD, SH, CFL, FMC, D0, ELV, DJ, WS, WAZ, SAZ, FFMC, ISI, BUI, PC, PDF, GFL, BUIEFF, GS, CC, ACCEL, THETA)), row.names=NULL)
  return(FBP)
}
