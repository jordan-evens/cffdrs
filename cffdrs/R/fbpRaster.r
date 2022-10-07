#' Raster-based Fire Behavior Prediction System Calculations
#' 
#' \code{fbpRaster} calculates the outputs from the Canadian Forest Fire
#' Behavior Prediction (FBP) System (Forestry Canada Fire Danger Group 1992)
#' based on raster format fire weather and fuel moisture conditions (from the
#' Canadian Forest Fire Weather Index (FWI) System (Van Wagner 1987)), fuel
#' type, date, and slope. Fire weather, for the purpose of FBP System
#' calculation, comprises observations of 10 m wind speed and direction at the
#' time of the fire, and two associated outputs from the Fire Weather Index
#' System, the Fine Fuel Moisture Content (FFMC) and Buildup Index (BUI).
#' Raster-based FWI System components can be calculated with the sister
#' function \code{\link{fwiRaster}}.
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
#' not included in the system. This fbp function included these updates and
#' corrections to the original equations and provides a complete suite of fire
#' behavior prediction variables. Default values of optional input variables
#' provide a reasonable mid-range setting. Latitude, longitude, elevation, and
#' the date are used to calculate foliar moisture content, using a set of
#' models defined in the FBP System; note that this latitude/longitude-based
#' function is only valid for Canada. If the Foliar Moisture Content (FMC) is
#' specified directly as an input, the fbp function will use this value
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
#' Because raster format data cannot hold characters, we have to code these fuel
#' types in numeric codes. In sequence, the codes are c(1:19). FuelType could 
#' also be converted as factor and assigned to the raster layer, the function 
#' will still work.
#' 
#' \tabular{ll}{
#' \bold{Fuel Type} \tab \bold{code} \cr 
#' \verb{C-1}       \tab 1           \cr 
#' \verb{C-2}       \tab 2           \cr 
#' \verb{C-3}       \tab 3           \cr
#' \verb{C-4}       \tab 4           \cr 
#' \verb{C-5}       \tab 5           \cr 
#' \verb{C-6}       \tab 6           \cr 
#' \verb{C-7}       \tab 7           \cr 
#' \verb{D-1}       \tab 8           \cr 
#' \verb{M-1}       \tab 9           \cr 
#' \verb{M-2}       \tab 10          \cr 
#' \verb{M-3}       \tab 11          \cr 
#' \verb{M-4}       \tab 12          \cr 
#' \verb{NF}        \tab 13          \cr 
#' \verb{O-1a}      \tab 14          \cr 
#' \verb{O-1b}      \tab 15          \cr 
#' \verb{S-1}       \tab 16          \cr 
#' \verb{S-2}       \tab 17          \cr 
#' \verb{S-3}       \tab 18          \cr 
#' \verb{WA}        \tab 19          \cr\cr}
#' 
#' @return Either Primary, Secondary, or all FBP outputs in a raster stack
#' 
#' @param input The input data, a RasterStack containing fuel types, fire
#' weather component, and slope layers (see below). Each vector of inputs
#' defines a single FBP System prediction for a single fuel type and set of
#' weather conditions. The RasterStack can be used to evaluate the FBP System
#' for a single fuel type and instant in time, or multiple records for a single
#' point (e.g., one weather station, either hourly or daily for instance) or
#' multiple points (multiple weather stations or a gridded surface). All input
#' variables have to be named as listed below, but they are case insensitive,
#' and do not have to be in any particular order. Fuel type is of type
#' character; other arguments are numeric. Missing values in numeric variables
#' could either be assigned as NA or leave as blank.
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
#' @param output FBP output offers 3 options (see details in \bold{Values}
#' section):
#' 
#' \tabular{lc}{ \bold{Outputs} \tab \bold{Number of outputs}\cr \var{Primary
#' (\bold{default})} \tab 8\cr \var{Secondary} \tab 34\cr \var{All} \tab 42\cr
#' }
#' @param select Selected outputs
#' 
#' @return \code{fbpRaster} returns a RasterStack with primary, secondary, or 
#' all output variables, a combination of the primary and secondary outputs. 
#' Primary FBP output includes the following 8 raster layers: 
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
#' Secondary FBP System outputs include the following 34 raster layers. In order 
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
#' 
#' @author Xianli Wang, Alan Cantin, Marc-André Parisien, Mike Wotton, Kerry
#' Anderson, and Mike Flannigan
#' @seealso \code{\link{fbp}, \link{fwiRaster}, \link{hffmcRaster}}
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
#' # The dataset is the standard test data for FBP system
#' # provided by Wotton et al (2009), and randomly assigned
#' # to a stack of raster layers
#' test_fbpRaster <- rast(system.file("extdata", "test_fbpRaster.tif", package="cffdrs"))
#' input<-test_fbpRaster
#' # Rast doesn't hold the raster layer names, we have to assign
#' # them:
#' names(input)<-c("FuelType","LAT","LONG","ELV","FFMC","BUI", "WS","WD","GS","Dj","D0","hr","PC",
#' "PDF","GFL","cc","theta","Accel","Aspect","BUIEFF","CBH","CFL","ISI")
#' # Primary outputs:
#' system.time(foo<-fbpRaster(input = input))
#' # Using the "select" option:
#' system.time(foo<-fbpRaster(input = input,select=c("HFI","TFC", "ROS")))
#' # Secondary outputs:
#' system.time(foo<-fbpRaster(input = input,output="S"))
#' # All outputs:
#' #system.time(foo<-fbpRaster(input = input,output="A"))
#' 
#' ### Additional, longer running examples  ###
#' # Keep only the required input layers, the other layers would be
#' # assigned with default values:
#' # keep only the required inputs:
#' dat0<-input[[c("FuelType","LAT","LONG","FFMC","BUI","WS","GS", "Dj","Aspect")]]
#' system.time(foo<-fbpRaster(input = dat0,output="A"))
#' 
#' @export fbpRaster
#' 
fbpRaster <- function(input, output = "Primary", select=NULL){
  
  #  Quite often users will have a data frame called "input" already attached
  #  to the workspace. To mitigate this, we remove that if it exists, and warn
  #  the user of this case. This is also done in Fbp, but we require use
  #  of this variable here before it gets to Fbp
  if (!is.na(charmatch("input", search()))) {
    warning("Attached dataset 'input' is being detached to use fbp() function.")
    detach(input)
  }
  # # input_orig <- input
  # # input <- copy(input)
  # # This will detect if the user has input rasters and will return rasters even
  # # though the whole function will operate with terra. This will deprecate with
  # # raster.
  if(class(input) %in% c("Raster","RasterStack","RasterBrick")){
    old_names <- names(input)
    input <- rast(input)
    names(input) <- old_names
  }
  # input <- rast(input)
  # names(input) <- tolower(names(input))

  #Setup correct output names
  allNames <- c("CFB","CFC","FD","HFI","RAZ","ROS","SFC","TFC","BE","SF","ISI",
                "FFMC", "FMC","D0", "RSO","CSI","FROS","BROS","HROSt","FROSt",
                "BROSt","FCFB", "BCFB","FFI","BFI", "FTFC","BTFC","TI","FTI",
                "BTI","LB","LBt","WSV", "DH","DB","DF","TROS","TROSt", "TCFB",
                "TFI","TTFC","TTI")
  primaryNames <- allNames[1:8]
  secondaryNames <- allNames[9:length(allNames)]
  #If outputs are specified, then check if they exist and stop with an error
  #  if not.
  if (!is.null(select)){
    select <- toupper(select)
    select <- select[!duplicated(select)]
    if(output == "SECONDARY" | output == "S"){
      if (!sort(select %in% secondaryNames)[1]){
        stop("Selected variables are not in the outputs")}
    }
    if (output == "PRIMARY" | output == "P"){
      if (!sort(select %in% primaryNames)[1]){
        stop("Selected variables are not in the outputs")} 
    }
    if (output == "ALL" | output == "A"){
      if (!sort(select %in% allNames)[1]){
        stop("Selected variables are not in the outputs")} 
    }
  }
  names(input) <- toupper(names(input))
  output <- toupper(output)
  # callFBP <- Vectorize(function(FUELTYPE, LAT, LONG, ELV, FFMC, BUI, WS, WD, GS, DJ, D0, HR, PC, PDF, GFL, CC, THETA, ACCEL, ASPECT, BUIEFF, CBH, CFL, ISI)
  # {
  #   output <- "ALL"
  #   ID <- 1
  #   SD <- NULL
  #   SH <- NULL
  #   FMC <- NULL
  #   return(.FireBehaviourPrediction(FUELS[[FUELTYPE]], output, ID, HR, LAT, LONG, CBH, SD, SH, CFL, FMC, D0, ELV, DJ, WS, WD, ASPECT, FFMC, ISI, BUI, PC, PDF, GFL, BUIEFF, GS, CC, ACCEL, THETA))
  # }
  # )
  # FBP <- lapp(x = input,
  #             fun = callFBP)
  #             
  
  #######################################################################################################################
  #######################
  
  #set local scope variables from the parameters for simpler to referencing
  # names(input) <- toupper(names(input))
  ############################################################################
  #                         BEGIN
  # Set warnings for missing and required input variables.
  # Set defaults for inputs that are not already set.
  ############################################################################
  defaults <- c("FUELTYPE"=2,
                "FFMC"=90,
                "BUI"=60,
                "WS"=10,
                "GS"=0,
                "LAT"=55,
                "LONG"=-120,
                "DJ"=180,
                "ASPECT"=0)
  for (i in 1:length(defaults))
  {
    name <- names(defaults)[[i]]
    default <- defaults[[i]]
    if (!(name %in% names(input)))
    {
      warning(paste0(name, " is a required input, default ", name, " = ",
                     default, " is used in the calculation"))
      input[[name]] <- default
    }
  }
  non_null_defaults <- c("WD"=0,
                         "FMC"=0,
                         "ELV"=0,
                         "SD"=0,
                         "SH"=0,
                         "D0"=0,
                         "HR"=1,
                         "PC"=50,
                         "PDF"=35,
                         "GFL"=0.35,
                         "CC"=80,
                         "THETA"=0,
                         "BUIEFF"=1,
                         "CBH"=0,
                         "CFL"=0,
                         "ISI"=0,
                         "ACCEL"=0)
  for (i in 1:length(non_null_defaults))
  {
    name <- names(non_null_defaults)[[i]]
    default <- non_null_defaults[[i]]
    if (!(name %in% names(input)))
    {
      warning(paste0(name, " is a required input, default ", name, " = ",
                     default, " is used in the calculation"))
      input[[name]] <- default
    }
    subst(input[[name]], NA, default)
  }
  is_fuel <- (input[["FUELTYPE"]] != 19) * (input[["FUELTYPE"]] != 13)
  # HACK: fix 0 FFMC being returned from non-fuel
  input[["FFMC"]] <- input[["FFMC"]] * is_fuel
  #Convert Wind Direction from degrees to radians
  input[["WD"]] <- subst(input[["WD"]], NA, 0)
  input[["WD"]] <- input[["WD"]] * pi/180
  #Convert Theta from degrees to radians
  input[["THETA"]] <- input[["THETA"]] * pi/180
  input[["ASPECT"]] <- subst(input[["ASPECT"]], NA, 0)
  # input[["ASPECT"]] <- input[["ASPECT"]] %% 360
  # keep old behaviour
  input[["ASPECT"]] <- lapp(input[["ASPECT"]], fun=function(x){ifelse(x < 0, x + 360, x)})
  #Convert Aspect from degrees to radians
  input[["ASPECT"]] <- input[["ASPECT"]] * pi/180
  # ACCEL <- ifelse(is.na(ACCEL) | ACCEL < 0, 0, ACCEL)
  # if (length(ACCEL[!ACCEL %in% c(0, 1)]) > 0) 
  #   warning("Input variable Accel is out of range, will be assigned to 1")
  # ACCEL <- ifelse(!ACCEL %in% c(0, 1), 1, ACCEL)
  input[["DJ"]] <- classify(input[["DJ"]], rbind(c(-Inf, 0, 0), c(366, Inf, 0)), right=NA)
  input[["DJ"]] <- subst(input[["DJ"]], NA, 180)
  input[["D0"]] <- classify(input[["D0"]], rbind(c(-Inf, 0, 0), c(366, Inf, 0)), right=NA)
  input[["D0"]] <- subst(input[["D0"]], NA, 0)
  # HACK: replicate old behaviour
  input[["D0"]] <- input[["D0"]] * is_fuel
  input[["ELV"]] <- classify(input[["ELV"]], rbind(c(-Inf, 0, 0), c(10000, Inf, 0)), right=NA)
  input[["ELV"]] <- subst(input[["ELV"]], NA, 0)
  #input[["BUIEFF"]] <- classify(input[["BUIEFF"]], c(-Inf, 0, 0))
  input[["BUIEFF"]] <- lapp(input[["BUIEFF"]], fun=function(x){ifelse(x <= 0, 0, 1)})
  # HACK: can't replace NA if everything is NA for some reason. Get:
  # Warning message:
  # [subst] all 'from' values are missing, returning a copy of 'x'
  if (all(is.na(input[["BUIEFF"]][,]))) {
    input[["BUIEFF"]] <- 1
  } else {
    input[["BUIEFF"]] <- subst(input[["BUIEFF"]], NA, 1)
  }
  # originally:
  # HR <- ifelse(HR < 0, -HR, HR)
  # HR <- ifelse(HR > 366 * 24, 24, HR)
  # HACK: for some reason abs(<SpatRaster>) is causing a lot of issues
  fix_abs <- Vectorize(function(x) {ifelse(x < 0, -x, x)})
  #input[["HR"]] <- abs(input[["HR"]])
  #input[["HR"]] <- classify(input[["HR"]], c(366 * 24, Inf, 24), right=NA)
  #input[["HR"]] <- lapp(input[["HR"]], fun=function(x){ifelse(x > 366 * 24.0, 24.0, x)})
  input[["HR"]] <- lapp(input[["HR"]], fun=fix_abs)
  input[["HR"]] <- lapp(input[["HR"]], fun=function(x){ifelse(x > 366 * 24.0, 24.0, x)})
  input[["FFMC"]] <- classify(input[["FFMC"]], rbind(c(-Inf, 0, 0), c(101, Inf, 0)), right=NA)
  input[["ISI"]] <- classify(input[["ISI"]], rbind(c(-Inf, 0, 0), c(300, Inf, 0)), right=NA)
  input[["BUI"]] <- classify(input[["BUI"]], rbind(c(-Inf, 0, 0), c(1000, Inf, 0)), right=NA)
  input[["WS"]] <- classify(input[["WS"]], rbind(c(-Inf, 0, 0), c(300, Inf, 0)), right=NA)
  input[["WD"]] <- classify(input[["WD"]], rbind(c(-Inf, -2 * pi, 0), c(2 * pi, Inf, 0)), right=NA)
  input[["GS"]] <- classify(input[["GS"]], rbind(c(-Inf, 0, 0), c(200, Inf, 0)), right=NA)
  # keep old behaviour
  # GS <- ifelse(ASPECT < -2 * pi | ASPECT > 2 * pi, 0, GS)
  input[["GS"]] <- lapp(input[[c("GS", "ASPECT")]], fun=function(GS, ASPECT){ifelse(ASPECT < -2 * pi | ASPECT > 2 * pi, 0, GS)})
  input[["PC"]] <- classify(input[["PC"]], rbind(c(-Inf, 0, 50), c(100, Inf, 50)), right=NA)
  input[["PDF"]] <- classify(input[["PDF"]], rbind(c(-Inf, 0, 35), c(100, Inf, 35)), right=NA)
  input[["CC"]] <- classify(input[["CC"]], rbind(c(-Inf, 0, 95), c(100, Inf, 95)), right=FALSE)
  input[["GFL"]] <- classify(input[["GFL"]], rbind(c(-Inf, 0, 0.35), c(100, Inf, 0.35)), right=FALSE)
  input[["LAT"]] <- classify(input[["LAT"]], rbind(c(-Inf, -90, 0), c(90, Inf, 0)), right=NA)
  input[["LONG"]] <- classify(input[["LONG"]], rbind(c(-Inf, -180, 0), c(360, Inf, 0)), right=NA)
  input[["THETA"]] <- classify(input[["THETA"]], rbind(c(-Inf, -2 * pi, 0), c(2 * pi, Inf, 0)), right=NA)
  input[["SD"]] <- classify(input[["SD"]], rbind(c(-Inf, 0, -999), c(1e+05, Inf, -999)), right=NA)
  input[["SH"]] <- classify(input[["SH"]], rbind(c(-Inf, 0, -999), c(1e+05, Inf, -999)), right=NA)

  ############################################################################
  #                         END
  ############################################################################
  ############################################################################
  #                         START
  # Corrections
  ############################################################################
  #Convert hours to minutes
  input[["HR"]] <- input[["HR"]] * 60
  #Corrections to reorient Wind Azimuth(WAZ) and Uphill slope azimuth(SAZ)
  input[["WAZ"]] <- input[["WD"]] + pi
  fctRadians <- Vectorize(function(v) { return(ifelse(v >= 2 * pi, v - 2 * pi, v))})
  # WAZ <- ifelse(input[["WAZ"]] > 2 * pi, input[["WAZ"]] - 2 * pi, input[["WAZ"]])
  input[["WAZ"]] <- lapp(x=input[["WAZ"]],
                         fun=fctRadians)
  input[["SAZ"]] <- input[["ASPECT"]] + pi
  input[["SAZ"]] <- lapp(x=input[["SAZ"]],
                         fun=fctRadians)
  # SAZ <- ifelse(SAZ > 2 * pi, SAZ - 2 * pi, SAZ)
  # FIX: why would you do this and not make everything 0 - 360?
  # #Any negative longitudes (western hemisphere) are translated to positive 
  # #  longitudes
  # LONG <- ifelse(LONG < 0, -LONG, LONG)
  input[["LONG"]] <- lapp(input[["LONG"]], fun=fix_abs)
  # #Create an id field if it does not exist
  # if (exists("ID") && !is.null(ID)) ID<-ID else ID <- row.names(input)
  ############################################################################
  #                         END
  ############################################################################
  #######################
  # 
  # SFC <- TFC <- HFI <- CFB <- ROS <- 0
  # RAZ <- -999
  # if (output == "SECONDARY" | output == "ALL" | output == "S" | 
  #     output == "A") {
  #   FROS <- BROS <- TROS <- HROSt <- FROSt <- BROSt <- TROSt <- FCFB <- 
  #     BCFB <- TCFB <- FFI <- BFI <- TFI <- FTFC <- BTFC <- TTFC <- 0
  #   TI <- FTI <- BTI <- TTI <- LB <- WSV <- -999
  # }
  # HACK: for now just put everything in one stack
  input[["CBH"]] <- lapp(x=input[[c("FUELTYPE", "CBH", "SD", "SH")]],
                         fun=CrownBaseHeight)
  fctCFL <- Vectorize(function(FUELTYPE, CFL)
  {
    return(ifelse(CFL <= 0 | CFL > 2 | is.na(CFL), FUELS[[FUELTYPE]]$CFL, CFL))
  })
  input[["CFL"]] <- lapp(x=input[[c("FUELTYPE", "CFL")]],
                         fun=fctCFL)
  fctFMC <- Vectorize(function(FUELTYPE, FMC, LAT, LONG, ELV, DJ, D0)
  {
    # if (FUELS[[FUELTYPE]]$name %in% c("D1", "S1", "S2", "S3", "O1A", "O1B"))
    # HACK: include non-fuel as well to match original behaviour
    if (FUELS[[FUELTYPE]]$name %in% c("D1", "S1", "S2", "S3", "O1A", "O1B", "WA", "NF"))
    {
      return(0)
    }
    return(ifelse(FMC <= 0 | FMC > 120 | is.na(FMC),
                  FoliarMoistureContent(LAT, LONG, ELV, DJ, D0),
                  FMC))
  })
  input[["FMC"]] <- lapp(x=input[[c("FUELTYPE", "FMC", "LAT", "LONG", "ELV", "DJ", "D0")]],
                         fun=fctFMC)
  ############################################################################
  #                         END
  ############################################################################
  #Calculate Surface fuel consumption (SFC)
  input[["SFC"]] <- lapp(x=input[[c("FUELTYPE", "FFMC", "BUI", "PC", "GFL")]],
                         fun=SurfaceFuelConsumption)
  #Disable BUI Effect if necessary
  input[["BUI"]] <- lapp(x=input[[c("BUI", "BUIEFF")]],
                         fun=Vectorize(function(BUI, BUIEFF) { return(ifelse(BUIEFF != 1, 0, BUI)) }))
  # HACK: for now just do calculations twice I guess
  fctWSV <- Vectorize(function(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ,
                               FMC, SFC, PC, PDF, CC, CBH, ISI)
  {
    return(SlopeAdjust(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)$WSV)
  })
  input[["WSV0"]] <- lapp(x=input[[c("FUELTYPE", "FFMC", "BUI", "WS", "WAZ", "GS", "SAZ", "FMC", "SFC", "PC", "PDF", "CC", "CBH", "ISI")]],
                          fun=fctWSV)
  fctRAZ <- Vectorize(function(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ,
                               FMC, SFC, PC, PDF, CC, CBH, ISI)
  {
    return(SlopeAdjust(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)$RAZ)
  })
  input[["RAZ0"]] <- lapp(x=input[[c("FUELTYPE", "FFMC", "BUI", "WS", "WAZ", "GS", "SAZ", "FMC", "SFC", "PC", "PDF", "CC", "CBH", "ISI")]],
                          fun=fctRAZ)
  #######################################
  # FIX: do this
  # WSV <- ifelse(GS > 0 & FFMC > 0, WSV0, WS)
  # RAZ <- ifelse(GS > 0 & FFMC > 0, RAZ0, WAZ)
  ##########
  # NOT THIS
  m <- ((input[["GS"]] > 0) & (input[["FFMC"]] > 0))
  not_m <- (1 != m)
  input[["WSV"]] <- (input[["WS"]] * not_m) + (input[["WSV0"]] * m)
  input[["RAZ"]] <- (input[["WAZ"]] * not_m) + (input[["RAZ0"]] * m)
  #######################################
  # Why wouldn't you calculate this all the time??
  # #Calculate or keep Initial Spread Index (ISI)
  # ISI <- ifelse(ISI > 0, ISI, InitialSpreadIndex(FFMC, WSV, TRUE))
  # input[["ISI"]] <- lapp(x=input[[c("FFMC", "WSV")]],
  #                        fun=Vectorize(function(FFMC, WSV) { return(InitialSpreadIndex(FFMC, WSV, TRUE)) }))
  # keep old behaviour of only calculating ISI when it's not > 0
  input[["ISI"]] <- lapp(x=input[[c("ISI", "FFMC", "WSV")]],
                         fun=Vectorize(function(ISI, FFMC, WSV) { return(ifelse(ISI > 0, ISI, InitialSpreadIndex(FFMC, WSV, TRUE))) }))
  input[["CSI"]] <- lapp(x=input[[c("FUELTYPE", "FMC", "CBH")]],
                         fun=CriticalSurfaceIntensity)
  input[["RSO"]] <- lapp(x=input[[c("CSI", "SFC")]],
                         fun=CriticalSurfaceRateOfSpread)
  input[["ROS"]] <- lapp(x=input[[c("FUELTYPE", "ISI", "BUI", "FMC", "SFC", "PC", "PDF", "CC", "CBH")]],
                         fun=RateOfSpread)
  fctCFB <- Vectorize(function(FUELTYPE, CFL, ROS, RSO)
  { return(ifelse(CFL > 0,
                  CrownFractionBurned(FUELTYPE, ROS, RSO),
                  0))
  })
  # HACK: this was producing the wrong result for C6 because it double counts the CFB since it gets used to calculate the ROS
  fctCFB <- Vectorize(function(FUELTYPE, CFL, ROS, RSO, ISI, FMC, BUI, CBH, SFC)
  {
    if (FUELTYPE == 6) {
      # HACK: special case for C6 because it doesn't use final ROS for CFB since final ROS uses CFB
      RSI <- IntermediateSurfaceRateOfSpreadC6(ISI, FMC)
      RSS <- SurfaceRateOfSpreadC6(RSI, BUI)
      RSC <- CrownRateOfSpreadC6(ISI, FMC)
      CSI <- CriticalSurfaceIntensity(FUELTYPE, FMC, CBH)
      #Eq. 57 (FCFDG 1992) Surface fire rate of spread (m/min)
      RSO <- CSI / (300 * SFC)
      CFB <- ifelse(RSC > RSS, CrownFractionBurned(FUELTYPE, RSS, RSO), 0)
      return(CFB)
    }
    return(ifelse(CFL > 0,
                  CrownFractionBurned(FUELTYPE, ROS, RSO),
                  0))
  })
  input[["CFB"]] <- lapp(x=input[[c("FUELTYPE", "CFL", "ROS", "RSO", "ISI", "FMC", "BUI", "CBH", "SFC")]],
                         fun=fctCFB)
  input[["CFC"]] <- lapp(x=input[[c("FUELTYPE", "CFL", "CFB", "PC", "PDF")]],
                         fun=CrownFuelConsumption)
  input[["TFC"]] <- lapp(x=input[[c("CFC", "SFC")]],
                         fun=TotalFuelConsumption)
  input[["HFI"]] <- lapp(x=input[[c("TFC", "ROS")]],
                         fun=FireIntensity)
  # #Adjust Crown Fraction Burned
  # CFB <- ifelse(HR < 0, -CFB, CFB)
  # HACK: assuming you'd only get negative CFB if HR < 0?
  # for some reason this results in TI getting all 0's for CFB
  #input[["CFB"]] <- abs(input[["CFB"]])
  input[["RAZ"]] <- input[["RAZ"]] * 180/pi
  input[["RAZ"]] <- subst(input[["RAZ"]], 360, 0)
  #Calculate Fire Type (S = Surface, C = Crowning, I = Intermittent Crowning)
  # FD <- ifelse(CFB < 0.1, "S", ifelse(CFB >= 0.9, "C", "I"))
  # input[["FD"]] <- copy(input[["CFB"]])
  input[["FD"]] <- input[["CFB"]]
  # HACK: need to include 0 in classification for 1, so go below it
  input[["FD"]] <- classify(input[["FD"]], rbind(c(-Inf, 0.1, 1), c(0.1, 0.9, 2), c(0.9, 1.0, 3)))
  # HACK: set non-fuel to have NaN for FD
  input[["FD"]] <- input[["FD"]] * subst(is_fuel, 0, NaN)
  #Calculate the Secondary Outputs
  if (output == "SECONDARY" | output == "ALL" | output == "S" | 
      output == "A") {
    m <- input[["GS"]] >= 70
    not_m <- (1 != m)
    #Eq. 39 (FCFDG 1992) Calculate Spread Factor (GS is group slope)
    input[["SF"]] <- exp(3.533 * (input[["GS"]]/100)^1.2) * not_m + 10 * m
    # HACK: replicate original behaviour for WA and NF
    input[["SF"]] <- input[["SF"]] * is_fuel
    input[["BE"]] <- lapp(x=input[[c("FUELTYPE", "BUI")]],
                          fun=BuildupEffect)
    input[["LB"]] <- lapp(x=input[[c("FUELTYPE", "WSV")]],
                          fun=LengthToBreadthRatio)
    m <- input[["ACCEL"]] == 0
    not_m <- (1 != m)
    input[["LBt"]] <- m * input[["LB"]] +
                      not_m * lapp(x=input[[c("FUELTYPE", "LB", "HR", "CFB")]],
                                   fun=LengthToBreadthRatioAtTime)
    input[["BROS"]] <- lapp(x=input[[c("FUELTYPE", "FFMC", "BUI", "WSV", "FMC", "SFC", "PC", "PDF", "CC", "CBH")]],
                            fun=BackRateOfSpread)
    input[["FROS"]] <- lapp(x=input[[c("ROS", "BROS", "LB")]],
                            fun=FlankRateOfSpread)
    #Calculate the eccentricity  
    input[["E"]] <- sqrt(1 - 1/input[["LB"]]/input[["LB"]])
    #Calculate the rate of spread towards angle theta (TROS)
    input[["TROS"]] <- input[["ROS"]] * (1 - input[["E"]])/(1 - input[["E"]] * cos(input[["THETA"]] - input[["RAZ"]]))
    #Calculate rate of spread at time t for Flank, Back of fire and at angle 
    #  theta.
    m <- input[["ACCEL"]] == 0
    not_m <- (1 != m)
    input[["ROSt"]] <- m * input[["ROS"]] +
      not_m * lapp(x=input[[c("FUELTYPE", "ROS", "HR", "CFB")]],
                   fun=RateOfSpreadAtTime)
    input[["BROSt"]] <- m * input[["BROS"]] +
      not_m * lapp(x=input[[c("FUELTYPE", "BROS", "HR", "CFB")]],
                   fun=RateOfSpreadAtTime)
    input[["FROSt"]] <- m * input[["FROS"]] +
      not_m * lapp(x=input[[c("ROSt", "BROSt", "LBt")]],
                   fun=FlankRateOfSpread)
    #Calculate rate of spread towards angle theta at time t (TROSt)
    input[["TROSt"]] <- m * input[["TROS"]] +
      not_m * (input[["ROSt"]] * (1 - sqrt(1 - 1/input[["LBt"]]/input[["LBt"]])) /
                 (1 - sqrt(1 - 1/input[["LBt"]]/input[["LBt"]]) * cos(input[["THETA"]] - input[["RAZ"]])))
    #Calculate Crown Fraction Burned for Flank, Back of fire and at angle theta.
    m <- input[["CFL"]] != 0
    # HACK: maintain previous behaviour
    fctCFBC6 <- Vectorize(function(FUELTYPE, ROS, RSO)
    { return(ifelse(6 == FUELTYPE,
                    0,
                    CrownFractionBurned(FUELTYPE, ROS, RSO)))
    })
    input[["FCFB"]] <- m * lapp(x=input[[c("FUELTYPE", "FROS", "RSO")]],
                                fun=fctCFBC6)
    input[["BCFB"]] <- m * lapp(x=input[[c("FUELTYPE", "BROS", "RSO")]],
                                fun=fctCFBC6)
    input[["TCFB"]] <- m * lapp(x=input[[c("FUELTYPE", "TROS", "RSO")]],
                                fun=fctCFBC6)
    fctTFC <- Vectorize(function(FUELTYPE, CFL, CFB, PC, PDF, SFC)
    {
      return(TotalFuelConsumption(CrownFuelConsumption(FUELTYPE, CFL, CFB, PC, PDF), SFC))
    })
    input
    #Calculate Total fuel consumption for the Flank fire, Back fire and at
    #  angle theta
    input[["FTFC"]] <- lapp(x=input[[c("FUELTYPE", "CFL", "FCFB", "PC", "PDF", "SFC")]],
                            fun=fctTFC)
    input[["BTFC"]] <- lapp(x=input[[c("FUELTYPE", "CFL", "BCFB", "PC", "PDF", "SFC")]],
                            fun=fctTFC)
    input[["TTFC"]] <- lapp(x=input[[c("FUELTYPE", "CFL", "TCFB", "PC", "PDF", "SFC")]],
                            fun=fctTFC)
    #Calculate the Fire Intensity at the Flank, Back and at angle theta fire
    input[["FFI"]] <- lapp(x=input[[c("FTFC", "FROS")]],
                           fun=FireIntensity)
    input[["BFI"]] <- lapp(x=input[[c("BTFC", "BROS")]],
                           fun=FireIntensity)
    input[["TFI"]] <- lapp(x=input[[c("TTFC", "TROS")]],
                           fun=FireIntensity)
    #Calculate Rate of spread at time t for the Head, Flank, Back of fire and
    #  at angle theta.
    # HACK: assume HR <0 is only reason these would be negative
    # HROSt <- ifelse(HR < 0, -ROSt, ROSt)
    # FROSt <- ifelse(HR < 0, -FROSt, FROSt)
    # BROSt <- ifelse(HR < 0, -BROSt, BROSt)
    # TROSt <- ifelse(HR < 0, -TROSt, TROSt)
    input[["HROSt"]] <- lapp(input[["ROSt"]], fun=fix_abs)
    input[["FROSt"]] <- lapp(input[["FROSt"]], fun=fix_abs)
    input[["BROSt"]] <- lapp(input[["BROSt"]], fun=fix_abs)
    input[["TROSt"]] <- lapp(input[["TROSt"]], fun=fix_abs)
    
    #Calculate the elapsed time to crown fire initiation for Head, Flank, Back
    # fire and at angle theta. The (a# variable is a constant for Head, Flank, 
    # Back and at angle theta used in the *TI equations)
    # HACK: old version used non-constant equation for every FUELTYPE
    fctTI <- Vectorize(function(FUELTYPE, RSO, ROS, CFB)
    {
      # print(paste(FUELTYPE, RSO, ROS, CFB))
      # # HACK: using .Alpha is giving different results
      # a1 <- 0.115 - (18.8 * CFB^2.5 * exp(-8 * CFB))
      # return(log(ifelse(1 - RSO/ROS > 0, 1 - RSO/ROS, 1))/(-a1))
      return(log(ifelse(1 - RSO/ROS > 0, 1 - RSO/ROS, 1))/(-.Alpha..FuelBase(FUELS[[FUELTYPE]], CFB)))
      #    return(log(ifelse(1 - RSO/ROS > 0, 1 - RSO/ROS, 1))/(-.Alpha(FUELS[[FUELTYPE]], CFB)))
    })
    input[["TI"]] <- lapp(x=input[[c("FUELTYPE", "RSO", "ROS", "CFB")]],
                          fun=fctTI)
    input[["FTI"]] <- lapp(x=input[[c("FUELTYPE", "RSO", "FROS", "FCFB")]],
                           fun=fctTI)
    input[["BTI"]] <- lapp(x=input[[c("FUELTYPE", "RSO", "BROS", "BCFB")]],
                           fun=fctTI)
    input[["TTI"]] <- lapp(x=input[[c("FUELTYPE", "RSO", "TROS", "TCFB")]],
                           fun=fctTI)

    # FIX: shouldn't it be this?
    # TI <- log(ifelse(1 - RSO/ROS > 0, 1 - RSO/ROS, 1))/(-.Alpha(this, CFB))
    # FTI <- log(ifelse(1 - RSO/FROS > 0, 1 - RSO/FROS, 1))/(-.Alpha(this, FCFB))
    # BTI <- log(ifelse(1 - RSO/BROS > 0, 1 - RSO/BROS, 1))/(-.Alpha(this, BCFB))
    # TTI <- log(ifelse(1 - RSO/TROS > 0, 1 - RSO/TROS, 1))/(-.Alpha(this, TCFB))
    
    m <- input[["ACCEL"]] == 0
    not_m <- (1 != m)
    #Fire spread distance for Head, Back, and Flank of fire
    input[["DH"]] <- m * (input[["ROS"]] * input[["HR"]]) +
      not_m * lapp(x=input[[c("FUELTYPE", "ROS", "HR", "CFB")]],
                   fun=DistanceAtTime)
    input[["DB"]] <- m * (input[["BROS"]] * input[["HR"]]) +
      not_m * lapp(x=input[[c("FUELTYPE", "BROS", "HR", "CFB")]],
                   fun=DistanceAtTime)
    input[["DF"]] <- m * ((input[["DH"]] + input[["DB"]])/(input[["LB"]] * 2)) +
      not_m * ((input[["DH"]] + input[["DB"]])/(input[["LBt"]] * 2))
  }
  #######################################
  # seems okay until here so far
  #######################################
  # #if Primary is selected, wrap the primary outputs into a data frame and
  # #  return them
  # if (output == "PRIMARY" | output == "P") {
  #   message("FD = 1,2,3 representing Surface (S),Intermittent (I), and Crown (C) fire")
  #   FBP <- input[[c("CFB", "CFC", "FD", "HFI", "RAZ", "ROS", "SFC", "TFC")]]
  # }
  # #If Secondary is selected, wrap the secondary outputs into a data frame
  # #  and return them.
  # else if (output == "SECONDARY" | output == "S") {
  #   FBP <- input[[c("BE", "SF", "ISI", "FFMC", "FMC", "D0", "RSO",
  #                   "CSI", "FROS", "BROS", "HROSt", "FROSt", "BROSt", "FCFB", "BCFB",
  #                   "FFI", "BFI", "FTFC", "BTFC", "TI", "FTI", "BTI", "LB", "LBt", "WSV",
  #                   "DH", "DB", "DF", "TROS", "TROSt", "TCFB", "TFI", "TTFC", "TTI")]]
  # }
  # #If all outputs are selected, then wrap all outputs into a data frame and
  # #  return it.
  # else if (output == "ALL" | output == "A") {
  #   message("FD = 1,2,3 representing Surface (S),Intermittent (I), and Crown (C) fire")
  #   FBP <- input[[c("CFB", "CFC", "FD", "HFI", "RAZ", "ROS", "SFC", "TFC",
  #                   "BE", "SF", "ISI", "FFMC", "FMC", "D0", "RSO",
  #                   "CSI", "FROS", "BROS", "HROSt", "FROSt", "BROSt", "FCFB", "BCFB",
  #                   "FFI", "BFI", "FTFC", "BTFC", "TI", "FTI", "BTI", "LB", "LBt", "WSV",
  #                   "DH", "DB", "DF", "TROS", "TROSt", "TCFB", "TFI", "TTFC", "TTI")]]
  # }
  # 
  # HACK: set everything to 0 if using non-fuel
  for (lyr in names(input)) {
    if (!(lyr %in% c("FUELTYPE", "FD"))) {
      # this doesn't work if lyr already has NaN values in it
      #input[[lyr]] <- input[[lyr]] * is_fuel
      input[[lyr]] <- lapp(c(input[[lyr]], is_fuel), fun=function(x,y) { ifelse(0==y,0,x)})
    }
  }
  #If caller specifies select outputs, then create a raster stack that contains
  #  only those outputs
  FBP <- NULL
  if (!is.null(select))
  {
    FBP <- input[[select]]
    if ("FD" %in% select)
    {
      message("FD = 1,2,3 representing Surface (S),Intermittent (I), and Crown (C) fire")
    }
  } else if (output == "PRIMARY" | output == "P")
  {
    message("FD = 1,2,3 representing Surface (S),Intermittent (I), and Crown (C) fire")
    FBP <- input[[primaryNames]]
  } else if(output == "SECONDARY" | output == "S")
  {
    FBP <- input[[secondaryNames]]
  } else if(output == "ALL" | output == "A")
  {
    message("FD = 1,2,3 representing Surface (S),Intermittent (I), and Crown (C) fire")
    FBP <- input[[allNames]]
  }
  #return the raster stack to the caller
  return(FBP)
}
