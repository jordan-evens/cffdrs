.FuelBase <- setClass(".FuelBase",
                      representation(
                        name="character"
                      )
)
setGeneric(".Alpha",
           function(this, CBH) standardGeneric(".Alpha")
)
setGeneric(".BackRateOfSpread",
           function(this, FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH) standardGeneric(".BackRateOfSpread")
)
setGeneric(".BuildupEffect",
           function(this, BUI) standardGeneric(".BuildupEffect")
)
setGeneric(".CriticalSurfaceIntensity",
           function(this, FMC, CBH) standardGeneric(".CriticalSurfaceIntensity")
)
setGeneric(".CrownBaseHeight",
           function(this, CBH, SD, SH) standardGeneric(".CrownBaseHeight")
)
setGeneric(".CrownFuelConsumption",
           function(this, CFL, CFB, PC, PDF) standardGeneric(".CrownFuelConsumption")
)
setGeneric(".DistanceAtTime",
           function(this, ROSeq, HR, CFB) standardGeneric(".DistanceAtTime")
)
setGeneric(".FireBehaviourPrediction",
           function(this, output, ID, HR, LAT, LONG, CBH, SD, SH, CFL, FMC, D0, ELV, DJ, WS, WAZ, SAZ, FFMC, ISI, BUI, PC, PDF, GFL, BUIEFF, GS, CC, ACCEL, THETA) standardGeneric(".FireBehaviourPrediction")
)
setGeneric(".FoliarMoistureContent",
           function(this, LAT, LONG, ELV, DJ, D0) standardGeneric(".FoliarMoistureContent")
)
setGeneric(".LengthToBreadthRatio",
           function(this, WSV) standardGeneric(".LengthToBreadthRatio")
)
setGeneric(".LengthToBreadthRatioAtTime",
           function(this, LB, HR, CFB) standardGeneric(".LengthToBreadthRatioAtTime")
)
setGeneric(".RateOfSpread",
           function(this, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH) standardGeneric(".RateOfSpread")
)
setGeneric(".RateOfSpreadAtTime",
           function(this, ROSeq, HR, CFB) standardGeneric(".RateOfSpreadAtTime")
)
setGeneric(".SlopeAdjust",
           function(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI) standardGeneric(".SlopeAdjust")
)
setGeneric(".SurfaceFuelConsumption",
           function(this, FFMC, BUI, PC, GFL) standardGeneric(".SurfaceFuelConsumption")
)
Fuel <- setClass("Fuel",
                 representation(
                   a="numeric",
                   b="numeric",
                   c0="numeric",
                   BUIo="numeric",
                   Q="numeric",
                   sfcA="numeric",
                   sfcB="numeric",
                   sfcC="numeric",
                   sfcD="numeric",
                   CBH="numeric",
                   CFL="numeric"
                 ),
                 contains=".FuelBase"
)
setMethod(".CrownBaseHeight",
          "Fuel",
          function(this, CBH, SD, SH)
          {
            CBH <- ifelse(CBH <= 0 | CBH > 50 | is.na(CBH),
                          ifelse((this@name == "C6") & SD > 0 & SH > 0,
                                 -11.2 + 1.06 * SH + 0.0017 * SD,
                                 this@CBH),
                          CBH)
            CBH <- ifelse(CBH < 0, 1e-07, CBH)
            return(CBH)
          }
)
setMethod(".FoliarMoistureContent",
          "Fuel",
          function(this, LAT, LONG, ELV, DJ, D0)
          {
            return(FoliarMoistureContent(LAT, LONG, ELV, DJ, D0))
          }
)
.FUELS <- list(WA=list(a=as.numeric(NA),
                       b=as.numeric(NA),
                       c0=as.numeric(NA),
                       BUIo=as.numeric(NA),
                       Q=as.numeric(NA),
                       sfcA=as.numeric(NA),
                       sfcB=as.numeric(NA),
                       sfcC=as.numeric(NA),
                       sfcD=as.numeric(NA),
                       CBH=as.numeric(NA),
                       CFL=as.numeric(NA)),
               NF=list(a=as.numeric(NA),
                       b=as.numeric(NA),
                       c0=as.numeric(NA),
                       BUIo=as.numeric(NA),
                       Q=as.numeric(NA),
                       sfcA=as.numeric(NA),
                       sfcB=as.numeric(NA),
                       sfcC=as.numeric(NA),
                       sfcD=as.numeric(NA),
                       CBH=as.numeric(NA),
                       CFL=as.numeric(NA)),
               C1=list(a=90, b=0.0649, c0=4.5, BUIo=72, Q=0.9,
                       sfcA=as.numeric(NA), sfcB=as.numeric(NA), sfcC=as.numeric(NA), sfcD=as.numeric(NA),
                       CBH=2, CFL=0.75),
               C2=list(a=110, b=0.0282, c0=1.5, BUIo=64, Q=0.7,
                       sfcA=5.0, sfcB=-0.0115, sfcC=1.0, sfcD=as.numeric(NA),
                       CBH=3, CFL=0.8),
               C3=list(a=110, b=0.0444, c0=3.0, BUIo=62, Q=0.75,
                       sfcA=5.0, sfcB=-0.0164, sfcC=2.24, sfcD=as.numeric(NA),
                       CBH=8, CFL=1.15),
               C4=list(a=110, b=0.0293, c0=1.5, BUIo=66, Q=0.8,
                       sfcA=5.0, sfcB=-0.0164, sfcC=2.24, sfcD=as.numeric(NA),
                       CBH=4, CFL=1.2),
               C5=list(a=30, b=0.0697, c0=4.0, BUIo=56, Q=0.8,
                       sfcA=5.0, sfcB=-0.0149, sfcC=2.48, sfcD=as.numeric(NA),
                       CBH=18, CFL=1.2),
               C6=list(a=30, b=0.0800, c0=3.0, BUIo=62, Q=0.8,
                       sfcA=5.0, sfcB=-0.0149, sfcC=2.48, sfcD=as.numeric(NA),
                       CBH=7, CFL=1.8),
               C7=list(a=45, b=0.0305, c0=2.0, BUIo=106, Q=0.85,
                       sfcA=as.numeric(NA), sfcB=as.numeric(NA), sfcC=as.numeric(NA), sfcD=as.numeric(NA),
                       CBH=10, CFL=0.5),
               D1=list(a=30, b=0.0232, c0=1.6, BUIo=32, Q=0.9,
                       sfcA=1.5, sfcB=-0.0183, sfcC=1.0, sfcD=as.numeric(NA),
                       CBH=0, CFL=0),
               M1=list(a=0, b=0, c0=0, BUIo=50, Q=0.8,
                       sfcA=as.numeric(NA), sfcB=as.numeric(NA), sfcC=as.numeric(NA), sfcD=as.numeric(NA),
                       CBH=6, CFL=0.8),
               M2=list(a=0, b=0, c0=0, BUIo=50, Q=0.8,
                       sfcA=as.numeric(NA), sfcB=as.numeric(NA), sfcC=as.numeric(NA), sfcD=as.numeric(NA),
                       CBH=6, CFL=0.8),
               M3=list(a=120, b=0.0572, c0=1.4, BUIo=50, Q=0.8,
                       sfcA=5.0, sfcB=-0.0115, sfcC=1.0, sfcD=as.numeric(NA),
                       CBH=6, CFL=0.8),
               M4=list(a=100, b=0.0404, c0=1.48, BUIo=50, Q=0.8,
                       sfcA=5.0, sfcB=-0.0115, sfcC=1.0, sfcD=as.numeric(NA),
                       CBH=6, CFL=0.8),
               S1=list(a=75, b=0.0297, c0=1.3, BUIo=38, Q=0.75,
                       sfcA=4.0, sfcB=-0.025, sfcC=4.0, sfcD=-0.034,
                       CBH=0, CFL=0),
               S2=list(a=40, b=0.0438, c0=1.7, BUIo=63, Q=0.75,
                       sfcA=10.0, sfcB=-0.013, sfcC=6.0, sfcD=-0.060,
                       CBH=0, CFL=0),
               S3=list(a=55, b=0.0829, c0=3.2, BUIo=31, Q=0.75,
                       sfcA=12.0, sfcB=-0.0166, sfcC=20.0, sfcD=-0.0210,
                       CBH=0, CFL=0),
               O1A=list(a=190, b=0.0310, c0=1.4, BUIo=01, Q=1.0,
                        sfcA=as.numeric(NA), sfcB=as.numeric(NA), sfcC=as.numeric(NA), sfcD=as.numeric(NA),
                        CBH=0, CFL=0),
               O1B=list(a=250, b=0.0350, c0=1.7, BUIo=01, Q=1.0,
                        sfcA=as.numeric(NA), sfcB=as.numeric(NA), sfcC=as.numeric(NA), sfcD=as.numeric(NA),
                        CBH=0, CFL=0)
)

.FuelClosed <- setClass(".FuelClosed", contains="Fuel")
.FuelOpen <- setClass(".FuelOpen", contains="Fuel")

setMethod(".Alpha",
          ".FuelClosed",
          function(this, CBH)
          {
            #Eq. 72 (FCFDG 1992)
            #Calculate the alpha constant for the DISTt calculation
            alpha <- 0.115 - 18.8 * (CBH**2.5) * exp(-8* CBH)
            return (alpha)
          }
)
setMethod(".Alpha",
          ".FuelOpen",
          function(this, CBH)
          {
            #Eq. 72 (FCFDG 1992)
            #Calculate the alpha constant for the DISTt calculation
            alpha <- 0.115
            return (alpha)
          }
)

setMethod(".FoliarMoistureContent",
          ".FuelOpen",
          function(this, LAT, LONG, ELV, DJ, D0)
          {
            return(0)
          }
)
.FuelGrass <- setClass(".FuelGrass", contains=".FuelOpen")
.FuelSlash <- setClass(".FuelSlash", contains=".FuelOpen")
.FuelMixedwood <- setClass(".FuelMixedwood", contains=".FuelClosed")
.FuelMixedDead <- setClass(".FuelMixedDead", contains=".FuelClosed")
