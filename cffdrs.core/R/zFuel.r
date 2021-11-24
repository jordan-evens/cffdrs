Fuel <- setClass("Fuel",
                    representation(
                      name="character",
                      a="numeric",
                      b="numeric",
                      c0="numeric"
                    )
)
setGeneric(".BackRateOfSpread",
           function(this, FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH) standardGeneric(".BackRateOfSpread")
)
setGeneric(".BuildupEffect",
           function(this, BUI) standardGeneric(".BuildupEffect")
)
setGeneric(".CrownFuelConsumption",
           function(this, CFL, CFB, PC, PDF) standardGeneric(".CrownFuelConsumption")
)
setGeneric(".DistanceAtTime",
           function(this, ROSeq, HR, CFB) standardGeneric(".DistanceAtTime")
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
.C3 <- setClass(".C3", contains="Fuel")
.C4 <- setClass(".C4", contains="Fuel")
.C5 <- setClass(".C5", contains="Fuel")
.C6 <- setClass(".C6", contains="Fuel")
.C7 <- setClass(".C7", contains="Fuel")
.D1 <- setClass(".D1", contains="Fuel")
.M1 <- setClass(".M1", contains="Fuel")
.M2 <- setClass(".M2", contains="Fuel")
.M3 <- setClass(".M3", contains="Fuel")
.M4 <- setClass(".M4", contains="Fuel")
.S1 <- setClass(".S1", contains="Fuel")
.S2 <- setClass(".S2", contains="Fuel")
.S3 <- setClass(".S3", contains="Fuel")
.O1A <- setClass(".O1A", contains="Fuel")
.O1B <- setClass(".O1B", contains="Fuel")
