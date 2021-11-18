# provides backwards compatibility with old version of package that had all
# the code that is now in cffdrs.core
library(cffdrs.core)
#' @noRd
.BEcalc <- cffdrs.core::BuildupEffect

#' @noRd
.BROScalc <- cffdrs.core::BackRos

#' @noRd
.buiCalc <- cffdrs.core::Bui

#' @noRd
.C6calc <- cffdrs.core::FbpC6

#' @noRd
.CFBcalc <- cffdrs.core::Cfb

#' @noRd
.dcCalc <- cffdrs.core::Dc

#' @noRd
.direction <- cffdrs.core::Direction

#' @noRd
.DISTtcalc <- cffdrs.core::DistT

#' @noRd
.dmcCalc <- cffdrs.core::Dmc

#' @noRd
.FBPcalc <- cffdrs.core::Fbp

#' @noRd
.ffmcCalc <- cffdrs.core::Ffmc

#' @noRd
.FIcalc <- cffdrs.core::Fi

#' @noRd
.FMCcalc <- cffdrs.core::Fmc

#' @noRd
.FROScalc <- cffdrs.core::FlankRos

#' @noRd
.fwiCalc <- cffdrs.core::Fwi

#' @noRd
.ISIcalc <- cffdrs.core::Isi

#' @noRd
.LBcalc <- cffdrs.core::Lb

#' @noRd
.LBtcalc <- cffdrs.core::LbT

#' @noRd
.ROScalc <- cffdrs.core::Ros

#' @noRd
.ROStcalc <- cffdrs.core::RosT

#' @noRd
.ROSthetacalc <- cffdrs.core::RosTheta

#' @noRd
.SFCcalc <- cffdrs.core::Sfc

#' @noRd
.Slopecalc <- cffdrs.core::Slope

#' @noRd
.TFCcalc <- cffdrs.core::Tfc
