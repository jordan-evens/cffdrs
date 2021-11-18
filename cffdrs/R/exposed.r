# provides backwards compatibility with old version of package that had all
# the code that is now in cffdrs.core
library(cffdrs.core)

#' Fire Behavior Prediction System function
#' 
#' See \code{\link[cffdrs.core:fbp]{cffdrs.core::fbp()}}
#' @export fbp
fbp <- cffdrs.core::fbp

#' Fire Season Start and End
#' 
#' See \code{\link[cffdrs.core:fireSeason]{cffdrs.core::fireSeason()}}
#' @export fireSeason
fireSeason <- cffdrs.core::fireSeason

#' Fire Weather Index System
#' 
#' See \code{\link[cffdrs.core:fwi]{cffdrs.core::fwi()}}
#' @export fwi
fwi <- cffdrs.core::fwi

#' Grass Fuel Moisture Code
#' 
#' See \code{\link[cffdrs.core:gfmc]{cffdrs.core::gfmc()}}
#' @export gfmc
gfmc <- cffdrs.core::gfmc

#' Hourly Fine Fuel Moisture Code
#' 
#' See \code{\link[cffdrs.core:hffmc]{cffdrs.core::hffmc()}}
#' @export hffmc
hffmc <- cffdrs.core::hffmc

#' Sheltered Duff Moisture Code
#' 
#' See \code{\link[cffdrs.core:sdmc]{cffdrs.core::sdmc()}}
#' @export sdmc
sdmc <- cffdrs.core::sdmc

#' Overwintering Drought Code
#' 
#' See \code{\link[cffdrs.core:wDC]{cffdrs.core::wDC()}}
#' @export wDC
wDC <- cffdrs.core::wDC

#' See \code{\link[cffdrs.core:test_fbp]{cffdrs.core::test_fbp}}
#' @export test_fbp
test_fbp <- cffdrs.core::test_fbp

#' See \code{\link[cffdrs.core:test_fwi]{cffdrs.core::test_fwi}}
#' @export test_fwi
test_fwi <- cffdrs.core::test_fwi

#' See \code{\link[cffdrs.core:test_gfmc]{cffdrs.core::test_gfmc}}
#' @export test_gfmc
test_gfmc <- cffdrs.core::test_gfmc

#' See \code{\link[cffdrs.core:test_hffmc]{cffdrs.core::test_hffmc}}
#' @export hffmc
test_hffmc <- cffdrs.core::test_hffmc

#' See \code{\link[cffdrs.core:test_sdmc]{cffdrs.core::test_sdmc}}
#' @export test_sdmc
test_sdmc <- cffdrs.core::test_sdmc

#' See \code{\link[cffdrs.core:test_wDC_fs]{cffdrs.core::test_wDC_fs}}
#' @export test_wDC_fs
test_wDC_fs <- cffdrs.core::test_wDC_fs

#' See \code{\link[cffdrs.core:test_wDC]{cffdrs.core::test_wDC}}
#' @export test_wDC
test_wDC <- cffdrs.core::test_wDC
