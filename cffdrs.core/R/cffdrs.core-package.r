#' Canadian Forest Fire Danger Rating System
#' 
#' The cffdrs.core package allows R users to calculate the outputs of the two main
#' components of the Canadian Forest Fire Danger Rating System (CFFDRS;
#' \url{http://cwfis.cfs.nrcan.gc.ca/background/summary/fdr}): the Fire Weather
#' Index (FWI) System
#' (\url{http://cwfis.cfs.nrcan.gc.ca/background/summary/fwi}) and the Fire
#' Behaviour Prediction (FBP) System
#' (\url{http://cwfis.cfs.nrcan.gc.ca/background/summary/fbp}) along with
#' additional methods created and used Canadian fire modelling. These systems
#' are widely used internationally to assess fire danger (FWI System) and
#' quantify fire behavior (FBP System).
#' 
#' The FWI System (Van Wagner 1987) is based on the moisture content and the
#' effect of wind of three classes of forest fuels on fire behavior. It
#' consists of six components: three fuel moisture codes (Fire Fuel Moisture
#' Code, Duff Moisture Code, Drought Code), and three fire behavior indexes
#' representing rate of spread (Initial Spread Index), fuel consumption
#' (Buildup Index), and fire intensity (Fire Weather Index). The FWI System
#' outputs are determined from daily noon weather observations: temperature,
#' relative humidity, wind speed, and 24-hour rainfall.
#' 
#' The FBP System (Forestry Canada Fire Danger Group 1992; Hirsch 1996)
#' provides a set of primary and secondary measures of fire behavior. The
#' primary outputs consist of estimates of fire spread rate, fuel consumption,
#' fire intensity, and fire description (i.e., surface, intermittent, or crown
#' fire). The secondary outputs, which are not used nearly as often, give
#' estimates of fire area, perimeter, perimeter growth rate, and flank and back
#' fire behavior based on a simple elliptical fire growth model. Unlike the FWI
#' System, which is weather based, the FBP System also requires information on
#' vegetation (hereafter, fuel types) and slope (if any) to calculate its
#' outputs. Sixteen fuel types are included in the FBP System, covering mainly
#' major vegetation types in Canada.
#' 
#' \tabular{ll}{ Package: \tab cffdrs.core\cr Type: \tab Package\cr Version: \tab
#' 1.8.16\cr Date: \tab 2020-05-26\cr License: \tab GPL-2\cr }
#' 
#' @name cffdrs.core-package
#' @aliases cffdrs.core-package cffdrs.core
#' @docType package
#' @author Xianli Wang, Alan Cantin, Marc-André Parisien, Mike Wotton, Kerry
#' Anderson, Brett Moore, Tom Schiks, Mike Flannigan, and Jordan Evens
#' 
#' Maintainer: Jordan Evens \email{jordan.evens@nrcan-rncan.gc.ca}
#' @seealso \code{\link{fbp}}, \code{\link{fireSeason}}, \code{\link{fwi}},
#' \code{\link{gfmc}}, \code{\link{hffmc}}, \code{\link{sdmc}},
#' \code{\link{wDC}}
#' @references 1. Van Wagner, C.E. and T.L. Pickett. 1985. Equations and
#' FORTRAN program for the Canadian Forest Fire Weather Index System. Can. For.
#' Serv., Ottawa, Ont. For. Tech. Rep. 33. 18 p.
#' 
#' 2. Van Wagner, C.E. 1987. Development and structure of the Canadian forest
#' fire weather index system. Forest Technology Report 35. (Canadian Forestry
#' Service: Ottawa).
#' 
#' 3. Lawson, B.D. and O.B. Armitage. 2008. Weather guide for the Canadian
#' Forest Fire Danger Rating System. Nat. Resour. Can., Can. For. Serv., North.
#' For. Cent., Edmonton, AB.
#' 
#' 4. Hirsch K.G. 1996. Canadian Forest Fire Behavior Prediction (FBP) System:
#' user's guide. Nat. Resour. Can., Can. For. Serv., Northwest Reg., North.
#' For. Cent., Edmonton, Alberta. Spec. Rep. 7. 122p.
#' 
#' 5. Forestry Canada Fire Danger Group. 1992. Development and structure of the
#' Canadian Forest Fire Behavior Prediction System. Forestry Canada, Ottawa,
#' Ontario Information Report ST-X-3. 63 p.
#' \url{http://cfs.nrcan.gc.ca/pubwarehouse/pdfs/10068.pdf}
#' 
#' 6. Wotton, B.M., Alexander, M.E., Taylor, S.W. 2009. Updates and revisions
#' to the 1992 Canadian forest fire behavior prediction system. Nat. Resour.
#' Can., Can. For. Serv., Great Lakes For. Cent., Sault Ste. Marie, Ontario,
#' Canada. Information Report GLC-X-10, 45p.
#' \url{http://publications.gc.ca/collections/collection_2010/nrcan/Fo123-2-10-2009-eng.pdf}
#' 
#' 7. Tymstra, C., Bryce, R.W., Wotton, B.M., Armitage, O.B. 2009. Development
#' and structure of Prometheus: the Canadian wildland fire growth simulation
#' Model. Nat. Resour. Can., Can. For. Serv., North. For. Cent., Edmonton, AB.
#' Inf. Rep. NOR-X-417.
#' @keywords package
NULL
