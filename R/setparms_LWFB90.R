#' Create the list of model parameters
#'
#' @param ... Named arguments to be included in return value.
#' @return A list with model parameters for use as \code{param.b90}-argument in \code{\link{runLWFB90}}.
#' @details
#' \tabular{llcl}{
#'  \strong{Name} \tab \strong{Description} \tab \strong{Unit} \tab \strong{Group} \cr
#'  czr          \tab Ratio of roughness length to mean height for smooth closed canopies for heights greater than HR when LAI>LPC. Default: 0.05 \tab -                 \tab Canopy                 \cr
#'  czs          \tab Ratio of roughness length to mean height for smooth closed canopies for heights less than HS when LAI>LPC. Default: 0.13 \tab m                 \tab Canopy                 \cr
#'  hr           \tab Smallest height to which CZR applies. Default: 10 \tab m                 \tab Canopy                 \cr
#'  hs           \tab Largest height to which CZS applies. Default: 1\tab m                 \tab Canopy                 \cr
#'  lpc          \tab Minimum leaf area index defining a closed canopy. Default: 4\tab -                 \tab Canopy                 \cr
#'  lwidth       \tab Average leaf width. Default: 0.1\tab m  \tab Canopy                 \cr
#'  nn           \tab Eddy diffusivity extinction coefficient within canopy. Default: 2.5\tab -                 \tab Canopy                 \cr
#'  rhotp        \tab Ratio of total leaf area to projected area. Default: 2\tab -                  \tab Canopy                 \cr
#'  zminh        \tab Reference height for weather data above the canopy top height. Default: 2\tab m \tab Canopy                 \cr
#'  dslope       \tab slope for downslope-flow. Default: 0 \tab deg               \tab Flow \cr
#'  bypar        \tab Switch to allow (1) or prevent (0) bypass flow in deeper layers. Default: 0 \tab -\tab Flow                   \cr
#'  drain        \tab Switch for lower boundary condition to be free drainage (1) or no flow (0). Default: 1  \tab -\tab Flow                   \cr
#'  slopelen     \tab slope length for downslope-flow. Default: 200   \tab m                 \tab Flow                   \cr
#'  gsc          \tab Rate constant for ground water discharge (remember that a first order groundwater reservoir is placed below the soil profile), for value 0 there is no discharge. Default: 0  \tab d-1               \tab Flow                   \cr
#'  gsp          \tab Seepage fraction of groundwater discharge. Default: \tab -  \tab Flow                   \cr
#'  ilayer       \tab Number of layers from top to which infiltration is distributed. Default: 0\tab -                  \tab Flow                   \cr
#'  imperv       \tab Fraction of area which has an impermeable surface (like roads). Default: 0 \tab -                  \tab Flow                   \cr
#'  infexp       \tab Shape parameter for distribution of infiltration in first ILayer, for value 0 infiltration is in top layer only. Default: 0 \tab -                  \tab Flow                   \cr
#'  qffc         \tab Quickflow fraction of infiltrating water at field capacity, for value 0 there is no quickflow (bypass or surface) unless soil profile surface becomes saturated. Default: 0  \tab -                  \tab Flow                   \cr
#'  qfpar        \tab Quickflow shape parameter. Default: 1 \tab -                  \tab Flow                   \cr
#'  qlayer       \tab Number of layers which are considered for generation of surface or source area flow. Default: 0\tab -                  \tab Flow                   \cr
#'  gwatini      \tab Initial value of groundwater storage. Default: 0\tab mm                  \tab Initial                \cr
#'  snowini      \tab Initial value of water content of snow pack. Default: 0\tab mm \tab Initial                \cr
#'  intrainini   \tab Initial value of intercepted rain. Default: 0\tab mm                  \tab Initial                \cr
#'  intsnowini   \tab Initial value of intercepted snow. Default: 0\tab -                  \tab Initial                \cr
#'  psiini       \tab Initial pressure head of soil layers. May have the same length as row.names(soil). Default: -6.3 \tab kPa                \tab Initial  \cr
#'  cintrl       \tab Maximum interception storage of rain per unit LAI. Default: 0.15 \tab mm                \tab Interception           \cr
#'  cintrs       \tab Maximum interception storage of rain per unit SAI. Default: 0.15\tab mm                \tab Interception           \cr
#'  cintsl       \tab Maximum interception storage of snow per unit LAI. Default: 0.6 \tab mm                \tab Interception           \cr
#'  cintss       \tab Maximum interception storage of snow per unit SAI. Default: 0.6 \tab mm                \tab Interception           \cr
#'  frintlai     \tab Intercepted fraction of rain per unit LAI. Default: 0.06 \tab -                  \tab Interception           \cr
#'  frintsai     \tab Intercepted fraction of rain per unit SAI. Default: 0.04\tab -                  \tab Interception           \cr
#'  fsintlai     \tab Intercepted fraction of snow per unit LAI. Default: 0.06 \tab -                  \tab Interception           \cr
#'  fsintsai     \tab Intercepted fraction of snow per unit SAI. Default: 0.04 \tab -                  \tab Interception           \cr
#'  pdur         \tab Average duration of precipitation events for each month of the year. Default: rep(4,12) \tab hours                  \tab Interception           \cr
#'  alb          \tab Albedo of soil/vegetation surface without snow. Default: 0.2 \tab -                  \tab Meteo                  \cr
#'  albsn        \tab Albedo of soil/vegetation surface with snow. Default: 0.5 \tab -                  \tab Meteo                  \cr
#'  c1           \tab Intercept of relation of solar radiation to sunshine duration. Default:  0.25\tab -                  \tab Meteo                  \cr
#'  c2           \tab Intercept of relation of solar radiation to sunshine duration. Default: 0.5                                                                                                    \tab -                 \tab Meteo                  \cr
#'  c3           \tab Constant between 0 and 1 that determines the cloud correction to net longwave radiation from sunshine duration. Default: 0.2\tab -                  \tab Meteo                  \cr
#'  fetch        \tab Fetch upwind of the weather station at which wind speed was measured. Default: 5000 \tab m                 \tab Meteo                  \cr
#'  ksnvp        \tab Correction factor for snow evaporation. Default: 0.3 \tab -                  \tab Meteo                  \cr
#'  wndrat       \tab Average ratio of nighttime to daytime wind speed. Default: 0.3\tab -                  \tab Meteo                  \cr
#'  z0s          \tab Surface roughness of snow cover. Default: 0.001 \tab m                \tab Meteo                  \cr
#'  z0w          \tab Roughness length at the weather station at which wind speed was measured. Default: 0.005 (Grass) \tab m                 \tab Meteo                  \cr
#'  coords_x     \tab Longitude value (decimal degrees) of the simulation location (has no effect on simulation results). Default: 9.91 \tab m                 \tab Meteo                  \cr
#'  coords_y     \tab Latitude value (decimal degrees) of the simulation location. Default: 51.54 \tab m                 \tab Meteo                  \cr
#'  zw           \tab Height at which wind speed was measured. Default: 2 \tab m                 \tab Meteo                  \cr
#'  eslope       \tab slope for evapotranspiration and snowmelt calculation. Default:  0\tab deg               \tab Meteo                  \cr
#'  aspect       \tab Mean exposition of soil surface at soil profile (north: 0, east: 90, south: 180, west: 270). Default: 0\tab deg               \tab Meteo                  \cr
#'  obsheight    \tab Mean height of obstacles on soil surface (grass, furrows etc.), used to calculate soil surface roughness. Default: 0.025 \tab m                 \tab Meteo                  \cr
#'  prec.corr.statexp \tab station exposure situation of prec measurements (passed to \code{\link{prec_corr}} Default: 'mg' \tab                  \tab Meteo                  \cr
#'  dpsimax      \tab maximum potential difference considered equal. Default: 5e-04 \tab kPa               \tab Numerical              \cr
#'  dswmax       \tab maximum change allowed in SWATI. Default: 0.05 \tab percent of SWATMX \tab Numerical              \cr
#'  dtimax       \tab maximum iteration time step. Default: 0.5 \tab d                 \tab Numerical              \cr
#'  budburst.species  \tab Name of tree species for estimating budburst doy using Menzel-model (passed to \code{\link[vegperiod]{vegperiod}}) Default: 'Fagus sylvatica' \tab -                  \tab Plant                  \cr
#'  budburstdoy  \tab Budburst day of year - passed to \code{\link{MakeSeasLAI}}. Default: 121\tab doy                  \tab Plant                  \cr
#'  emergedur    \tab Leaf growth duration until maxlai is reached.. Default: 28\tab d                 \tab Plant                  \cr
#'  height       \tab plant height. Default: 25 \tab m                 \tab Plant                  \cr
#'  height.ini   \tab initial plant height at the beginning of the simulaton. Used for interpolation , ignored if length(height) . Default: 25 \tab m                 \tab Plant                  \cr
#'  leaffalldoy  \tab number of days until maximum lai is reached - passed to \code{\link{MakeSeasLAI}} Default: 279\tab doy               \tab Plant                  \cr
#'  leaffalldur  \tab number of days until minimum lai is reached - passed to \code{\link{MakeSeasLAI}} Default: 58 \tab d                 \tab Plant                  \cr
#'  sai          \tab steam area index. Default: 1 \tab -                 \tab Plant                  \cr
#'  sai.ini      \tab steam area index at the end of the simulation. Ignored if length(height) == 1, Default: 1 \tab -                 \tab Plant                  \cr
#'  shape.leaffall  \tab Shape parameter for leaf fall phase - passed to \code{\link{MakeSeasLAI}} Default: 0.3\tab -                  \tab Plant                  \cr
#'  shape.budburst  \tab shape parameter for leaf growth phase - passed to \code{\link{MakeSeasLAI}} Default: 3\tab -                  \tab Plant                  \cr
#'  shape.optdoy \tab day of year when optimum value is reached - passed to \code{\link{MakeSeasLAI}} Default: 210 \tab doy               \tab Plant                  \cr
#'  lai.doy \tab day of year values for lai-interpolation - passed to \code{\link{MakeSeasLAI}} \tab doy               \tab Plant                  \cr
#'  lai.frac \tab fractional lai values for lai interpolation, corresponding to lai.doy - passed to \code{\link{MakeSeasLAI}} Default: 210 \tab doy               \tab Plant                  \cr
#'  winlaifrac   \tab Minimum LAI as a fraction of maxlai. Default: 0 \tab -                  \tab Plant                  \cr
#'  standprop.table \tab Data.frame with yearly values of vegetation properties with columns 'year','age', 'height', 'maxlai', 'sai', 'densef' \tab                   \tab Plant                  \cr
#'  cs           \tab Ratio of projected stem area index to canopy height. Default: 0.035 \tab m-1\tab Plant\cr
#'  densef       \tab Density factor for MaxLAI, CS, RtLen, RPlant, not <.001, 1 for typical stand. Default: 1\tab -                  \tab Plant                  \cr
#'  densef.ini   \tab density factor (see densef) at the end of the simulation. Ignored if length(densef) == 1. Default: 1\tab -                  \tab Plant\cr
#'  maxlai       \tab Maximum projected leaf area index - passed to \code{\link{MakeSeasLAI}} Default: 5 \tab - \tab Plant                  \cr
#'  radex        \tab Extinction coefficient for solar radiation and net radiation in the canopy. Default: 0.5\tab -                  \tab Potential Transpiration \cr
#'  cvpd         \tab Vapour pressure deficit at which leaf conductance is halved. Default: 2\tab kPa               \tab Potential Transpiration\cr
#'  glmax        \tab Maximum leaf vapour conductance when stomata are fully open. Default: 0.0053 \tab m s-1             \tab Potential Transpiration\cr
#'  glmin        \tab Minimum leaf vapour conductance when stomata are closed. Default: 0.0003\tab m s-1             \tab Potential Transpiration\cr
#'  r5           \tab Solar radiation level at which leaf conductance is half of its value at RM. Default: 100\tab W m-2             \tab Potential Transpiration\cr
#'  rm           \tab Nominal maximum solar shortwave radiation possible on a leaf (to reach glmax). Default: 1000 \tab W m-2             \tab Potential Transpiration\cr
#'  t1           \tab Lower suboptimal temperature threshold for stomata opening - temperature relation. Default: 10\tab deg C             \tab Potential Transpiration\cr
#'  t2           \tab Upper suboptimal temperature threshold for stomata opening - temperature relation. Default: 30 \tab deg C             \tab Potential Transpiration\cr
#'  th           \tab Upper temperature threshold for stomata closure. Default: 40 \tab deg C             \tab Potential Transpiration\cr
#'  tl           \tab Lower temperature threshold for stomata closure. Default: 0\tab deg C             \tab Potential Transpiration\cr
#'  betaroot     \tab Shape parameter for rootlength density depth distribution. Default: 0.97 \tab -                 \tab Roots                  \cr
#'  maxrootdepth \tab Maximum root depth (positive downward) - passed to MakeRelRootDens. Default: -1.5 \tab m                \tab Roots                  \cr
#'  rootden.table \tab Data.frame of relative root density depth distribution with columns 'depth' and 'rootden' \tab                   \tab Roots\cr
#'  rstemp       \tab base temperature for snow-rain transition. Default: -0.5 \tab deg C              \tab Snow                   \cr
#'  ccfac        \tab cold content factor. Default: 0.3 \tab MJ m-2 d-1 K-1    \tab Snow                   \cr
#'  grdmlt       \tab rate of groundmelt of snowpack. Default: 0.35 \tab mm d-1            \tab Snow                   \cr
#'  laimlt       \tab parameter for snowmelt dependence on LAI. Default: 0.2 \tab -                 \tab Snow                   \cr
#'  maxlqf       \tab maximum liquid water fraction of Snow. Default: 0.05 \tab -                  \tab Snow                   \cr
#'  melfac       \tab degree day melt factor for open. Default: 1.5\tab MJ m-2 d-1 K-1    \tab Snow                   \cr
#'  saimlt       \tab parameter for snowmelt dependence on SAI. Default: 0.5 \tab -                  \tab Snow                   \cr
#'  snoden       \tab snow density. Default: 0.3 \tab mm mm-1           \tab Snow                   \cr
#'  rssa         \tab soil evaporation resistance at field capacity. Default: 100 \tab s m-1  \tab Soilevap               \cr
#'  rssb         \tab exponent in relation of RSS to water potential. Default: 1 \tab - \tab Soilevap               \cr
#'  soil_nodes   \tab a data.frame with soil nodes discretization passed to LWF-Brook90 \tab - \tab Soil               \cr
#'  soil_materials \tab a data.frame with soil materials (hydrualic parameters) passed to LWF-Brook90 \tab -  \tab Soil               \cr
#'  age.ini      \tab Age of stand (for root development). Default: 100\tab a                 \tab Water supply           \cr
#'  initrdep     \tab Initial root depth. Default: 0.25\tab m                 \tab Water supply           \cr
#'  initrlen     \tab Initial water-absorbing root length per unit area. Default: 12\tab m/m-2             \tab Water supply           \cr
#'  rgroper      \tab Period of net root growth. Default: 30 \tab a                  \tab Water supply           \cr
#'  rgrorate     \tab Vertical root growth rate. Default: 0.03 \tab m a-1             \tab Water supply           \cr
#'  fxylem       \tab Fraction of internal plant resistance to water flow that is in the Xylem. Default: 0.5 \tab -                  \tab Water supply           \cr
#'  maxrlen      \tab Total length of fine roots per unit ground area. Default: 3000 \tab m m-2             \tab Water supply           \cr
#'  mxkpl        \tab Maximum internal conductivity for water flow through the plants. Default: 8 \tab mm d-1 MPa-1      \tab Water supply           \cr
#'  nooutf       \tab Switch that prevents outflow from the root to the soil when the soil is dry. Default: 1\tab -                  \tab Water supply           \cr
#'  psicr        \tab Critical leaf water potential at which stomates close. Default: -2 \tab MPa               \tab Water supply           \cr
#'  rrad         \tab Average radius of the fine or water-absorbing roots. Default: 0.35 \tab mm                \tab Water supply
#'}
#' @examples
#' # Default parameter
#' parms <- setparam_LWFB90()
#' # Include specific parameters
#' parms_maxlai <- setparam_LWFB90(maxlai = c(4,6,5), height =20)
#' @export
setparam_LWFB90 <- function(...) {

  param <- list(
    maxlai = 5,
    sai = 1,
    sai.ini = 1,
    height = 25,
    height.ini = 25,
    densef = 1,
    densef.ini = 1,
    age.ini = 100,
    standprop.table = NULL,
    winlaifrac = 0,
    budburst.species = "Fagus sylvatica",
    budburstdoy = 121,
    leaffalldoy = 279,
    shape.budburst = 0.3,
    shape.leaffall = 3,
    shape.optdoy = 210,
    emergedur = 28,
    leaffalldur = 58,
    lai.doy = NULL,
    lai.frac = NULL,
    alb = 0.2,
    albsn = 0.5,
    ksnvp = 0.3,
    fxylem = 0.5,
    mxkpl = 8,
    lwidth = 0.1,
    psicr = -2,
    nooutf = 1,
    lpc = 4,
    cs = 0.035,
    czs = 0.13,
    czr = 0.05,
    hs = 1,
    hr = 10,
    rhotp = 2,
    nn = 2.5,
    maxrlen = 3000,
    initrlen = 12,
    initrdep = 0.25,
    rrad = 0.35,
    rgrorate = 0.03,
    rgroper = 30,
    maxrootdepth = -1.5,
    betaroot = 0.97,
    rootden.table = NULL,
    radex = 0.5,
    glmax = 0.0053,
    glmin = 3e-04,
    rm = 1000,
    r5 = 100,
    cvpd = 2,
    tl = 0,
    t1 = 10,
    t2 = 30,
    th = 40,
    frintlai = 0.06,
    frintsai = 0.06,
    fsintlai = 0.04,
    fsintsai = 0.04,
    cintrl = 0.15,
    cintrs = 0.15,
    cintsl = 0.6,
    cintss = 0.6,
    infexp = 0,
    bypar = 0,
    qfpar = 1,
    qffc = 0,
    imperv = 0,
    drain = 1,
    gsc = 0,
    gsp = 0,
    ilayer = 1,
    qlayer = 0,
    z0s = 0.001,
    rstemp = -0.5,
    melfac = 1.5,
    ccfac = 0.3,
    laimlt = 0.2,
    saimlt = 0.5,
    grdmlt = 0.35,
    maxlqf = 0.05,
    snoden = 0.3,
    obsheight = 0.025,
    prec.corr.statexp = 'mg',
    rssa = 100,
    rssb = 1,
    soil_nodes = NULL,
    soil_materials = NULL,
    dtimax = 0.5,
    dswmax = 0.05,
    dpsimax = 5e-04,
    wndrat = 0.3,
    fetch = 5000,
    z0w = 0.005,
    zw = 2,
    zminh = 2,
    coords_x = 9.9095,
    coords_y = 51.544,
    c1 = 0.25,
    c2 = 0.5,
    c3 = 0.2,
    pdur = rep(4,times = 12),
    eslope = 0,
    aspect = 0,
    dslope = 0,
    slopelen = 200,
    intrainini = 0,
    intsnowini = 0,
    gwatini = 0,
    snowini = 0,
    psiini = -6.3
)

  dots <- list(...)

  if (length(dots) > 0 ) {
    if (length(dots[which(names(dots) %in% names(param))]) < length(dots)) {
      warning(paste("Not all arguments found in list! Check names:",
                    names(dots[which(!names(dots) %in% names(param))])
      ))
    }
    param[match(names(dots),names(param))] <- dots
  }
  param
}
