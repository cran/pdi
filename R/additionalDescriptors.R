#' Crown condition
#' @description Calculate crown condition (\%).
#' @param m missing crown (\%)
#' @param t crown transparency (\%)
#' @examples 
#' crownCondition(50,60)
#' @export

crownCondition <- function(m,t) {
  (100 - m) * (1 - t/100)
}

#' Estimated crown volume (m^3)
#' @description Calculate estimated crown volume.
#' @param r crown radius (m)
#' @param h total height (m)
#' @param l lower crown height (m)
#' @param c crown condition (\%)
#' @examples 
#' crownVolume(3,15,10,50)
#' @export

crownVolume <- function(r,h,l,c) {
  (pi * r ^ 2 * c * (h - l)) / 200
}

#' Estimated bleed prevalence (\%)
#' @description Calculate estimated bleed prevalence.
#' @param a average active bleed size (mm)
#' @param A number of active bleeds
#' @param b average black stain size (mm)
#' @param B number of black stains
#' @param d diameter at breast height (m)
#' @param s height to which stem surveyed from the tree base (m)
#' @examples 
#' bleedPrevalence(30,10,40,5,1,1.3)
#' @export

bleedPrevalence <- function(a,A,b,B,d,s = 3) {
  s <- s * 1000
  d <- d * 1000
  
  (a^2 * A + b^2 * B)/(s * d * pi) * 100
}

#' Agrilus exit hole density (m^-2)
#' @description Calculate Agrilus biguttatus exit hole density.
#' @param n number of Agrilus exit holes
#' @param d diameter at breast height (m)
#' @param s height to which stem surveyed from the tree base (m)
#' @examples 
#' agrilusExitHoleDensity(2,1.02,1.3)
#' @export

agrilusExitHoleDensity <- function(n,d,s = 2) {
  n/((s * d) * pi)
}

#' Live crown ratio
#' @description Calculate the live crown ratio
#' @param h total height (m)
#' @param l lower crown height (m)
#' @examples 
#' liveCrownRatio(15,10)
#' @export

liveCrownRatio <- function(h,l){
  (h - l) / h * 100
}

#' Crown surface area (m^2)
#' @description Calculate the crown surface area
#' @param r crown radius (m)
#' @param h total height (m)
#' @param l lower crown height (m)
#' @param c crown condition (\%)
#' @examples 
#' crownSurfaceArea(3,15,10,50)
#' @export

crownSurfaceArea <- function(r,h,l,c){
  cl <- h - l
  ((4 * pi * cl) / (3 * r ^ 2)) * ((r ^ 2 + r ^ 4 / (4 * cl ^ 2)) ^ 1.5 - (r ^ 4 / (4 * cl ^ 2)) ^ 1.5) * c / 100
}

#' Crown production efficiency
#' @description Calculate the crown production efficiency
#' @param crown_surface_area crown surface area (m^2)
#' @param crown_volume crown volume (m^3)
#' @examples 
#' crownProductionEfficiency(34,35)
#' @export

crownProductionEfficiency <- function(crown_surface_area,crown_volume){
  crown_surface_area / crown_volume
}