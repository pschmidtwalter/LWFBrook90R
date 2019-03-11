#' Retention-Curve: water content vs. pressure head, van Genuchten
#'
#' @param psi vector of pressure heads
#' @param alpha van Genuchten alpha, unit: reciprocal pressure head
#' @param n van Genuchten parameter N
#' @param ThS van Genuchten parameter Ths (saturation water content)
#' @param ThR van Genuchten parameter Thr (residual water content)
#' @param m van Genuchten parameter M. Default: 1-1/n
#'
#' @return vector of water contents
#' @export
MvG.swc <- function(
  psi, #pressure head in hPa
  alpha, #MvG alpha
  n, # MvG n
  ThS,
  ThR,
  m = 1-1/n)
{
  wetness <- 1/((1 + (alpha * psi)^n))^(m)
  theta <- wetness * (ThS-ThR) + ThR
  return(theta)
}
