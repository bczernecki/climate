#' Compute relative humidity from air temperature and dew-point temperature
#'
#' Uses the August-Roche-Magnus approximation to derive relative humidity from
#' the 2-metre air temperature and dew-point temperature.
#'
#' @param t2m Numeric vector. Air temperature (2 m) in degrees Celsius.
#' @param dpt2m Numeric vector. Dew-point temperature (2 m) in degrees Celsius.
#'   Must be the same length as `t2m`.
#'
#' @return Numeric vector of relative humidity values in percent (0–100).
#'   Returns `NA` where either input is `NA`. Values are not clamped, so
#'   rounding errors may produce results marginally outside 0–100.
#'
#' @details
#' The August-Roche-Magnus approximation is:
#'
#' \deqn{RH = 100 \times
#'   \frac{\exp\!\bigl(\tfrac{17.625\,T_d}{243.04 + T_d}\bigr)}
#'        {\exp\!\bigl(\tfrac{17.625\,T}{243.04 + T}\bigr)}}
#'
#' where \eqn{T} is the air temperature and \eqn{T_d} is the dew-point
#' temperature, both in degrees Celsius. The coefficients (17.625 and 243.04)
#' follow Alduchov & Eskridge (1996).
#'
#' @references
#' Alduchov, O. A., & Eskridge, R. E. (1996). Improved Magnus form approximation
#' of saturation vapor pressure. *Journal of Applied Meteorology*, 35(4), 601–609.
#'
#' @examples
#' compute_relative_humidity(t2m = 20, dpt2m = 10)   # ~52 %
#' compute_relative_humidity(t2m = 0,  dpt2m = 0)    # 100 %
#' compute_relative_humidity(t2m = c(20, 15, NA), dpt2m = c(10, 12, 8))
#'
#' @export
compute_relative_humidity = function(t2m, dpt2m) {
  if (!is.numeric(t2m) || !is.numeric(dpt2m)) {
    stop("`t2m` and `dpt2m` must be numeric vectors")
  }
  if (length(t2m) != length(dpt2m)) {
    stop("`t2m` and `dpt2m` must have the same length")
  }
  a = 17.625
  b = 243.04
  100 * exp((a * dpt2m) / (b + dpt2m)) / exp((a * t2m) / (b + t2m))
}
