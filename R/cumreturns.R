#' Calculate cumulative returns to an asset or cumulative value of investment in an asset.
#'
#' @param returns Numerical vector of daily returns to an asset.
#' @param growth Logical. TRUE: calculate cumulative growth rate. FALSE: calculate cumulative value of investment. Defaults to TRUE.
#' @param principal Numerical. Value of the principal investment. Only used if `growth = FALSE`.
#' @return Numerical vector.
#' @export
cumreturns = function(returns, growth = TRUE, principal = 1L){
  if(growth) cumprod(1 + returns) - 1
  else cumprod(principal + returns)
}
