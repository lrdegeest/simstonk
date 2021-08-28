#' Fetch asset data and calculate cumulative returns. Mostly a wrapper for `tidyquant::tq_get()`.
#'
#' @param ticker Character. An asset (e.g., "AAPL"). Call multiple assets by putting them in a vector.
#' @param from  Character. Start date. Defaults to "2007-01-01".
#' @param to Character. End date. Defaults to today.
#' @param returns Logical. Calculate cumulative returns?
#' @param loud Logical. Loud!
#' @param ... additional arguments for`tidyquant::tq_get()`.
#' @return tibble of asset data.
#' @import tidyquant
#' @importFrom lubridate year
#' @import dplyr
#' @export
get_stonk = function(ticker, from = "2007-01-01", to = Sys.Date(), returns = TRUE, loud = TRUE, ...){
  stonk = tq_get(x = ticker, from = from, to = to, ...)
  stonk = stonk %>%
    group_by(symbol) %>%
    mutate(year = year(date)) %>%
    tq_mutate(select = adjusted, mutate_fun = periodReturn, period = "daily") %>%
    ungroup()
  if(returns){
    stonk = stonk %>%
      group_by(symbol) %>%
      mutate(cumreturn = cumreturns(daily.returns)) %>%
      ungroup()
  }
  if(loud) message("Prices of ", paste(ticker, collapse = ' and '), " going back to ", min(stonk$date))
  return(stonk)
}
