#' Plot distribution of simulated portfolio returns.
#'
#' @param .data a data frame from `simulate_returns()`.
#' @param which_portfolio Integer or character. Choose a portfolio id (integer) or choose "max" or "min".
#' @param adjust_start Integer. Adjust the start date when pulling asset data. Defaults to zero.
#' @param adjust_end Integer. Adjust the end date when pulling asset data. Defaults to zero.
#' @param prices Logical. Plot asset prices instead of cumulative returns?
#' @param ... additional arguments to `get_stonk()` (which get passed on to `tidyquant::tq_get()`.
#' @return a ggplot object.
#' @export
#
plot_sim_port = function(.data, which_portfolio, adjust_start = 0, adjust_end = 0, prices = FALSE, ...){

  if(is.numeric(which_portfolio)){
    port = .data %>%
      filter(id == which_portfolio)
  }
  else if(which_portfolio == 'min'){
    port = .data %>%
      filter(expected_return == min(expected_return)) %>%
      distinct(start_date, .keep_all = TRUE)

  }
  else if(which_portfolio == 'max'){
    port = .data %>%
      filter(expected_return == max(expected_return)) %>%
      distinct(start_date, .keep_all = TRUE)
  }

  d = get_stonk(port$ticker, from = (port$start_date + adjust_start), to = (port$end_date + adjust), loud = FALSE, ...)

  d = d %>%
    mutate(cumreturns = cumreturns(daily.returns))

  p = d %>%
    ggplot() +
    geom_hline(yintercept = 0, color = "#ff0055", linetype = "dotted") +
    labs(x = "Date",
         title = port$ticker,
         subtitle = paste0(port$start_date, " to ", port$end_date)) +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank())

  if(prices){
    p = p +
      geom_line(aes(x = date, y = close), size = 1, color = "#9999CC") +
      scale_y_continuous(labels = scales::dollar_format()) +
      labs(y = "Closing Price")
  }
  else {
    p = p +
      geom_line(aes(x = date, y = r), size = 1, color = "#9999CC") +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(y = "Cumulative return")
  }

  return(p)
}
