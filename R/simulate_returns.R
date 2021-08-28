#' Simulate portfolios.
#'
#' @param .data a data frame of an asset.
#' @param n_days_in_market Integer. How long is the asset investment.
#' @param cost Numeric. Cost or expense ratio to asset.
#' @param n_iterations. Integer. Number of iterations to run the simulation.
#' @param loud Logical. Loud!
#' @return data frame of simulation results.
#' @export
#
simulate_returns = function(.data, n_days_in_market=365, cost = 0, n_iterations=1e4, loud = TRUE){

  min_date = min(.data$date)
  max_date = max(.data$date)
  stonk = unique(.data$symbol)
  if(length(stonk) > 1) stop("Multiple symbols in the data. Can only run a simulation for one.", call. = FALSE)

  if(loud){
    message('Simulating returns for ', stonk)
    message(n_days_in_market, " random, consecutive days in the market between ", min_date, " and ", max_date)
  }

  returns = vector("double", length = n_iterations)
  start_dates = vector("double", length = n_iterations)
  end_dates = vector("double", length = n_iterations)
  pb = progress_bar$new(total =n_iterations, format = "[:bar] :current/:total (:percent) (:eta)")

  for(i in 1:n_iterations){

    sample = .data %>%
      slice_sequence(n=n_days_in_market) %>%
      tq_transmute(select = adjusted, mutate_fun = periodReturn, period = "daily") %>%
      mutate(cumreturn = cumreturns(daily.returns)) %>%
      arrange(date)

    start_dates[i] = min(sample$date)

    end_dates[i] = max(sample$date)

    returns[i] = sample %>%
      slice_tail(n=1) %>%
      pull(cumreturn)

    pb$tick()
  }

  tb = tibble("id" = i,
              "ticker" = stonk,
              "start_date" = as.Date(start_dates),
              "end_date" = as.Date(end_dates),
              "expected_return" = returns - cost,
              "positive_return" = ifelse(expected_return > 0, 1, 0),
              "sample_range_min" = min_date,
              "sample_range_max" = max_date
              )

  return(tb)
}
