#' Plot distribution of simulated portfolio returns.
#'
#' @param .data a data frame from `simulate_returns()`.
#' @param normalized Logical. Plot a normalized histogram?
#' @param cumulative Logical. Overlay the cumulative distribution?
#' @param ... additional ggplot arguments.
#' @return a ggplot object.
#' @export
#
plot_sim = function(.data, normalized = FALSE, cumulative = FALSE, ...){

  n_sims = nrow(.data)
  min_range = .data$sample_range_min
  max_range = .data$sample_range_max
  pr = round(100*mean(.data$positive_return))

  dots = list(...)
  if(is.null(dots$title)) dots$title = unique(.data$ticker)
  if(is.null(dots$subtitle)) dots$subtitle = paste0(pr, "% chance of gains")
  if(is.null(dots$bins)) dots$bins = 100; message("Bins = 100")
  if(is.null(dots$ytitle)){
    if(normalized) dots$ytitle = "Normalized Count"
    else dots$ytitle = "Count"
  }
  if(is.null(dots$xtitle)) dots$xtitle = "Expected Return"
  if(is.null(dots$y2title)) dots$y2title = "ECDF"
  if(is.null(dots$caption)) dots$caption = paste0(scales::comma(n_sims), " Monte Carlo simulations\nData from ",
                                                  min_range, " to ", max_range)

  p = ggplot(.data, aes(x = expected_return)) +
    geom_vline(xintercept = 0, color = "#ff0055", linetype = "dotted") +
    scale_x_continuous(labels = scales::percent) +
    labs(x = dots$xtitle, y = dots$ytitle,
         title = dots$title,
         subtitle = dots$subtitle,
         caption = dots$caption) +
    theme_stonk()

  # handle edge case where pr = 1 (I want positive to always be blue)
  ## kinda hackey but whatever
  if(pr < 100){
    p = p +
      scale_fill_manual(values = c("#617a89", "#0b53c1"),  guide = FALSE) +
      scale_color_manual(values = c("#617a89", "#0b53c1"), guide = FALSE)
  } else {
    p = p +
      scale_fill_manual(values = c("#0b53c1"), guide = FALSE) +
      scale_color_manual(values = c("#0b53c1"), guide = FALSE)
  }

  if(normalized){
    p = p + geom_histogram(
      aes(y = ..ncount..,
          fill = as.factor(positive_return),
          color = NA),
      alpha = 0.75,
      bins = dots$bins)
  } else {
    p = p +
      geom_histogram(
        aes(y = ..count..,
            fill = as.factor(positive_return),
            color = NA),
            alpha = 0.75,
            bins = dots$bins)
  }

  if(cumulative){
    p_data = ggplot_build(p)$data[[2]]
    if(normalized) max_count = max(p_data$ncount)
    else max_count = max(p_data$count)
    p = p +
      stat_ecdf(aes(y= ..y..* max_count), pad = FALSE, size = 1.25) +
      scale_y_continuous(sec.axis=sec_axis(trans = ~./ 1, name=dots$y2title))
  }

  return(p)

}
