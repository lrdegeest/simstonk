#' Slice a random, *ordered* sequence from the data. Similar in spirit to `dplyr::slice_sample()`.
#'
#' @param .data a data frame.
#' @param n Integer. Sample size.
#' @return a sampled data frame.
#' @import dplyr
#' @export
#
slice_sequence = function(.data, n){
  repeat{
    start = base::sample(x = 1:nrow(.data), size = 1)
    end = start + (n-1)
    if(end <= nrow(.data)) break
  }
  sdf = dplyr::slice(.data, start:end)
  return(sdf)
}
