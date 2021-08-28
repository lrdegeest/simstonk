#' A ggplot theme.
#'
#' @param base_size Integer. Base size of font.
#' @param family  Character. Font family. Defaults to "sans".
#' @param face Character. Font face. Defaults to "bold".
#' @param hjust Integer. Adjustment of y axis title position. Defaults to 0.
#' @return ggplot theme.
#' @import ggplot2
#' @export
theme_stonk = function(base_size = 16, family = "sans", face = "bold", hjust = 0){
  theme_minimal(base_size) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank()) +
    theme(text = element_text(family = family),
          axis.title.y.left = element_text(size=base_size/2,face=face, hjust = hjust, vjust = 2),
          axis.title.y.right = element_text(size=base_size/2,face=face, hjust = hjust, vjust = 2),
          axis.title.x = element_text(size=base_size/2,face=face, hjust = 1),
          plot.caption = element_text(size = (base_size/2)-1))
}
