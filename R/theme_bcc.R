#' Creates the BCC Theme for ggplot
#'
#' Creates a base theme for ggplot objects.
#'
#' @param base_family The font to use in this theme. Defaults to "Verdana"
#' @param base_size Base font size, in pts. Defaults to 16
#' @param gridline_x Should horizontal gridlines be applied? Defaults to TRUE
#' @param gridline_y Should vertical gridlines be applied? Defaults to TRUE
#' @param legend_position Position of legend in plot. One of "none", "top", "right", "bottom", "left" or "inside" - defaults to "right"
#'
#' @export
#'
#' @import ggplot2 sysfonts showtext
#'
theme_bcc <- function(base_family = "Verdana",
                      base_size = 16,
                      gridline_x = T,
                      gridline_y = T,
                      legend_position = "right") {

  gridline_major <- ggplot2::element_line(
    linetype = "solid",
    linewidth = 0.15,
    color = "#999999"
  )

  gridline_minor <- ggplot2::element_line(
    linetype = "dashed",
    linewidth = 0.15,
    color = "#999999"
  )

  gridline_x_major <- if (isTRUE(gridline_x)) {
    gridline_major
  } else {
    ggplot2::element_blank()
  }

  gridline_y_major <- if (isTRUE(gridline_y)) {
    gridline_major
  } else {
    ggplot2::element_blank()
  }

  gridline_x_minor <- if (isTRUE(gridline_x)) {
    gridline_minor
  } else {
    ggplot2::element_blank()
  }

  gridline_y_minor <- if (isTRUE(gridline_y)) {
    gridline_minor
  } else {
    ggplot2::element_blank()
  }

  ggplot2::theme_classic(base_family = base_family,
                         base_size = base_size) +
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      panel.grid.major.x = gridline_x_major,
      panel.grid.major.y = gridline_y_major,
      panel.grid.minor.x = gridline_x_minor,
      panel.grid.minor.y = gridline_y_minor,
      # set plot title elements
      plot.title = ggplot2::element_text(family = base_family,
                                         face="bold",
                                         #size = 28,
                                         color = bcc_cols("black")),
      plot.title.position = "panel",
      # set plot subtitle/caption elements
      plot.subtitle = ggplot2::element_text(family = base_family,
                                            face="italic",
                                            #size = 22,
                                            color = bcc_cols("black"),
                                            margin=ggplot2::margin(9,0,9,0)),
      plot.caption = ggplot2::element_text(family = base_family,
                                           face="italic",
                                           #size = 14,
                                           color = bcc_cols("black")),
      plot.caption.position = "panel",
      # legend format
      # legend.title = ggplot2::element_text(family = base_family,
      #                                      size = 20,
      #                                      color = bcc_cols("black")),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(family = base_family,
                                          #size = 18,
                                          color= bcc_cols("black")),
      legend.position = legend_position,
      # axis format
      axis.title = ggplot2::element_text(family = base_family,
                                         #size = 22,
                                         color= bcc_cols("black")),
      axis.text = ggplot2::element_text(family = base_family,
                                        #size = 18,
                                        color = bcc_cols("black")),
      axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
      # facet strip format
      strip.background = ggplot2::element_rect(fill="white"),
      strip.text = ggplot2::element_text(family = base_family,
                                         #size  = 22,
                                         color = bcc_cols("black"))
    )
}
