library(tidyverse)
library(systemfonts)

# need to set graphics device to AGG as per https://systemfonts.r-lib.org/articles/systemfonts.html#using-ragg
# to get fonts to work

#' Creates the BCC Theme for ggplot
#'
#' Creates a base theme for ggplot objects.
#'
#' @param base_family The font to use in this theme. Defaults to "Verdana"
#' @param gridline_x The font to use in this theme
#' @param gridline_y The font to use in this theme
#'
#' @export
#'
#' @import ggplot2 sysfonts showtext
#'

theme_bcc <- function(base_family = "Verdana",
                      gridline_x = T,
                      gridline_y = T) {
  
  gridline_major <- element_line(
    linetype = "solid",
    linewidth = 0.15,
    color = "#999999"
  )
  
  gridline_minor <- element_line(
    linetype = "dashed",
    linewidth = 0.15,
    color = "#999999"
  )
  
  gridline_x_major <- if (isTRUE(gridline_x)) {
    gridline_major
  } else {
    element_blank()
  }
  
  gridline_y_major <- if (isTRUE(gridline_y)) {
    gridline_major
  } else {
    element_blank()
  }
  
  gridline_x_minor <- if (isTRUE(gridline_x)) {
    gridline_minor
  } else {
    element_blank()
  }
  
  gridline_y_minor <- if (isTRUE(gridline_y)) {
    gridline_minor
  } else {
    element_blank()
  }
  
  theme_classic(base_family = base_family) +
    theme(
      panel.background = element_blank(),
      #panel.grid = element_blank(),
      panel.grid.major.x = gridline_x_major,
      panel.grid.major.y = gridline_y_major,
      panel.grid.minor.x = gridline_x_minor,
      panel.grid.minor.y = gridline_y_minor,
      plot.title = element_text(face="bold", size = 16),
      plot.subtitle = element_text(face="italic",size = 10),
      legend.title = element_text(face = "bold")
      )
}

ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
  geom_point(size = 4) +
  scale_colour_bcc(discrete = FALSE, palette = "orange", reverse = TRUE) +
  ggtitle("Test Plot",
          subtitle = "Testing new theme") +
  theme_bcc(gridline_x = F,
            gridline_y = F)

ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
  geom_point(size = 4) +
  scale_colour_bcc(discrete = FALSE, palette = "orange", reverse = TRUE) +
  ggtitle("Test Plot",
          subtitle = "Testing new theme") +
  theme_minimal()

# next look at footer with logo and source text
# based on bbplot https://github.com/bbc/bbplot/blob/master/R/finalise_plot.R