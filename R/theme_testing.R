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
                      gridline_y = T,
                      legend_position = c("top", "right", "bottom", "left")) {
  
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
  
  ggplot2::theme_classic(base_family = base_family) +
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      panel.grid.major.x = gridline_x_major,
      panel.grid.major.y = gridline_y_major,
      panel.grid.minor.x = gridline_x_minor,
      panel.grid.minor.y = gridline_y_minor,
      # set plot title elements
      plot.title = ggplot2::element_text(family = base_family,
                                         face="bold", 
                                         size = 28,
                                         color = bcc_cols("black")),
      # set plot subtitle elements
      plot.subtitle = ggplot2::element_text(family = base_family,
                                            face="italic",
                                            size = 22,
                                            color = bcc_cols("black")),
      # legend format
      # legend.title = ggplot2::element_text(family = base_family,
      #                                      size = 20,
      #                                      color = bcc_cols("black")),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(family = base_family,
                                          size = 18,
                                          color= bcc_cols("black")),
      legend.position = legend_position,
      # axis format
      axis.title = ggplot2::element_text(family = base_family,
                                         size = 22,
                                         color= bcc_cols("black")),
      axis.text = ggplot2::element_text(family = base_family,
                                        size = 18,
                                        color = bcc_cols("black")),
      axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10))
      )
}

ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
  geom_point(size = 4) +
  scale_colour_bcc(discrete = FALSE, palette = "orange", reverse = TRUE) +
  labs(title = "Test Plot",
       subtitle = "Testing new theme",
       caption = "Source") +
  theme_bcc(gridline_x = F,
            gridline_y = T)

ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
  geom_point(size = 4) +
  scale_colour_bcc(discrete = T, palette = "multi", reverse = TRUE) +
  labs(title = "Test Plot",
       subtitle = "Testing new theme",
       caption = "Source") +
  theme_bcc(gridline_x = F,
            gridline_y = T,
            legend_position = "top")

ggplot(iris, aes(Species, Sepal.Width, fill = Species)) +
  geom_col() +
  scale_fill_bcc("pink") +
  theme_bcc(legend_position = "bottom") +
  labs(title = "Test Plot",
       subtitle = "Testing new theme",
       caption = "Source")


# next look at footer with logo and source text
# based on bbplot https://github.com/bbc/bbplot/blob/master/R/finalise_plot.R 
# and https://www.markhw.com/blog/logos

# create footer
footer <- grid::grobTree(grid::rasterGrob(png::readPNG("data/logo.png"), x = 0.2))

create_footer <- function (logo_image_path) {
  #Make the footer
  footer <- grid::grobTree(grid::rasterGrob(png::readPNG(logo_image_path), x = 0.8))
  return(footer)
  
}

footer <- create_footer("data/logo.png")

# create plot
plot <- ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
  geom_point(size = 4) +
  scale_colour_bcc(discrete = T, palette = "multi", reverse = TRUE) +
  labs(title = "Test Plot",
       subtitle = "Testing new theme") +
  theme_bcc(gridline_x = F,
            gridline_y = T,
            legend_position = "right")

# combine the two
ggpubr::ggarrange(plot, footer,
                  ncol = 1, nrow = 2,
                  heights = c(1, 0.045/(200/450)))

gridExtra::grid.arrange(plot, footer,
                        height = c(.93, .07))

# try out left align function from bbplot
# aligns title and subtitle to left
left_align <- function(plot_name, pieces){
  grob <- ggplot2::ggplotGrob(plot_name)
  n <- length(pieces)
  grob$layout$l[grob$layout$name %in% pieces] <- 2
  return(grob)
}

plot <- ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
  geom_point(size = 4) +
  scale_colour_bcc(discrete = T, palette = "multi", reverse = TRUE) +
  labs(title = "Test Plot",
       subtitle = "Testing new theme") +
  theme_bcc(gridline_x = F,
            gridline_y = T,
            legend_position = "right")

plot_aligned <- left_align(plot,
                pieces = c("title", "subtitle"))

ggpubr::ggarrange(plot_aligned, footer,
                  ncol = 1, nrow = 2,
                  heights = c(1, 0.045/(200/450)))

save_plot <- function (plot_grid, width, height, save_filepath) {
  grid::grid.draw(plot_grid)
  #save it
  ggplot2::ggsave(filename = save_filepath,
                  plot=plot_grid, width=(width/72), height=(height/72),  bg="white")
}

# combine with finalise_plot from bbplot
finalise_plot <- function(plot_name,
                          save_filepath=file.path(Sys.getenv("TMPDIR"), "tmp-nc.png"),
                          width_pixels=640,
                          height_pixels=450,
                          logo_image_path) {
  
  footer <- create_footer(logo_image_path)
  
  #Draw your left-aligned grid
  plot_left_aligned <- left_align(plot_name, c("subtitle", "title", "caption"))
  plot_grid <- ggpubr::ggarrange(plot_left_aligned, footer,
                                 ncol = 1, nrow = 2,
                                 heights = c(1, 0.045/(height_pixels/450)))
  ## print(paste("Saving to", save_filepath))
  save_plot(plot_grid, width_pixels, height_pixels, save_filepath)
  ## Return (invisibly) a copy of the graph. Can be assigned to a
  ## variable or silently ignored.
  invisible(plot_grid)
}

finalise_plot(plot,
              save_filepath = "testplot3.png",
              width_pixels = 640,
              height_pixels = 450,
              logo_image_path = "data/logo.png")
