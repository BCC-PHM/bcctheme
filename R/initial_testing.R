library(tidyverse)

bcc_colours <- c(`purple` = "#84329B",
                 `pink`   = "#D00070",
                 `orange` = "#DC582A",
                 `yellow` = "#FFAD00",
                 `green`  = "#75BC22",
                 `blue`   = "#00A9E0",
                 `black`  = "#3c3c3b",
                 `white`  = "#FFFFFF")

#' Function to extract BCC colors as hex codes
#'
#' @param ... Character names of bcc_colours 
#'
bcc_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (bcc_colours)
  
  bcc_colours[cols]
}

bcc_cols("pink", "white")
# test to see what happens if non valid colour is provided
bcc_cols("turquoise", "blue")

bcc_palettes <- list(`purple` = c(bcc_cols("purple"), "#F1E8F3"),
                     `pink` = c(bcc_cols("pink"), "#FCF1F7"),
                     `orange` = c(bcc_cols("orange"), "#FBECE7"),
                     `yellow` = c(bcc_cols("yellow"), "#FFF5E2"),
                     `green` = c(bcc_cols("green"), "#EFF7E6"),
                     `blue` = c(bcc_cols("blue"), "#E2F5FB"),
                     `black` = c(bcc_cols("black"), "#E9E9E9"),
                     `multi` = bcc_cols("purple", "pink", "orange", "yellow", "green", "blue"),
                     `warm` = bcc_cols("purple", "pink", "orange", "yellow"),
                     `cool` = bcc_cols("green", "blue"))


#' Return function to interpolate a bcc color palette
#'
#' @param palette Character name of palette in bcc_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
bcc_pal <- function(palette = "purple", reverse = FALSE, ...) {
  # get palette
  pal <- bcc_palettes[[palette]]
  
  # reverse if reverse = true
  if (reverse) pal <- rev(pal)
  
  # interpolate between colours
  colorRampPalette(pal, ...)
}

bcc_pal("purple")(10)

#' Color scale constructor for bcc colors
#'
#' @param palette Character name of palette in bcc_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_colour_bcc <- function(palette = "purple", discrete = TRUE, reverse = FALSE, ...) {
  pal <- bcc_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour",  palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for bcc colors
#'
#' @param palette Character name of palette in bcc_palettes
#' @param discrete Boolean indicating whether fill aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_fill_bcc <- function(palette = "purple", discrete = TRUE, reverse = FALSE, ...) {
  pal <- bcc_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

# scatter plot, discrete
ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
  geom_point(size = 4) +
  scale_colour_bcc("green")

# scatter plot, continuous
ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
  geom_point(size = 4) +
  scale_colour_bcc(discrete = FALSE, palette = "orange", reverse = TRUE) +
  theme_minimal()

ggplot(iris, aes(Species, Sepal.Width, fill = Species)) +
  geom_col() +
  scale_fill_bcc("orange")

