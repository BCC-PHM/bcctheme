# vector of BCC corporate colours
bcc_colours <- c(`purple` = "#84329B",
                 `pink`   = "#D00070",
                 `orange` = "#DC582A",
                 `yellow` = "#FFAD00",
                 `green`  = "#75BC22",
                 `blue`   = "#00A9E0",
                 `black`  = "#3c3c3b",
                 `white`  = "#FFFFFF")

# list of BCC palettes
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

#' Extract BCC colors as hex codes
#'
#' @param ... character Names of bcc_colours. If empty the full list of colours is returned.
#' @returns A vector of hex codes for the specified colour names.
#' @examples
#' bcc_cols()
#' bcc_cols("pink", "white")
#' bcc_cols("yellow", "green", "blue")
#' @export
bcc_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (bcc_colours)

  bcc_colours[cols]
}

#' Interpolate a BCC colour palette
#'
#' @param palette string Name of palette in bcc_palettes. Defaults to "purple".
#' @param reverse boolean Should the palette should be reversed? Defaults to FALSE.
#' @param ... Additional arguments to pass to colorRampPalette().
#' @returns A function to interpolate the colours present in the palette.
#' @examples
#' bcc_pal("pink")(10)
#' bcc_pal("multi")(25)
#' @export
bcc_pal <- function(palette = "purple", reverse = FALSE, ...) {
  # get palette
  pal <- bcc_palettes[[palette]]

  # reverse if reverse = true
  if (reverse) pal <- rev(pal)

  # interpolate between colours
  colorRampPalette(pal, ...)
}

#' Color scale constructor for BCC colours
#'
#' @param palette string Name of palette in bcc_palettes
#' @param discrete boolean Is the colour aesthetic discrete? Defaults to TRUE.
#' @param reverse boolean Should the palette should be reversed? Defaults to FALSE.
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#' @returns A colour scale function for ggplot2.
#' @examples
#' iris <- data(iris)
#'
#' p <- ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
#' geom_point(size = 4)
#' p + scale_colour_bcc(discrete = FALSE, palette = "orange", reverse = TRUE, guide = "none")
#' @export
scale_colour_bcc <- function(palette = "purple", discrete = TRUE, reverse = FALSE, ...) {
  pal <- bcc_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour",  palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for BCC colours
#'
#'@param palette string Name of palette in bcc_palettes
#' @param discrete boolean Is the fill aesthetic discrete? Defaults to TRUE.
#' @param reverse boolean Should the palette should be reversed? Defaults to FALSE.
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#' @returns A fill scale function for ggplot2.
#' @examples
#' iris <- data(iris)
#'
#' p <- ggplot(iris, aes(Species, Sepal.Width, fill = Species)) +
#' geom_col()
#' p + scale_fill_bcc("multi", guide = "none")
#' @export
scale_fill_bcc <- function(palette = "purple", discrete = TRUE, reverse = FALSE, ...) {
  pal <- bcc_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
