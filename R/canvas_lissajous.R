# Copyright (C) 2021-2023 Koen Derks

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Draw a Lissajous Curve
#'
#' @description This function draws lissajous curves with points connected via a k-nearest neighbor approach.
#'
#' @usage canvas_lissajous(
#'   colors,
#'   background = "#000000",
#'   iterations = 2,
#'   neighbors = 50,
#'   noise = FALSE
#' )
#'
#' @param colors      a string or character vector specifying the color(s) used for the artwork.
#' @param background  a character specifying the color used for the background.
#' @param iterations  a positive integer specifying the number of iterations of the algorithm.
#' @param neighbors   a positive integer specifying the number of neighbors a block considers when drawing the connections.
#' @param noise       logical. Whether to add perlin noise to the coordinates of the nodes.
#'
#' @return A \code{ggplot} object containing the artwork.
#'
#' @references \url{https://en.wikipedia.org/wiki/Lissajous_curve}
#'
#' @author Koen Derks, \email{koen-derks@hotmail.com}
#'
#' @keywords artwork canvas
#'
#' @seealso \code{colorPalette}
#'
#' @examples
#' \donttest{
#' set.seed(13)
#'
#' # Simple example
#' canvas_lissajous(colors = colorPalette("blossom"))
#' }
#'
#' @export

canvas_lissajous <- function(colors,
                             background = "#000000",
                             iterations = 2,
                             neighbors = 50,
                             noise = FALSE) {
  .checkUserInput(iterations = iterations, background = background)
  stopifnot("'neighbors' must be > 0" = neighbors > 0)
  d <- stats::runif(1, 1, 100)
  e <- stats::runif(1, 1, 100)
  a <- stats::runif(1, 1, 10)
  b <- stats::runif(1, 1, 10)
  c <- stats::runif(1, 1, 10)
  A <- stats::runif(1, 1, 10)
  B <- stats::runif(1, 1, 10)
  C <- stats::runif(1, 1, 10)
  npoints <- 1000
  t <- seq(from = 0, to = iterations * pi, length.out = npoints)
  x <- A * sin(a * t + d)
  y <- B * sin(b * t)
  z <- C * sin(c * t + e)
  if (noise) {
    x <- x + as.numeric(.noise(dims = c(1, npoints), type = "perlin", limits = c(-1, 1)))
    y <- y + as.numeric(.noise(dims = c(1, npoints), type = "perlin", limits = c(-1, 1)))
    z <- z + as.numeric(.noise(dims = c(1, npoints), type = "perlin", limits = c(-1, 1)))
  }
  df <- data.frame(t = t, x = x, y = y, z = z)
  n <- nrow(df)
  all_neighbors <- lapply(1:n, function(ptnum) {
    xi <- df$x[ptnum]
    yi <- df$y[ptnum]
    zi <- df$z[ptnum]
    dists <- sqrt((df$x - xi)^2 + (df$y - yi)^2 + (df$z - zi)^2)
    order <- order(dists)
    nn <- order[2:(neighbors + 1)]
    xend <- rep(xi, neighbors)
    yend <- rep(yi, neighbors)
    dftmp <- data.frame(
      x = df$x[nn],
      y = df$y[nn],
      xend = xend,
      yend = yend,
      z = df$z[nn]
    )
    return(dftmp)
  })
  canvas <- do.call(rbind, all_neighbors)
  breaks <- pretty(c(canvas$x, canvas$y, canvas$xend, canvas$yend))
  if (length(colors) == 1) {
    cols <- colors
  } else {
    cols <- colors[cut(canvas$z, breaks = length(colors))]
  }
  alphas <- 1 - scales::rescale(canvas$z, to = c(0.5, 0.8), from = range(canvas$z))
  artwork <- ggplot2::ggplot(data = canvas, mapping = ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
    ggplot2::geom_segment(linewidth = 0.01, colour = cols, alpha = alphas) +
    ggplot2::scale_x_continuous(limits = range(breaks)) +
    ggplot2::scale_y_continuous(limits = range(breaks))
  artwork <- theme_canvas(artwork, background = background)
  return(artwork)
}
