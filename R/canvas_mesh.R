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

#' Draw Meshes
#'
#' @description This function draws one or more rotating circular morphing meshes on the canvas.
#'
#' @usage canvas_mesh(
#'   colors,
#'   background = "#fafafa",
#'   transform = c("perlin", "fbm", "simplex", "cubic",
#'                 "worley", "knn", "rf", "svm"),
#'   lines = 500,
#'   iterations = 500,
#'   mixprob = 0
#' )
#'
#' @param colors         a string or character vector specifying the color(s) used for the artwork.
#' @param background     a character specifying the color used for the background (and the hole).
#' @param transform      a character specifying the type of transformation to use for the radius.
#' @param lines          an integer specifying the number of lines to darw.
#' @param iterations     a positive integer specifying the number of iterations of the algorithm.
#' @param mixprob        a value between 0 and 1 specifying the probability of a line segment getting another color.
#'
#' @return A \code{ggplot} object containing the artwork.
#'
#' @author Koen Derks, \email{koen-derks@hotmail.com}
#'
#' @references \url{https://web.archive.org/web/20231130151102/http://rectangleworld.com/blog/archives/462}
#'
#' @keywords artwork canvas
#'
#' @seealso \code{colorPalette}
#'
#' @examples
#' \donttest{
#' set.seed(2)
#'
#' # Simple example
#' canvas_mesh(colors = colorPalette("origami"))
#' }
#'
#' @export

canvas_mesh <- function(colors,
                        background = "#fafafa",
                        transform = c(
                          "perlin", "fbm", "simplex", "cubic",
                          "worley", "knn", "rf", "svm"
                        ),
                        lines = 500,
                        iterations = 500,
                        mixprob = 0) {
  transform <- match.arg(transform)
  .checkUserInput(iterations = iterations, background = background)
  artwork <- ggplot2::ggplot()
  miny <- Inf
  maxy <- -Inf
  for (j in seq_along(colors)) {
    circle_points <- seq(from = -pi, to = pi, length.out = lines)
    start <- stats::runif(1, min = -10, max = 10)
    centers <- data.frame(x = 0:iterations, y = c(start, start + .bmline(n = iterations, lwd = stats::runif(1, min = 1, max = 10))))
    circle_centers <- predict(stats::loess(y ~ x, data = centers), newdata = centers)
    if (transform == "fbm") {
      radius <- data.frame(x = 1:lines, y = .bmline(n = lines, lwd = stats::runif(1, min = 0.5, max = 1)))
      circle_radius <- predict(stats::loess(y ~ x, data = radius), newdata = radius)
    } else {
      circle_radius <- c(.noise(c(1, lines), type = transform, limits = c(0.5, 1)))
    }
    radius_increase <- data.frame(x = 1:lines, y = stats::rnorm(lines, mean = 0, sd = stats::runif(1, min = 0.01, max = 0.5)))
    circle_radius_increase <- predict(stats::loess(y ~ x, data = radius_increase), newdata = radius_increase)
    x <- rep(0:iterations, each = lines) + 0.75 * cos(circle_points)
    mesh <- cpp_mesh(
      canvas = matrix(NA, nrow = lines * (iterations + 1), ncol = 2),
      points = circle_points,
      centers = circle_centers,
      iterations = iterations,
      start = start,
      order = seq_len(lines),
      radii = circle_radius,
      increase = circle_radius_increase
    )
    if (mixprob > 0) {
      probs <- rep(1, length(colors))
      probs[j] <- probs[j] + 1 / mixprob
      col <- sample(colors, size = length(mesh[, 1]), replace = TRUE, prob = probs)
    } else {
      col <- colors[j]
    }
    df <- data.frame(x = x, y = mesh[, 1], z = mesh[, 2], col = col)
    if (min(df[["y"]]) < miny) miny <- min(df[["y"]])
    if (max(df[["y"]]) > maxy) maxy <- max(df[["y"]])
    artwork <- artwork + ggplot2::geom_line(
      data = df,
      mapping = ggplot2::aes(x = x, y = y, group = z, col = col),
      alpha = 0.2, linewidth = 0.05
    )
  }
  artwork <- artwork + ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_x_continuous(limits = c(-1, iterations + 1)) +
    ggplot2::scale_y_continuous(limits = c(miny - abs(maxy - miny) / 10, maxy + abs(maxy - miny) / 10))
  artwork <- theme_canvas(artwork, background = background)
  return(artwork)
}
