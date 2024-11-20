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

#' Draw A Slime Mold
#'
#' @description This function draws the Physarum polycephalum slime mold on a
#'   canvas. The algorithm simulates particles on a two-dimensional grid that
#'   move towards areas on the grid with a high intensity.
#'
#' @usage canvas_slime(
#'   colors,
#'   background = "#000000",
#'   iterations = 2000,
#'   agents = 1000,
#'   layout = c(
#'      "random", "gaussian", "circle", "grid",
#'      "clusters", "arrows", "wave", "spiral"
#'   ),
#'   resolution = 1000
#' )
#'
#' @param colors      a character (vector) specifying the color(s) used for the
#'   artwork.
#' @param background  a character specifying the color used for the background.
#' @param iterations  a positive integer specifying the number of iterations of
#'   the algorithm.
#' @param agents      a positive integer specifying the number of agents to use.
#' @param layout      a character specifying the initial layout of the agents.
#'   Possible options are \code{random} (default), \code{gaussian},
#'   \code{circle}, \code{grid}, \code{clusters}, \code{arrows} and \code{wave}.
#' @param resolution  resolution of the artwork in pixels per row/column.
#'   Increasing the resolution increases the quality of the artwork but also
#'   increases the computation time exponentially.
#'
#' @return A \code{ggplot} object containing the artwork.
#'
#' @references \url{https://cargocollective.com/sagejenson/physarum}
#' @references \url{https://fronkonstin.com/2020/08/11/abstractions/}
#'
#' @author Koen Derks, \email{koen-derks@hotmail.com}
#'
#' @keywords artwork canvas
#'
#' @seealso \code{colorPalette}
#'
#' @examples
#' \donttest{
#' set.seed(1)
#'
#' # Simple example
#' canvas_slime(colors = colorPalette("neon1"))
#' }
#'
#' @export

canvas_slime <- function(colors,
                         background = "#000000",
                         iterations = 2000,
                         agents = 1000,
                         layout = c(
                           "random", "gaussian", "circle", "grid",
                           "clusters", "arrows", "wave", "spiral"
                         ),
                         resolution = 1000) {
  layout <- match.arg(layout)
  if (layout == "random") {
    layout <- sample(c("gaussian", "circle", "grid", "clusters", "arrows", "wave", "spiral"), 1)
  }
  .checkUserInput(
    background = background, resolution = resolution, iterations = iterations
  )
  full_canvas <- iterate_slime(
    canvas = matrix(0, resolution, resolution),
    agents = .slime_agents(agents, layout, resolution),
    decay_factor = stats::runif(1, 0.05, 0.2),
    forward_left = stats::runif(1, 22.5, 45) * pi / 180,
    forward_right = -stats::runif(1, 22.5, 45) * pi / 180,
    rotation_angle = stats::runif(1, 40, 50) * pi / 180,
    sensor_offset = sample(5:10, 1),
    step_size = 1,
    decomposition = stats::runif(1, 5, 20),
    iters = iterations
  )
  full_canvas <- .unraster(full_canvas, c("x", "y", "z"))
  full_canvas$z[full_canvas$z == 0] <- NA
  artwork <- ggplot2::ggplot(data = full_canvas, ggplot2::aes(x = x, y = y, fill = log(z))) +
    ggplot2::geom_raster(interpolate = TRUE) +
    ggplot2::scale_fill_gradientn(colours = colors, na.value = background)
  artwork <- theme_canvas(artwork)
  return(artwork)
}

.slime_agents <- function(n_rows, layout, resolution) {
  if (layout == "gaussian") {
    center_x <- stats::runif(1, 0, resolution)
    center_y <- stats::runif(1, 0, resolution)
    sd_x <- stats::runif(1, 0, resolution / 2)
    sd_y <- stats::runif(1, 0, resolution / 2)
    agents <- matrix(c(
      pmin(pmax(stats::rnorm(n_rows, center_x, sd_x), 0), resolution - 1),
      pmin(pmax(stats::rnorm(n_rows, center_y, sd_y), 0), resolution - 1),
      stats::rnorm(n_rows, 0, 1)
    ), nrow = n_rows)
  } else if (layout == "circle") {
    center_x <- stats::runif(1, 0, resolution)
    center_y <- stats::runif(1, 0, resolution)
    radius_x <- stats::runif(1, 0, resolution / 20)
    radius_y <- stats::runif(1, 0, resolution / 20)
    circle <- seq(0, 2 * pi, length.out = n_rows)
    agents <- matrix(c(
      radius_x * cos(circle) + center_x,
      radius_y * sin(circle) + center_y,
      jitter(circle + pi)
    ), nrow = n_rows)
  } else if (layout == "grid") {
    grid <- expand.grid(
      x = seq(1, resolution, length.out = floor(sqrt(n_rows))),
      y = seq(1, resolution, length.out = ceiling(n_rows / floor(sqrt(n_rows))))
    )
    grid <- grid[sample.int(nrow(grid), n_rows), ]
    agents <- matrix(c(grid$x, grid$y, stats::runif(n_rows, 0, 2 * pi)), nrow = n_rows)
  } else if (layout == "clusters") {
    num_clusters <- floor(n_rows / 10)
    cluster_centers <- matrix(stats::runif(num_clusters * 2, 0, resolution), ncol = 2)
    agents <- matrix(ncol = 3, nrow = 0)
    for (cluster in 1:num_clusters) {
      cluster_size <- floor(n_rows / num_clusters)
      center <- cluster_centers[cluster, ]
      x <- stats::rnorm(cluster_size, mean = center[1], sd = resolution / 100)
      y <- stats::rnorm(cluster_size, mean = center[2], sd = resolution / 100)
      h <- stats::runif(cluster_size, 0, 2 * pi)
      agents <- rbind(agents, cbind(x, y, h))
    }
    if (nrow(agents) < n_rows) {
      extra_agents <- n_rows - nrow(agents)
      extra <- matrix(c(
        stats::runif(extra_agents, 0, resolution),
        stats::runif(extra_agents, 0, resolution),
        stats::runif(extra_agents, 0, 2 * pi)
      ), ncol = 3)
      agents <- rbind(agents, extra)
    }
  } else if (layout == "arrows") {
    center_x <- stats::runif(1, resolution / 4, 3 * resolution / 4)
    center_y <- stats::runif(1, resolution / 4, 3 * resolution / 4)
    diag_x <- stats::runif(1, resolution / 10, resolution / 5)
    diag_y <- stats::runif(1, resolution / 10, resolution / 5)
    edge <- sample(1:4, n_rows, replace = TRUE)
    t <- stats::runif(n_rows, 0, 1)
    delta_x <- ifelse(edge %% 2 == 1, t * diag_x, diag_x - t * diag_x)
    delta_y <- ifelse(edge <= 2, -t * diag_y, diag_y - t * diag_y)
    x_coords <- center_x + ifelse(edge <= 2, delta_x, -delta_x)
    y_coords <- center_y + ifelse(edge %% 2 == 1, delta_y, -delta_y)
    agents <- matrix(c(
      x_coords,
      y_coords,
      stats::runif(n_rows, 0, 2 * pi)
    ), nrow = n_rows)
  } else if (layout == "wave") {
    x_coords <- seq(resolution * stats::runif(1, 0, 0.5), resolution * stats::runif(1, 0.5, 1), length.out = n_rows)
    if (stats::runif(1, 0, 1) < 0.5) {
      y_coords <- resolution / 2 + cos(x_coords / resolution * 2 * pi) * resolution / 4
    } else {
      y_coords <- resolution / 2 + sin(x_coords / resolution * 2 * pi) * resolution / 4
    }
    if (stats::runif(1, 0, 1) < 0.5) {
      x_new <- x_coords
      y_new <- y_coords
    } else {
      x_new <- y_coords
      y_new <- x_coords
    }
    agents <- matrix(c(
      x_new,
      y_new,
      stats::runif(n_rows, 0, 2 * pi)
    ), nrow = n_rows)
  } else if (layout == "spiral") {
    center_x <- stats::runif(1, 0, resolution)
    center_y <- stats::runif(1, 0, resolution)
    angles <- seq(0, 4 * pi, length.out = n_rows)
    radii <- seq(0, resolution / stats::runif(1, 2, 10), length.out = n_rows)
    agents <- matrix(c(
      center_x + radii * cos(angles),
      center_y + radii * sin(angles),
      stats::runif(n_rows, 0, 2 * pi)
    ), nrow = n_rows)
  }
  return(agents)
}
