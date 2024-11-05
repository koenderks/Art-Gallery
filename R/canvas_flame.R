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

#' Draw a Fractal Flame
#'
#' @description This function implements the fractal flame algorithm.
#'
#' @usage canvas_flame(
#'   colors,
#'   background = "#000000",
#'   iterations = 1000000,
#'   variations = 0,
#'   symmetry = 0,
#'   blend = TRUE,
#'   weighted = FALSE,
#'   post = FALSE,
#'   final = FALSE,
#'   extra = FALSE,
#'   display = c("colored", "logdensity"),
#'   zoom = 1,
#'   resolution = 1000,
#'   gamma = 1
#' )
#'
#' @param colors      a string or character vector specifying the color(s) used for the artwork.
#' @param background  a character specifying the color used for the background.
#' @param iterations  a positive integer specifying the number of iterations of the algorithm. Using more iterations results in images of higher quality but also increases the computation time.
#' @param variations  an integer (vector) with a minimum of 0 and a maximum of 48 specifying the variations to be included in the flame. The default \code{0} includes only a linear variation. Including multiple variations (e.g., \code{c(1, 2, 3)}) increases the computation time. See the details section for more information about possible variations.
#' @param symmetry    an integer with a minimum of -6 and a maximum of 6 indicating the type of symmetry to include in the flame. The default \code{0} includes no symmetry. Including symmetry decreases the computation time as a function of the absolute \code{symmetry} value. See the details section for more information about possible symmetries.
#' @param blend       logical. Whether to blend the variations (\code{TRUE}) or pick a unique variation in each iteration (\code{FALSE}). \code{blend = TRUE} increases computation time as a function of the number of included variations.
#' @param weighted    logical. Whether to weigh the functions and the variations (\code{TRUE}) or pick a function at random and equally weigh all variations (\code{FALSE}). \code{weighted = TRUE} significantly increases the computation time.
#' @param post        logical. Whether to apply a post transformation in each iteration.
#' @param final       logical. Whether to apply a final transformation in each iteration.
#' @param extra       logical. Whether to apply an additional post transformation after the final transformation. Only has an effect when \code{final = TRUE}.
#' @param display     a character indicating how to display the flame. \code{colored} (the default) displays colors according to which function they originate from. \code{logdensity} plots a gradient using the log density of the pixel count.
#' @param zoom        a positive value specifying the amount of zooming.
#' @param resolution  resolution of the artwork in pixels per row/column. Increasing the resolution does not increases the computation time of this algorithm.
#' @param gamma       a numeric value specifying the gamma correction (only used when \code{display = "colored"}). Larger values result in brighter images and vice versa.
#'
#' @details           The \code{variation} argument can be used to include specific variations into the flame. See the appendix in the references for examples of all variations. Possible variations are:
#'
#' \itemize{
#'  \item{\code{0}: Linear (default)}
#'  \item{\code{1}: Sine}
#'  \item{\code{2}: Spherical}
#'  \item{\code{3}: Swirl}
#'  \item{\code{4}: Horsehoe}
#'  \item{\code{5}: Polar}
#'  \item{\code{6}: Handkerchief}
#'  \item{\code{7}: Heart}
#'  \item{\code{8}: Disc}
#'  \item{\code{9}: Spiral}
#'  \item{\code{10}: Hyperbolic}
#'  \item{\code{11}: Diamond}
#'  \item{\code{12}: Ex}
#'  \item{\code{13}: Julia}
#'  \item{\code{14}: Bent}
#'  \item{\code{15}: Waves}
#'  \item{\code{16}: Fisheye}
#'  \item{\code{17}: Popcorn}
#'  \item{\code{18}: Exponential}
#'  \item{\code{19}: Power}
#'  \item{\code{20}: Cosine}
#'  \item{\code{21}: Rings}
#'  \item{\code{22}: Fan}
#'  \item{\code{23}: Blob}
#'  \item{\code{24}: PDJ}
#'  \item{\code{25}: Fan2}
#'  \item{\code{26}: Rings2}
#'  \item{\code{27}: Eyefish}
#'  \item{\code{28}: Bubble}
#'  \item{\code{29}: Cylinder}
#'  \item{\code{30}: Perspective}
#'  \item{\code{31}: Noise}
#'  \item{\code{32}: JuliaN}
#'  \item{\code{33}: JuliaScope}
#'  \item{\code{34}: Blur}
#'  \item{\code{35}: Gaussian}
#'  \item{\code{36}: RadialBlur}
#'  \item{\code{37}: Pie}
#'  \item{\code{38}: Ngon}
#'  \item{\code{39}: Curl}
#'  \item{\code{40}: Rectangles}
#'  \item{\code{41}: Arch}
#'  \item{\code{42}: Tangent}
#'  \item{\code{43}: Square}
#'  \item{\code{44}: Rays}
#'  \item{\code{45}: Blade}
#'  \item{\code{46}: Secant}
#'  \item{\code{47}: Twintrian}
#'  \item{\code{48}: Cross}
#' }
#'
#' @details           The \code{symmetry} argument can be used to include symmetry into the flame. Possible options are:
#'
#' \itemize{
#'  \item{\code{0}: No symmetry (default)}
#'  \item{\code{-1}: Dihedral symmetry}
#'  \item{\code{1}: Two-way rotational symmetry}
#'  \item{\code{(-)2}: (Dihedral) Three-way rotational symmetry}
#'  \item{\code{(-)3}: (Dihedral) Four-way rotational symmetry}
#'  \item{\code{(-)4}: (Dihedral) Five-way rotational symmetry}
#'  \item{\code{(-)5}: (Dihedral) Six-way rotational symmetry}
#'  \item{\code{(-)6}: (Dihedral) Snowflake symmetry}
#' }
#'
#' @return A \code{ggplot} object containing the artwork.
#'
#' @references \url{https://flam3.com/flame_draves.pdf}
#'
#' @author Koen Derks, \email{koen-derks@hotmail.com}
#'
#' @keywords artwork canvas
#'
#' @seealso \code{colorPalette}
#'
#' @examples
#' \donttest{
#' set.seed(3)
#'
#' # Simple example, linear variation, relatively few iterations
#' canvas_flame(colors = c("dodgerblue", "green"), variations = 0)
#'
#' # Simple example, linear variation, dihedral symmetry
#' canvas_flame(colors = c("hotpink", "yellow"), variations = 0, symmetry = -1, iterations = 1e7)
#'
#' # Advanced example (no-blend, weighted, sine and spherical variations)
#' canvas_flame(
#'   colors = colorPalette("origami"), variations = c(1, 2),
#'   blend = FALSE, weighted = TRUE, iterations = 1e8
#' )
#'
#' # More iterations give much better images
#' set.seed(123)
#' canvas_flame(colors = c("red", "blue"), iterations = 1e8, variations = c(10, 17))
#' }
#'
#' @export

canvas_flame <- function(colors,
                         background = "#000000",
                         iterations = 1000000,
                         variations = 0,
                         symmetry = 0,
                         blend = TRUE,
                         weighted = FALSE,
                         post = FALSE,
                         final = FALSE,
                         extra = FALSE,
                         display = c("colored", "logdensity"),
                         zoom = 1,
                         resolution = 1000,
                         gamma = 1) {
  display <- match.arg(display)
  .checkUserInput(
    resolution = resolution, background = background
  )
  if (is.null(variations) || !all(variations %% 1 == 0) || min(variations) < 0 || max(variations) > 48) {
    stop("'variations' must be an integer (vector) with a minimum of 0 and a maximum of 48")
  }
  if (is.null(symmetry) || symmetry %% 1 != 0 || symmetry < -6 || symmetry > 6) {
    stop("'symmetry' must be an integer with a minimum of -6 and a maximum of 6")
  }
  iterations <- iterations + 20
  if (display == "logdensity") {
    n <- sample(x = 3:10, size = 1)
    color_mat <- matrix(stats::runif(n * 3), nrow = n, ncol = 3)
  } else {
    n <- sample(x = 3:max(10, length(colors)), size = 1)
    colors <- c(colors, sample(x = colors, size = n, replace = TRUE))
    color_mat <- matrix(t(grDevices::col2rgb(colors) / 255), nrow = length(colors), ncol = 3)
  }
  fs <- .flame_system(n, symmetry)
  w_i <- stats::runif(n = fs[["n"]], min = 0, max = 1)
  v_ij <- matrix(stats::runif(fs[["n"]] * length(variations), min = 0, max = 1), nrow = fs[["n"]], ncol = length(variations))
  for (i in seq_len(nrow(v_ij))) {
    v_ij[i, ] <- v_ij[i, ] / sum(v_ij[i, ])
  }
  canvas <- cpp_flame( # 1 = x, 2 = y, 3 = red, 4 = green, 5 = blue
    canvas = array(0, dim = c(resolution + 1, resolution + 1, 4)), iterations = iterations,
    resolution = resolution,
    edge = 2 * (1 / zoom),
    blend = blend,
    weighted = weighted,
    post = post,
    final = final,
    extra = extra,
    colors = color_mat,
    functions = 0:(fs[["n"]] - 1),
    funcWeights = w_i / sum(w_i),
    funcPars = fs[["parameters"]],
    variations = variations,
    varWeights = v_ij,
    varParams = .flame_variations(),
    postPars = matrix(stats::runif(fs[["n"]] * 6, min = -1, max = 1), nrow = fs[["n"]], ncol = 6),
    finalPars = stats::runif(6, min = -1, max = 1),
    extraPars = stats::runif(6, min = -1, max = 1),
    bsym = fs[["indicator"]]
  )
  if (length(which(canvas[, , 1] > 0)) <= 1) {
    stop("Too few points are drawn on the canvas")
  }
  full_canvas <- .unraster(canvas[, , 1], c("x", "y", "z"))
  full_canvas[["z"]][full_canvas[["z"]] != 0] <- log(full_canvas[["z"]][full_canvas[["z"]] != 0], base = 1.2589)
  full_canvas[["z"]][full_canvas[["z"]] == 0] <- NA
  if (display == "logdensity") {
    artwork <- ggplot2::ggplot(data = full_canvas, mapping = ggplot2::aes(x = x, y = y, fill = z)) +
      ggplot2::geom_raster(interpolate = TRUE) +
      ggplot2::scale_fill_gradientn(colors = colors, na.value = background)
  } else {
    canvas <- .flame_scaling(canvas)
    canvas[, , 2:4] <- canvas[, , 2:4]^(1 / gamma) # Gamma correction
    maxVal <- max(c(1, c(canvas[, , 2]), c(canvas[, , 3]), c(canvas[, , 4])))
    if (maxVal == 0) {
      stop("No points are drawn on the canvas")
    }
    full_canvas[["col"]] <- grDevices::rgb(red = canvas[, , 2], green = canvas[, , 3], blue = canvas[, , 4], maxColorValue = maxVal)
    full_canvas[["col"]][which(is.na(full_canvas[["z"]]))] <- background
    artwork <- ggplot2::ggplot(data = full_canvas, mapping = ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_raster(interpolate = TRUE, fill = full_canvas[["col"]])
  }
  artwork <- theme_canvas(artwork, background = background)
  return(artwork)
}

.flame_variations <- function() {
  return(c(
    stats::runif(1, 1, 2), stats::runif(1, -2, -1), sample(3:7, size = 1), # blob.high, blob.low, blob.waves
    stats::runif(4, 0, 1), # padj.a, pdj.b, pdj.c, pdj.d
    stats::runif(1, 0, 1), # rings2.val
    stats::runif(1, 10, 80), stats::runif(1, 1, 10), # perspective.angle, perspective.dist
    stats::runif(1, 1, 5), stats::runif(1, 0.1, 1), # juliaN.power, juliaN.dist
    stats::runif(1, 1, 5), stats::runif(1, 0.1, 1), # juliaScope.power, juliaScope.dist
    stats::runif(1, 0, 360), stats::runif(1, 0.5, 10), # radialBlur.angle, v_36
    sample(x = 3:8, size = 1), stats::runif(1, 1, 360), stats::runif(1, 0, 1), # pie.slices, pie.rotation, pie.thickness
    stats::runif(1, 1, 2), sample(3:9, size = 1), sample(3:9, size = 1), stats::runif(1, 0, 1), # ngon.power, ngon.sides, ngon.corners, ngon.circle
    stats::runif(1, -1.5, -0.5), stats::runif(1, 0.5, 1.5), # curl.c1, curl.c2
    stats::runif(1, -1, 1), stats::runif(1, -1, 1), # rectangles.x, rectangles.y
    stats::runif(1, -1, 1), # v_41
    stats::runif(2, 0, 1), # v_44, # v_45
    stats::runif(1, 1, 7), # v_46
    stats::runif(1, 0, 10) # v_47
  ))
}

.flame_scaling <- function(canvas) {
  hits <- which(canvas[, , 1] > 0)
  scaling <- log(canvas[, , 1][hits]) / canvas[, , 1][hits]
  canvas[, , 2][hits] <- canvas[, , 2][hits] * scaling
  canvas[, , 3][hits] <- canvas[, , 3][hits] * scaling
  canvas[, , 4][hits] <- canvas[, , 4][hits] * scaling
  return(canvas)
}

.flame_system <- function(n, symmetry) {
  parameters <- matrix(stats::runif(n * 6, min = -1, max = 1), nrow = n, ncol = 6)
  indicator <- nrow(parameters)
  if (symmetry != 0) {
    if (symmetry != -1) { # Rotation functions
      nrotations <- abs(symmetry) + 1
      for (i in 1:(nrotations - 1)) {
        angle <- (2 * pi) / nrotations * i
        rotation <- matrix(rep(c(cos(angle), -sin(angle), 0, sin(angle), cos(angle), 0), times = n), byrow = TRUE, nrow = n, ncol = 6)
        parameters <- rbind(parameters, rotation)
      }
      n <- nrow(parameters)
    }
    if (symmetry < 0) { # Dihedral function
      dihedral <- matrix(rep(c(-1, 0, 0, 0, 1, 0), times = n), byrow = TRUE, nrow = n, ncol = 6)
      parameters <- rbind(parameters, dihedral)
      n <- nrow(parameters)
    }
  }
  out <- list()
  out[["n"]] <- n
  out[["parameters"]] <- parameters
  out[["indicator"]] <- indicator
  return(out)
}
