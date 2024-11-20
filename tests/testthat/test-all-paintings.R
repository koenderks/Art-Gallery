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

test_that("colorPalette()", {
  set.seed(1)
  .runtime(expression = {
    palette <- colorPalette(name = "random", n = 4)
  }, name = "Palettes")
  expect_equal(length(palette), 4)
})

test_that("canvas_strokes()", {
  set.seed(1)
  .runtime(expression = {
    artwork <- canvas_strokes(colors = c("black", "white"))
  }, name = "Strokes")
  expect_equal(!is.null(artwork), TRUE)
})

test_that("canvas_ribbons()", {
  set.seed(1)
  .runtime(expression = {
    artwork <- canvas_ribbons(colors = c("black", "white"))
  }, name = "Ribbons")
  expect_equal(!is.null(artwork), TRUE)
})

test_that("canvas_turmite()", {
  set.seed(1)
  .runtime(expression = {
    artwork <- canvas_turmite(color = "black")
  }, name = "Turmite")
  expect_equal(!is.null(artwork), TRUE)
})

test_that("canvas_ant()", {
  set.seed(1)
  .runtime(expression = {
    artwork <- canvas_ant(colors = "black")
  }, name = "Ant")
  expect_equal(!is.null(artwork), TRUE)
})

test_that("canvas_planet()", {
  set.seed(1)
  .runtime(expression = {
    artwork <- canvas_planet(colors = c("black", "white"))
  }, name = "Planet")
  expect_equal(!is.null(artwork), TRUE)
})

test_that("canvas_squares()", {
  set.seed(1)
  .runtime(expression = {
    artwork <- canvas_squares(colors = c("black", "white"))
  }, name = "Squares")
  expect_equal(!is.null(artwork), TRUE)
})

test_that("canvas_circlemap()", {
  set.seed(1)
  .runtime(expression = {
    artwork <- canvas_circlemap(colors = c("black", "white"))
  }, name = "Circlemap")
  expect_equal(!is.null(artwork), TRUE)
})

test_that("canvas_function()", {
  set.seed(1)
  .runtime(expression = {
    artwork <- canvas_function(color = "black")
  }, name = "Function")
  expect_equal(!is.null(artwork), TRUE)
})

test_that("canvas_polylines()", {
  set.seed(1)
  .runtime(expression = {
    artwork <- canvas_polylines(colors = c("black", "gray", "white"))
  }, name = "Polylines")
  expect_equal(!is.null(artwork), TRUE)
})

test_that("canvas_diamonds()", {
  set.seed(1)
  .runtime(expression = {
    artwork <- canvas_diamonds(colors = c("black", "gray", "white"))
  }, name = "Diamonds")
  expect_equal(!is.null(artwork), TRUE)
})

test_that("canvas_segments()", {
  set.seed(1)
  .runtime(expression = {
    artwork <- canvas_segments(colors = c("black", "gray", "white"))
  }, name = "Segments")
  expect_equal(!is.null(artwork), TRUE)
})

test_that("canvas_mandelbrot()", {
  set.seed(1)
  .runtime(expression = {
    artwork <- canvas_mandelbrot(colors = c("black", "gray", "white"))
  }, name = "Mandelbrot")
  expect_equal(!is.null(artwork), TRUE)
})

test_that("canvas_collatz()", {
  set.seed(1)
  .runtime(expression = {
    artwork <- canvas_collatz(colors = c("black", "gray", "white"))
  }, name = "Collatz")
  expect_equal(!is.null(artwork), TRUE)
})

test_that("canvas_mosaic()", {
  set.seed(1)
  .runtime(expression = {
    artwork <- canvas_mosaic(colors = c("black", "gray", "white"))
  }, name = "Mosaic")
  expect_equal(!is.null(artwork), TRUE)
})

test_that("canvas_forest()", {
  set.seed(1)
  .runtime(expression = {
    artwork <- canvas_forest(colors = c("black", "gray", "white"))
  }, name = "Forest")
  expect_equal(!is.null(artwork), TRUE)
})

test_that("canvas_gemstone()", {
  set.seed(1)
  .runtime(expression = {
    artwork <- canvas_gemstone(colors = c("black", "gray", "white"))
  }, name = "Gemstone")
  expect_equal(!is.null(artwork), TRUE)
})

test_that("canvas_blacklight()", {
  set.seed(1)
  .runtime(expression = {
    artwork <- canvas_blacklight(colors = c("black", "gray", "white"))
  }, name = "Blacklight")
  expect_equal(!is.null(artwork), TRUE)
})

test_that("canvas_stripes()", {
  set.seed(1)
  .runtime(expression = {
    artwork <- canvas_stripes(colors = c("black", "gray", "white"))
  }, name = "Stripes")
  expect_equal(!is.null(artwork), TRUE)
})

test_that("canvas_nebula()", {
  set.seed(1)
  .runtime(expression = {
    artwork <- canvas_nebula(colors = c("black", "gray", "white"))
  }, name = "Nebula")
  expect_equal(!is.null(artwork), TRUE)
})

test_that("canvas_watercolors()", {
  set.seed(1)
  .runtime(expression = {
    artwork <- canvas_watercolors(colors = c("black", "gray", "white"))
  }, name = "Watercolors")
  expect_equal(!is.null(artwork), TRUE)
})

test_that("canvas_flow()", {
  set.seed(1)
  .runtime(expression = {
    artwork <- canvas_flow(colors = c("black", "gray", "white"))
  }, name = "Flow")
  expect_equal(!is.null(artwork), TRUE)
})

test_that("canvas_maze()", {
  set.seed(1)
  .runtime(expression = {
    artwork <- canvas_maze(color = "black")
  }, name = "Maze")
  expect_equal(!is.null(artwork), TRUE)
})

test_that("canvas_recaman()", {
  set.seed(1)
  .runtime(expression = {
    artwork <- canvas_recaman(colors = "black")
  }, name = "Recaman")
  expect_equal(!is.null(artwork), TRUE)
})

test_that("canvas_phyllotaxis()", {
  set.seed(1)
  .runtime(expression = {
    artwork <- canvas_phyllotaxis(colors = "black")
  }, name = "Phyllotaxis")
  expect_equal(!is.null(artwork), TRUE)
})

test_that("canvas_cobweb()", {
  set.seed(1)
  .runtime(expression = {
    artwork <- canvas_cobweb(colors = "black")
  }, name = "Cobweb")
  expect_equal(!is.null(artwork), TRUE)
})

test_that("canvas_chladni()", {
  set.seed(1)
  .runtime(expression = {
    artwork <- canvas_chladni(colors = "black")
  }, name = "Chladni")
  expect_equal(!is.null(artwork), TRUE)
})

test_that("canvas_petri()", {
  set.seed(1)
  .runtime(expression = {
    artwork <- canvas_petri(colors = "black")
  }, name = "Petri")
  expect_equal(!is.null(artwork), TRUE)
})

test_that("canvas_splits()", {
  set.seed(1)
  .runtime(expression = {
    artwork <- canvas_splits(colors = "black")
  }, name = "Splits")
  expect_equal(!is.null(artwork), TRUE)
})

test_that("canvas_mesh()", {
  set.seed(1)
  .runtime(expression = {
    artwork <- canvas_mesh(colors = "black")
  }, name = "Mesh")
  expect_equal(!is.null(artwork), TRUE)
})

test_that("canvas_flame()", {
  set.seed(2)
  .runtime(expression = {
    artwork <- canvas_flame(colors = c("dodgerblue", "green"))
  }, name = "Flame")
  expect_equal(!is.null(artwork), TRUE)
})

test_that("canvas_smoke()", {
  set.seed(2)
  .runtime(expression = {
    artwork <- canvas_smoke(colors = c("red", "blue", "green"), resolution = 10)
  }, name = "Smoke")
  expect_equal(!is.null(artwork), TRUE)
})

test_that("canvas_tiles()", {
  set.seed(2)
  .runtime(expression = {
    artwork <- canvas_tiles(colors = c("red", "blue", "green"), iterations = 100)
  }, name = "Tiles")
  expect_equal(!is.null(artwork), TRUE)
})

test_that("canvas_swirls()", {
  set.seed(2)
  .runtime(expression = {
    artwork <- canvas_swirls(colors = c("red", "blue", "green"), n = 10, iterations = 10)
  }, name = "Swirls")
  expect_equal(!is.null(artwork), TRUE)
})

test_that("canvas_lissajous()", {
  set.seed(2)
  .runtime(expression = {
    artwork <- canvas_lissajous(colors = c("red", "blue", "green"))
  }, name = "Lissajous")
  expect_equal(!is.null(artwork), TRUE)
})

test_that("canvas_slime()", {
  set.seed(1)
  .runtime(expression = {
    artwork <- canvas_slime(colors = c("red", "blue", "green"))
  }, name = "Slime")
  expect_equal(!is.null(artwork), TRUE)
})
