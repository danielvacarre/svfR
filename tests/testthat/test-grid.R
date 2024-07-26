library(testthat)

# Datos específicos para las pruebas
fixed_data <- data.frame(
  x = list(c(1,2,3,4),c(1,3,1,2)),
  y = list(c(2,4,3,5),c(1,2,3,4))
)

# Tests for the Grid class and its methods
test_that("Grid class constructor works correctly", {
  data <- fixed_data
  inputs <- list('x1','x2')
  outputs <- list('y1','y2')
  d <- 2

  grid <- Grid(data, inputs, outputs, d)

  expect_s3_class(grid, "Grid")
  expect_equal(grid$data, data)
  expect_equal(grid$inputs, inputs)
  expect_equal(grid$outputs, outputs)
  expect_equal(grid$d, d)
  expect_null(grid$data_grid)
  expect_null(grid$knot_list)
})


test_that("search_dmu.Grid works correctly n1", {
  data <- fixed_data
  inputs <- list('x1')
  outputs <- list('y1','y2')
  d <- 2

  grid <- Grid(data, inputs, outputs, d)
  grid$knot_list <- list(c(1,2.5,4))
  dmu <- c(2)

  cell <- search_dmu.Grid(grid, dmu)

  expect_equal(cell, c(1))
})

test_that("search_dmu.Grid works correctly n2", {
  data <- fixed_data
  inputs <- list('x1','x2')
  outputs <- list('y1','y2')
  d <- 2

  grid <- Grid(data, inputs, outputs, d)
  grid$knot_list <- list(c(1,2.5,4),c(1,2,3))
  dmu <- c(2, 3)

  cell <- search_dmu.Grid(grid, dmu)

  expect_equal(cell, c(1,3))
})

test_that("search_dmu.Grid works correctly n1", {
  data <- fixed_data
  inputs <- list('x1')
  outputs <- list('y1','y2')
  d <- 2

  grid <- Grid(data, inputs, outputs, d)
  grid$knot_list <- list(c(1,2.5,4))
  dmu <- c(2)

  cell <- search_dmu.Grid(grid, dmu)

  expect_equal(cell, c(1))
})

test_that("transformation works correctly", {
  expect_equal(transformation(0.5, 0.3), 1)
  expect_equal(transformation(0.5, 0.5), 0)
  expect_equal(transformation(0.5, 0.7), -1)
})
