library(testthat)

# Define lists of inputs, outputs and the number of partitions
data <- data.frame(x1 = c(1, 2, 3, 4),
                   x2 = c(1, 3, 1, 2),
                   x3 = c(2, 4, 3, 6),
                   y1 = c(2, 4, 3, 5),
                   y2 = c(1, 2, 3, 4))
inputs <- c("x1", "x2","x3")
outputs <- c("y1", "y2")
d <- 2

# Test for SVFGrid constructor
test_that("SVFGrid function initializes correctly", {
  grid_obj <- SVFGrid(data, inputs, outputs, d)

  expect_s3_class(grid_obj, "SVFGrid")
  expect_s3_class(grid_obj, "Grid")
  expect_equal(grid_obj$data, data)
  expect_equal(grid_obj$inputs, inputs)
  expect_equal(grid_obj$outputs, outputs)
  expect_equal(grid_obj$d, d)
  expect_true(is.data.frame(grid_obj$df_grid))
  expect_true(is.data.frame(grid_obj$data_grid))
})

# Test for search_contiguous_cell
test_search_contiguous_cell <- function() {
  test_that("Test for 1-dimensional cell", {
    cell_1d <- c(2)
    expected_1d <- list(c(1))
    result_1d <- search_contiguous_cell(cell_1d)
    expect_equal(result_1d, expected_1d)
  })

  test_that("Test for 2-dimensional cell", {
    cell_2d <- c(2, 2)
    expected_2d <- list(c(1, 2), c(2, 1))
    result_2d <- search_contiguous_cell(cell_2d)
    expect_equal(result_2d, expected_2d)
  })

  test_that("Test for 3-dimensional cell", {
    cell_3d <- c(2, 2, 2)
    expected_3d <- list(
      c(1, 2, 2),
      c(2, 1, 2),
      c(2, 2, 1)
    )
    result_3d <- search_contiguous_cell(cell_3d)
    expect_equal(result_3d, expected_3d)
  })
}
