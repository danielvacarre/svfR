library(testthat)

# Define lists of inputs, outputs and the number of partitions
data <- data.frame(x1 = c(1, 2, 3, 4),
                   x2 = c(1, 3, 1, 2),
                   x3 = c(2, 4, 3, 6),
                   y1 = c(2, 4, 3, 5),
                   y2 = c(1, 2, 3, 4))
inputs <- c("x1", "x2")
outputs <- c("y1", "y2")
d <- 2

# Test SVFGrid constructor
test_that("SVFGrid constructor creates an object with correct structure", {
  grid <- SVFGrid(data = data, inputs = inputs, outputs = outputs, d = d)

  expect_s3_class(grid, "SVFGrid")
  expect_equal(grid$data, data[c(inputs, outputs)])
  expect_equal(grid$inputs, inputs)
  expect_equal(grid$outputs, outputs)
  expect_equal(grid$d, 2)
  expect_null(grid$df_grid)
  expect_null(grid$knot_list)
})


# Test create_grid.SVFGrid function
test_that("create_grid.SVFGrid creates grid correctly", {
  grid <- SVFGrid(data, inputs, outputs, d)
  grid <- create_grid.SVFGrid(grid)

  expect_s3_class(grid, "SVFGrid")
  expect_true(!is.null(grid$df_grid))
  expect_true(!is.null(grid$knot_list))

  # Check df_grid structure
  expect_true("id_cells" %in% names(grid$df_grid))
  expect_true("values" %in% names(grid$df_grid))
  expect_true("phi" %in% names(grid$df_grid))
  expect_true("c_cells" %in% names(grid$df_grid))
})

# Test search_contiguous_cell function
test_that("search_contiguous_cell finds contiguous cells correctly", {
  cell <- c(2, 2)
  contiguous <- search_contiguous_cell(cell)

  expected <- list(
    c(1, 2),
    c(2, 1)
  )

  expect_equal(contiguous, expected)

  cell <- c(2, 2, 2)
  contiguous <- search_contiguous_cell(cell)

  expected <- list(
    c(1, 2, 2),
    c(2, 1, 2),
    c(2, 2, 1)
  )

  expect_equal(contiguous, expected)

  cell <- c(2, 1, 1)
  contiguous <- search_contiguous_cell(cell)

  expected <- list(
    c(1, 1, 1)
  )

  expect_equal(contiguous, expected)

})

# Test calculate_dmu_phi.SVFGrid function
test_that("calculate_dmu_phi.SVFGrid calculates phi values correctly", {
  grid <- SVFGrid(data = data, inputs = inputs, outputs = outputs, d = d)
  grid <- create_grid.SVFGrid(grid)

  cell <- c(2, 1)  # Example cell for testing
  phi_values <- calculate_dmu_phi.SVFGrid(grid, cell)

  # The expected phi values need to be computed based on the test data
  expected_phi_values <- c(1,0,0,1,0,0,0,0,0)

  expect_equal(phi_values[[1]], expected_phi_values)
})

# Test calculate_df_grid.SVFGrid function
test_that("calculate_df_grid.SVFGrid updates df_grid correctly", {
  grid <- SVFGrid(data = data, inputs = inputs, outputs = outputs, d = d)
  grid <- create_grid.SVFGrid(grid)
  grid <- calculate_df_grid.SVFGrid(grid)

  expect_true("phi" %in% names(grid$df_grid))
  expect_true("c_cells" %in% names(grid$df_grid))
})

# Test calculate_data_grid.SVFGrid function
test_that("calculate_data_grid.SVFGrid updates data_grid correctly", {
  grid <- SVFGrid(data = data, inputs = inputs, outputs = outputs, d = d)
  grid <- create_grid.SVFGrid(grid)
  grid <- calculate_data_grid.SVFGrid(grid)

  expect_true("phi" %in% names(grid$data_grid))
  expect_true("c_cells" %in% names(grid$data_grid))
})

