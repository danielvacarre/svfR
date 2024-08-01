#' Grid Class for SVF Model
#'
#' This class represents a grid on which the SVF model is performed.
#' A grid is a partition of the input space divided into cells.
#'
#' @param data DataFrame with the dataset on which the grid is built.
#' @param inputs List of inputs.
#' @param outputs List of outputs.
#' @param d Number of partitions in which the grid is divided.
#' @field data_grid Grid of intial dataset, initially NULL.
#' @field knot_list List of nodes in the grid, initially NULL.
#'
#' @return An object of class Grid.
Grid <- function(data, inputs, outputs, d) {
  # Validate inputs
  if (!is.data.frame(data)) stop("data must be a DataFrame")
  if (!is.character(inputs)) stop("inputs must be a character")
  if (!is.character(outputs)) stop("outputs must be a character")
  if (!is.numeric(d) || length(d) != 1) stop("d must be a single numeric value")

  # Check if inputs and outputs are columns in data
  data_cols <- colnames(data)
  invalid_inputs <- setdiff(inputs, data_cols)
  invalid_outputs <- setdiff(outputs, data_cols)

  if (length(invalid_inputs) > 0)
    stop("The following inputs are not columns in data: ",
         paste(invalid_inputs, collapse = ", "))
  if (length(invalid_outputs) > 0)
    stop("The following outputs are not columns in data: ",
         paste(invalid_outputs, collapse = ", "))

  selected_columns <- c(inputs, outputs)
  data <- data[selected_columns]

  # Create and return the Grid object
  structure(
    list(
      data = data,
      inputs = inputs,
      outputs = outputs,
      d = d,
      data_grid = NULL,
      knot_list = NULL
    ),
    class = "Grid"
  )
}

#' Find the Cell of an Observation in the Grid
#'
#' This method searches for a specific DMU in the grid and returns the cell
#' in which the observation is located.
#'
#' @param grid Object of class Grid.
#' @param dmu Observation to search for in the grid.
#' @return Vector with the position of the observation in the grid.
search_dmu.Grid <- function(grid, dmu) {
  if (!inherits(grid, "Grid")) stop("grid must be an object of class Grid")
  if (!is.numeric(dmu)) stop("dmu must be numeric")
  r <- lapply(grid$knot_list, unlist)
  cell <- numeric(length(dmu))
  for (l in seq_along(dmu)) {
    encontrado <- FALSE
    for (m in seq_along(r[[l]])) {
      trans <- transformation(dmu[l], r[[l]][m])
      if (trans < 0) {
        cell[l] <- m - 1
        encontrado <- TRUE
        break
      } else if (trans == 0) {
        cell[l] <- m
        encontrado <- TRUE
        break
      }
    }
    if (!encontrado) {
      cell[l] <- length(r[[l]])
    }
  }

  return(cell)
}

#' Value Transformation in the Grid
#'
#' This function evaluates whether the value of an observation is greater than,
#' equal to, or less than the value of a node in the grid. It returns 1 if greater,
#' 0 if equal, and -1 if less.
#'
#' @param x_i Value of the cell to evaluate.
#' @param t_k Value of the node to compare against.
#' @return Result of the comparison: 1, 0, -1.
transformation <- function(x_i, t_k) {
  z <- x_i - t_k
  if (z < 0) {
    return(-1)
  } else if (z == 0) {
    return(0)
  } else {
    return(1)
  }
}
