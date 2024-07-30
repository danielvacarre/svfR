#' Constructor for the SVFGrid class
#'
#' This function creates an instance of the SVFGrid class, which is an
#' extension of the Grid class with additional functionalities specific
#' to handling grids in the context of SVF analysis.
#'
#' @param data Data set on which the grid is built.
#' @param inputs List of inputs.
#' @param outputs List of outputs.
#' @param d Number of partitions in which the grid is divided.
#'
#' @return An object of class SVFGrid.
#'
#'
#' @export
SVFGrid <- function(data, inputs, outputs, d) {
  grid <- list(data = data,
               inputs = inputs,
               outputs = outputs,
               d = d,
               df_grid = data.frame(),
               data_grid = data.frame()
               )
  class(grid) <- c("SVFGrid", "Grid")
  return(grid)
}

#' Search for Contiguous Cells in SVFGrid
#'
#' This function identifies and returns the cells contiguous to a specified cell
#' in the grid. Contiguous cells are those that share at least one edge or point
#' with the specified cell.
#'
#' @param cell A vector specifying the position of the cell in the grid.
#'
#' @return A list of cells contiguous to the specified cell.
#'
#'
#' @export
search_contiguous_cell <- function(cell) {
  con_c_list <- list()

  for (dim in seq_along(cell)) {
    value <- cell[dim] - 1
    con_cell <- cell

    if (value >= 1) {
      con_cell[dim] <- value
      con_c_list <- c(con_c_list, list(con_cell))
    }
  }

  return(con_c_list)
}



#' Function that creates a grid based on data and parameter d
#'
#' This method creates a grid based on the data and parameter d provided
#' to the constructor. This grid is a representation of the data in a space
#' divided into cells defined by parameter d.
#'
#' @param grid SVFGrid object to operate on.
#'
#' @return The SVFGrid object with the grid created.
#'
#'
#' @export
create_grid.SVFGrid <- function(grid) {
  x <- grid$data[, grid$inputs, drop = FALSE]
  n_dim <- ncol(x)
  knot_list <- list()
  knot_index <- list()

  for (col in seq_len(n_dim)) {
    knot_min <- min(x[, col], na.rm = TRUE)
    knot_max <- max(x[, col], na.rm = TRUE)
    knots <- seq(knot_min, knot_max, length.out = grid$d + 1)
    knot_list[[col]] <- knots
    knot_index[[col]] <- 1:(grid$d + 1)
  }

  grid$knot_list <- knot_list

  if (n_dim == 1) {
    id_cells <- as.data.frame(knot_index[[1]])
    colnames(id_cells) <- grid$inputs
    values <- as.data.frame(knot_list[[1]])
    colnames(values) <- grid$inputs
  } else {
    id_cells <- expand.grid(knot_index)
    id_cells <- id_cells[, ncol(id_cells):1]
    values <- expand.grid(rev(knot_list))
    values <- values[, ncol(values):1]
  }

  grid$df_grid <- list(id_cells = id_cells, values = values, phi = vector("list", nrow(values)))
  grid <- calculate_df_grid.SVFGrid(grid)
  grid <- calculate_data_grid.SVFGrid(grid)

  return(grid)
}


