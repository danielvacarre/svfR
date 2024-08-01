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
  # Validate inputs
  if (!is.data.frame(data))
    stop("data must be a DataFrame")
  if (!is.character(inputs))
    stop("inputs must be a character")
  if (!is.character(outputs))
    stop("outputs must be a character")
  if (!is.numeric(d) || length(d) != 1 || d <= 0)
    stop("d must be a positive numeric value")

  # Check if inputs and outputs are columns in data
  data_cols <- colnames(data)
  invalid_inputs <- setdiff(inputs, data_cols)
  invalid_outputs <- setdiff(outputs, data_cols)

  if (length(invalid_inputs) > 0)
    stop("The following inputs are not columns in data: ", paste(invalid_inputs, collapse = ", "))
  if (length(invalid_outputs) > 0)
    stop("The following outputs are not columns in data: ", paste(invalid_outputs, collapse = ", "))

  # Create and return the SVFGrid object, inheriting from Grid
  grid <- Grid(data, inputs, outputs, d)
  structure(
    list(
      data = grid$data,
      inputs = grid$inputs,
      outputs = grid$outputs,
      d = grid$d,
      data_grid = grid$data_grid,
      knot_list = grid$knot_list,
      df_grid = NULL
    ),
    class = c("SVFGrid", "Grid")
  )
}

#' Create a Grid Based on Data and Hyperparameter d
#'
#' This method creates a grid based on the data and hyperparameter d provided
#' to the constructor. The grid represents the data in a space divided into
#' cells defined by the parameter d.
#'
#' The grid is constructed by dividing each input dimension into `d` partitions,
#' resulting in a grid where each cell represents a partition of the input space.
#' The function also calculates the phi values for each cell and the data grid
#' based on the new grid structure.
#'
#' @param grid An SVFGrid object on which to operate. This object should
#'   contain the `data` and the `inputs` parameters necessary for grid creation.
#'
#' @return The SVFGrid object with the created grid and updated grid dataframe.
#'   The grid object will include:
#'   - `knot_list`: A list of knot points defining the grid boundaries for each dimension.
#'   - `df_grid`: A dataframe with grid cell IDs, values, phi values, and contiguous cells.
#'
#' @details
#' The function performs the following steps:
#' 1. Extracts the input data based on the specified input columns.
#' 2. Defines knot points for each dimension by dividing the range of each input
#'    into `d` partitions plus one additional point for the boundary.
#' 3. Creates a dataframe of cell IDs and values based on the defined knots.
#' 4. Updates the `SVFGrid` object with the new grid information and calculates
#'    additional details such as phi values and the data grid.
#'
#' @example
#' # Assuming `svf_grid` is an existing SVFGrid object
#' svf_grid <- create_grid(svf_grid)
#' head(svf_grid$df_grid)
#'
#' @export
#' Create Grid for SVFGrid Objects
#'
#' This function creates a grid for an SVFGrid object by generating a grid of knots
#' for each input dimension and storing it in the `df_grid` component of the object.
#' The grid is constructed based on the range of each input variable and the number of
#' knots specified by the `d` parameter of the SVFGrid object.
#'
#' @param grid An object of class `SVFGrid`. This object must contain data and
#'   the number of knots specified.
#'
#' @return The function modifies the input `grid` object by adding a `df_grid` component.
#'   This component includes:
#'   - `id_cells`: A data frame with indices for each cell in the grid.
#'   - `values`: A data frame with the values of the knots for each cell in the grid.
#'   - `phi`: A list of length equal to the number of rows in `values`, initially empty.
#'
#' @details
#' The function performs the following steps:
#' 1. Checks if the input `grid` is of class `SVFGrid`. If not, it raises an error.
#' 2. Extracts the input variables from the grid data and calculates the minimum and
#'    maximum values for each dimension.
#' 3. Creates a list of knots for each dimension, with `d + 1` equally spaced values
#'    between the minimum and maximum values.
#' 4. Constructs the `id_cells` and `values` data frames. If there is only one dimension,
#'    the data frames are created directly from the knot indices and values. For multiple
#'    dimensions, the `expand.grid` function is used to create a grid of all possible combinations.
#' 5. Updates the `grid` object with the new `df_grid` component, and automatically
#'    reverses the column names for `id_cells` and `values`.
#'
#' @examples
#' # Assuming `svf_grid` is an SVFGrid object:
#' create_grid.SVFGrid(svf_grid)
#'
#' @export
#' Create Grid for SVFGrid Objects
#'
#' This function generates a grid for an SVFGrid object by calculating a grid of knots
#' for each input dimension and storing the result in the `df_grid` component of the object.
#' It then invokes additional functions to further process the grid and data.
#'
#' @param grid An object of class `SVFGrid`. This object must contain data and
#'   the number of knots specified by the `d` parameter.
#'
#' @return The function modifies the input `grid` object by adding a `df_grid` component
#'   and subsequently calls `calculate_df_grid.SVFGrid` and `calculate_data_grid.SVFGrid`.
#'   The `df_grid` component includes:
#'   - `id_cells`: A data frame with indices for each cell in the grid.
#'   - `values`: A data frame with the values of the knots for each cell in the grid.
#'   - `phi`: A list of length equal to the number of rows in `values`, initially empty.
#'
#' @details
#' The function performs the following steps:
#' 1. Checks if the input `grid` is of class `SVFGrid`. If not, it raises an error.
#' 2. Extracts the input variables from the grid data and calculates the minimum and
#'    maximum values for each dimension.
#' 3. Generates a list of knots for each dimension, with `d + 1` equally spaced values
#'    between the minimum and maximum values.
#' 4. Constructs the `id_cells` and `values` data frames. For a single dimension, the data
#'    frames are created directly from the knot indices and values. For multiple dimensions,
#'    the `expand.grid` function is used to create a grid of all possible combinations.
#' 5. Updates the `grid` object with the new `df_grid` component, and automatically
#'    reverses the column names for `id_cells` and `values`.
#' 6. Calls `calculate_df_grid.SVFGrid` to process the `df_grid` component further.
#' 7. Calls `calculate_data_grid.SVFGrid` to perform additional calculations on the grid data.
#'
#' @seealso
#' \code{\link{calculate_df_grid.SVFGrid}}, \code{\link{calculate_data_grid.SVFGrid}}
#'
#' @examples
#' # Assuming `svf_grid` is an SVFGrid object:
#' create_grid.SVFGrid(svf_grid)
#'
#' @export
create_grid.SVFGrid <- function(grid) {
  # Check if the input object is of class SVFGrid
  if (!inherits(grid, "SVFGrid")) stop("grid must be an object of class SVFGrid")

  # Extract input variables from the grid data
  x <- grid$data[, grid$inputs, drop = FALSE]
  n_dim <- ncol(x)

  # Initialize lists for knots and indices
  knot_list <- list()
  knot_index <- list()

  # Generate knots for each dimension
  for (col in seq_len(n_dim)) {
    # Determine the range of values for the current dimension
    knot_min <- min(x[, col], na.rm = TRUE)
    knot_max <- max(x[, col], na.rm = TRUE)
    # Create a sequence of knots with `d + 1` equally spaced values
    knots <- seq(knot_min, knot_max, length.out = grid$d + 1)
    knot_list[[col]] <- knots
    knot_index[[col]] <- 1:(grid$d + 1)
  }

  # Store the list of knots in the grid object
  grid$knot_list <- knot_list

  # Create data frames for cell indices and knot values
  if (n_dim == 1) {
    # For a single dimension, use direct indices and values
    id_cells <- as.data.frame(knot_index[[1]])
    colnames(id_cells) <- grid$inputs
    values <- as.data.frame(knot_list[[1]])
    colnames(values) <- grid$inputs
  } else {
    # For multiple dimensions, create a grid of all possible combinations
    id_cells <- expand.grid(knot_index)
    id_cells <- id_cells[, ncol(id_cells):1]  # Reverse column order to match input order
    values <- expand.grid(rev(knot_list))
    values <- values[, ncol(values):1]  # Reverse column order to match input order
  }

  # Update the grid object with the new df_grid component
  grid$df_grid <- list(id_cells = id_cells, values = values, phi = vector("list", nrow(values)))

  # Automatically reverse column names for id_cells and values
  new_colnames <- rev(colnames(grid$df_grid$id_cells))  # Reverse the vector of column names
  colnames(grid$df_grid$id_cells) <- new_colnames  # Assign new column names to id_cells
  colnames(grid$df_grid$values) <- new_colnames  # Assign new column names to values

  # Call additional functions to further process the grid and data
  grid <- calculate_df_grid.SVFGrid(grid)
  grid <- calculate_data_grid.SVFGrid(grid)

  # Return the updated grid object
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

#' Calculate DMU Phi Values for SVFGrid
#'
#' This function calculates the phi values for a given cell in the SVFGrid.
#' The phi value is determined based on the comparison between the input cell
#' and the cells in the grid. A phi value of 1 indicates that the input cell
#' has values less than or equal to the corresponding values in the grid cell
#' across all dimensions. Otherwise, the phi value is 0.
#'
#' @param grid An SVFGrid object that contains the grid information, including
#' the data frame `df_grid` with cell IDs and outputs.
#' @param cell A vector specifying the position of the cell in the grid.
#'
#' @return A list of numeric vectors, each corresponding to the phi values
#' calculated for the given cell with respect to each output in the grid.
#'
#' @export
calculate_dmu_phi.SVFGrid <- function(grid, cell) {
  # Extract the data frame from the grid object
  df_grid <- grid$df_grid
  # Get the number of rows in the data frame
  n_rows <- nrow(df_grid$id_cells)
  # Initialize a list to store phi values for each output
  phi_list <- vector("list", length(grid$outputs))

  # Loop over each output in the grid
  for (output_index in seq_along(grid$outputs)) {
    # Initialize a numeric vector to store phi values for the current output
    phi <- numeric(n_rows)
    # Loop over each row in the data frame
    for (i in seq_len(n_rows)) {
      # Start with a phi value of 1
      value <- 1
      # Compare the input cell with the current grid cell
      for (j in seq_along(cell)) {
        if (cell[j] < df_grid$id_cells[i, j]) {
          # Set phi to 0 if the input cell value is less than the grid cell value
          value <- 0
          break
        }
      }
      # Assign the calculated phi value to the current position
      phi[i] <- value
    }
    # Store the phi values for the current output in the list
    phi_list[[output_index]] <- phi
  }

  # Return the list of phi values
  return(phi_list)
}

#' Method to add transformed Values to the Grid DataFrame
#'
#' This method calculates and adds additional information to the grid dataframe
#' associated with an SVFGrid object. This information includes the phi values
#' for each grid cell and the contiguous cells for each cell.
#'
#' @param grid An SVFGrid object on which to operate.
#'
#' @return The SVFGrid object with the updated grid dataframe.
#'
#' @details
#' The function iterates over each cell in the grid's dataframe. For each cell,
#' it computes the corresponding phi values and finds the contiguous cells.
#' It then appends this information to the dataframe in the SVFGrid object.
#'
#' The resulting dataframe includes:
#' - `phi`: A list of phi values for each cell, indicating whether the cell's
#'   values are less than or equal to the corresponding values in the grid cell.
#' - `c_cells`: A list of contiguous cells for each cell.
#'
#' @export
calculate_df_grid.SVFGrid <- function(grid) {
  # Number of cells in the grid dataframe
  n <- nrow(grid$df_grid$id_cells)

  # Initialize lists to store phi values and contiguous cells
  phi_list <- vector("list", n)
  c_cells_list <- vector("list", n)

  for (i in 1:n) {
    # Get the current cell's values as numeric
    cell <- as.numeric(grid$df_grid$values[i, , drop = FALSE])

    # Find the position of the cell in the grid
    p <- search_dmu.Grid(grid, cell)

    # Calculate phi values for the cell
    phi <- calculate_dmu_phi.SVFGrid(grid, p)[[1]]

    # Find contiguous cells for the cell
    c_cells <- search_contiguous_cell(p)

    # Store the results in the lists
    phi_list[[i]] <- list(phi)
    c_cells_list[[i]] <- c_cells
  }

  # Update the grid dataframe with phi values and contiguous cells
  grid$df_grid$phi <- phi_list
  grid$df_grid$c_cells <- c_cells_list

  # Return the updated SVFGrid object
  return(grid)
}

#' Method to add the transform value of each observation to the grid dataframe
#'
#' This function processes each observation of the data_grid based on the columns specified in inputs,
#' calculating phi values and contiguous cells (c_cells), and updating the grid object with these results.
#'
#' @param grid SVFGrid object to operate on.
#'
#' @return Modified grid object with the results of phi and c_cells added.
#'
#' @example examples/example_data.R
#'
#' @export
calculate_data_grid.SVFGrid <- function(grid) {
  # Subset the data to include only inputs and outputs
  grid$data_grid <- grid$data[, as.character(grid$inputs, grid$outputs), drop = FALSE]
  n_rows <- nrow(grid$data_grid)  # Number of rows in the subset data

  # Initialize lists to store phi values and contiguous cells for each row
  phi_list <- vector("list", n_rows)
  c_cells_list <- vector("list", n_rows)

  # Loop over each row of the data grid
  for (i in seq_len(n_rows)) {
    # Extract input values for the current row
    x <- as.numeric(grid$data_grid[i, as.character(grid$inputs), drop = FALSE])

    # Find the corresponding decision-making unit (DMU)
    p <- search_dmu.Grid(grid, x)

    # Calculate phi for the found DMU
    phi <- calculate_dmu_phi.SVFGrid(grid, p)

    # Find contiguous cells for the DMU
    c_cells <- search_contiguous_cell(p)

    # Store the results in the lists
    phi_list[[i]] <- phi
    c_cells_list[[i]] <- c_cells
  }

  # Add phi and contiguous cells as new columns in the data grid
  grid$data_grid$phi <- phi_list
  grid$data_grid$c_cells <- c_cells_list

  # Return the modified grid object
  return(grid)
}

