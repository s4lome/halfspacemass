#' Plot method for objects of class 'halfspaces'
#'
#' visualize half-space depth/mass values for 2D-data on a grid of points
#' if no grid is provided, a grid over min/max +/- .2 * range of data is created.
#'
#' @param x an object of class 'halfspaces'
#' @param data a 2d data.frame with columns z1 and z2
#' @param grid a 2d grid of points given as matrix
#' @param points logical determining if points of data argument should be added
#' to the plot
#' @param gridlength numeric determining the range of the grid
#' @param metric "mass" or "depth" determining which metric should be plotted
#' @param display_style one of "contour", "heatmap" or "combined", determining
#' if a heatmap, a contour plot or a combination should be plotted
#' @param ... further arguments, especially intended for ggplot arguments
#' @describeIn halfspaces plot halfspaces
#' @return An ggplot2 object, containing a plot of given inputs
#'
#' @examples \dontrun{
#' # halfspace mass for faithful data. Gets automatically rescaled in order to
#' # ensure reasonable visualization!
#'
#' faith <- data.frame(faithful)
#' faith_hs <- halfspaces(faith, seed = 187471431)
#' plot(faith_hs, data = faith, display_style = "heatmap")
#'
#' #############################################################################
#'
#' set.seed(187471431)
#' # 2D standard Normal:
#' cluster <- data.frame(z1 = rnorm(50) / 2,
#'                       z2 = rnorm(50) / 2,
#'                       group = "cluster")
#' # polar coordinates:points with distance 3 to 5 from the origin,at 90° - 270°:
#'  left_anomalies <- data.frame(angle = runif(10, pi / 2, 3 * pi / 2),
#'                               length = runif(10, 3, 5))
#' # convert to cartesian coords
#' left_anomalies <- with(left_anomalies, data.frame(
#'                        z1 = length * cos(angle),
#'                        z2 = length * sin(angle),
#'                        group = "anomaly"))
#' # ~ N_2(\mu = (6,0), \Sigma = I_2)
#' right_anomalies <- data.frame(z1 = rnorm(20) / 5 + 6,
#'                               z2 = rnorm(20) / 5,
#'                               group = "anomaly")
#' data <- rbind(cluster,
#'                   left_anomalies,
#'                   right_anomalies)
#'
#' hs <- halfspaces(data[,1:2])
#' plot(hs, data = data[,1:2], display_style = "combined", rescale = FALSE)
#' }
#'
#' @import ggplot2 checkmate
#' @md
#' @export

plot.halfspaces <- function(x, data, grid = NULL, points = TRUE,
                            gridlength = 70, metric = "mass",
                            display_style = c("contour", "heatmap","combined"),
                            ...) {
  # input checking
  check_halfspaces(x, data)
  checkmate::assert(checkmate::check_matrix(data, mode = numeric),
                    checkmate::check_data_frame(data),
                    combine = 'or')
  checkmate::assert_matrix(grid, null.ok = TRUE)
  checkmate::assert_logical(points, len = 1)
  checkmate::assert_number(gridlength)
  checkmate::assert_choice(metric, choices = c("mass", "depth"))
  checkmate::assert_choice(display_style, choices = c("contour", "heatmap",
                                                      "combined"))
  # input homgenization
  names(data)[1] <- "z1"
  names(data)[2] <- "z2"

  # create grid
  if (is.null(grid)) {grid <- create_grid(data, gridlength,
                                          rescale = attr(x, "rescale"))
  }
  grid_depth <- evaluate_depth(data = as.matrix(grid), halfspaces = x,
                               metric = metric, rescale = attr(x, "rescale"))
  grid_halfspaces <- cbind(grid, depth = grid_depth)

  # produce graph according to input
  display_style <- match.arg(display_style)
  plot <- switch(display_style,
                 "contour"  = create_contour_plot(grid_halfspaces),
                 "heatmap" = create_heatmap(grid_halfspaces, metric),
                 "combined" = {create_heatmap(grid_halfspaces, metric) +
                              ggplot2::geom_contour(ggplot2::aes(z = depth))}
  )
  # add data points
  if (attr(x,"rescale")) data <- as.data.frame(scale(data))
  if (points & !is.null(data)) {
    plot <- plot + ggplot2::geom_point(data = data, ggplot2::aes(x = z1, y = z2))
  }
  plot
}
################################################################################
#' Helper function for plot.halfspaces()
#'
#' create a grid for plotting halfspace mass or depth on
#'
#' @inheritParams plot.halfspaces
#' @param rescale logical for rescaling accordingly
#' @return a 2d matrix representing the grid
#'
#' @md

create_grid <- function(data, gridlength, rescale) {
  # input homogenization
  data <- as.data.frame(data)
  if (rescale) data <- as.data.frame(scale(data))
  # set range
  range_1 <- range(data$z1)
  range_2 <- range(data$z2)

  #create grid
  grid <- expand.grid(
    z1 = seq(range_1[1] - .2 * diff(range_1),
             range_1[2] + .2 * diff(range_1),
             length = gridlength
    ),
    z2 = seq(range_2[1] - .2 * diff(range_2),
             range_2[2] + .2 * diff(range_2),
             length = gridlength
    )
  )
}

################################################################################
#' Helper function for plot.halfspaces()
#'
#' uses ggplot2 to create a contour plot
#'
#' @param grid_halfspaces output of create_grid()
#' @param ... further arguments
#' @return An ggplot2 object, containing a plot of given inputs
#' @import ggplot2
#' @md

create_contour_plot <- function(grid_halfspaces, ...) {
  ggplot2::ggplot(grid_halfspaces, ggplot2::aes(x = z1, y = z2)) +
    ggplot2::geom_contour(ggplot2::aes(z = depth,
                                       colour = ggplot2::after_stat(level)))
}
################################################################################
#' Helper function for plot.halfspaces()
#'
#' uses ggplot2 to create a heatmap
#'
#' @param grid_halfspaces output of create_grid()
#' @inheritParams plot.halfspaces
#' @param ... further arguments
#'
#' @return An ggplot2 object, containing a plot of given inputs
#'
#' @import ggplot2
#' @md

create_heatmap <- function(grid_halfspaces, metric, ...) {
  # Colors for Heatmap
  spectralcolors <- c(
    "darkblue", "blue", "cyan", "lightgreen",
    "yellow", "orange", "red", "darkred"
  )
  # create grid plot
  plot <- ggplot2::ggplot(grid_halfspaces, ggplot2::aes(x = z1, y = z2)) +
    # create tile plot
    ggplot2::geom_tile(ggplot2::aes(fill = depth, colour = depth)) +
    # add fill colors
    ggplot2::scale_fill_gradientn(metric, colors = spectralcolors) +
    # remove tiles
    ggplot2::scale_colour_gradientn(metric, colors = spectralcolors) +
    ggplot2::theme_minimal()
}
