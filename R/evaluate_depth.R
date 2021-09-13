#' Helper function for predict.halfspaces() and plot.halfspaces()
#'
#' implements Algorithm 2 of Chen et al, according to [halfspaces()]
#'
#' @inheritParams halfspaces
#' @param metric determines metric to be calculated
#' @param halfspaces a valid halfspace object
#'
#' @return numeric vector with depth metric values for data:
#' either HS mass or HS depth
#'
#' @import checkmate
#' @md

evaluate_depth <- function(data, halfspaces, metric = c("mass", "depth"),
                           rescale = TRUE) {
  # rescale data if wanted
  #if (rescale) data <- scale(data)
  # input homogenization
  if (inherits(data, "data.frame")) data <- as.matrix(data)

  checkmate::assert_matrix(data,
                           mode = "numeric", any.missing = FALSE, min.cols = 2,
                           min.rows = 1
  )

  check_halfspaces(halfspaces, data)
  metric <- match.arg(metric)

  # calculate metrics
  switch(metric,
         "mass"  = get_mass(data, halfspaces),
         "depth" = get_depth(data, halfspaces))
}

################################################################################
#' Helper function for evaluate_depth()
#'
#' computes approximate halfspace mass
#'
#' @inheritParams evaluate_depth
#'
#' @return see evaluate_depth()
#'
#' @md

get_mass <- function(data, halfspaces) {
  result <- numeric(NROW(data))

  for (halfspace in halfspaces) {
    projections <- project_scalar(data, halfspace$normal)
    # for each combination of a halfspace and a data point, use the mass on the
    # side of the split which the data point lies on...
    result <- result + ifelse(
      projections >= halfspace$split,
      yes = halfspace$mass_above,
      no = 1 - halfspace$mass_above
    )
  }
  # ... and take the mean:
  result / length(halfspaces)
}


################################################################################
#' Helper function for evaluate_depth()
#'
#' computes approximate halfspace depth
#'
#' @inheritParams evaluate_depth
#'
#' @return see evaluate_depth()
#'
#' @md

get_depth <- function(data, halfspaces) {
  # init in 1 to iteratively find minimal masses
  result <- rep(1, NROW(data))
  for (halfspace in halfspaces) {
    projections <- project_scalar(data, halfspace$normal)
    result <- pmin(result, ifelse(projections >= halfspace$split,
                                  yes = halfspace$mass_above,
                                  no = 1 - halfspace$mass_above
    ))
  }
  # Tukey depth is usually given in [0, nrow(training data)/2] not [0, 0.5]:
  result * nrow(attr(halfspaces, "train_data")) * attr(halfspaces, "subsample")
}
