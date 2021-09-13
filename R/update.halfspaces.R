#' Update method for 'halfspace' objects
#'
#' Retrains the halfspaces on given data and updates halfspaces on given number.
#'
#' @param object an object of class 'halfspace'
#' @param data numeric matrix or dataframe of the same dimension as given halfspace
#' object on which the
#' halfspaces will be retrained. Halfspaces of the originial object are kept in
#' case of retraining
#' @param n_halfspace number of halfspaces to which n_halfspaces of the given
#' halfspace object will be extend (if greater than current value) or lowered
#' (if smaller than current value) to.  In case of deletion, halfspaces are
#' deleted beginning with the first element of the given halfspaces object
#' @param add if stated as TRUE, retraining of the halfspaces will be conducted
#' on the union set of halfspaces$train_data and the data given by the data
#' argument. If FALSE, only the given data will be used for retraining
#' @param ... further arguments
#' @describeIn halfspaces update halfspaces
#' @return A retrained and resized halfspace object. Other properties of the
#' object are kept.
#'
#' @examples \dontrun{
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
#' hs <- halfspaces(data[1:40,1:2], seed = 187471431)
#'
#' hs_update_1 <- update(hs, data = data[60,80, 1:2], add = TRUE)
#' hs_update_2 <- update(hs, data = data[60,80, 1:2], add = FALSE)
#' hs_update_3 <- update(hs, n_halfspace = 900)
#' hs_update_4 <- update(hs, n_halfspace = 1500)
#' hs_update_5 <- update(hs, data = data[60,80, 1:2], add = TRUE,
#'                       n_halfspace = 900)
#' hs_update_6 <- update(hs, data = data[60,80, 1:2], add = FALSE,
#'                       n_halfspace = 1500)
#' }
#' @import checkmate
#' @md
#' @export

update.halfspaces <- function(object, data = NULL,  n_halfspace = NULL ,
                              add = FALSE, ...) {

  # input checking
  check_halfspaces(halfspaces = object, data = data)
  checkmate::assert_number(n_halfspace, lower = 0, null.ok = TRUE)
  checkmate::assert_logical(add, len = 1)

  # input homogenization
  if (inherits(data, "data.frame")) data <- as.matrix(data)

  # retrain halfspaces on given data
  if (!is.null(data)) {
    object <- retrain_halfspaces(object, data, add)
  }

  # update amount of halfspaces
  if (!is.null(n_halfspace)) {
    object <- update_n_halfspaces(object, n_halfspace)
  }

  message("Normal vectors of halfspaces were preserved for retraining.
          If no seed was given, splitpoints were not preserved as they are
          determined stochastically")

  object
}




################################################################################
#' Helper function for update.halfspaces()
#'
#' Retrains the halfspaces on given data set.
#'
#' @inheritParams update.halfspaces
#' @param halfspaces object of class 'halfspaces'
#'
#' @return A halfspace object retrained on given data set.
#'
#' @md
#' @export

retrain_halfspaces <- function(halfspaces, data, add) {
  # get halfspaces of given object
  normals <- sapply(halfspaces, "[[", "normal")

  # create union set if add is TRUE
  if (add) {data <- rbind(attr(halfspaces, "train_data"), data)}

  # retrain halfspaces on given data
  halfspaces_updated <- apply(normals, 2, get_halfspace,
                              data = data,
                              subsample = attr(halfspaces, "subsample"),
                              scope = attr(halfspaces, "scope")
  )

  # set and restore attributes
  attributes(halfspaces_updated) <- attributes(halfspaces)
  attr(halfspaces_updated, "n_halfspace") <- length(halfspaces_updated)
  attr(halfspaces_updated, "train_data") <- data
  halfspaces_updated
}

################################################################################
#' Helper function for update.halfspaces()
#'
#' Delete or add halfspaces based on n_halfspace
#' @param halfspaces object of class 'halfspaces'
#' @inheritParams update.halfspaces
#'
#' @return A halfspace object with updated amount of halfspaces.
#'
#' @md
#' @export

update_n_halfspaces <- function(halfspaces, n_halfspace, ...){
  # caculate difference to check if halfspaces need to be deleted or added
  difference <- n_halfspace - attr(halfspaces, "n_halfspace")

  # using a new variable here is neccessary in order to keep the other
  # attributes of the halfspace object
  halfspaces_updated <- halfspaces

  # delete halfspaces
  if (difference < 0) {
    halfspaces_updated <- halfspaces[-c(1:abs(difference))]
    attributes(halfspaces_updated) <- attributes(halfspaces)
    attr(halfspaces_updated, "n_halfspace") <- length(halfspaces_updated)
    return(halfspaces_updated)
  }

  # add halfspaces. Seed + 1 ensures that new halfspaces are drawn, if a seed
  # was given.
  if (difference > 0) {
    new_halfspaces <- train_depth(data = attr(halfspaces, "train_data"),
                                  n_halfspace = abs(difference),
                                  subsample = attr(halfspaces, "subsample"),
                                  seed = attr(halfspaces, "seed") + 1,
                                  scope = attr(halfspaces, "scope"))
    # set and restore attributes
    halfspaces_updated <- c(halfspaces, new_halfspaces)
    attributes(halfspaces_updated) <- attributes(halfspaces)
    attr(halfspaces_updated, "n_halfspace") <- length(halfspaces_updated)
  }
  halfspaces_updated
}
