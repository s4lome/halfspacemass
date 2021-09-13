#' Helper function for input checking
#'
#' checks if given object is a valid halfspace object and if given data
#' is compatible with given halfspace object
#'
#' @param halfspaces an object of class 'halfspaces'
#' @param data dataframe or datamatrix to check that should be compatible to
#' 'halfspaces' object
#'
#' @return empty if inputs are valid, error if not
#'
#' @md

check_halfspaces <- function(halfspaces, data) {
  dim_data <- ncol(data)
  for (i in 1:length(halfspaces)) {
    checkmate::assert_list(halfspaces[[i]],
                         len = 3, types = rep("numeric", 3))
    checkmate::assert_names(names(halfspaces[[i]]),
                            subset.of = c("normal", "split", "mass_above"))
    checkmate::assert_numeric(halfspaces[[i]]$normal,
                              finite = TRUE, any.missing = FALSE, len = dim_data)
    checkmate::assert_number(halfspaces[[i]]$split)
    checkmate::assert_number(halfspaces[[i]]$mass_above,
                             lower = 0, upper = 1)

  }

}




