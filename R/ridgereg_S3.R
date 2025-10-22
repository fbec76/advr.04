#' Ridge Regression using Least Squares
#'
#' Computes ridge regression coefficients and fitted values based on a given formula, dataset, and regularization parameter, lambda.
#' The predictors are normalized before fitting the model.
#'
#' @param formula A formula object specifying the model. (class: formula)
#' @param data A data frame containing the variables in the formula.
#' @param lambda A non-negative numeric value specifying the ridge penalty parameter.
#'
#' @return An object of class \code{"ridgereg"} containing:
#' \item{formula}{The model formula.}
#' \item{coefficients}{A numeric vector of estimated ridge regression coefficients.}
#' \item{fitted_values}{A numeric vector of fitted values.}
#'
#' @export
ridgereg <- function(formula, data, lambda) {
  stopifnot(inherits(formula, "formula"))
  stopifnot(inherits(data, "data.frame"))
  stopifnot(is.numeric(lambda), length(lambda) == 1, lambda >= 0)

  mf <- model.frame(formula, data)
  y <- model.response(mf)
  X <- model.matrix(formula, data)

  X_scaled <- scale(X[, -1], center = TRUE, scale = FALSE)
  X <- cbind(Intercept = 1, X_scaled)

  y_centered <- y - mean(y)

  p <- ncol(X)
  I <- diag(p)
  I[1, 1] <- 0

  reg_coeffs <- solve(t(X) %*% X + lambda * I) %*% (t(X) %*% y_centered)

  x_means <- colMeans(model.matrix(formula, data)[, -1])
  reg_coeffs[1] <- mean(y) - sum(reg_coeffs[-1] * x_means)

  fitted_vals <- X %*% reg_coeffs

  object <- list(
    formula = formula,
    coefficients = reg_coeffs,
    fitted_values = fitted_vals
  )
  class(object) <- "ridgereg"
  return(object)
}
#' Print method for ridgereg objects
#'
#' @param x An object of class \code{"ridgereg"}.
#' @param ... Additional arguments (ignored).
#'
#' @export
print.ridgereg <- function(x, ...) {
  cat("Call:\n")
  print(x$formula)
  cat("\nCoefficients:\n")
  print(drop(x$coefficients))
  invisible(x)
}
#' Predict method for ridgereg objects
#'
#' @param object An object of class \code{"ridgereg"}.
#' @param newdata Optional data frame for prediction. If omitted, fitted values are returned.
#' @param ... Additional arguments (ignored).
#'
#' @return A numeric vector of predicted values.
#' @export
predict.ridgereg <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) {
    return(drop(object$fitted_values))
  } else {
    X_new <- model.matrix(delete.response(terms(object$formula)), newdata)
    X_scaled <- scale(X_new[, -1])
    X_new <- cbind(Intercept = 1, X_scaled)
    return(drop(X_new %*% object$coefficients))
  }
}
#' Extract coefficients from ridgereg object
#'
#' @param object An object of class \code{"ridgereg"}.
#' @param ... Additional arguments (ignored).
#'
#' @return A numeric vector of ridge regression coefficients.
#' @export
coef.ridgereg <- function(object, ...) {
  drop(object$coefficients)
}
