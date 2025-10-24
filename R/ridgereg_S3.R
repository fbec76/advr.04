#' Ridge Regression using Least Squares
#'
#' Computes ridge regression coefficients and fitted values based on a given formula, dataset, and regularization parameter, lambda.
#' The predictors are normalized before fitting the model.
#' With penalized least-squares.
#' Uses OR Decomposition for lambda == 0
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
#' @importFrom stats model.frame model.response model.matrix .getXlevels sd
#'
#' @export
ridgereg <- function(formula, data, lambda) {
  stopifnot(inherits(formula, "formula"))
  stopifnot(is.data.frame(data))
  stopifnot(is.numeric(lambda), length(lambda) == 1, lambda >= 0)

  mf <- model.frame(formula, data)
  y <- model.response(mf)
  trm <- terms(mf)
  X <- model.matrix(trm, data = mf)
  xlevels <- .getXlevels(trm, mf)
  contrasts <- attr(X, "contrasts")

  if (lambda == 0) {
    # OLS via QR
    qrX <- qr(X)
    coefs <- qr.coef(qrX, y)
    fitted <- drop(X %*% coefs)
    return(structure(list(
      formula = formula,
      terms = trm,
      xlevels = xlevels,
      contrasts = contrasts,
      lambda = 0,
      coefficients = coefs,
      x_center = numeric(0),
      x_scale = numeric(0),
      fitted_values = fitted
    ), class = "ridgereg"))
  }

  ybar <- mean(y)
  Xp <- X[, -1, drop = FALSE]

  x_center <- colMeans(Xp)

  x_scale <- apply(Xp, 2, sd)
  x_scale[is.na(x_scale) | x_scale == 0] <- 1


  Z <- sweep(Xp, 2, x_center, "-")
  Z <- sweep(Z, 2, x_scale, "/")
  yc <- y - ybar

  p <- ncol(Z)
  K <- crossprod(Z) + lambda * diag(p)
  beta_std <- solve(K, crossprod(Z, yc))

  beta <- beta_std / x_scale
  beta0 <- ybar - sum(beta * x_center)
  coefs <- c("(Intercept)" = beta0, beta)

  fitted <- drop(X %*% coefs)

  structure(list(
    formula = formula,
    terms = trm,
    xlevels = xlevels,
    contrasts = contrasts,
    lambda = lambda,
    coefficients = coefs,
    x_center = x_center,
    x_scale = x_scale,
    fitted_values = fitted
  ), class = "ridgereg")
}

#' Print method for ridgereg objects
#'
#' @param x An object of class \code{"ridgereg"}.
#' @param ... Additional arguments (ignored).
#'
#' @export
print.ridgereg <- function(x, ...) {
  cat("Call:\n")
  print(x$formula, ...)
  cat("\nCoefficients:\n")
  print(drop(x$coefficients), ...)
  invisible(x)
}

#' Predict method for ridgereg objects
#'
#' @param object An object of class \code{"ridgereg"}.
#' @param newdata Optional data frame for prediction. If omitted, fitted values are returned.
#'
#' @importFrom stats model.matrix delete.response terms
#'
#' @return A numeric vector of predicted values.
#' @export
predict.ridgereg <- function(object, newdata = NULL) {
  if (is.null(newdata)) {
    return(drop(object$fitted_values))
  } else {
    mf_new <- model.frame(
      delete.response(object$terms),
      data = newdata,
      xlev = object$xlevels
    )
    X_new <- model.matrix(
      delete.response(object$terms),
      data = mf_new,
      contrasts.arg = object$contrasts
    )
    return(drop(X_new %*% object$coefficients))
  }
}

#' Extract coefficients from ridgereg object
#'
#' @param object An object of class \code{"ridgereg"}.
#'
#' @return A numeric vector of ridge regression coefficients.
#' @export
coef.ridgereg <- function(object) {
  drop(object$coefficients)
}
