#' linreg Reference Class
#'
#' Object-oriented implementation of ordinary least squares linear regression.
#' @importFrom ggplot2 ggplot aes geom_point geom_hline stat_summary labs theme_classic
#' @importFrom gridExtra grid.arrange
#' @importFrom methods setRefClass
#'
#' @field formula Formula specifying the regression model. (class: formula)
#' @field data Data frame containing all variables referenced in formula.
#' @field .x Design matrix of predictor variables constructed by model.matrix().
#' @field .y Numeric vector of response variable values.
#' @field .reg_coeffs Fitted regression coefficients (including intercept).
#' @field .fitted_vals Fitted values of the response variable.
#' @field .residuals Residuals (observed - fitted values).
#' @field .degs_of_freedom Degrees of freedom for residuals (n - p).
#' @field .residual_var Estimated residual (error) variance.
#' @field .reg_coeffs_var Estimated variance of regression coefficients.
#' @field .coeff_t_values t-statistics for the coefficients.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{print()}}{Prints the object creation and the calculated coefficients}
#'   \item{\code{plot()}}{Plots two graphs with "Residuals vs Fitted" and "Scale−Location"}
#'   \item{\code{resid()}}{Returns the vector of residuals: e}
#'   \item{\code{pred()}}{Returns the predicted values: y}
#'   \item{\code{coef()}}{Returns a named vector for the coefficients}
#'   \item{\code{summary()}}{Prints the summary of the linear regression}
#' }
#'
#' @export linreg
#' @exportClass linreg
linreg <- setRefClass(
  "linreg",
  fields = list(
    formula = "formula",
    data = "data.frame",
    .x = "matrix",
    .y = "numeric",
    .reg_coeffs = "matrix",
    .fitted_vals = "matrix",
    .residuals = "matrix",
    .degs_of_freedom = "integer",
    .residual_var = "numeric",
    .reg_coeffs_var = "numeric",
    .coeff_t_values = "matrix"
  ),
  methods = list(
    initialize = function(formula = NULL, data = NULL, ...) {
      callSuper(formula = formula, data = data)
      .x <<- model.matrix(formula, data = data)
      y_name <- all.vars(formula)[1]
      .y <<- data[[y_name]]
      .reg_coeffs <<- solve(t(.x) %*% .x) %*% (t(.x) %*% .y)
      .fitted_vals <<- .x %*% .reg_coeffs
      .residuals <<- .y - .fitted_vals
      .degs_of_freedom <<- nrow(.x) - ncol(.x)
      .residual_var <<- as.numeric(t(.residuals) %*% .residuals) / .degs_of_freedom
      .reg_coeffs_var <<- .residual_var * diag(solve(t(.x) %*% .x))
      .coeff_t_values <<- .reg_coeffs / sqrt(.reg_coeffs_var)
    },
    print = function() {
      cat(sprintf("%s(formula = %s, data = %s)\n",
                  class(.self), deparse(.self$formula), "iris"))
      cat("Coefficients:\n")
      print.table(t(.self$.reg_coeffs))
      cat("\n")
    },
    plot = function() {

      residuals_vs_fitted <- data.frame(fitted = .self$pred(),
                                        residuals = .self$resid())
      std_residuals_vs_fitted <- data.frame(fitted = .self$pred(),
                                            residuals = sqrt(abs(.self$resid() / sqrt(.self$.residual_var))))
      plot1 <- ggplot(residuals_vs_fitted, aes(x = fitted, y = residuals)) +
        geom_point(shape = 1, size = 2, stroke = 1) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
        stat_summary(fun = "median", geom = "line", color = "red") +
        labs(
          x = "Fitted Values",
          y = "Residuals",
          title = "Residuals vs. Fitted Values"
        ) +
        theme_classic()


      plot2 <- ggplot(std_residuals_vs_fitted, aes(x = fitted, y = residuals)) +
        geom_point(shape = 1, size = 2, stroke = 1) +
        stat_summary(fun = "mean", geom = "line", color = "red") +
        labs(
          x = "Fitted Values",
          y = expression(sqrt(abs("Standardized Residuals"))),
          title = "Scale - Location"
        ) +
        theme_classic()

      gridExtra::grid.arrange(plot1, plot2)
    },
    resid = function() {
      .self$.residuals
    },
    pred = function() {
      .self$.fitted_vals
    },
    coef = function() {
      setNames(c(.self$.reg_coeffs), nm = colnames(.self$.x))
    },
    summary = function() {
      cat("Coefficients:\n")
      cm <- cbind(
        .self$coef(),
        abs(.self$.reg_coeffs) / abs(.self$.coeff_t_values),
        .self$.coeff_t_values,
        2 * (1 - pt(abs(.self$.coeff_t_values), .self$.degs_of_freedom))
      )
      rownames(cm) <- names(.self$coef())
      colnames(cm) <- c("coeffs", "Std. Error", "t value", "Pr(>|t|)")
      printCoefmat(cm, digits = 4, signif.stars = TRUE)
      cat(sprintf(
        "\nResidual standard error: %.4f on %d degrees of freedom\n",
        sqrt(.self$.residual_var), .self$.degs_of_freedom
      ))
    }
  )
)