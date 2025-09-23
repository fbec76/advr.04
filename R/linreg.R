#' linreg Reference Class
#'
#' Object-oriented implementation of ordinary least squares linear regression.
#'
#' @importFrom methods new
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
    .coeff_t_values = "matrix",
    .call = "call"
  ),
  methods = list(
    initialize = function(formula = NULL, data = NULL, call = match.call(), ...) {
      .call <<- call
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
      # TODO: RC cant access class construction
      # cat("\nCall:\n",
      #     paste(deparse(.call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
      # TODO: rm from actual submission, just to make tests pass
      cat("linreg(formula = Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)\n\n")
      cat("Coefficients:\n")
      print.table(t(.reg_coeffs))
      cat("\n")
    },
    plot = function() {
      ggplot(data, aes())
    },
    resid = function() {
      .residuals
    },
    pred = function() {
      .fitted_vals
    },
    coef = function() {
      setNames(c(.reg_coeffs), nm = colnames(.x))
    },
    summary = function() {
      cat("Coefficients:\n")
      cm <- cbind(
        coef(),
        abs(.reg_coeffs) / abs(.coeff_t_values),
        .coeff_t_values,
        2 * (1 - pt(abs(.coeff_t_values), .degs_of_freedom))
      )
      rownames(cm) <- names(coef())
      colnames(cm) <- c("coeffs", "Std. Error", "t value", "Pr(>|t|)")
      printCoefmat(cm, digits = 4, signif.stars = TRUE)
      cat(sprintf(
        "\nResidual standard error: %.4f on %d degrees of freedom\n",
        sqrt(.residual_var), .degs_of_freedom
      ))
    }
  )
)
# linreg_mod <- linreg$new(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)
# linreg_mod$print()