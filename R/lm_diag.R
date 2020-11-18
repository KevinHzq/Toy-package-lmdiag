
#' Diagnose Linear Model Assumptions
#'
#' @description
#' Diagnose linear model assumptions by testing model residuals for normality using Shapiro-Wilk test, and for homogeneity using Breusch-Pagan test.
#'
#' @param model an object of class 'lm'
#' @param output.p logical that specify output information type. If FALSE outputs 'pass' when failed to reject both normality and equal variance null hypotheses, or outputs 'fail', otherwise. If TRUE outputs a list containing two p-values.
#' @param alpha numeric alpha level for determining whether null hypotheses are rejected. Applicable only if output.p is FALSE.
#'
#' @param ... additional arguments to be passed to the residuals function

#'
#' @return
#' Character string of test result, 'pass' or 'fail', or list of p-values depending on the `output.p` argument.
#' @export
#'
#' @examples
#' lm_model <- lm(lifeExp ~ gdpPercap, data = gapminder::gapminder)
#' lm_diag(lm_model)
#'
#'
lm_diag <- function(model, output.p = FALSE, alpha = 0.05, ...) {
  if (class(model) != "lm") stop("The input model is not a lm object")
  if (alpha<=0 | alpha>=1) stop("Alpha level is not in range of (0,1)")
  #extract p-value from Shapiro-Wilk normality test
  p_norm <- stats::shapiro.test(stats::residuals(model, ...))[["p.value"]]
  #extract p-value from Breusch-Pagan test for equal variances
  p_homo <- car::ncvTest(model)[["p"]]
  #different outputs depending on output.p
  if (!output.p) {
    if ((p_norm > 0.05) & (p_homo > 0.05)) {
      return("pass")
    }
    else return("fail")
  }
  else return(list(p_normality = p_norm, p_homogeneity = p_homo))
}
