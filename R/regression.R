# buildFormula --------------------------------------------------------------

#' Build a model formula from jamovi options
#'
#' Shared helper for all regression types (linear, logistic, etc).
#'
#' @param dep Dependent variable name
#' @param terms modelTerms list from self$options$modelTerms
#' @return A formula object
buildFormula <- function(dep, terms) {
  as.formula(
    paste(
      jmvcore::composeTerm(dep),
      "~",
      paste(jmvcore::composeTerms(terms), collapse = " + ")
    )
  )
}


# buildMultiRegTable ---------------------------------------------------------

#' Build a multivariable tbl_regression table
#'
#' Constructs tbl_regression() arguments from jamovi options and returns the
#' table directly. Named *MultiReg* to distinguish from the future
#' buildUniRegTable() for univariable regression.
#'
#' @param model A fitted model object (lm, glm, etc.)
#' @param options Jamovi options object
#' @return A tbl_regression object
buildMultiRegTable <- function(model, options) {
  args <- list(x = model)

  args$conf.int <- options$confInt
  args$conf.level <- options$confLevel / 100
  args$intercept <- options$intercept

  args$add_estimate_to_reference_rows <- options$addRefRowEstimate

  if (options$digitsCoef != "auto") {
    args$estimate_fun <- gtsummary::label_style_number(
      digits = as.integer(options$digitsCoef)
    )
  }

  if (options$digitsPvalue != "auto") {
    args$pvalue_fun <- gtsummary::label_style_pvalue(
      digits = as.integer(options$digitsPvalue)
    )
  }

  do.call(gtsummary::tbl_regression, args)
}


# pipeAddGlobalP ------------------------------------------------------------

#' Add global p-values to a regression table
#'
#' Shared pipeline step. Uses car::Anova() (Type III) to replace individual
#' p-values with global p-values for multi-level factors.
#'
#' @param table A tbl_regression object
#' @param options Jamovi options object
#' @param collector Collector environment from newCollector()
#' @return The table with global p-values (or unchanged)
pipeAddGlobalP <- function(table, options, collector) {
  if (!options$globalP) {
    return(table)
  }

  runSafe(
    gtsummary::add_global_p(table),
    collector
  )
}


# pipeAddVif ----------------------------------------------------------------

#' Add variance inflation factor to a regression table
#'
#' Shared pipeline step. Uses car::vif() to calculate VIF (continuous-only
#' models) or GVIF (models with categorical predictors). Auto-detected by
#' gtsummary.
#'
#' @param table A tbl_regression object
#' @param options Jamovi options object
#' @param collector Collector environment from newCollector()
#' @return The table with VIF column added (or unchanged)
pipeAddVif <- function(table, options, collector) {
  if (!options$addVif) {
    return(table)
  }

  runSafe(gtsummary::add_vif(table), collector)
}


# pipeAddNReg ---------------------------------------------------------------

#' Add observation counts to a regression table
#'
#' Shared pipeline step for regression tables. Adds N at label row,
#' level row, or both.
#'
#' @param table A tbl_regression object
#' @param options Jamovi options object
#' @param collector Collector environment from newCollector()
#' @return The table with N column added (or unchanged)
pipeAddNReg <- function(table, options, collector) {
  if (!options$addN) {
    return(table)
  }

  location <- if (options$addNLocation == "both") {
    c("label", "level")
  } else {
    options$addNLocation
  }

  runSafe(
    gtsummary::add_n(table, location = location),
    collector
  )
}


# pipeAddSignificanceStars --------------------------------------------------

#' Add significance stars to a regression table
#'
#' Shared pipeline step. Appends star annotations to coefficient estimates
#' based on p-value thresholds. Can optionally hide CI, p-value, and SE
#' columns.
#'
#' @param table A tbl_regression object
#' @param options Jamovi options object
#' @param collector Collector environment from newCollector()
#' @return The table with significance stars (or unchanged)
pipeAddSignificanceStars <- function(table, options, collector) {
  if (!options$addStars) {
    return(table)
  }

  runSafe(
    gtsummary::add_significance_stars(
      table,
      hide_ci = !options$starsShowCi,
      hide_p = !options$starsShowP,
      hide_se = !options$starsShowSe
    ),
    collector
  )
}


# pipeAddGlance -------------------------------------------------------------

#' Add model fit statistics to a regression table
#'
#' Shared pipeline step. Appends all model fit statistics returned by
#' broom::glance() either as table rows or as a source note.
#'
#' @param table A tbl_regression object
#' @param options Jamovi options object
#' @param collector Collector environment from newCollector()
#' @return The table with model statistics (or unchanged)
pipeAddGlance <- function(table, options, collector) {
  if (options$modelFit == "none") {
    return(table)
  }

  glanceFn <- if (options$modelFit == "table") {
    gtsummary::add_glance_table
  } else {
    gtsummary::add_glance_source_note
  }

  runSafe(glanceFn(table), collector)
}
