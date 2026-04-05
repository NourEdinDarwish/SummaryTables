# validateVarNames ----------------------------------------------------------

#' Validate variable names for unsupported special characters
#'
#' Checks covariate and factor names for characters that break R formula
#' parsing or the broom.helpers pipeline. Currently rejects: backslash
#' (which also catches \\n and \\t), backtick, and colon.
#'
#' @param vars Character vector of variable names to validate
validateVarNames <- function(vars) {
  bad <- list(
    "\\" = "backslash (\\)",
    "`"  = "backtick (`)",
    ":"  = "colon (:)"
  )

  for (char in names(bad)) {
    affected <- vars[grepl(char, vars, fixed = TRUE)]
    if (length(affected) > 0) {
      jmvcore::reject(
        jmvcore::format(
          "Variable name '{}' contains a {} character which is not supported in regression analysis. Please rename the variable.", # nolint
          affected[1], bad[[char]]
        )
      )
    }
  }
}


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


# buildMultiRegTable --------------------------------------------------------

#' Build a multivariable tbl_regression table
#'
#' Constructs tbl_regression() arguments from jamovi options and returns the
#' table directly.
#'
#' @param model A fitted model object (lm, glm, etc.)
#' @param options Jamovi options object
#' @return A tbl_regression object
buildMultiRegTable <- function(model, options) {
  args <- list(x = model)

  # Preserve user's UI term order. Steps:
  # 1. composeTerms maps UI arrays to R names (e.g. c("age","age") → "I(age^2)")
  # 2. .clean_backticks strips `` `T Stage` `` → "T Stage"
  # 3. Match user terms to model's own term labels, handling R's
  #    interaction reordering (e.g. user: "Patient Died:Grade"
  #    but model stores "Grade:Patient Died").
  user_terms <- broom.helpers::.clean_backticks(
    jmvcore::composeTerms(options$modelTerms),
    variable_names = names(model$model)
  )
  model_labels <- broom.helpers::.clean_backticks(
    labels(model$terms),
    variable_names = names(model$model)
  )
  args$include <- vapply(user_terms, function(ut) {
    if (ut %in% model_labels) return(ut)
    # Interaction components may be in different order — match by sorted parts
    ut_parts <- sort(strsplit(ut, ":")[[1]])
    for (ml in model_labels) {
      if (!grepl(":", ml, fixed = TRUE)) next
      if (identical(ut_parts, sort(strsplit(ml, ":")[[1]]))) return(ml)
    }
    ut
  }, character(1), USE.NAMES = FALSE)

  args$exponentiate <- optTrue(options$exponentiate)
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


# buildUniRegTable ----------------------------------------------------------

#' Build a univariable tbl_uvregression table
#'
#' Constructs tbl_uvregression() arguments from jamovi options and returns the
#' table directly. Each predictor is regressed individually against the outcome.
#'
#' @param data Data frame
#' @param dep Dependent variable name (string)
#' @param include Character vector of predictor variable names
#' @param method Regression function (e.g. lm, glm)
#' @param method.args Named list of additional arguments passed to method
#'   (e.g. `list(family = binomial)`). Captured via `substitute()` so that
#'   tbl_uvregression's internal NSE works through `do.call()`.
#' @param options Jamovi options object
#' @return A tbl_uvregression object
buildUniRegTable <- function(
  data,
  dep,
  include,
  method,
  method.args = list(),
  options
) {
  args <- list(
    data = data,
    method = method,
    y = jmvcore::composeTerm(dep),
    include = include,
    method.args = substitute(method.args),
    hide_n = TRUE
  )

  args$exponentiate <- optTrue(options$exponentiate)
  args$conf.int <- options$confInt
  args$conf.level <- options$confLevel / 100
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

  do.call(gtsummary::tbl_uvregression, args)
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
  if (!options$globalP || options$addStars ||
      options$journal == "qjecon") {
    return(table)
  }

  runSafe(
    gtsummary::add_global_p(table, keep = options$globalPKeep),
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

  # runtime fix for non-syntactic variable names
  .patchArdCarVif()

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


# pipeAddNEvent -------------------------------------------------------------

#' Add event counts to a regression table
#'
#' Pipeline step for logistic regression tables. Adds event N at label row,
#' level row, or both.
#'
#' @param table A tbl_regression object
#' @param options Jamovi options object
#' @param collector Collector environment from newCollector()
#' @return The table with event N column added (or unchanged)
pipeAddNEvent <- function(table, options, collector) {
  if (!options$addNEvent) {
    return(table)
  }

  location <- if (options$addNEventLocation == "both") {
    c("label", "level")
  } else {
    options$addNEventLocation
  }

  runSafe(
    gtsummary::add_nevent(table, location = location),
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
  if (!options$addStars || options$globalP ||
      options$journal %in% c("jama", "qjecon")) {
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


# pipeCiMergeReg ------------------------------------------------------------

#' Merge coefficient and CI columns in a regression table
#'
#' Combines the estimate and confidence interval columns into a single cell
#' using modify_column_merge(). The CI separator respects the active gtsummary
#' theme (e.g. JAMA uses " to ", default uses ", "). Wrapped in tryCatch so
#' that unexpected column layouts fail silently.
#'
#' @param table A tbl_regression or tbl_uvregression object
#' @param options Jamovi options object (must have ciMerge, confInt)
#' @return The table with merged columns (or unchanged on error)
pipeCiMergeReg <- function(table, options) {
  if (!options$ciMerge || !options$confInt ||
      options$journal %in% c("jama", "qjecon")) {
    return(table)
  }

  tryCatch(
    {
      ciSep <- gtsummary:::get_theme_element(
        "pkgwide-str:ci.sep",
        default = ", "
      )

      hasStars <- "stars" %in% names(table$table_body)

      new_header_text <- paste0(
        table$table_styling$header |>
          dplyr::filter(.data$column == "estimate") |>
          dplyr::pull("label"),
        " **(", 
        gtsummary::style_number(table$inputs$conf.level, scale = 100),
        "% CI)**"
      )

      pattern <- paste0(
        "{estimate} ({conf.low}", ciSep, "{conf.high})",
        if (hasStars) "{stars}" else ""
      )

      table |>
        gtsummary::modify_column_merge(
          rows = !!rlang::expr(
            .data$variable %in% !!table$table_body$variable &
              !is.na(.data$estimate) &
              !.data$reference_row %in% TRUE
          ),
          pattern = pattern
        ) |>
        gtsummary::modify_header(estimate = new_header_text)
    },
    error = function(e) table
  )
}
