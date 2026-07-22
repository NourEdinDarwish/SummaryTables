# validateFactorPolynomials -------------------------------------------------

#' Validate that polynomial terms are not applied to factors
#'
#' @param data Data frame containing the variables
#' @param terms modelTerms list from self$options$modelTerms
#' @return NULL (stops with an error if validation fails)
validateFactorPolynomials <- function(data, terms) {
  for (term in terms) {
    counts <- table(term)
    polys <- names(counts)[counts > 1]

    if (length(polys) > 0) {
      for (poly_var in polys) {
        if (is.factor(data[[poly_var]])) {
          stop(sprintf(
            "Polynomial terms cannot be applied to Factors. Please remove the polynomial term for '%s'.", # nolint
            poly_var
          ))
        }
      }
    }
  }
}


# buildFormula --------------------------------------------------------------

#' Build a model formula from a pre-formatted LHS and jamovi model terms
#'
#' Shared helper for all regression types (linear, logistic, Cox, etc).
#' The caller is responsible for formatting the left-hand side, e.g.
#' `jmvcore::composeTerm(dep)` for standard regression or
#' `sprintf("survival::Surv(%s, %s)", ...)` for Cox regression.
#'
#' @param y Pre-formatted left-hand side string
#' @param terms modelTerms list from self$options$modelTerms
#' @return A formula object
buildFormula <- function(y, terms) {
  stats::as.formula(
    paste(
      y,
      "~",
      paste(jmvcore::composeTerms(terms), collapse = " + ")
    )
  )
}


# buildMultiRegTable --------------------------------------------------------

#' Build a multivariable tbl_regression table
#'
#' Constructs tbl_regression() arguments from jamovi options and returns the
#' table directly. Expects a model fitted with B64-encoded column names;
#' labels on the data columns drive the display names in the table.
#'
#' @param model A fitted model object (lm, glm, etc.)
#' @param options Jamovi options object
#' @param b64Map Named character vector from `buildB64Map()` (B64 → original)
#' @return A tbl_regression object
buildMultiRegTable <- function(model, options, b64Map) {
  args <- list(x = model)

  # Main Goal: Preserve the user's UI term row order in the gtsummary table. By
  # default, R's lm/glm forces all interaction terms to the very bottom of the
  # output table, ignoring the order the user specified them in the jamovi UI.
  # To override this, we explicitly define the `include` argument for gtsummary
  # using the exact order from the UI (`user_terms`).
  #
  # Technical Hurdle: R reorders the variables *inside* an interaction term
  # based on the order the main effects first appeared in the formula (e.g., UI
  # sends "Age:BMI", but R might store it as "BMI:Age"). If we pass the exact UI
  # spelling, gtsummary crashes because it can't find it. Therefore, we loop
  # through the UI terms and match them against the model's actual terms. If an
  # exact match fails, we split by ":" and sort the pieces to find the reordered
  # equivalent, guaranteeing gtsummary finds the correct term and prints the row
  # in the user's requested order.
  termsB64 <- lapply(options$modelTerms, jmvcore::toB64)
  user_terms <- jmvcore::composeTerms(termsB64)
  model_terms <- labels(model$terms)

  args$include <- vapply(
    user_terms,
    function(ut) {
      if (ut %in% model_terms) {
        return(ut)
      }
      # Interaction components may be in different order — match by sorted parts
      ut_parts <- sort(strsplit(ut, ":")[[1]])
      for (mt in model_terms) {
        if (!grepl(":", mt, fixed = TRUE)) {
          next
        }
        if (identical(ut_parts, sort(strsplit(mt, ":")[[1]]))) return(mt)
      }
      ut
    },
    character(1),
    USE.NAMES = FALSE
  )

  args$exponentiate <- optTrue(options$exponentiate)
  args$conf.int <- options$confInt && options$journal != "qjecon"
  args$conf.level <- options$confLevel / 100
  args$intercept <- optTrue(options$intercept)

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

  tbl <- do.call(gtsummary::tbl_regression, args)

  # Fix I(b64^N) polynomial labels → "Original Name²" etc.
  tbl <- fixPolynomialLabels(tbl, b64Map)

  tbl
}


# lmStd ---------------------------------------------------------------------

#' Fit standard linear regression model on standardized variables
#'
#' @param formula Model formula
#' @param data Data frame
#' @param ... Extra arguments passed to lm
#' @return An lm model object fitted on standardized complete cases
lmStd <- function(formula, data, ...) {
  # Goal: produce the same standardized coefficients (betas) as
  # parameters::standardize_parameters(method = "refit").
  #
  # How standardize_parameters(method = "refit") works internally:
  #   1. Extracts the model frame via insight::get_data(model, source = "mf"),
  #      which is the data AFTER listwise deletion — rows with NA in ANY
  #      variable used by the model (numeric OR factor) are already gone.

  #   2. Standardizes that complete-case data with datawizard::standardize()
  #      (factors are left untouched by default since force = FALSE).
  #   3. Refits the model on the standardized complete-case data.
  #
  # Previously we used:
  #   datawizard::standardize(data, select = select_vars, remove_na = "selected")
  # This had a bug: with force = FALSE (default), datawizard internally
  # excludes factor columns from the "selected" set, so remove_na = "selected"
  # only checked numeric columns for NAs. Rows with NA in a factor (but not
  # in any numeric column) were KEPT, meaning mean/SD was computed from MORE
  # rows than lm() actually uses (lm() does its own listwise deletion).
  # This produced different standardized coefficients than
  # parameters::standardize_parameters(method = "refit").
  #
  # We also cannot use remove_na = "all" on the full data frame, because
  # "all" checks EVERY column — including ones not in the formula. NAs in
  # unrelated columns would incorrectly drop rows.
  #
  # Fix: subset to formula columns first (data[select_vars]), then use
  # remove_na = "all". This performs listwise deletion on exactly the
  # variables the model uses, matching what lm() and
  # parameters::standardize_parameters(method = "refit") do.
  select_vars <- all.vars(formula)
  data <- datawizard::standardize(
    data[select_vars],
    remove_na = "all"
  )
  stats::lm(formula, data = data, ...)
}


# buildUniRegTable ----------------------------------------------------------

#' Build a univariable tbl_uvregression table
#'
#' Constructs tbl_uvregression() arguments from jamovi options and returns the
#' table directly. Each predictor is regressed individually against the outcome.
#'
#' @param data Data frame
#' @param y Pre-formatted outcome (string or call). For standard regression
#'   pass a string from `composeTerm(dep)` (e.g. `"age"` or `` "`T Stage`" ``).
#'   For Cox regression pass a call object from
#'   `str2lang("survival::Surv(time, event)")`.
#' @param include Character vector of predictor variable names
#' @param method Regression function (e.g. lm, glm, survival::coxph)
#' @param method.args Named list of additional arguments passed to method
#'   (e.g. `list(family = binomial)`). Captured via `substitute()` so that
#'   tbl_uvregression's internal NSE works through `do.call()`.
#' @param options Jamovi options object
#' @return A tbl_uvregression object
buildUniRegTable <- function(
  data,
  y,
  include,
  method,
  method.args = list(),
  options
) {
  args <- list(
    data = data,
    method = method,
    y = y,
    include = include,
    method.args = substitute(method.args),
    hide_n = TRUE
  )

  args$exponentiate <- optTrue(options$exponentiate)
  args$conf.int <- options$confInt && options$journal != "qjecon"
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


# pipeOverrideEstimateHeader ------------------------------------------------

#' Safely override the estimate column header in a regression table
#'
#' Replaces the base estimate header (e.g. "**Beta**") with a custom label while
#' preserving any suffixes added by themes (like " (95% CI)" from JAMA).
#'
#' @param table A tbl_regression or tbl_uvregression object
#' @param options Jamovi options object
#' @return The table with the updated header
pipeOverrideEstimateHeader <- function(table, options) {
  coefHeader <- if (options$standardize) {
    "**Standardized Coefficient**"
  } else {
    "**Coefficient**"
  }

  current_header <- table$table_styling$header$label[
    table$table_styling$header$column == "estimate"
  ]

  new_header <- sub("^\\*\\*.*?\\*\\*", coefHeader, current_header)
  table |>
    gtsummary::modify_header(estimate = new_header)
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
  if (!options$globalP || options$addStars || options$journal == "qjecon") {
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
  if (!options$addVif || length(options$modelTerms) < 2) {
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
#' Shared pipeline step. Warns when exponentiated coefficients are shown with
#' SE, including under the QJE theme, then appends star annotations based on
#' p-value thresholds. Can optionally hide CI, p-value, and SE columns.
#'
#' @param table A tbl_regression object
#' @param options Jamovi options object
#' @param collector Collector environment from newCollector()
#' @param ratioName Optional character value: `"odds"` or `"hazard"`.
#'   Used to warn when exponentiated coefficients are shown with standard
#'   errors from the unexponentiated model.
#' @return The table with significance stars (or unchanged)
pipeAddSignificanceStars <- function(
  table,
  options,
  collector,
  ratioName = NULL
) {
  showsSe <- options$journal == "qjecon" ||
    (options$addStars &&
      options$starsShowSe &&
      !options$globalP &&
      options$journal != "jama")

  if (showsSe && optTrue(options$exponentiate)) {
    runSafe(
      warning(
        paste(
          "Standard errors (SE) correspond to the unexponentiated",
          "coefficients, not the displayed",
          ratioName,
          "ratios."
        ),
        call. = FALSE
      ),
      collector
    )
  }

  if (
    !options$addStars ||
      options$globalP ||
      options$journal %in% c("jama", "qjecon")
  ) {
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
  if (
    !options$ciMerge ||
      !options$confInt ||
      options$journal %in% c("jama", "qjecon")
  ) {
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
        " **(**",
        table$table_styling$header |>
          dplyr::filter(.data$column == "conf.low") |>
          dplyr::pull("label"),
        "**)**"
      )

      pattern <- paste0(
        "{estimate} ({conf.low}",
        ciSep,
        "{conf.high})",
        if (hasStars) "{stars}" else ""
      )

      table |>
        gtsummary::modify_column_merge(
          rows = !!rlang::expr(
            .data$variable %in%
              !!table$table_body$variable &
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
