# resolveVarChoices ---------------------------------------------------------

#' Resolve default and per-variable choices
#'
#' @param vars Character vector of selected variable names
#' @param default The default choice
#' @param specific Per-variable choices from jamovi options
#' @param field Name of the choice field in each per-variable item
#' @return Named list of final choices for the selected variables
resolveVarChoices <- function(vars, default, specific, field) {
  choices <- stats::setNames(rep(default, length(vars)), vars)
  for (item in specific) {
    if (item$var %in% vars && item[[field]] != "useDefault") {
      choices[[item$var]] <- item[[field]]
    }
  }
  as.list(choices)
}


# resolveTestAlias ----------------------------------------------------------

#' Resolve jamovi test aliases to gtsummary test names
#'
#' Translates friendly aliases used in the jamovi UI into the actual function
#' names that gtsummary expects.
#'
#' @param alias Character: test alias from jamovi options
#' @return Character: the resolved gtsummary test name
resolveTestAlias <- function(alias) {
  switch(
    alias,
    t.test.equalVar = "t.test",
    oneway.test.equalVar = "oneway.test",
    prop.test.no.correct = "prop.test",
    alias
  )
}


# pipeAddP ------------------------------------------------------------------

#' Add p-values to a gtsummary table
#'
#' Shared pipeline step. Builds test + test.args arguments from jamovi options,
#' then calls add_p().
#'
#' @param table A gtsummary table
#' @param varsCont Character vector of continuous variable names
#' @param varsCat Character vector of categorical variable names
#' @param hasGroupVar Logical: TRUE when a grouping variable is present
#' @param options Jamovi options object
#' @param collector Collector environment from newCollector()
#' @return The table with p-values added (or unchanged)
pipeAddP <- function(
  table,
  varsCont,
  varsCat,
  hasGroupVar,
  options,
  collector
) {
  if (!options$addPvalue || !hasGroupVar) {
    return(table)
  }

  # Resolve each variable's final test before building test.args,
  # so a variable-specific test does not inherit arguments intended only for
  # the default test (e.g. var.equal = TRUE when Welch's t-test is selected).

  # Continuous tests --------------------------------------------------------
  contTests <- resolveVarChoices(
    varsCont,
    options$testContDefault,
    options$testContSpecific,
    field = "test"
  )
  if (options$testContDefault == "parametric") {
    gtsummary::set_gtsummary_theme(list(
      "add_p.tbl_summary-attr:test.continuous_by2" = "t.test",
      "add_p.tbl_summary-attr:test.continuous" = "oneway.test"
    ))
  }

  # Categorical tests -------------------------------------------------------
  catTests <- resolveVarChoices(
    varsCat,
    options$testCatDefault,
    options$testCatSpecific,
    field = "test"
  )

  # Test configuration ------------------------------------------------------
  # Keep test.args as a plain named list. Mixing selector formulas with
  # named nested lists can cause gtsummary/cards to drop variable-specific
  # arguments.
  allTests <- c(contTests, catTests)

  # Omit automatic choices so gtsummary selects the tests.
  allTests <- Filter(
    function(test) !test %in% c("parametric", "nonparametric", "auto"),
    allTests
  )
  tests <- lapply(allTests, resolveTestAlias)
  testArgs <- list()
  for (v in names(allTests)) {
    test <- allTests[[v]]
    if (test %in% c("t.test.equalVar", "oneway.test.equalVar")) {
      testArgs[[v]] <- list(var.equal = TRUE)
    }
  }

  # Call add_p --------------------------------------------------------------
  pvDigits <- options$digitsPvalue
  addPArgs <- list(x = table)
  if (length(tests) > 0) {
    addPArgs$test <- tests
  }
  if (length(testArgs) > 0) {
    addPArgs$test.args <- testArgs
  }
  if (pvDigits != "auto") {
    addPArgs$pvalue_fun <- gtsummary::label_style_pvalue(
      digits = as.integer(pvDigits)
    )
  }
  runSafe(do.call(gtsummary::add_p, addPArgs), collector)
}


# pipeAddQ ------------------------------------------------------------------

#' Add q-values (adjusted p-values) to a gtsummary table
#'
#' Shared pipeline step. Calls add_q() to adjust p-values for multiple
#' comparisons. Requires that a p.value column already exists in the table
#' (from add_p(), add_difference(), or tbl_regression()).
#'
#' @param table A gtsummary table
#' @param hasPvalue Logical: TRUE when the table has a p.value column
#' @param options Jamovi options object
#' @param collector Collector environment from newCollector()
#' @return The table with q-values added (or unchanged)
pipeAddQ <- function(table, hasPvalue, options, collector) {
  if (!optTrue(options$addQ) || !hasPvalue) {
    return(table)
  }

  runSafe(
    gtsummary::add_q(table, method = options$qMethod),
    collector
  )
}


# pipeAddDifference ---------------------------------------------------------

#' Add difference statistics to a gtsummary table
#'
#' Builds test, estimate_fun, test.args, and pvalue_fun arguments from jamovi
#' options, then calls add_difference().
#'
#' @param table A gtsummary table
#' @param varsCont Character vector of continuous variable names
#' @param varsDichot Character vector of dichotomous variable names
#' @param hasGroupVar Logical: TRUE when a grouping variable is present
#' @param options Jamovi options object
#' @param collector Collector environment from newCollector()
#' @return The table with difference estimates added (or unchanged)
pipeAddDifference <- function(
  table,
  varsCont,
  varsDichot,
  hasGroupVar,
  options,
  collector
) {
  if (!options$addDifference || !hasGroupVar) {
    return(table)
  }

  # Resolve each variable's final method before building test.args,
  # so a variable-specific method does not inherit arguments intended only for
  # the default method (e.g. var.equal = TRUE when Welch's t-test is selected).

  # Continuous methods ------------------------------------------------------
  contMethods <- resolveVarChoices(
    varsCont,
    options$diffContDefault,
    options$diffContSpecific,
    field = "method"
  )

  # Dichotomous methods -----------------------------------------------------
  dichotMethods <- resolveVarChoices(
    varsDichot,
    options$diffDichotDefault,
    options$diffDichotSpecific,
    field = "method"
  )

  # Test configuration ------------------------------------------------------
  # Keep test.args as a plain named list. Mixing selector formulas with
  # named nested lists can cause gtsummary/cards to drop variable-specific
  # arguments.
  allMethods <- c(contMethods, dichotMethods)
  diffTests <- lapply(allMethods, resolveTestAlias)
  diffTestArgs <- list()
  for (v in names(allMethods)) {
    method <- allMethods[[v]]
    if (method == "t.test.equalVar") {
      diffTestArgs[[v]] <- list(var.equal = TRUE)
    } else if (method == "prop.test.no.correct") {
      diffTestArgs[[v]] <- list(correct = FALSE)
    }
  }

  # Estimate rounding -------------------------------------------------------
  diffEstFun <- list()

  if (options$diffDigitsCont != "auto") {
    diffEstFun <- c(
      diffEstFun,
      list(
        gtsummary::all_continuous() ~ gtsummary::label_style_number(
          digits = as.integer(options$diffDigitsCont)
        )
      )
    )
  }

  if (options$diffDigitsDichot != "auto") {
    d <- as.integer(options$diffDigitsDichot)
    for (v in varsDichot) {
      if (options$diffDichotDefault == "smd") {
        # SMD is not a proportion — plain digits
        diffEstFun[[v]] <- gtsummary::label_style_number(digits = d)
      } else {
        # prop.test — difference is a proportion, show as %
        diffEstFun[[v]] <- gtsummary::label_style_number(
          digits = d,
          scale = 100,
          suffix = "%"
        )
      }
    }
  }

  if (options$diffDigitsCat != "auto") {
    diffEstFun <- c(
      diffEstFun,
      list(
        gtsummary::all_categorical(FALSE) ~ gtsummary::label_style_number(
          digits = as.integer(options$diffDigitsCat)
        )
      )
    )
  }

  # Call add_difference -----------------------------------------------------
  addDiffArgs <- list(
    x = table,
    test = diffTests,
    conf.level = options$diffConfLevel / 100
  )
  if (length(diffEstFun) > 0) {
    addDiffArgs$estimate_fun <- diffEstFun
  }
  if (length(diffTestArgs) > 0) {
    addDiffArgs$test.args <- diffTestArgs
  }

  diffPvDigits <- options$diffDigitsPvalue
  if (diffPvDigits != "auto") {
    addDiffArgs$pvalue_fun <- gtsummary::label_style_pvalue(
      digits = as.integer(diffPvDigits)
    )
  }

  runSafe(do.call(gtsummary::add_difference, addDiffArgs), collector)
}


# pipeCiMergeDiff -----------------------------------------------------------

#' Merge estimate and CI columns for add_difference tables
#'
#' Follows the same pattern as gtsummary's JAMA theme
#' (`add_difference-fn:addnl-fn-to-run`). Reads the CI header from the
#' conf.low column label and the separator from the active theme.
#'
#' @param table A gtsummary table with add_difference columns
#' @param hasGroupVar Logical: TRUE when a grouping variable is present
#' @param options Jamovi options object (must have diffCiMerge, addDifference)
#' @return The table with merged columns (or unchanged on error)
pipeCiMergeDiff <- function(table, hasGroupVar, options) {
  if (
    !options$diffCiMerge ||
      !options$addDifference ||
      !hasGroupVar ||
      options$journal == "jama"
  ) {
    return(table)
  }

  tryCatch(
    {
      ciSep <- gtsummary:::get_theme_element(
        "pkgwide-str:ci.sep",
        default = ", "
      )

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

      table |>
        gtsummary::modify_column_merge(
          rows = !!rlang::expr(
            .data$variable %in%
              !!table$table_body$variable &
              !is.na(.data$estimate)
          ),
          pattern = paste0("{estimate} ({conf.low}", ciSep, "{conf.high})")
        ) |>
        gtsummary::modify_header(estimate = new_header_text)
    },
    error = function(e) table
  )
}


# pipeAddCi -----------------------------------------------------------------

#' Add confidence intervals to a gtsummary table
#'
#' Shared pipeline step. Builds method, statistic, style_fun, and pattern
#' arguments from jamovi options, then calls add_ci().
#'
#' @param table A gtsummary table
#' @param varsCont Character vector of continuous variable names
#' @param varsCat Character vector of categorical variable names
#' @param options Jamovi options object
#' @param themeStrings Theme strings from getThemeStrings()
#' @param collector Collector environment from newCollector()
#' @return The table with confidence intervals added (or unchanged)
pipeAddCi <- function(
  table,
  varsCont,
  varsCat,
  options,
  themeStrings,
  collector
) {
  if (!options$addCi) {
    return(table)
  }

  # Methods -----------------------------------------------------------------
  ciMethodArgs <- list(
    gtsummary::all_continuous() ~ options$ciContDefault
  )
  for (item in options$ciContSpecific) {
    if (item$method != "useDefault" && item$var %in% varsCont) {
      ciMethodArgs[[item$var]] <- item$method
    }
  }
  ciMethodArgs <- c(
    ciMethodArgs,
    list(gtsummary::all_categorical() ~ options$ciCatDefault)
  )
  for (item in options$ciCatSpecific) {
    if (item$method != "useDefault" && item$var %in% varsCat) {
      ciMethodArgs[[item$var]] <- item$method
    }
  }

  # Pattern -----------------------------------------------------------------
  statsWithParens <- c("meanSd", "medianIqr", "medianRange", "nPercent", "eda")

  effectiveContStats <- stats::setNames(
    rep(options$statContDefault, length(varsCont)),
    varsCont
  )
  for (item in options$statContSpecific) {
    if (item$stat != "useDefault") {
      effectiveContStats[[item$var]] <- item$stat
    }
  }

  effectiveCatStats <- stats::setNames(
    rep(options$statCatDefault, length(varsCat)),
    varsCat
  )
  for (item in options$statCatSpecific) {
    if (item$stat != "useDefault") {
      effectiveCatStats[[item$var]] <- item$stat
    }
  }

  selectedStats <- c(
    effectiveContStats,
    effectiveCatStats
  )
  useSquareBrackets <- any(selectedStats %in% statsWithParens)

  ciPattern <- if (options$ciMerge) {
    if (useSquareBrackets) "{stat} [{ci}]" else "{stat} ({ci})"
  }

  # Statistic format --------------------------------------------------------
  ciStatArg <- list(
    gtsummary::all_continuous() ~ paste0(
      "{conf.low}",
      themeStrings$ciSep,
      "{conf.high}"
    ),
    gtsummary::all_categorical() ~ paste0(
      "{conf.low}",
      themeStrings$ciPctSuffix,
      themeStrings$ciSep,
      "{conf.high}",
      themeStrings$ciPctSuffix
    )
  )

  # Style overrides ---------------------------------------------------------
  ciStyleFun <- list()
  if (options$ciDigitsCont != "auto") {
    ciStyleFun <- c(
      ciStyleFun,
      list(
        gtsummary::all_continuous() ~ gtsummary::label_style_number(
          digits = as.integer(options$ciDigitsCont)
        )
      )
    )
  }
  if (options$ciDigitsCat != "auto") {
    ciStyleFun <- c(
      ciStyleFun,
      list(
        gtsummary::all_categorical() ~ gtsummary::label_style_number(
          digits = as.integer(options$ciDigitsCat),
          scale = 100
        )
      )
    )
  }

  # Call add_ci -------------------------------------------------------------
  args <- list(
    x = table,
    method = ciMethodArgs,
    statistic = ciStatArg,
    conf.level = options$confLevel / 100,
    pattern = ciPattern
  )
  if (length(ciStyleFun) > 0) {
    args$style_fun <- ciStyleFun
  }

  runSafe(do.call(gtsummary::add_ci, args), collector)
}


# pipeAddN ------------------------------------------------------------------

#' Add observation counts to a gtsummary table
#'
#' @param table A gtsummary table
#' @param options Jamovi options object
#' @param collector Collector environment from newCollector()
#' @return The table with N column added (or unchanged)
pipeAddN <- function(table, options, collector) {
  if (!options$addN) {
    return(table)
  }

  runSafe(
    gtsummary::add_n(
      table,
      last = options$addNLast,
      footnote = options$addNFootnote
    ),
    collector
  )
}


# pipeAddOverall ------------------------------------------------------------

#' Add overall column to a stratified gtsummary table
#'
#' @param table A gtsummary table
#' @param hasGroupVar Logical: TRUE when a grouping variable is present
#' @param options Jamovi options object
#' @param collector Collector environment from newCollector()
#' @return The table with overall column added (or unchanged)
pipeAddOverall <- function(table, hasGroupVar, options, collector) {
  if (!options$addOverall || !hasGroupVar) {
    return(table)
  }

  runSafe(
    gtsummary::add_overall(
      table,
      last = options$addOverallLast
    ),
    collector
  )
}
