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

  testArguments <- list()
  testArgsArguments <- list()

  # Default continuous test
  contDefault <- options$testContDefault
  if (contDefault == "parametric") {
    gtsummary::set_gtsummary_theme(list(
      "add_p.tbl_summary-attr:test.continuous_by2" = "t.test",
      "add_p.tbl_summary-attr:test.continuous" = "oneway.test"
    ))
  } else if (contDefault != "nonparametric") {
    testArguments <- c(
      testArguments,
      list(gtsummary::all_continuous() ~ resolveTestAlias(contDefault))
    )
    if (contDefault %in% c("t.test.equalVar", "oneway.test.equalVar")) {
      testArgsArguments <- c(
        testArgsArguments,
        list(gtsummary::all_continuous() ~ list(var.equal = TRUE))
      )
    }
  }

  # Per-variable continuous overrides
  for (item in options$testContSpecific) {
    if (item$var %in% varsCont && item$test != "useDefault") {
      testArguments[[item$var]] <- resolveTestAlias(item$test)
      if (item$test %in% c("t.test.equalVar", "oneway.test.equalVar")) {
        testArgsArguments[[item$var]] <- list(var.equal = TRUE)
      }
    }
  }

  # Default categorical test
  catDefault <- options$testCatDefault
  if (catDefault != "auto") {
    testArguments <- c(
      testArguments,
      list(gtsummary::all_categorical() ~ catDefault)
    )
  }

  # Per-variable categorical overrides
  for (item in options$testCatSpecific) {
    if (item$var %in% varsCat && item$test != "useDefault") {
      testArguments[[item$var]] <- item$test
    }
  }

  # Call add_p --------------------------------------------------------------
  pvDigits <- options$digitsPvalue
  addPArgs <- list(x = table)
  if (length(testArguments) > 0) {
    addPArgs$test <- testArguments
  }
  if (length(testArgsArguments) > 0) {
    addPArgs$test.args <- testArgsArguments
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

  diffTestArgs <- list()
  diffTestArgsArgs <- list()

  # Continuous methods ------------------------------------------------------
  contDiffDefault <- options$diffContDefault
  diffTestArgs <- c(
    diffTestArgs,
    list(gtsummary::all_continuous() ~ resolveTestAlias(contDiffDefault))
  )
  if (contDiffDefault == "t.test.equalVar") {
    diffTestArgsArgs <- c(
      diffTestArgsArgs,
      list(gtsummary::all_continuous() ~ list(var.equal = TRUE))
    )
  }

  for (item in options$diffContSpecific) {
    if (item$var %in% varsCont && item$method != "useDefault") {
      diffTestArgs[[item$var]] <- resolveTestAlias(item$method)
      if (item$method == "t.test.equalVar") {
        diffTestArgsArgs[[item$var]] <- list(var.equal = TRUE)
      }
    }
  }

  # Categorical methods -----------------------------------------------------
  diffTestArgs <- c(
    diffTestArgs,
    list(gtsummary::all_dichotomous() ~ options$diffDichotDefault)
  )
  for (item in options$diffDichotSpecific) {
    # It is important to use here varsDichot and not varsCat
    # as we keeping it for Dichotomous variables only
    if (item$var %in% varsDichot && item$method != "useDefault") {
      diffTestArgs[[item$var]] <- item$method
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
    test = diffTestArgs,
    conf.level = options$diffConfLevel / 100
  )
  if (length(diffEstFun) > 0) {
    addDiffArgs$estimate_fun <- diffEstFun
  }
  if (length(diffTestArgsArgs) > 0) {
    addDiffArgs$test.args <- diffTestArgsArgs
  }

  diffPvDigits <- options$diffDigitsPvalue
  if (diffPvDigits != "auto") {
    addDiffArgs$pvalue_fun <- gtsummary::label_style_pvalue(
      digits = as.integer(diffPvDigits)
    )
  }

  runSafe(do.call(gtsummary::add_difference, addDiffArgs), collector)
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
  selectedStats <- c(
    options$statContDefault,
    options$statCatDefault,
    vapply(options$statContSpecific, \(x) x$stat, character(1)),
    vapply(options$statCatSpecific, \(x) x$stat, character(1))
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
