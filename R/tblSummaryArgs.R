# buildTypeArgs -------------------------------------------------------------

#' Build type arguments for tbl_summary
#'
#' Constructs the `type` and `value` (dichotomous) argument lists for
#' gtsummary::tbl_summary(). Handles the default continuous type
#' (continuous vs continuous2 for EDA), per-variable overrides, and
#' automatic dichotomous detection for 2-level categorical variables
#' when add_difference is enabled.
#'
#' @param data The data frame
#' @param varsCont Character vector of continuous variable names
#' @param varsCat Character vector of categorical variable names
#' @param groupVar The grouping variable name (or NULL)
#' @param hasGroupVar Logical: TRUE when a grouping variable is present
#' @param options Jamovi options object
#' @return Named list with `type` and `dichotValue` elements
buildTypeArgs <- function(
  data,
  varsCont,
  varsCat,
  groupVar,
  hasGroupVar,
  options
) {
  defaultType <- if (options$statContDefault == "eda") {
    "continuous2"
  } else {
    "continuous"
  }
  typeArgsBuilder <- stats::setNames(
    rep(defaultType, length(varsCont)),
    varsCont
  )

  for (item in options$statContSpecific) {
    if (item$var %in% varsCont && item$stat != "useDefault") {
      typeArgsBuilder[[item$var]] <- if (item$stat == "eda") {
        "continuous2"
      } else {
        "continuous"
      }
    }
  }

  typeArguments <- as.list(c(
    typeArgsBuilder,
    stats::setNames(rep("categorical", length(varsCat)), varsCat)
  ))

  # Override categorical -> dichotomous for 2-level vars
  dichotValueArgs <- list()
  if (options$addDifference && hasGroupVar && length(varsCat) > 0) {
    for (v in varsCat) {
      if (v == groupVar) {
        next
      }
      lvls <- levels(data[[v]])
      if (length(lvls) == 2) {
        typeArguments[[v]] <- "dichotomous"
        dichotValueArgs[[v]] <- lvls[2]
      }
    }
  }

  list(type = typeArguments, dichotValue = dichotValueArgs)
}


# buildStatArgs -------------------------------------------------------------

#' Build statistic arguments for tbl_summary
#'
#' Constructs the `statistic` argument list for gtsummary::tbl_summary().
#' Maps jamovi option names to glue format strings, then applies
#' per-variable overrides.
#'
#' @param varsCont Character vector of continuous variable names
#' @param varsCat Character vector of categorical variable names
#' @param options Jamovi options object
#' @param themeStrings Theme strings from getThemeStrings()
#' @return A formula-list suitable for the `statistic` argument
buildStatArgs <- function(varsCont, varsCat, options, themeStrings) {
  statStrings <- list(
    continuous = list(
      meanSd = "{mean} ({sd})",
      medianIqr = paste0("{median} ({p25}", themeStrings$iqrSep, "{p75})"),
      medianRange = paste0("{median} ({min}", themeStrings$rangeSep, "{max})"),
      eda = c(
        paste0("{median} ({p25}", themeStrings$iqrSep, "{p75})"),
        "{mean} ({sd})",
        paste0("{min}", themeStrings$rangeSep, "{max}")
      ),
      mean = "{mean}",
      median = "{median}"
    ),
    categorical = list(
      nPercent = paste0("{n} ({p}", themeStrings$pctSuffix, ")"),
      n = "{n}",
      percent = paste0("{p}", themeStrings$pctSuffix)
    )
  )

  # Default continuous and categorical statistics
  defaultContinuousStat <- statStrings$continuous[[options$statContDefault]]
  defaultCategoricalStat <- statStrings$categorical[[options$statCatDefault]]

  statisticArguments <- list(
    gtsummary::all_continuous() ~ defaultContinuousStat,
    gtsummary::all_categorical() ~ defaultCategoricalStat
  )

  # Per-variable continuous overrides
  for (item in options$statContSpecific) {
    if (item$stat != "useDefault" && item$var %in% varsCont) {
      val <- statStrings$continuous[[item$stat]]
      statisticArguments[[item$var]] <- val
    }
  }

  # Per-variable categorical overrides
  for (item in options$statCatSpecific) {
    if (item$stat != "useDefault" && item$var %in% varsCat) {
      val <- statStrings$categorical[[item$stat]]
      statisticArguments[[item$var]] <- val
    }
  }

  statisticArguments
}


# buildDigitsArgs -----------------------------------------------------------

#' Build digits arguments for tbl_summary
#'
#' @param options Jamovi options object
#' @return A formula-list suitable for the `digits` argument
buildDigitsArgs <- function(options) {
  digitsArguments <- list()

  if (options$digitsCont != "auto") {
    digitsArguments <- c(
      digitsArguments,
      list(
        gtsummary::all_continuous() ~ as.integer(options$digitsCont)
      )
    )
  }
  if (options$digitsCatPct != "auto") {
    digitsArguments <- c(
      digitsArguments,
      list(
        gtsummary::all_categorical() ~ c(
          0L,
          as.integer(options$digitsCatPct)
        )
      )
    )
  }

  digitsArguments
}