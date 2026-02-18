tblSummaryClass <- R6::R6Class(
  "tblSummaryClass",
  inherit = tblSummaryBase,
  active = list(
    table = function() {
      if (is.null(private$.table)) {
        private$.table <- private$.computeTable(private$.collector)
      }
      private$.table
    }
  ),
  private = list(
    .table = NULL,
    .collector = NULL,

    .constructStatArgs = function(varsCont, varsCat) {
      journal <- self$options$journal

      # Theme-aware style decisions
      iqrSep    <- if (journal %in% c("jama", "nejm", "lancet")) " \U2013 " else ", "
      rangeSep  <- if (journal %in% c("jama", "nejm", "lancet")) " \U2013 " else ", "
      pctSuffix <- if (journal %in% c("jama", "nejm"))           ""        else "%"

      # Lookup tables: option name \U2192 glue format string
      statStrings <- list(
        continuous = c(
          meanSd      = "{mean} ({sd})",
          medianIqr   = paste0("{median} ({p25}", iqrSep, "{p75})"),
          medianRange = paste0("{median} ({min}", rangeSep, "{max})")
        ),
        categorical = c(
          nPercent = paste0("{n} ({p}", pctSuffix, ")"),
          n        = "{n}",
          percent  = paste0("{p}", pctSuffix)
        )
      )

      args <- list()

      # Default continuous
      contStr <- statStrings$continuous[self$options$statContDefault]
      if (!is.na(contStr)) {
        args <- c(args, list(gtsummary::all_continuous() ~ contStr))
      }

      # Default categorical
      catStr <- statStrings$categorical[self$options$statCatDefault]
      if (!is.na(catStr)) {
        args <- c(args, list(gtsummary::all_categorical() ~ catStr))
      }

      # Per-variable overrides \U2014 Continuous
      for (item in self$options$statsContSpecific) {
        if (item$stat != "use_default" && item$var %in% varsCont) {
          val <- statStrings$continuous[item$stat]
          if (!is.na(val)) args[[item$var]] <- val
        }
      }

      # Per-variable overrides \U2014 Categorical
      for (item in self$options$statsCatSpecific) {
        if (item$stat != "use_default" && item$var %in% varsCat) {
          val <- statStrings$categorical[item$stat]
          if (!is.na(val)) args[[item$var]] <- val
        }
      }

      args
    },

    .constructDigitsArgs = function(varsCont, varsCat) {
      contDigits <- self$options$digitsCont
      pctDigits  <- self$options$digitsPct

      args <- list()

      # Continuous: only pass when user explicitly chose (not "auto")
      # When "auto": each variable gets its own spread-based decimal count
      if (contDigits != "auto") {
        args <- c(args, list(gtsummary::all_continuous() ~ as.integer(contDigits)))
      }

      # Categorical: only pass when user explicitly chose (not "auto")
      # When "auto": theme's percent_fun controls (0 dec default, 1 dec for QJEcon)
      if (pctDigits != "auto") {
        args <- c(args, list(gtsummary::all_categorical() ~ c(0L, as.integer(pctDigits))))
      }

      args
    },

    .constructSortArgs = function(varsCat) {
      args <- list()

      # Default sort for all categorical
      sortDefault <- self$options$sortCatDefault
      args <- c(args, list(gtsummary::all_categorical(FALSE) ~ sortDefault))

      # Per-variable overrides
      for (item in self$options$sortCatSpecific) {
        if (item$sort != "use_default" && item$var %in% varsCat) {
          args[[item$var]] <- item$sort
        }
      }

      args
    },

    .constructTestArgs = function(varsCont, varsCat) {
      testArguments <- list()

      # Helper to resolve nonparametric/parametric shortcuts based on group count
      resolveTestShortcut <- function(testName, nGroups) {
        if (testName == "nonparametric") {
          if (nGroups == 2) {
            return("wilcox.test")
          }
          if (nGroups >= 3) return("kruskal.test")
        }
        if (testName == "parametric") {
          if (nGroups == 2) {
            return("t.test")
          }
          if (nGroups >= 3) return("oneway.test")
        }
        return(testName) # Return unchanged if not a shortcut
      }

      # Determine number of levels in groupBy variable
      groupBy <- self$options$groupBy
      nGroups <- 0
      if (!is.null(groupBy) && groupBy %in% names(self$data)) {
        nGroups <- length(unique(self$data[[groupBy]]))
      }

      # Helper to resolve test selection
      resolveTest <- function(specificChoice, defaultChoice) {
        if (specificChoice == "use_default") {
          return(defaultChoice)
        }
        return(specificChoice)
      }

      # 1. Continuous Variables
      for (item in self$options$testsContSpecific) {
        if (item$var %in% varsCont) {
          selection <- resolveTest(item$test, self$options$testContDefault)
          if (selection != "auto") {
            resolvedTest <- resolveTestShortcut(selection, nGroups)
            testArguments[[item$var]] <- resolvedTest
          }
        }
      }

      # 2. Categorical Variables
      for (item in self$options$testsCatSpecific) {
        if (item$var %in% varsCat) {
          selection <- resolveTest(item$test, self$options$testCatDefault)
          if (selection != "auto") {
            resolvedTest <- resolveTestShortcut(selection, nGroups)
            testArguments[[item$var]] <- resolvedTest
          }
        }
      }

      return(testArguments)
    },

    .computeTable = function(collector) {

      # 1. Validation & Setup
      varsCont <- self$options$varsCont
      varsCat <- self$options$varsCat

      # Check if any variables are selected
      if (length(varsCont) == 0 && length(varsCat) == 0) {
        return(NULL)
      }

      # Explicitly select data to ensure clean environment
      data <- self$data

      # 2. Argument Mapping & Data Preparation
      # Combine variables (Categorical first, then Continuous)
      allVars <- c(varsCat, varsCont)

      # Determine Types
      typeArguments <- list()
      # Determine continuous type based on summaryType option
      contType <- "continuous"
      if (length(varsCont) > 0) {
        for (v in varsCont) {
          typeArguments[[v]] <- contType
        }
      }
      if (length(varsCat) > 0) {
        for (v in varsCat) {
          typeArguments[[v]] <- "categorical"
        }
      }

      # Statistic Arguments (delegated)
      statisticArguments <- private$.constructStatArgs(varsCont, varsCat)

      # Digits Arguments (for rounding control)
      digitsArguments <- private$.constructDigitsArgs(varsCont, varsCat)

      # Sort Arguments (for category ordering)
      sortArguments <- private$.constructSortArgs(varsCat)

      # Map 'by' option
      byVariable <- self$options$groupBy
      if (is.null(byVariable)) {
        byVariable <- NULL
      }

      # 3. Execution (gtsummary)
      # Core analysis
      table <- runSafe(
        {
          gtsummary::tbl_summary(
            data = data,
            include = allVars,
            by = byVariable,
            type = typeArguments,
            statistic = statisticArguments,
            digits = digitsArguments,
            missing = self$options$missing,
            missing_text = self$options$missingText,
            percent = self$options$percent,
            sort = sortArguments
          )
        },
        collector
      )

      # Add p-values if requested AND grouping variable is present
      if (!is.null(table) && self$options$pValue && !is.null(byVariable)) {
        testArguments <- private$.constructTestArgs(varsCont, varsCat)

        # Only pass pvalue_fun when user explicitly chose (not "auto")
        # When "auto": theme's pkgwide-fn:pvalue_fun controls
        pvDigits <- self$options$digitsPvalue

        table <- runSafe(
          {
            if (pvDigits != "auto") {
              gtsummary::add_p(
                table,
                test = testArguments,
                pvalue_fun = gtsummary::label_style_pvalue(
                  digits = as.integer(pvDigits)
                )
              )
            } else {
              gtsummary::add_p(table, test = testArguments)
            }
          },
          collector
        )
      }

      # ── Text formatting ──
      if (!is.null(table)) {
        if (isTRUE(self$options$boldLabels))
          table <- gtsummary::bold_labels(table)
        if (isTRUE(self$options$boldLevels))
          table <- gtsummary::bold_levels(table)
        if (isTRUE(self$options$italicizeLabels))
          table <- gtsummary::italicize_labels(table)
        if (isTRUE(self$options$italicizeLevels))
          table <- gtsummary::italicize_levels(table)

        # Bold significant p-values (only when p-values are shown)
        if (isTRUE(self$options$boldP) &&
            self$options$pValue &&
            !is.null(self$options$groupBy)) {
          table <- gtsummary::bold_p(
            table, t = self$options$boldPThreshold
          )
        }

        # Separate p-value footnotes (one per test, instead of combined)
        if (isTRUE(self$options$separatePFootnotes) &&
            self$options$pValue &&
            !is.null(self$options$groupBy)) {
          table <- gtsummary::separate_p_footnotes(table)
        }
      }

      table
    },

    .run = function() {
      # 1. Create collector for messages/warnings
      private$.collector <- newCollector()

      # 2. Theme setup - stays active through render + export
      on.exit(resetTheme(), add = TRUE)
      applyTheme(
        journalOption = self$options$journal,
        languageOption = self$options$language,
        compactOption = self$options$compact,
        collector = private$.collector
      )

      # 3. Compute table
      table <- self$table

      # 4. Render Table (if available)
      if (!is.null(table)) {
        renderHtml(table, self$results$tbl, private$.collector)

        # 5. Export (if requested)
        if (isTRUE(self$options$export)) {
          path <- resolveExportPath(self$options$path)
          if (!is.null(path)) {
            exportDocx(
              table,
              path,
              self$options,
              self$results,
              private$.collector
            )
          }
        }
      }

      # 6. Display Notices
      displayNotices(private$.collector, self$options, self$results)
    }
  )
)
