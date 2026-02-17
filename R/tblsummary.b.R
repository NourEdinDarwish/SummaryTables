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
      # Helper to map option to gtsummary string
      getStatString <- function(option, type) {
        if (type == "continuous") {
          switch(
            option,
            "meanSd" = "{mean} ({sd})",
            "medianIqr" = "{median} ({p25}, {p75})",
            "medianRange" = "{median} ({min}, {max})",
            "range" = "{min}, {max}"
          )
        } else if (type == "categorical") {
          switch(
            option,
            "nPercent" = "{n} ({p}%)",
            "n" = "{n}",
            "percent" = "{p}%"
          )
        }
      }

      statisticArguments <- list()

      # Default statistics (skip adding argument if "auto")
      statContDefault <- self$options$statContDefault
      statCatDefault <- self$options$statCatDefault

      defaultStatCont <- getStatString(statContDefault, "continuous")
      if (!is.null(defaultStatCont)) {
        statisticArguments <- c(
          statisticArguments,
          list(gtsummary::all_continuous() ~ defaultStatCont)
        )
      }
      defaultStatCat <- getStatString(statCatDefault, "categorical")
      if (!is.null(defaultStatCat)) {
        statisticArguments <- c(
          statisticArguments,
          list(gtsummary::all_categorical() ~ defaultStatCat)
        )
      }

      # Specific Overrides - Continuous
      for (item in self$options$statsContSpecific) {
        if (item$stat != "use_default" && item$var %in% varsCont) {
          val <- getStatString(item$stat, "continuous")
          if (!is.null(val)) {
            statisticArguments[[item$var]] <- val
          }
        }
      }

      # Specific Overrides - Categorical
      for (item in self$options$statsCatSpecific) {
        if (item$stat != "use_default" && item$var %in% varsCat) {
          val <- getStatString(item$stat, "categorical")
          if (!is.null(val)) {
            statisticArguments[[item$var]] <- val
          }
        }
      }

      return(statisticArguments)
    },

    .constructDigitsArgs = function(varsCont, varsCat) {
      digitsArguments <- list()

      # Get user options (convert string to integer)
      contDigits <- as.integer(self$options$digitsCont)
      pctDigits <- as.integer(self$options$digitsPct)

      # Add continuous digits if continuous variables exist
      if (length(varsCont) > 0 && !is.na(contDigits)) {
        digitsArguments <- c(
          digitsArguments,
          list(gtsummary::all_continuous() ~ contDigits)
        )
      }

      # Add categorical digits if categorical variables exist
      # For {n} ({p}%) format: n always 0 decimals, p uses user selection
      if (length(varsCat) > 0 && !is.na(pctDigits)) {
        digitsArguments <- c(
          digitsArguments,
          list(gtsummary::all_categorical() ~ c(0, pctDigits))
        )
      }

      return(digitsArguments)
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
      # Theme setup - ensure cleanup even on error
      on.exit(resetTheme(), add = TRUE)
      applyTheme(
        journalOption = self$options$journal,
        languageOption = self$options$language,
        compactOption = self$options$compact,
        collector = collector
      )

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
            percent = self$options$percent
          )
        },
        collector
      )

      # Add p-values if requested AND grouping variable is present
      if (!is.null(table) && self$options$pValue && !is.null(byVariable)) {
        testArguments <- private$.constructTestArgs(varsCont, varsCat)

        table <- runSafe(
          {
            # We must pass the list, even if empty (empty list = all auto)
            gtsummary::add_p(table, test = testArguments)
          },
          collector
        )
      }

      table
    },

    .run = function() {
      # 1. Create collector for messages/warnings
      private$.collector <- newCollector()

      # 2. Compute table (triggers theme setup via applyTheme)
      table <- self$table

      # 3. Render Table (if available)
      if (!is.null(table)) {
        renderHtml(table, self$results$tbl, private$.collector)

        # 4. Export (if requested)
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

      # 5. Display Notices
      displayNotices(private$.collector, self$options, self$results)
    }
  )
)
