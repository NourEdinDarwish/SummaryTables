tblSummaryClass <- R6::R6Class(
  "tblSummaryClass",
  inherit = tblSummaryBase,
  active = list(
    table = function() {
      if (is.null(private$.table)) {
        collector <- newCollector()
        on.exit(resetTheme(), add = TRUE)
        private$.table <- private$.computeTable(collector)
      }
      private$.table
    }
  ),
  private = list(
    .table = NULL,

    .constructStatArgs = function(varsCont, varsCat) {
      # Helper to map option to gtsummary string
      getStatString <- function(option, type) {
        if (type == "continuous") {
          if (option == "mean_sd") {
            return("{mean} ({sd})")
          }
          if (option == "median_iqr") {
            return("{median} ({p25}, {p75})")
          }
          if (option == "n_percent") {
            return("{n} ({p}%)")
          }
          if (option == "range") return("{min}, {max}")
        } else if (type == "categorical") {
          if (option == "n_percent") {
            return("{n} ({p}%)")
          }
          if (option == "n_total_percent") return("{n} / {N} ({p}%)")
        }
        return(NULL)
      }

      statisticArguments <- list()

      # Global Defaults
      globalStatCont <- getStatString(self$options$statContGlobal, "continuous")
      globalStatCat <- getStatString(self$options$statCatGlobal, "categorical")

      if (!is.null(globalStatCont)) {
        statisticArguments <- c(
          statisticArguments,
          list(gtsummary::all_continuous() ~ globalStatCont)
        )
      }
      if (!is.null(globalStatCat)) {
        statisticArguments <- c(
          statisticArguments,
          list(gtsummary::all_categorical() ~ globalStatCat)
        )
      }

      # Specific Overrides - Continuous
      for (item in self$options$statsContSpecific) {
        if (item$stat != "use_global" && item$var %in% varsCont) {
          val <- getStatString(item$stat, "continuous")
          if (!is.null(val)) {
            statisticArguments <- c(
              statisticArguments,
              list(stats::as.formula(paste0("`", item$var, "` ~ '", val, "'")))
            )
          }
        }
      }

      # Specific Overrides - Categorical
      for (item in self$options$statsCatSpecific) {
        if (item$stat != "use_global" && item$var %in% varsCat) {
          val <- getStatString(item$stat, "categorical")
          if (!is.null(val)) {
            statisticArguments <- c(
              statisticArguments,
              list(stats::as.formula(paste0("`", item$var, "` ~ '", val, "'")))
            )
          }
        }
      }

      return(statisticArguments)
    },

    .constructTestArgs = function(varsCont, varsCat) {
      testArguments <- list()

      # Helper to resolve test selection
      resolveTest <- function(specificChoice, globalChoice) {
        if (specificChoice == "use_global") {
          return(globalChoice)
        }
        return(specificChoice)
      }

      # 1. Continuous Variables
      for (item in self$options$testsContSpecific) {
        if (item$var %in% varsCont) {
          selection <- resolveTest(item$test, self$options$testContGlobal)
          if (selection != "auto") {
            testArguments[[item$var]] <- selection
          }
        }
      }

      # 2. Categorical Variables
      for (item in self$options$testsCatSpecific) {
        if (item$var %in% varsCat) {
          selection <- resolveTest(item$test, self$options$testCatGlobal)
          if (selection != "auto") {
            testArguments[[item$var]] <- selection
          }
        }
      }

      return(testArguments)
    },

    .computeTable = function(collector) {
      # Theme setup - ensure cleanup even on error
      on.exit(resetTheme(), add = TRUE)
      applyTheme(
        themeOption = self$options$theme,
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
      if (length(varsCont) > 0) {
        for (v in varsCont) {
          typeArguments[[v]] <- "continuous"
        }
      }
      if (length(varsCat) > 0) {
        for (v in varsCat) {
          typeArguments[[v]] <- "categorical"
        }
      }

      # Statistic Arguments (delegated)
      statisticArguments <- private$.constructStatArgs(varsCont, varsCat)

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
      # Create collector at start
      collector <- newCollector()

      # Compute table (this also applies themes)
      table <- private$.computeTable(collector)

      # Render if table exists
      if (!is.null(table)) {
        renderHtml(table, self$results$tbl, collector)

        # Export if enabled
        if (isTRUE(self$options$export)) {
          path <- resolveExportPath(self$options$path)
          if (!is.null(path)) {
            exportDocx(table, path, self$options, self$results, collector)
          }
        }
      }

      # Display notices
      displayNotices(collector, self$options, self$results)
    }
  )
)
