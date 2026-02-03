tblSummaryClass <- R6::R6Class(
  "tblSummaryClass",
  inherit = tblSummaryBase,
  active = list(
    table = function() {
      if (is.null(private$.table)) {
        private$.table <- private$.computeTable()
      }
      private$.table
    }
  ),
  private = list(
    .table = NULL,
    .messageMsgs = character(),
    .warningMsgs = character(),

    .addMessage = function(msg) {
      private$.messageMsgs <- c(private$.messageMsgs, msg)
    },

    .addWarning = function(msg) {
      if (!grepl("C:/Rtools/home/builder", msg, fixed = TRUE)) {
        private$.warningMsgs <- c(private$.warningMsgs, msg)
      }
    },

    .runSafe = function(expr) {
      withCallingHandlers(
        expr,
        warning = function(w) {
          private$.addWarning(w$message)
          invokeRestart("muffleWarning")
        },
        message = function(m) {
          private$.addMessage(m$message)
          invokeRestart("muffleMessage")
        }
      )
    },

    .constructStatArgs = function(varsCont, varsCat) {
      # Helper to map option to gtsummary string
      getStatString <- function(option, type) {
        if (type == "continuous") {
          if (option == "mean_sd") return("{mean} ({sd})")
          if (option == "median_iqr") return("{median} ({p25}, {p75})")
          if (option == "n_percent") return("{n} ({p}%)")
          if (option == "range") return("{min}, {max}")
        } else if (type == "categorical") {
          if (option == "n_percent") return("{n} ({p}%)")
          if (option == "n_total_percent") return("{n} / {N} ({p}%)")
        }
        return(NULL)
      }

      statisticArguments <- list()

      # Global Defaults
      globalStatCont <- getStatString(self$options$statContGlobal, "continuous")
      globalStatCat <- getStatString(self$options$statCatGlobal, "categorical")

      if (!is.null(globalStatCont)) {
        statisticArguments <- c(statisticArguments, list(gtsummary::all_continuous() ~ globalStatCont))
      }
      if (!is.null(globalStatCat)) {
        statisticArguments <- c(statisticArguments, list(gtsummary::all_categorical() ~ globalStatCat))
      }

      # Specific Overrides - Continuous
      for (item in self$options$statsContSpecific) {
        if (item$stat != "use_global" && item$var %in% varsCont) {
          val <- getStatString(item$stat, "continuous")
          if (!is.null(val)) {
            statisticArguments <- c(statisticArguments, list(stats::as.formula(paste0("`", item$var, "` ~ '", val, "'"))))
          }
        }
      }

      # Specific Overrides - Categorical
      for (item in self$options$statsCatSpecific) {
        if (item$stat != "use_global" && item$var %in% varsCat) {
          val <- getStatString(item$stat, "categorical")
          if (!is.null(val)) {
            statisticArguments <- c(statisticArguments, list(stats::as.formula(paste0("`", item$var, "` ~ '", val, "'"))))
          }
        }
      }

      return(statisticArguments)
    },

    .constructTestArgs = function(varsCont, varsCat) {
      testArguments <- list()

      # Helper to resolve test selection
      resolveTest <- function(specificChoice, globalChoice) {
        if (specificChoice == "use_global") return(globalChoice)
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

    .computeTable = function() {
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
        for (v in varsCont) typeArguments[[v]] <- "continuous"
      }
      if (length(varsCat) > 0) {
        for (v in varsCat) typeArguments[[v]] <- "categorical"
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
      table <- private$.runSafe({
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
      })

      # Add p-values if requested AND grouping variable is present
      if (!is.null(table) && self$options$pValue && !is.null(byVariable)) {
        testArguments <- private$.constructTestArgs(varsCont, varsCat)
        
        table <- private$.runSafe({
          # We must pass the list, even if empty (empty list = all auto)
          gtsummary::add_p(table, test = testArguments)
        })
      }

      table
    },

    .run = function() {
      # Check table which triggers computation
      table <- self$table

      # 1. Render Table (if available)
      if (!is.null(table)) {
        private$.render(table)
        private$.export(table)
      }

      # 2. Display Notices
      private$.notices()
    },

    .render = function(table) {
      private$.runSafe({
        htmlContent <- table |>
          gtsummary::as_gt() |>
          gt::as_raw_html()
        self$results$tbl$setContent(htmlContent)
      })
    },

    .export = function(table) {
      if (isTRUE(self$options$export)) {
        # Use utility function to resolve path (includes smart defaults/expansion)
        path <- resolveExportPath(self$options$path)

        if (is.null(path)) return()
        private$.runSafe({
          flexTableObject <- gtsummary::as_flex_table(table)
          flextable::save_as_docx(flexTableObject, path = path)

          # Create a specific Notice for the save message so it stands out
          saveNotice <- jmvcore::Notice$new(
            options = self$options,
            name = 'saveMessage',
            type = jmvcore::NoticeType$INFO
          )
          saveNotice$setContent(paste0("Successfully saved to: ", path))
          self$results$insert(1, saveNotice)
        })
      }
    },

    .notices = function() {
      # 1. Messages (INFO)
      if (length(private$.messageMsgs) > 0) {
        # Combine messages
        finalContent <- paste(private$.messageMsgs, collapse = "\n")

        # Create Notice with INFO type
        messageNotice <- jmvcore::Notice$new(
          options = self$options,
          name = 'runMessage',
          type = jmvcore::NoticeType$INFO
        )
        messageNotice$setContent(finalContent)
        self$results$insert(1, messageNotice)
      }

      # 2. Warnings (WARNING)
      if (length(private$.warningMsgs) > 0) {
        # Combine warnings
        finalContent <- paste(private$.warningMsgs, collapse = "\n")

        # Create Notice with WARNING type
        warningNotice <- jmvcore::Notice$new(
          options = self$options,
          name = 'runWarn',
          type = jmvcore::NoticeType$WARNING
        )
        warningNotice$setContent(finalContent)
        self$results$insert(1, warningNotice)
      }
    }
  )
)