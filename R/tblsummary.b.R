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
    .message_msgs = character(),
    .warning_msgs = character(),

    .add_message = function(msg) {
      private$.message_msgs <- c(private$.message_msgs, msg)
    },

    .add_warning = function(msg) {
      if (!grepl("C:/Rtools/home/builder", msg, fixed = TRUE)) {
        private$.warning_msgs <- c(private$.warning_msgs, msg)
      }
    },

    .run_safe = function(expr) {
      withCallingHandlers(
        expr,
        warning = function(w) {
          private$.add_warning(w$message)
          invokeRestart("muffleWarning")
        },
        message = function(m) {
          private$.add_message(m$message)
          invokeRestart("muffleMessage")
        }
      )
    },

    .computeTable = function() {
      # 1. Validation & Setup
      # Check if any variables are selected
      if (length(self$options$vars) == 0) {
        return(NULL)
      }

      # Explicitly select data to ensure clean environment
      data <- self$data

      # 2. Argument Mapping & Data Preparation
      # Get User Inputs (Unified List)
      vars_settings <- self$options$vars
      
      # Extract variable names and construction 'type' mapping
      all_vars <- vars_settings
      typeArguments <- list()
      
      # For now, we let gtsummary/data types decide. 
      # If specific logic is needed for ordinal vs continuous (e.g. Likert), 
      # it can be inferred from data class or handled in future updates.

      # Map 'statistic' option
      statOption <- self$options$statistic
      statisticArguments <- NULL # Defaults to auto

      if (statOption == "mean_sd") {
        statisticArguments <- list(
          gtsummary::all_continuous() ~ "{mean} ({sd})",
          gtsummary::all_categorical() ~ "{n} ({p}%)"
        )
      } else if (statOption == "median_iqr") {
        statisticArguments <- list(
          gtsummary::all_continuous() ~ "{median} ({p25}, {p75})",
          gtsummary::all_categorical() ~ "{n} ({p}%)"
        )
      } else if (statOption == "n_percent") {
        statisticArguments <- list(
          gtsummary::all_continuous() ~ "{n} ({p}%)",
          gtsummary::all_categorical() ~ "{n} ({p}%)"
        )
      }


      # Map 'by' option
      byVariable <- self$options$by
      if (is.null(byVariable)) {
        byVariable <- NULL
      }

      # 3. Execution (gtsummary)

      # Core analysis
      table <- private$.run_safe({
        gtsummary::tbl_summary(
          data = data,
          include = all_vars,
          by = byVariable,
          type = typeArguments,
          statistic = statisticArguments,
          missing = self$options$missing,
          missing_text = self$options$missingText,
          percent = self$options$percent
        )
      })

      # Add p-values if requested AND grouping variable is present
      if (!is.null(table) && self$options$p_value && !is.null(byVariable)) {
        table <- private$.run_safe({
          table |> gtsummary::add_p()
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
      private$.run_safe({
        htmlContent <- table |>
          gtsummary::as_gt() |>
          gt::as_raw_html()
        self$results$tbl$setContent(htmlContent)
      })
    },

    .export = function(table) {
      if (isTRUE(self$options$exportWord)) {
        # Use utility function to resolve path (includes smart defaults/expansion)
        path <- resolve_export_path(self$options$exportPath)

        if (is.null(path)) return()
        private$.run_safe({
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
      if (length(private$.message_msgs) > 0) {
        # Combine messages
        finalContent <- paste(private$.message_msgs, collapse = "\n")

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
      if (length(private$.warning_msgs) > 0) {
        # Combine warnings
        finalContent <- paste(private$.warning_msgs, collapse = "\n")

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