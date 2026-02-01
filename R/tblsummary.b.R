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
      vars_cont <- self$options$vars_cont
      vars_cat <- self$options$vars_cat
      
      # Check if any variables are selected
      if (length(vars_cont) == 0 && length(vars_cat) == 0) {
        return(NULL)
      }

      # Explicitly select data to ensure clean environment
      data <- self$data

      # 2. Argument Mapping & Data Preparation
      # Combine variables (Categorical first, then Continuous - or user preference?)
      # adhering to standard assumption: Cat then Cont
      all_vars <- c(vars_cat, vars_cont)
      
      # Determine Types
      typeArguments <- list()
      if (length(vars_cont) > 0) {
         for (v in vars_cont) typeArguments[[v]] <- "continuous"
      }
      if (length(vars_cat) > 0) {
         for (v in vars_cat) typeArguments[[v]] <- "categorical"
      }

      # Helper to map option to gtsummary string
      get_stat_string <- function(option, type) {
        if (type == "continuous") {
          if (option == "mean_sd") return("{mean} ({sd})")
          if (option == "median_iqr") return("{median} ({p25}, {p75})")
          if (option == "n_percent") return("{n} ({p}%)") # Unusual for cont, but possible
          if (option == "range") return("{min}, {max}")
        } else if (type == "categorical") {
          if (option == "n_percent") return("{n} ({p}%)")
          if (option == "n_total_percent") return("{n} / {N} ({p}%)")
        }
        return(NULL)
      }

      # Statistic Arguments (List of Formulas)
      statisticArguments <- list()

      # Global Defaults 
      # We use formulas for selectors like all_continuous()
      
      global_stat_cont <- get_stat_string(self$options$stat_cont_global, "continuous")
      global_stat_cat <- get_stat_string(self$options$stat_cat_global, "categorical")
      
      if (!is.null(global_stat_cont)) {
        statisticArguments <- c(statisticArguments, list(gtsummary::all_continuous() ~ global_stat_cont))
      }
      if (!is.null(global_stat_cat)) {
        statisticArguments <- c(statisticArguments, list(gtsummary::all_categorical() ~ global_stat_cat))
      }

      # Specific Overrides - Continuous
      # For specific variables, we can use named lists or formulas string ~ value
      for (item in self$options$stats_cont_specific) {
        if (item$stat != "use_global" && item$var %in% vars_cont) {
           val <- get_stat_string(item$stat, "continuous")
           if (!is.null(val)) {
             # safely append formula for specific variable
             statisticArguments <- c(statisticArguments, list(stats::as.formula(paste0("`", item$var, "` ~ '", val, "'"))))
           }
        }
      }

      # Specific Overrides - Categorical
      for (item in self$options$stats_cat_specific) {
        if (item$stat != "use_global" && item$var %in% vars_cat) {
           val <- get_stat_string(item$stat, "categorical")
           if (!is.null(val)) {
             statisticArguments <- c(statisticArguments, list(stats::as.formula(paste0("`", item$var, "` ~ '", val, "'"))))
           }
        }
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