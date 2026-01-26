tblSummaryClass <- if (requireNamespace('jmvcore', quietly = TRUE)) {
  R6::R6Class(
    "tblSummaryClass",
    inherit = tblSummaryBase,
    private = list(
      .run = function() {
        # ------------------------------------------------------------------
        # 1. Validation & Setup
        # ------------------------------------------------------------------
        if (length(self$options$include) == 0) {
          return()
        }

        # Debugging: Print structure of data to console
        # print("DEBUG: Data Structure")
        # print(str(self$data))

        # Helper: Ensure numeric types if needed (basic check)
        # Jamovi usually handles this, but we force numeric for continuous
        # logic if we were to implement advanced type handling.
        # For now, we rely on gtsummary's robust type detection.

        # ------------------------------------------------------------------
        # 2. Argument Mapping & Data Preparation
        # ------------------------------------------------------------------

        # Explicitly select data to ensure clean environment
        data <- self$data

        # ------------------------------------------------------------------
        # 2. Argument Mapping & Data Preparation
        # ------------------------------------------------------------------

        # Helper: Force numeric type for variables that are numeric in R
        # This overrides gtsummary's heuristic which treats <10 unique values as
        # categorical

        vars_numeric <- names(data)[sapply(data, is.numeric)]
        vars_categorical <- names(data)[sapply(data, function(x) {
          is.factor(x) || is.character(x)
        })]

        # Use named list for 'type' (safest for gtsummary)
        type_arg <- list()
        if (length(vars_numeric) > 0) {
          l_num <- as.list(rep("continuous", length(vars_numeric)))
          names(l_num) <- vars_numeric
          type_arg <- c(type_arg, l_num)
        }
        if (length(vars_categorical) > 0) {
          l_cat <- as.list(rep("categorical", length(vars_categorical)))
          names(l_cat) <- vars_categorical
          type_arg <- c(type_arg, l_cat)
        }

        # Map 'statistic' option
        stat_opt <- self$options$statistic
        statistic_arg <- NULL # Defaults to auto

        if (stat_opt == "mean_sd") {
          statistic_arg <- list(
            gtsummary::all_continuous() ~ "{mean} ({sd})",
            gtsummary::all_categorical() ~ "{n} ({p}%)"
          )
        } else if (stat_opt == "median_iqr") {
          statistic_arg <- list(
            gtsummary::all_continuous() ~ "{median} ({p25}, {p75})",
            gtsummary::all_categorical() ~ "{n} ({p}%)"
          )
        } else if (stat_opt == "n_percent") {
          statistic_arg <- list(
            # Unusual for continuous but possible
            gtsummary::all_continuous() ~ "{n} ({p}%)",
            gtsummary::all_categorical() ~ "{n} ({p}%)"
          )
        }

        # Map 'sort' option
        sort_arg <- NULL
        if (self$options$sort == "frequency") {
          sort_arg <- list(gtsummary::all_categorical() ~ "frequency")
        }

        # Map 'by' option
        by_var <- self$options$by
        if (is.null(by_var)) {
          by_var <- NULL
        } # Ensure NULL if empty

        # ------------------------------------------------------------------
        # 3. Execution (gtsummary)
        # ------------------------------------------------------------------

        # We use tryCatch to handle potential errors gracefully in Jamovi
        t1 <- tryCatch(
          {
            gtsummary::tbl_summary(
              data = data,
              include = self$options$include,
              by = by_var,
              type = type_arg,
              statistic = statistic_arg,
              missing = self$options$missing,
              missing_text = self$options$missingText,
              sort = sort_arg,
              percent = self$options$percent
            )
          },
          error = function(e) {
            stop(paste("Error in tbl_summary:", e$message))
          }
        )

        # Add p-values if requested AND grouping variable is present
        if (self$options$p_value && !is.null(by_var)) {
          # With 'broom' and 'cardx' now in DESCRIPTION, specific test selectors
          # should work.
          # We use formula syntax for robustness.

          test_arg <- list()

          if (length(vars_numeric) > 0) {
            for (v in vars_numeric) {
              if (v != by_var) {
                test_arg <- c(
                  test_arg,
                  list(as.formula(paste(v, "~ 'wilcox.test'")))
                )
              }
            }
          }
          if (length(vars_categorical) > 0) {
            for (v in vars_categorical) {
              if (v != by_var) {
                test_arg <- c(
                  test_arg,
                  list(as.formula(paste(v, "~ 'chisq.test'")))
                )
              }
            }
          }

          t1 <- t1 |> gtsummary::add_p(test = test_arg)
        }

        # ------------------------------------------------------------------
        # 4. Rendering
        # ------------------------------------------------------------------

        # Convert to gt, then to raw HTML string
        # self$results$tbl is the Html object defined in .r.yaml

        html_content <- t1 |>
          gtsummary::as_gt() |>
          gt::as_raw_html()

        self$results$tbl$setContent(html_content)
      }
    )
  )
}