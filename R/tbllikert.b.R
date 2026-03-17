tblLikertClass <- R6::R6Class(
  "tblLikertClass",
  inherit = tblLikertBase,
  private = list(
    .run = function() {
      # Guard ---------------------------------------------------------------
      vars <- self$options$vars
      if (length(vars) == 0) {
        renderPlaceholder(
          "Add Likert scale variables to generate the table",
          self$results$tbl
        )
        return()
      }

      # Collector ---------------------------------------------------------
      collector <- newCollector()

      # Theme ---------------------------------------------------------------
      on.exit(gtsummary::reset_gtsummary_theme(), add = TRUE)
      applyTheme(
        journalOption = self$options$journal,
        languageOption = self$options$language,
        compactOption = self$options$compact
      )

      # Data prep -----------------------------------------------------------
      data <- self$data

      # Coerce all selected variables to factor
      data[vars] <- lapply(data[vars], as.factor)

      # Unify factor levels across all variables (first variable = reference)
      data[vars] <- forcats::fct_unify(data[vars])

      # Build statistic argument --------------------------------------------
      defaultStat <- switch(
        self$options$statistic,
        n = "{n}",
        nPercent = "{n} ({p}%)",
        percent = "{p}%"
      )

      statisticArg <- list(~defaultStat)

      for (item in self$options$statSpecific) {
        if (item$stat != "useDefault" && item$var %in% vars) {
          statisticArg[[item$var]] <- switch(
            item$stat,
            n = "{n}",
            nPercent = "{n} ({p}%)",
            percent = "{p}%"
          )
        }
      }

      # Build digits argument -----------------------------------------------
      digitsArg <- if (self$options$digits == "auto") {
        NULL
      } else {
        ~ list(p = as.numeric(self$options$digits))
      }

      # Build sort argument -------------------------------------------------
      sortArg <- switch(
        self$options$levelOrder,
        asFirst = "ascending",
        reversed = "descending"
      )

      # Core table ----------------------------------------------------------
      table <- runSafe(
        gtsummary::tbl_likert(
          data = data,
          include = vars,
          statistic = statisticArg,
          digits = digitsArg,
          sort = sortArg
        ),
        collector
      )

      # Add N ---------------------------------------------------------------
      table <- pipeAddN(table, self$options, collector)

      # Text formatting -----------------------------------------------------
      table <- applyTextFormatting(
        table,
        hasGroupVar = FALSE,
        options = self$options
      )

      # Render and export ---------------------------------------------------
      renderHtml(table, self$results$tbl)

      # Notices --------------------------------------------------------------
      displayNotices(collector, self$options, self$results)
    }
  )
)
