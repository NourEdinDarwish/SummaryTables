tblCrossClass <- R6::R6Class(
  "tblCrossClass",
  inherit = tblCrossBase,
  private = list(
    .run = function() {
      # Guard ---------------------------------------------------------------
      row <- self$options$row
      col <- self$options$col
      if (is.null(row) || is.null(col)) {
        renderPlaceholder(
          "Add a row variable and a column variable to generate the table",
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
      data[[row]] <- as.factor(data[[row]])
      data[[col]] <- as.factor(data[[col]])

      # Build statistic string ----------------------------------------------
      statisticArgument <- switch(
        self$options$statistic,
        n = "{n}",
        nPercent = "{n} ({p}%)",
        percent = "{p}%"
      )

      # Build percent argument ----------------------------------------------
      # "percent" controls how gtsummary computes p:
      # When statistic is count-only ({n}), percent type is irrelevant
      # but gtsummary still needs a value — default to "cell"
      percentArgument <- if (self$options$statistic == "n") {
        "none"
      } else {
        self$options$percent
      }

      # Build margin argument -----------------------------------------------
      marginArgument <- switch(
        self$options$margin,
        both = c("column", "row"),
        column = "column",
        row = "row",
        none = NULL
      )

      # Build digits argument -----------------------------------------------
      digitsArgument <- if (self$options$digits == "auto") {
        NULL
      } else {
        list(p = as.numeric(self$options$digits))
      }

      # Core table ----------------------------------------------------------
      table <- runSafe(
        gtsummary::tbl_cross(
          data = data,
          row = row,
          col = col,
          statistic = statisticArgument,
          percent = percentArgument,
          margin = marginArgument,
          margin_text = self$options$marginText,
          missing = self$options$missing,
          missing_text = self$options$missingText,
          digits = digitsArgument
        ),
        collector
      )

      # P-value -------------------------------------------------------------
      if (self$options$addPvalue) {
        testArgument <- if (self$options$testDefault == "auto") {
          NULL
        } else {
          self$options$testDefault
        }

        addPArgs <- list(
          x = table,
          source_note = self$options$sourceNote
        )

        if (!is.null(testArgument)) {
          addPArgs$test <- testArgument
        }

        pvDigits <- self$options$digitsPvalue
        if (pvDigits == "auto") {
          themeKey <- if (self$options$sourceNote) {
            "pkgwide-fn:prependpvalue_fun"
          } else {
            "pkgwide-fn:pvalue_fun"
          }
          addPArgs$pvalue_fun <- gtsummary:::get_theme_element(
            themeKey,
            default = gtsummary::label_style_pvalue(
              digits = 1,
              prepend_p = self$options$sourceNote
            )
          )
        } else {
          addPArgs$pvalue_fun <- gtsummary::label_style_pvalue(
            digits = as.integer(pvDigits),
            prepend_p = self$options$sourceNote
          )
        }

        table <- runSafe(do.call(gtsummary::add_p, addPArgs), collector)
      }

      # Text formatting -----------------------------------------------------
      # tblCross always has a "group" (the col variable), so hasGroupVar=TRUE
      table <- applyTextFormatting(
        table,
        hasGroupVar = TRUE,
        options = self$options
      )

      # Render and export ---------------------------------------------------
      renderHtml(table, self$results$tbl)

      # Notices --------------------------------------------------------------
      displayNotices(collector, self$options, self$results)
    }
  )
)
