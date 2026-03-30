tblRegLinearClass <- R6::R6Class(
  "tblRegLinearClass",
  inherit = tblRegLinearBase,
  private = list(
    .run = function() {
      # Guard ---------------------------------------------------------------
      dep <- self$options$dep
      terms <- self$options$modelTerms

      if (is.null(dep) || length(terms) == 0) {
        renderPlaceholder(
          "Add a dependent variable and at least one predictor to generate the table", #nolint
          self$results$tbl
        )
        self$results$status$setVisible(FALSE)
        return()
      }

      # Collector -----------------------------------------------------------
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
      data[[dep]] <- jmvcore::toNumeric(data[[dep]])

      covs <- self$options$covs
      factors <- self$options$factors

      data[covs] <- lapply(data[covs], jmvcore::toNumeric)
      data[factors] <- lapply(data[factors], as.factor)

      # Build formula -------------------------------------------------------
      composed <- jmvcore::composeTerms(terms)
      formula <- as.formula(
        paste(
          jmvcore::composeTerm(dep), "~",
          paste(composed, collapse = " + ")
        )
      )

      # Fit model -----------------------------------------------------------
      model <- lm(formula, data = data)

      # Regression table ----------------------------------------------------
      table <- runSafe(
        gtsummary::tbl_regression(model),
        collector
      )

      # Pipeline ------------------------------------------------------------
      table <- pipeAddQ(
        table,
        hasPvalue = TRUE,
        options = self$options,
        collector = collector
      )

      # Text formatting -----------------------------------------------------
      table <- applyTextFormatting(
        table,
        hasPvalue = TRUE,
        options = self$options
      )

      # Render and export ---------------------------------------------------
      renderHtml(table, self$results$tbl)

      if (self$options$export) {
        path <- resolveExportPath(self$options$path)
        exportDocx(table, path, self$options, self$results)
      }

      # Notices -------------------------------------------------------------
      displayNotices(collector, self$options, self$results)

      # Hide status indicator ------------------------------------------------
      self$results$status$setVisible(FALSE)
    }
  )
)
