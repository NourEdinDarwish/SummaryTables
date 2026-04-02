tblRegLogisticClass <- R6::R6Class(
  "tblRegLogisticClass",
  inherit = tblRegLogisticBase,
  private = list(
    .run = function() {
      # Guard ---------------------------------------------------------------
      dep <- self$options$dep
      terms <- self$options$modelTerms

      if (is.null(dep) || length(terms) == 0) {
        renderPlaceholder(
          "Add a dependent variable and at least one term to generate the table", #nolint
          self$results$tbl
        )
        self$results$status$setVisible(FALSE)
        return()
      }

      validateVarNames(c(self$options$covs, self$options$factors))

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
      data[[dep]] <- as.factor(data[[dep]])

      if (length(levels(data[[dep]])) != 2) {
        jmvcore::reject(
          jmvcore::format(
            "The dependent variable '{}' must have exactly two levels for binomial logistic regression", # nolint
            dep
          )
        )
      }

      data[self$options$covs] <- lapply(
        data[self$options$covs],
        jmvcore::toNumeric
      )
      data[self$options$factors] <- lapply(
        data[self$options$factors],
        as.factor
      )

      # Formula and model ---------------------------------------------------
      formula <- buildFormula(dep, terms)
      model <- glm(formula, data = data, family = binomial)

      # Regression table ----------------------------------------------------
      table <- runSafe(
        buildMultiRegTable(model, self$options),
        collector
      )

      # Pipeline ------------------------------------------------------------
      table <- pipeAddGlobalP(
        table,
        options = self$options,
        collector = collector
      )

      table <- pipeAddQ(
        table,
        hasPvalue = TRUE,
        options = self$options,
        collector = collector
      )

      table <- pipeAddVif(
        table,
        options = self$options,
        collector = collector
      )

      table <- pipeAddNReg(
        table,
        options = self$options,
        collector = collector
      )

      table <- pipeAddNEvent(
        table,
        options = self$options,
        collector = collector
      )

      table <- pipeAddSignificanceStars(
        table,
        options = self$options,
        collector = collector
      )

      table <- pipeAddGlance(
        table,
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
