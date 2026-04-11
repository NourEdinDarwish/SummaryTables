tblRegCoxClass <- R6::R6Class(
  "tblRegCoxClass",
  inherit = tblRegCoxBase,
  private = list(
    .run = function() {
      on.exit(self$results$status$setVisible(FALSE), add = TRUE)
      # Guard ---------------------------------------------------------------
      elapsed <- self$options$elapsed
      event <- self$options$event
      terms <- self$options$modelTerms

      if (is.null(elapsed) || is.null(event) || length(terms) == 0) {
        renderPlaceholder(
          "Add a time variable, an event variable, and at least one term to generate the table", #nolint
          self$results$tbl
        )
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

      # Convert event variable to 0/1 numeric
      if (is.numeric(data[[event]])) {
        validateEventNumeric(data, event)
        data[[event]] <- jmvcore::toNumeric(data[[event]])
      } else {
        data[[event]] <- ifelse(
          data[[event]] == self$options$eventLevel,
          1,
          0
        )
      }

      data[[elapsed]] <- jmvcore::toNumeric(data[[elapsed]])

      data[self$options$covs] <- lapply(
        data[self$options$covs],
        jmvcore::toNumeric
      )
      data[self$options$factors] <- lapply(
        data[self$options$factors],
        as.factor
      )

      # Formula and model ---------------------------------------------------
      survLHS <- sprintf(
        "survival::Surv(%s, %s)",
        jmvcore::composeTerm(elapsed),
        jmvcore::composeTerm(event)
      )
      formula <- buildFormula(survLHS, terms)
      model <- runSafe(
        survival::coxph(formula, data = data, model = TRUE),
        collector
      )

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

      table <- pipeCiMergeReg(
        table,
        options = self$options
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
    }
  )
)
