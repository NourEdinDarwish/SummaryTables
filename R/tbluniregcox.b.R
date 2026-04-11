tblUniRegCoxClass <- R6::R6Class(
  "tblUniRegCoxClass",
  inherit = tblUniRegCoxBase,
  private = list(
    .run = function() {
      on.exit(self$results$status$setVisible(FALSE), add = TRUE)
      # Guard ---------------------------------------------------------------
      elapsed <- self$options$elapsed
      event <- self$options$event
      covs <- self$options$covs
      factors <- self$options$factors

      if (is.null(elapsed) || is.null(event) ||
          (length(covs) == 0 && length(factors) == 0)) {
        renderPlaceholder(
          "Add a time variable, an event variable, and at least one covariate or factor to generate the table", #nolint
          self$results$tbl
        )
        return()
      }

      validateVarNames(c(covs, factors))

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
        data[[event]] <- jmvcore::toNumeric(data[[event]])
      } else {
        data[[event]] <- ifelse(
          data[[event]] == self$options$eventLevel, 1, 0
        )
      }

      data[[elapsed]] <- jmvcore::toNumeric(data[[elapsed]])

      data[covs] <- lapply(data[covs], jmvcore::toNumeric)
      data[factors] <- lapply(data[factors], as.factor)

      # Variable ordering (cross-listbox) -----------------------------------
      orderState <- trackVariableOrder(
        savedState = self$results$tbl$state,
        factors,
        covs
      )
      allVars <- orderState$vars
      self$results$tbl$setState(orderState)

      # Regression table ----------------------------------------------------
      survY <- str2lang(sprintf(
        "survival::Surv(%s, %s)",
        jmvcore::composeTerm(elapsed),
        jmvcore::composeTerm(event)
      ))

      table <- runSafe(
        buildUniRegTable(
          data = data,
          y = survY,
          include = allVars,
          method = survival::coxph,
          options = self$options
        ),
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

      # Text formatting -----------------------------------------------------
      table <- applyTextFormatting(
        table,
        hasPvalue = TRUE,
        options = self$options
      )

      # Render --------------------------------------------------------------
      renderHtml(table, self$results$tbl)

      # Notices -------------------------------------------------------------
      displayNotices(collector, self$options, self$results)

    }
  )
)
