tblUniRegLinearClass <- R6::R6Class(
  "tblUniRegLinearClass",
  inherit = tblUniRegLinearBase,
  private = list(
    .init = function() {
      dep <- self$options$dep
      covs <- self$options$covs
      factors <- self$options$factors

      if (is.null(dep) || (length(covs) == 0 && length(factors) == 0)) {
        renderPlaceholder(
          "Add a dependent variable and at least one covariate or factor to generate the table", #nolint
          self$results$tbl
        )
        self$results$status$setVisible(FALSE)
      }
    },

    .run = function() {
      on.exit(self$results$status$setVisible(FALSE), add = TRUE)
      # Guard ---------------------------------------------------------------
      if (self$options$manualRun && !self$options$run) {
        return()
      }

      dep <- self$options$dep
      covs <- self$options$covs
      factors <- self$options$factors

      if (is.null(dep) || (length(covs) == 0 && length(factors) == 0)) {
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
      data[[dep]] <- jmvcore::toNumeric(data[[dep]])

      data[covs] <- lapply(data[covs], jmvcore::toNumeric)
      data[factors] <- lapply(data[factors], as.factor)

      allVars <- c(covs, factors)

      # Regression table ----------------------------------------------------
      table <- runSafe(
        buildUniRegTable(
          data = data,
          y = jmvcore::composeTerm(dep),
          include = allVars,
          method = lm,
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
