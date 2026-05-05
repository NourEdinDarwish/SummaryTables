tblRegCoxClass <- R6::R6Class(
  "tblRegCoxClass",
  inherit = tblRegCoxBase,
  private = list(
    .init = function() {
      elapsed <- self$options$elapsed
      event <- self$options$event
      terms <- self$options$modelTerms

      if (is.null(elapsed) || is.null(event) || length(terms) == 0) {
        renderPlaceholder(
          "Add a time variable, an event variable, and at least one term to generate the table", #nolint
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

      elapsed <- self$options$elapsed
      event <- self$options$event
      terms <- self$options$modelTerms

      if (is.null(elapsed) || is.null(event) || length(terms) == 0) {
        return()
      }

      # B64 encoding map ----------------------------------------------------
      b64Map <- buildB64Map(names(self$data))

      tryCatch(
        {
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

          validateFactorPolynomials(data, terms)

          # Encode data to B64
          data <- encodeB64(data)
          elapsedB64 <- jmvcore::toB64(elapsed)
          eventB64 <- jmvcore::toB64(event)

          # Formula and model ---------------------------------------------------
          survLHS <- sprintf(
            "survival::Surv(%s, %s)",
            elapsedB64,
            eventB64
          )
          termsB64 <- lapply(terms, jmvcore::toB64)
          formula <- buildFormula(survLHS, termsB64)
          model <- runSafe(
            survival::coxph(formula, data = data, model = TRUE),
            collector
          )

          # Regression table ----------------------------------------------------
          table <- runSafe(
            buildMultiRegTable(model, self$options, b64Map),
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

          # Render --------------------------------------------------------------
          renderHtml(table, self$results$tbl)

          # Notices -------------------------------------------------------------
          displayNotices(collector, self$options, self$results, b64Map)
        },
        error = function(e) decodeB64Error(e, b64Map)
      )
    }
  )
)
