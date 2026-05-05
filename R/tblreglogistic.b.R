tblRegLogisticClass <- R6::R6Class(
  "tblRegLogisticClass",
  inherit = tblRegLogisticBase,
  private = list(
    .init = function() {
      dep <- self$options$dep
      terms <- self$options$modelTerms

      if (is.null(dep) || length(terms) == 0) {
        renderPlaceholder(
          "Add a dependent variable and at least one term to generate the table", #nolint
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
      terms <- self$options$modelTerms

      if (is.null(dep) || length(terms) == 0) {
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

          validateFactorPolynomials(data, terms)

          # Encode data to B64
          data <- encodeB64(data)
          depB64 <- jmvcore::toB64(dep)

          # Formula and model ---------------------------------------------------
          termsB64 <- lapply(terms, jmvcore::toB64)
          formula <- buildFormula(depB64, termsB64)
          model <- runSafe(
            glm(formula, data = data, family = binomial),
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
