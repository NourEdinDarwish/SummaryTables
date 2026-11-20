tblUniRegLogisticClass <- R6::R6Class(
  "tblUniRegLogisticClass",
  inherit = tblUniRegLogisticBase,
  private = list(
    .init = function() {
      dep <- self$options$dep
      covs <- self$options$covs
      factors <- self$options$factors

      if (is.null(dep) || (length(covs) == 0 && length(factors) == 0)) {
        renderPlaceholder(
          paste(
            "Add a Dependent Variable and at least one Covariate or Factor",
            "to generate the table"
          ),
          self$results$tbl
        )
        self$results$status$setVisible(FALSE)
      }
    },

    .run = function() {
      on.exit(self$results$status$setVisible(FALSE), add = TRUE)
      # Guard ---------------------------------------------------------------
      if (
        self$options$manualRun &&
          !self$options$run &&
          !self$options$openDocx
      ) {
        return()
      }

      dep <- self$options$dep
      covs <- self$options$covs
      factors <- self$options$factors

      if (is.null(dep) || (length(covs) == 0 && length(factors) == 0)) {
        return()
      }

      # B64 encoding map ----------------------------------------------------
      b64Map <- buildB64Map(names(self$data))

      tryCatch(
        {
          # Collector ----------------------------------------------------------
          collector <- newCollector()

          # Theme --------------------------------------------------------------
          on.exit(gtsummary::reset_gtsummary_theme(), add = TRUE)
          applyTheme(
            journalOption = self$options$journal,
            languageOption = self$options$language,
            compactOption = self$options$compact
          )

          # Data prep ----------------------------------------------------------
          data <- self$data
          data[[dep]] <- as.factor(data[[dep]])

          if (length(levels(data[[dep]])) != 2) {
            jmvcore::reject(
              paste(
                'The dependent variable "{}" must have',
                "exactly two levels for binomial logistic regression"
              ),
              code = NULL,
              dep
            )
          }

          data[covs] <- lapply(data[covs], jmvcore::toNumeric)
          data[factors] <- lapply(data[factors], as.factor)

          # Encode data to B64
          data <- encodeB64(data)
          depB64 <- jmvcore::toB64(dep)

          # Regression table ---------------------------------------------------
          # The UI supplies varOrder. R calls without it list covariates first,
          # then factors, preserving each selection's order.
          varOrder <- self$options$varOrder
          if (length(varOrder) == 0) {
            varOrder <- c(covs, factors)
          }

          table <- runSafe(
            buildUniRegTable(
              data = data,
              y = depB64,
              include = jmvcore::toB64(varOrder),
              method = stats::glm,
              methodArgs = list(family = stats::binomial),
              options = self$options
            ),
            collector
          )

          # Pipeline -----------------------------------------------------------
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
            collector = collector,
            ratioName = "odds"
          )

          table <- pipeCiMergeReg(
            table,
            options = self$options
          )

          # Text formatting ----------------------------------------------------
          table <- applyTextFormatting(
            table,
            hasPvalue = TRUE,
            options = self$options
          )

          # Render and export --------------------------------------------------
          renderHtml(table, self$results$tbl)

          exportDocx(
            table,
            self$options,
            "Univariable Logistic Regression.docx"
          )

          # Notices ------------------------------------------------------------
          displayNotices(collector, self$options, self$results, b64Map)
        },
        error = function(e) decodeB64Error(e, b64Map)
      )
    }
  )
)
