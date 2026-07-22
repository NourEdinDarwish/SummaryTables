tblUniRegCoxClass <- R6::R6Class(
  "tblUniRegCoxClass",
  inherit = tblUniRegCoxBase,
  private = list(
    .init = function() {
      elapsed <- self$options$elapsed
      event <- self$options$event
      covs <- self$options$covs
      factors <- self$options$factors

      if (
        is.null(elapsed) ||
          is.null(event) ||
          (length(covs) == 0 && length(factors) == 0)
      ) {
        renderPlaceholder(
          "Add a Time variable, an Event variable, and at least one Covariate or Factor to generate the table", #nolint
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
      covs <- self$options$covs
      factors <- self$options$factors

      if (
        is.null(elapsed) ||
          is.null(event) ||
          (length(covs) == 0 && length(factors) == 0)
      ) {
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
          data[covs] <- lapply(data[covs], jmvcore::toNumeric)
          data[factors] <- lapply(data[factors], as.factor)

          # Encode data to B64
          data <- encodeB64(data)
          elapsedB64 <- jmvcore::toB64(elapsed)
          eventB64 <- jmvcore::toB64(event)

          # Regression table ----------------------------------------------------
          survY <- str2lang(sprintf(
            "survival::Surv(%s, %s)",
            elapsedB64,
            eventB64
          ))

          table <- runSafe(
            buildUniRegTable(
              data = data,
              y = survY,
              include = jmvcore::toB64(self$options$varOrder),
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
            collector = collector,
            ratioName = "hazard"
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
          displayNotices(collector, self$options, self$results, b64Map)
        },
        error = function(e) decodeB64Error(e, b64Map)
      )
    }
  )
)
