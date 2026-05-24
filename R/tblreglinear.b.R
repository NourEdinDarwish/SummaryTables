tblRegLinearClass <- R6::R6Class(
  "tblRegLinearClass",
  inherit = tblRegLinearBase,
  private = list(
    .init = function() {
      dep <- self$options$dep
      terms <- self$options$modelTerms

      if (is.null(dep) || length(terms) == 0) {
        renderPlaceholder(
          "Add a Dependent Variable and at least one Model Term to generate the table", #nolint
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
          data[[dep]] <- jmvcore::toNumeric(data[[dep]])

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

          if (self$options$standardize) {
            # Goal: produce the same standardized coefficients (betas) as
            # parameters::standardize_parameters(method = "refit").
            #
            # How standardize_parameters(method = "refit") works internally:
            #   1. Extracts the model frame (data AFTER listwise deletion —
            #      rows with NA in ANY variable, numeric OR factor, are gone).
            #   2. Standardizes that complete-case data with
            #      datawizard::standardize() (factors untouched, force = FALSE).
            #   3. Refits the model on the standardized complete-case data.
            #
            # Previously we used:
            #   standardize(data, select = ..., remove_na = "selected")
            # Bug: with force = FALSE (default), datawizard excludes factors
            # from the internal "selected" set, so remove_na = "selected" only
            # checked numeric columns for NAs. Rows with NA in a factor (but
            # not in any numeric) were KEPT, meaning mean/SD was computed from
            # MORE rows than lm() actually uses — producing wrong betas.
            #
            # We also cannot use remove_na = "all" on the full data frame,
            # because "all" checks EVERY column — including ones not in the
            # formula. NAs in unrelated columns would incorrectly drop rows.
            #
            # Fix: subset to formula columns first (data[select_vars]), then
            # use remove_na = "all". This performs listwise deletion on exactly
            # the variables the model uses, matching what lm() and
            # parameters::standardize_parameters(method = "refit") do.
            select_vars <- all.vars(formula)
            data <- datawizard::standardize(
              data[select_vars],
              remove_na = "all"
            )
          }

          model <- runSafe(stats::lm(formula, data = data), collector)

          # Regression table ----------------------------------------------------
          table <- runSafe(
            buildMultiRegTable(model, self$options, b64Map),
            collector
          )

          # Change header -------------------------------------------------------
          coefHeader <- if (self$options$standardize) {
            "**Standardized Coefficient**"
          } else {
            "**Coefficient**"
          }
          table <- table |>
            gtsummary::modify_header(estimate = coefHeader)

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
          displayNotices(collector, self$options, self$results, b64Map)
        },
        error = function(e) decodeB64Error(e, b64Map)
      )
    }
  )
)
