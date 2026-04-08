tblSurvfitClass <- R6::R6Class(
  "tblSurvfitClass",
  inherit = tblSurvfitBase,
  private = list(
    .run = function() {
      on.exit(self$results$status$setVisible(FALSE), add = TRUE)
      # Guard ---------------------------------------------------------------
      elapsed <- self$options$elapsed
      event <- self$options$event

      if (is.null(elapsed) || is.null(event)) {
        renderPlaceholder(
          "Add a time variable and an event variable to generate the table",
          self$results$tbl
        )
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
      # survfit.formula() cannot handle backtick-quoted names on the RHS,
      # but Surv() on the LHS is fine. Only strata names need cleaning.
      # rlang::inject() embeds the formula value so add_n/add_nevent work.
      strata <- self$options$strata
      strataClean <- make.names(strata, unique = TRUE)

      data <- self$data

      if (is.numeric(data[[event]])) {
        data[[event]] <- jmvcore::toNumeric(data[[event]])
      } else {
        data[[event]] <- ifelse(
          data[[event]] == self$options$eventLevel,
          1,
          0
        )
      }

      data[[elapsed]] <- jmvcore::toNumeric(data[[elapsed]])

      # Add cleaned strata columns
      for (i in seq_along(strata)) {
        data[[strataClean[i]]] <- as.factor(data[[strata[i]]])
      }

      # Parse times / probs -------------------------------------------------
      if (self$options$statistic == "times") {
        timesVec <- parseCommaNumeric(self$options$times)
        probsVec <- NULL
      } else {
        probsVec <- parseCommaNumeric(self$options$probs) / 100
        timesVec <- NULL
      }

      # Build survfit objects ------------------------------------------------
      confInt <- self$options$confLevel / 100

      fits <- buildSurvfitList(
        data = data,
        elapsed = elapsed,
        event = event,
        strataClean = strataClean,
        confInt = confInt
      )

      # Build tbl_survfit args -----------------------------------------------
      estimateFun <- NULL

      if (self$options$digitsEstimate != "auto") {
        d <- as.integer(self$options$digitsEstimate)
        if (
          self$options$statistic == "times" &&
            self$options$type %in% c("survival", "risk")
        ) {
          # Survival / cumulative incidence are probabilities → %
          estimateFun <- gtsummary::label_style_number(
            digits = d,
            scale = 100,
            suffix = "%"
          )
        } else {
          # Quantiles or cumulative hazard: plain number
          estimateFun <- gtsummary::label_style_number(digits = d)
        }
      } else if (self$options$type == "cumhaz") {
        # Auto mode: override gtsummary default (which wrongly shows %)
        estimateFun <- gtsummary::label_style_sigfig()
      }

      tblArgs <- list(
        x = fits,
        times = timesVec,
        probs = probsVec
      )

      # type is only relevant for time-point estimates
      if (self$options$statistic == "times") {
        tblArgs$type <- self$options$type
      }

      if (!is.null(estimateFun)) {
        tblArgs$estimate_fun <- estimateFun
      }

      # Restore original variable labels via the native label argument
      if (length(strataClean) > 0) {
        tblArgs$label <- as.list(setNames(strata, strataClean))
      }

      # Core table -----------------------------------------------------------
      table <- runSafe(
        do.call(gtsummary::tbl_survfit, tblArgs),
        collector
      )

      # Pipeline: add_n and add_nevent ---------------------------------------
      if (self$options$addN) {
        table <- runSafe(gtsummary::add_n(table), collector)
      }

      if (self$options$addNEvent) {
        table <- runSafe(gtsummary::add_nevent(table), collector)
      }

      # Text formatting ------------------------------------------------------
      table <- applyTextFormatting(
        table,
        hasPvalue = FALSE,
        options = self$options
      )

      # Render and export ----------------------------------------------------
      renderHtml(table, self$results$tbl)

      if (self$options$export) {
        path <- resolveExportPath(self$options$path)
        exportDocx(table, path, self$options, self$results)
      }

      # Notices --------------------------------------------------------------
      displayNotices(collector, self$options, self$results)
    }
  )
)
