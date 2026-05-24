tblSurvfitClass <- R6::R6Class(
  "tblSurvfitClass",
  inherit = tblSurvfitBase,
  private = list(
    .init = function() {
      elapsed <- self$options$elapsed
      event <- self$options$event

      if (is.null(elapsed) || is.null(event)) {
        renderPlaceholder(
          "Add a Time variable and an Event variable to generate the table",
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

      if (is.null(elapsed) || is.null(event)) {
        return()
      }

      # B64 encoding map ----------------------------------------------------
      strata <- self$options$strata
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
          data[strata] <- lapply(data[strata], as.factor)

          # Encode data to B64
          data <- encodeB64(data)
          elapsedB64 <- jmvcore::toB64(elapsed)
          eventB64 <- jmvcore::toB64(event)
          strataB64 <- if (length(strata) > 0) {
            jmvcore::toB64(strata)
          } else {
            character(0)
          }

          # Parse times / probs -------------------------------------------------
          if (self$options$statistic == "times") {
            timesVec <- parseCommaNumeric(self$options$times)
            probsVec <- NULL
          } else {
            probsVec <- 0.5
            timesVec <- NULL
          }

          # Build survfit objects -----------------------------------------------
          confInt <- self$options$confLevel / 100

          fits <- runSafe(
            buildSurvfitList(
              data = data,
              elapsedB64 = elapsedB64,
              eventB64 = eventB64,
              strataB64 = strataB64,
              confInt = confInt
            ),
            collector
          )

          # Build arguments -----------------------------------------------------
          estimateFun <- NULL

          # Rounding Numbers
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

          headers <- buildSurvfitHeader(
            statistic = self$options$statistic,
            type = self$options$type,
            confLevel = self$options$confLevel,
            timeSuffix = self$options$timeSuffix,
            nTimes = length(timesVec)
          )
          tblArgs$label_header <- headers$label_header

          # Core table ----------------------------------------------------------
          table <- runSafe(
            do.call(gtsummary::tbl_survfit, tblArgs),
            collector
          )

          # Spanning header for multiple time points
          if (!is.null(headers$spanning_header)) {
            table <- runSafe(
              gtsummary::modify_spanning_header(
                table,
                gtsummary::all_stat_cols() ~ headers$spanning_header
              ),
              collector
            )
          }

          # Add translated CI abbreviation to table footer (source note)
          # We use modify_abbreviation so it matches gtsummary's native
          # translation behavior
          table <- runSafe(
            gtsummary::modify_abbreviation(
              table,
              gtsummary:::translate_string("CI = Confidence Interval")
            ),
            collector
          )

          # Pipeline ------------------------------------------------------------
          if (self$options$addN) {
            table <- runSafe(gtsummary::add_n(table), collector)
          }

          if (self$options$addNEvent) {
            table <- runSafe(gtsummary::add_nevent(table), collector)
          }

          table <- pipeAddPSurvfit(
            table,
            strata = strataB64,
            options = self$options,
            collector = collector
          )

          hasPvalue <- self$options$addPvalue && length(strata) > 0

          table <- pipeAddQ(
            table,
            hasPvalue = hasPvalue,
            options = self$options,
            collector = collector
          )

          # Text formatting -----------------------------------------------------
          table <- applyTextFormatting(
            table,
            hasPvalue = hasPvalue,
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
