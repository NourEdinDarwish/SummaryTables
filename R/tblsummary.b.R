tblSummaryClass <- R6::R6Class(
  "tblSummaryClass",
  inherit = tblSummaryBase,
  private = list(

    # ── Entry point ──────────────────────────────────────────────────────────
    .run = function() {
      collector <- newCollector()

      # ── 0. Theme ───────────────────────────────────────────────────────────
      on.exit(gtsummary::reset_gtsummary_theme(), add = TRUE)
      applyTheme(
        journalOption = self$options$journal,
        languageOption = self$options$language,
        compactOption = self$options$compact
      )
      # Single source of truth for all theme-aware formatting strings
      ts <- getThemeStrings(self$options$journal)

      # ── 1. Guard ───────────────────────────────────────────────────────────
      varsCont <- self$options$varsCont
      varsCat <- self$options$varsCat
      hasCont <- length(varsCont) > 0
      hasCat <- length(varsCat) > 0

      if (!hasCont && !hasCat) {
        displayNotices(collector, self$options, self$results)
        return(NULL)
      }

      # ── 2. Data prep ───────────────────────────────────────────────────────
      data <- self$data
      allVars <- c(varsCat, varsCont)
      byVariable <- self$options$groupBy
      hasByVar <- !is.null(byVariable)

      # Jamovi stores all data as factors; convert continuous vars to numeric
      if (hasCont) {
        for (v in varsCont) {
          data[[v]] <- jmvcore::toNumeric(data[[v]])
        }
      }

      # ── 3. Type arguments ──────────────────────────────────────────────────
      # "continuous2" is needed for EDA (multi-row) display
      defaultType <- if (self$options$statContDefault == "eda") {
        "continuous2"
      } else {
        "continuous"
      }
      typeArgsBuilder <- stats::setNames(
        rep(defaultType, length(varsCont)),
        varsCont
      )

      for (item in self$options$statsContSpecific) {
        if (item$var %in% varsCont && item$stat != "use_default") {
          typeArgsBuilder[[item$var]] <- if (item$stat == "eda") {
            "continuous2"
          } else {
            "continuous"
          }
        }
      }

      typeArguments <- as.list(c(
        typeArgsBuilder,
        stats::setNames(rep("categorical", length(varsCat)), varsCat)
      ))

      # ── 4. Statistic arguments ─────────────────────────────────────────────
      # Lookup: option name -> glue format string, using centralised ts values
      statStrings <- list(
        continuous = list(
          meanSd = "{mean} ({sd})",
          medianIqr = paste0("{median} ({p25}", ts$iqrSep, "{p75})"),
          medianRange = paste0("{median} ({min}", ts$rangeSep, "{max})"),
          eda = c(
            paste0("{median} ({p25}", ts$iqrSep, "{p75})"),
            "{mean} ({sd})",
            paste0("{min}", ts$rangeSep, "{max}")
          ),
          mean = "{mean}",
          median = "{median}"
        ),
        categorical = list(
          nPercent = paste0("{n} ({p}", ts$pctSuffix, ")"),
          n = "{n}",
          percent = paste0("{p}", ts$pctSuffix)
        )
      )

      statisticArguments <- list()

      # Default continuous
      contStr <- statStrings$continuous[[self$options$statContDefault]]
      if (!is.null(contStr)) {
        statisticArguments <- c(
          statisticArguments,
          list(gtsummary::all_continuous() ~ contStr)
        )
      }

      # Default categorical
      catStr <- statStrings$categorical[[self$options$statCatDefault]]
      if (!is.null(catStr)) {
        statisticArguments <- c(
          statisticArguments,
          list(gtsummary::all_categorical() ~ catStr)
        )
      }

      # Per-variable continuous overrides
      for (item in self$options$statsContSpecific) {
        if (item$stat != "use_default" && item$var %in% varsCont) {
          val <- statStrings$continuous[[item$stat]]
          if (!is.null(val)) statisticArguments[[item$var]] <- val
        }
      }

      # Per-variable categorical overrides
      for (item in self$options$statsCatSpecific) {
        if (item$stat != "use_default" && item$var %in% varsCat) {
          val <- statStrings$categorical[[item$stat]]
          if (!is.null(val)) statisticArguments[[item$var]] <- val
        }
      }

      # ── 5. Digits arguments ────────────────────────────────────────────────
      digitsArguments <- list()

      if (self$options$digitsCont != "auto") {
        digitsArguments <- c(
          digitsArguments,
          list(
            gtsummary::all_continuous() ~ as.integer(self$options$digitsCont)
          )
        )
      }
      if (self$options$digitsPct != "auto") {
        digitsArguments <- c(
          digitsArguments,
          list(
            gtsummary::all_categorical() ~ c(
              0L,
              as.integer(self$options$digitsPct)
            )
          )
        )
      }

      # ── 6. Sort arguments ──────────────────────────────────────────────────
      sortArguments <- list(
        gtsummary::all_categorical(FALSE) ~ self$options$sortCatDefault
      )

      for (item in self$options$sortCatSpecific) {
        if (item$sort != "use_default" && item$var %in% varsCat) {
          sortArguments[[item$var]] <- item$sort
        }
      }

      # ── 7. tbl_summary ─────────────────────────────────────────────────────
      table <- runSafe(
        {
          gtsummary::tbl_summary(
            data = data,
            include = allVars,
            by = byVariable,
            type = typeArguments,
            statistic = statisticArguments,
            digits = digitsArguments,
            missing = self$options$missing,
            missing_text = self$options$missingText,
            missing_stat = switch(
              self$options$missingStat,
              n = "{N_miss}",
              nPercent = paste0("{N_miss} ({p_miss}", ts$pctSuffix, ")"),
              percent = paste0("{p_miss}", ts$pctSuffix)
            ),
            percent = self$options$percent,
            sort = sortArguments
          )
        },
        collector
      )

      # ── 8. IQR label patch for continuous2 (EDA) ──────────────────────────
      # JAMA/NEJM/Lancet themes rename "Q1 – Q3" → "IQR" via gsub on the
      # stat_label column, but for continuous2 that column is always NA —
      # the label text lives in table_body$label for level rows. Patch it here.
      if (!is.null(table) && ts$iqrJournals) {
        table <- gtsummary::modify_table_body(table, function(tb) {
          tb$label <- gsub("Q1 \U2013 Q3", "IQR", tb$label)
          tb
        })
      }

      # ── 9. add_p ───────────────────────────────────────────────────────────
      if (!is.null(table) && self$options$pValue && hasByVar) {
        # Parametric default: set via theme elements (same mechanism as
        # theme_gtsummary_mean_sd). Only set when user chose parametric;
        # nonparametric is gtsummary's built-in default.
        if (self$options$testContDefault == "parametric") {
          gtsummary::set_gtsummary_theme(list(
            "add_p.tbl_summary-attr:test.continuous_by2" = "t.test",
            "add_p.tbl_summary-attr:test.continuous" = "oneway.test"
          ))
        }

        # Per-variable test overrides
        by2 <- hasByVar &&
          byVariable %in% names(self$data) &&
          length(unique(self$data[[byVariable]])) == 2

        testArguments <- list()

        for (item in self$options$testsContSpecific) {
          if (item$var %in% varsCont && item$test != "use_default") {
            testArguments[[item$var]] <- switch(
              item$test,
              parametric = if (by2) "t.test" else "oneway.test",
              nonparametric = if (by2) "wilcox.test" else "kruskal.test",
              item$test
            )
          }
        }

        defaultCat <- self$options$testCatDefault
        for (item in self$options$testsCatSpecific) {
          if (item$var %in% varsCat) {
            selection <- if (item$test == "use_default") {
              defaultCat
            } else {
              item$test
            }
            if (selection != "auto") testArguments[[item$var]] <- selection
          }
        }

        pvDigits <- self$options$digitsPvalue
        table <- runSafe(
          {
            if (pvDigits != "auto") {
              gtsummary::add_p(
                table,
                test = testArguments,
                pvalue_fun = gtsummary::label_style_pvalue(
                  digits = as.integer(pvDigits)
                )
              )
            } else {
              gtsummary::add_p(table, test = testArguments)
            }
          },
          collector
        )

        # ── 9a. add_q ─────────────────────────────────────────────────────
        if (!is.null(table) && isTRUE(self$options$addQ)) {
          table <- runSafe(
            gtsummary::add_q(table, method = self$options$qMethod),
            collector
          )
        }
      }

      # ── 10. add_ci ─────────────────────────────────────────────────────────
      if (!is.null(table) && isTRUE(self$options$addCi)) {
        # Default + per-variable method overrides
        ciMethodArgs <- list(
          gtsummary::all_continuous() ~ self$options$ciMethodContDefault
        )
        for (item in self$options$ciMethodsContSpecific) {
          if (item$method != "use_default" && item$var %in% varsCont) {
            ciMethodArgs[[item$var]] <- item$method
          }
        }
        ciMethodArgs <- c(
          ciMethodArgs,
          list(gtsummary::all_categorical() ~ self$options$ciMethodCatDefault)
        )
        for (item in self$options$ciMethodsCatSpecific) {
          if (item$method != "use_default" && item$var %in% varsCat) {
            ciMethodArgs[[item$var]] <- item$method
          }
        }

        # Square-bracket wrapping when any selected stat already uses parens
        statsWithParens <- c(
          "meanSd",
          "medianIqr",
          "medianRange",
          "nPercent",
          "eda"
        )
        selectedStats <- c(
          self$options$statContDefault,
          self$options$statCatDefault,
          vapply(self$options$statsContSpecific, \(x) x$stat, character(1)),
          vapply(self$options$statsCatSpecific, \(x) x$stat, character(1))
        )
        useSquareBrackets <- any(selectedStats %in% statsWithParens)

        ciPattern <- if (isTRUE(self$options$ciCombine)) {
          if (useSquareBrackets) "{stat} [{ci}]" else "{stat} ({ci})"
        } else {
          NULL
        }

        # CI statistic format — use centralised ts values
        ciStatArg <- list(
          gtsummary::all_continuous() ~ paste0(
            "{conf.low}",
            ts$ciSep,
            "{conf.high}"
          ),
          gtsummary::all_categorical() ~ paste0(
            "{conf.low}",
            ts$ciPctSuffix,
            ts$ciSep,
            "{conf.high}",
            ts$ciPctSuffix
          )
        )

        # Style overrides when user picks explicit decimal places
        ciStyleFun <- list()
        if (self$options$ciDigitsCont != "auto") {
          ciStyleFun <- c(
            ciStyleFun,
            list(
              gtsummary::all_continuous() ~ gtsummary::label_style_number(
                digits = as.integer(self$options$ciDigitsCont)
              )
            )
          )
        }
        if (self$options$ciDigitsCat != "auto") {
          ciStyleFun <- c(
            ciStyleFun,
            list(
              gtsummary::all_categorical() ~ gtsummary::label_style_number(
                digits = as.integer(self$options$ciDigitsCat),
                scale = 100
              )
            )
          )
        }

        table <- runSafe(
          {
            args <- list(
              x = table,
              method = ciMethodArgs,
              statistic = ciStatArg,
              conf.level = self$options$confLevel / 100,
              pattern = ciPattern
            )
            if (length(ciStyleFun) > 0) {
              args$style_fun <- ciStyleFun
            }
            do.call(gtsummary::add_ci, args)
          },
          collector
        )
      }

      # ── 11. add_n ──────────────────────────────────────────────────────────
      if (!is.null(table) && isTRUE(self$options$addN)) {
        table <- gtsummary::add_n(
          table,
          last = isTRUE(self$options$addNLast),
          footnote = isTRUE(self$options$addNFootnote)
        )
      }

      # ── 12. add_overall ────────────────────────────────────────────────────
      if (
        !is.null(table) &&
          isTRUE(self$options$addOverall) &&
          hasByVar
      ) {
        table <- runSafe(
          gtsummary::add_overall(
            table,
            last = isTRUE(self$options$addOverallLast)
          ),
          collector
        )
      }

      # ── 13. Text formatting ────────────────────────────────────────────────
      if (!is.null(table)) {
        if (isTRUE(self$options$boldLabels)) {
          table <- gtsummary::bold_labels(table)
        }
        if (isTRUE(self$options$boldLevels)) {
          table <- gtsummary::bold_levels(table)
        }
        if (isTRUE(self$options$italicizeLabels)) {
          table <- gtsummary::italicize_labels(table)
        }
        if (isTRUE(self$options$italicizeLevels)) {
          table <- gtsummary::italicize_levels(table)
        }

        if (
          isTRUE(self$options$boldP) &&
            self$options$pValue &&
            hasByVar
        ) {
          table <- gtsummary::bold_p(table, t = self$options$boldPThreshold)
        }
        if (
          isTRUE(self$options$boldQ) &&
            isTRUE(self$options$addQ) &&
            self$options$pValue &&
            hasByVar
        ) {
          table <- gtsummary::bold_p(
            table,
            t = self$options$boldQThreshold,
            q = TRUE
          )
        }
        if (
          isTRUE(self$options$separatePFootnotes) &&
            self$options$pValue &&
            hasByVar
        ) {
          table <- gtsummary::separate_p_footnotes(table)
        }
      }

      # ── 14. Render & export ────────────────────────────────────────────────
      if (!is.null(table)) {
        renderHtml(table, self$results$tbl)

        if (isTRUE(self$options$export)) {
          path <- resolveExportPath(self$options$path)
          if (!is.null(path)) {
            exportDocx(table, path, self$options, self$results)
          }
        }
      }

      displayNotices(collector, self$options, self$results)
    }
  )
)
