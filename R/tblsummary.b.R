tblSummaryClass <- R6::R6Class(
  "tblSummaryClass",
  inherit = tblSummaryBase,
  private = list(
    .collector = NULL,

    # ── Entry point ──────────────────────────────────────────────────
    .run = function() {
      private$.collector <- newCollector()

      on.exit(resetTheme(), add = TRUE)
      applyTheme(
        journalOption = self$options$journal,
        languageOption = self$options$language,
        compactOption = self$options$compact
      )

      table <- private$.buildTable()

      if (!is.null(table)) {
        renderHtml(table, self$results$tbl)

        if (isTRUE(self$options$export)) {
          path <- resolveExportPath(self$options$path)
          if (!is.null(path)) {
            exportDocx(
              table,
              path,
              self$options,
              self$results
            )
          }
        }
      }

      displayNotices(private$.collector, self$options, self$results)
    },

    # ── Core table construction ──────────────────────────────────────
    .buildTable = function() {
      varsCont <- self$options$varsCont
      varsCat <- self$options$varsCat

      if (length(varsCont) == 0 && length(varsCat) == 0) {
        return(NULL)
      }

      data <- self$data
      allVars <- c(varsCat, varsCont)

      # Type assignment (must be a named list for gtsummary)
      typeArguments <- as.list(c(
        stats::setNames(rep("continuous", length(varsCont)), varsCont),
        stats::setNames(rep("categorical", length(varsCat)), varsCat)
      ))

      # Delegated argument builders
      statisticArguments <- private$.constructStatArgs(varsCont, varsCat)
      digitsArguments <- private$.constructDigitsArgs(varsCont, varsCat)
      sortArguments <- private$.constructSortArgs(varsCat)
      byVariable <- self$options$groupBy

      # Theme-aware percent suffix (shared logic with .constructStatArgs)
      pctSuffix <- if (self$options$journal %in% c("jama", "nejm")) "" else "%"

      # Build the gtsummary table
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
              nPercent = paste0("{N_miss} ({p_miss}", pctSuffix, ")"),
              percent = paste0("{p_miss}", pctSuffix)
            ),
            percent = self$options$percent,
            sort = sortArguments
          )
        },
        private$.collector
      )

      # Add p-values if requested AND grouping variable is present
      if (!is.null(table) && self$options$pValue && !is.null(byVariable)) {
        # Set continuous test defaults via theme elements
        # (same mechanism as theme_gtsummary_mean_sd)
        # Source: gtsummary/R/theme_gtsummary.R L424-434
        contDefault <- self$options$testContDefault
        if (contDefault == "parametric") {
          gtsummary::set_gtsummary_theme(list(
            "add_p.tbl_summary-attr:test.continuous_by2" = "t.test",
            "add_p.tbl_summary-attr:test.continuous" = "oneway.test"
          ))
        }
        # "nonparametric" needs no theme — gtsummary defaults are already
        # wilcox.test (2 groups) and kruskal.test (3+ groups)

        testArguments <- private$.constructTestArgs(varsCont, varsCat)
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
          private$.collector
        )
      }

      # Add N column
      if (!is.null(table) && isTRUE(self$options$addN)) {
        table <- gtsummary::add_n(
          table,
          last = isTRUE(self$options$addNLast),
          footnote = isTRUE(self$options$addNFootnote)
        )
      }

      # Add Overall column (only when groupBy is present)
      if (
        !is.null(table) &&
          isTRUE(self$options$addOverall) &&
          !is.null(self$options$groupBy)
      ) {
        table <- runSafe(
          gtsummary::add_overall(
            table,
            last = isTRUE(self$options$addOverallLast)
          ),
          private$.collector
        )
      }

      # Apply text formatting
      if (!is.null(table)) {
        table <- private$.applyFormatting(table)
      }

      table
    },

    # ── Text formatting ──────────────────────────────────────────────
    .applyFormatting = function(table) {
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

      # Bold significant p-values (only when p-values are shown)
      if (
        isTRUE(self$options$boldP) &&
          self$options$pValue &&
          !is.null(self$options$groupBy)
      ) {
        table <- gtsummary::bold_p(
          table,
          t = self$options$boldPThreshold
        )
      }

      # Separate p-value footnotes (one per test, instead of combined)
      if (
        isTRUE(self$options$separatePFootnotes) &&
          self$options$pValue &&
          !is.null(self$options$groupBy)
      ) {
        table <- gtsummary::separate_p_footnotes(table)
      }

      table
    },

    # ── Argument builders ────────────────────────────────────────────

    .constructDigitsArgs = function(varsCont, varsCat) {
      contDigits <- self$options$digitsCont
      pctDigits <- self$options$digitsPct

      args <- list()

      # Continuous: only pass when user explicitly chose (not "auto")
      if (contDigits != "auto") {
        args <- c(
          args,
          list(gtsummary::all_continuous() ~ as.integer(contDigits))
        )
      }

      # Categorical: only pass when user explicitly chose (not "auto")
      if (pctDigits != "auto") {
        args <- c(
          args,
          list(gtsummary::all_categorical() ~ c(0L, as.integer(pctDigits)))
        )
      }

      args
    },

    .constructSortArgs = function(varsCat) {
      args <- list()

      sortDefault <- self$options$sortCatDefault
      args <- c(args, list(gtsummary::all_categorical(FALSE) ~ sortDefault))

      for (item in self$options$sortCatSpecific) {
        if (item$sort != "use_default" && item$var %in% varsCat) {
          args[[item$var]] <- item$sort
        }
      }

      args
    },

    .constructStatArgs = function(varsCont, varsCat) {
      journal <- self$options$journal

      # Theme-aware style decisions
      iqrSep <- if (journal %in% c("jama", "nejm", "lancet")) {
        " \U2013 "
      } else {
        ", "
      }
      rangeSep <- if (journal %in% c("jama", "nejm", "lancet")) {
        " \U2013 "
      } else {
        ", "
      }
      pctSuffix <- if (journal %in% c("jama", "nejm")) "" else "%"

      # Lookup tables: option name -> glue format string
      statStrings <- list(
        continuous = c(
          meanSd = "{mean} ({sd})",
          medianIqr = paste0("{median} ({p25}", iqrSep, "{p75})"),
          medianRange = paste0("{median} ({min}", rangeSep, "{max})")
        ),
        categorical = c(
          nPercent = paste0("{n} ({p}", pctSuffix, ")"),
          n = "{n}",
          percent = paste0("{p}", pctSuffix)
        )
      )

      args <- list()

      # Default continuous
      contStr <- statStrings$continuous[self$options$statContDefault]
      if (!is.na(contStr)) {
        args <- c(args, list(gtsummary::all_continuous() ~ contStr))
      }

      # Default categorical
      catStr <- statStrings$categorical[self$options$statCatDefault]
      if (!is.na(catStr)) {
        args <- c(args, list(gtsummary::all_categorical() ~ catStr))
      }

      # Per-variable overrides — Continuous
      for (item in self$options$statsContSpecific) {
        if (item$stat != "use_default" && item$var %in% varsCont) {
          val <- statStrings$continuous[item$stat]
          if (!is.na(val)) args[[item$var]] <- val
        }
      }

      # Per-variable overrides — Categorical
      for (item in self$options$statsCatSpecific) {
        if (item$stat != "use_default" && item$var %in% varsCat) {
          val <- statStrings$categorical[item$stat]
          if (!is.na(val)) args[[item$var]] <- val
        }
      }

      args
    },

    # Build per-variable test overrides only.
    # Default test selection is handled via theme elements before add_p().
    .constructTestArgs = function(varsCont, varsCat) {
      testArguments <- list()

      # Group count needed to resolve parametric/nonparametric per-variable
      # shortcuts
      groupBy <- self$options$groupBy
      by2 <- FALSE
      if (!is.null(groupBy) && groupBy %in% names(self$data)) {
        by2 <- length(unique(self$data[[groupBy]])) == 2
      }

      # Continuous: per-variable overrides only
      # "parametric"/"nonparametric" are shortcuts that resolve based on group
      # count
      for (item in self$options$testsContSpecific) {
        if (item$var %in% varsCont && item$test != "use_default") {
          testArguments[[item$var]] <- switch(
            item$test,
            parametric = if (by2) "t.test" else "oneway.test",
            nonparametric = if (by2) "wilcox.test" else "kruskal.test",
            item$test # specific test name passed through
          )
        }
      }

      # Categorical: per-variable overrides only
      defaultCat <- self$options$testCatDefault
      for (item in self$options$testsCatSpecific) {
        if (item$var %in% varsCat) {
          selection <- if (item$test == "use_default") defaultCat else item$test
          if (selection != "auto") {
            testArguments[[item$var]] <- selection
          }
        }
      }

      testArguments
    }
  )
)
