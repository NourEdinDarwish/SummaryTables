tblContinuousClass <- R6::R6Class(
  "tblContinuousClass",
  inherit = tblContinuousBase,
  private = list(
    .run = function() {
      collector <- newCollector()

      # Theme ---------------------------------------------------------------
      on.exit(gtsummary::reset_gtsummary_theme(), add = TRUE)
      applyTheme(
        journalOption = self$options$journal,
        languageOption = self$options$language,
        compactOption = self$options$compact
      )
      themeStrings <- getThemeStrings(self$options$journal)

      # Guard ---------------------------------------------------------------
      contVar <- self$options$contVar
      varsCat <- self$options$varsCat

      if (is.null(contVar) || length(varsCat) == 0) {
        return(NULL)
      }

      # Data prep -----------------------------------------------------------
      data <- self$data

      groupVar <- self$options$groupVar
      hasGroupVar <- !is.null(groupVar)

      data[[contVar]] <- jmvcore::toNumeric(data[[contVar]])
      data[varsCat] <- lapply(data[varsCat], as.factor)

      if (hasGroupVar) {
        data[[groupVar]] <- as.factor(data[[groupVar]])
      }

      data <- sortByFreq(
        data = data,
        varsCat = varsCat,
        options = self$options
      )

      # Build arguments -----------------------------------------------------
      statisticArguments <- buildStatArgsCont(
        varsCat = varsCat,
        options = self$options,
        themeStrings = themeStrings
      )

      digitsArgument <- buildDigitsArgsCont(options = self$options)

      # Core table ----------------------------------------------------------
      table <- runSafe(
        gtsummary::tbl_continuous(
          data = data,
          variable = contVar,
          include = varsCat,
          by = groupVar,
          statistic = statisticArguments,
          digits = digitsArgument
        ),
        collector
      )

      # Pipeline ------------------------------------------------------------
      table <- pipeAddPCont(
        table,
        hasGroupVar = hasGroupVar,
        options = self$options,
        collector = collector
      )

      table <- pipeAddQ(
        table,
        hasGroupVar = TRUE,
        options = self$options,
        collector = collector
      )

      table <- pipeAddOverall(
        table,
        hasGroupVar = hasGroupVar,
        options = self$options,
        collector = collector
      )

      # Text formatting -----------------------------------------------------
      table <- applyTextFormatting(
        table,
        hasGroupVar = TRUE,
        options = self$options
      )

      # Render and export ---------------------------------------------------
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
