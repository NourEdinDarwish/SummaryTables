tblContinuousClass <- R6::R6Class(
  "tblContinuousClass",
  inherit = tblContinuousBase,
  private = list(
    .run = function() {
      # Guard ---------------------------------------------------------------
      contVar <- self$options$contVar
      varsCat <- self$options$varsCat

      if (is.null(contVar) || length(varsCat) == 0) {
        renderPlaceholder(
          "Add a continuous variable and at least one categorical variable to generate the table", # nolint
          self$results$tbl
        )
        self$results$status$setVisible(FALSE)
        return()
      }

      # Collector ---------------------------------------------------------
      collector <- newCollector()

      # Theme ---------------------------------------------------------------
      on.exit(gtsummary::reset_gtsummary_theme(), add = TRUE)
      applyTheme(
        journalOption = self$options$journal,
        languageOption = self$options$language,
        compactOption = self$options$compact
      )
      themeStrings <- getThemeStrings(self$options$journal)

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

      hasPvalue <- self$options$addPvalue

      table <- pipeAddQ(
        table,
        hasPvalue = hasPvalue,
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
        hasPvalue = hasPvalue,
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

      # Hide status indicator ------------------------------------------------
      self$results$status$setVisible(FALSE)
    }
  )
)
