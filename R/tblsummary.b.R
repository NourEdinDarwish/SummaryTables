tblSummaryClass <- R6::R6Class(
  "tblSummaryClass",
  inherit = tblSummaryBase,
  private = list(
    .run = function() {
      # Guard ---------------------------------------------------------------
      varsCont <- self$options$varsCont
      varsCat <- self$options$varsCat
      hasCont <- length(varsCont) > 0
      hasCat <- length(varsCat) > 0

      if (!hasCont && !hasCat) {
        renderPlaceholder(
          "Add continuous or categorical variables to generate the table",
          self$results$tbl
        )
        self$results$status$setVisible(FALSE)
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
      themeStrings <- getThemeStrings(self$options$journal)

      # Data prep -----------------------------------------------------------
      data <- self$data

      orderState <- trackVariableOrder(
        savedState = self$results$tbl$state,
        varsCat,
        varsCont
      )
      allVars <- orderState$vars
      self$results$tbl$setState(orderState)

      groupVar <- self$options$groupVar
      hasGroupVar <- !is.null(groupVar)

      if (hasCont) {
        data[varsCont] <- lapply(data[varsCont], jmvcore::toNumeric)
      }

      if (hasCat) {
        data[varsCat] <- lapply(data[varsCat], as.factor)
      }

      if (hasGroupVar) {
        data[[groupVar]] <- as.factor(data[[groupVar]])
      }

      data <- sortByFreq(
        data = data,
        varsCat = varsCat,
        options = self$options
      )

      # Build arguments -----------------------------------------------------
      typeArguments <- buildTypeArgs(
        data = data,
        varsCont = varsCont,
        varsCat = varsCat,
        groupVar = groupVar,
        hasGroupVar = hasGroupVar,
        options = self$options
      )

      statisticArguments <- buildStatArgs(
        varsCont = varsCont,
        varsCat = varsCat,
        options = self$options,
        themeStrings = themeStrings
      )

      digitsArguments <- buildDigitsArgs(options = self$options)

      # Core table ----------------------------------------------------------
      table <- runSafe(
        gtsummary::tbl_summary(
          data = data,
          include = allVars,
          by = groupVar,
          type = typeArguments$type,
          value = typeArguments$dichotValue,
          statistic = statisticArguments,
          digits = digitsArguments,
          missing = self$options$missing,
          missing_text = self$options$missingText,
          missing_stat = switch(
            self$options$missingStat,
            n = "{N_miss}",
            nPercent = paste0(
              "{N_miss} ({p_miss}",
              themeStrings$pctSuffix,
              ")"
            ),
            percent = paste0("{p_miss}", themeStrings$pctSuffix)
          ),
          percent = if (hasGroupVar) self$options$percent else "column"
        ),
        collector
      )

      # IQR label patch -----------------------------------------------------
      if (themeStrings$iqrJournals) {
        table <- gtsummary::modify_table_body(table, function(tbl) {
          tbl$label <- gsub("Q1 \U2013 Q3", "IQR", tbl$label)
          tbl
        })
      }

      # Pipeline ------------------------------------------------------------
      varsDichot <- names(typeArguments$dichotValue)

      table <- pipeAddDifference(
        table,
        varsCont = varsCont,
        varsDichot = varsDichot,
        hasGroupVar = hasGroupVar,
        options = self$options,
        collector = collector
      )

      table <- pipeAddP(
        table,
        varsCont = varsCont,
        varsCat = varsCat,
        hasGroupVar = hasGroupVar,
        options = self$options,
        collector = collector
      )

      hasPvalue <- hasGroupVar &&
        (self$options$addPvalue || self$options$addDifference)

      table <- pipeAddQ(
        table,
        hasPvalue = hasPvalue,
        options = self$options,
        collector = collector
      )

      table <- pipeAddCi(
        table,
        varsCont = varsCont,
        varsCat = varsCat,
        options = self$options,
        themeStrings = themeStrings,
        collector = collector
      )

      table <- pipeAddN(table, options = self$options, collector = collector)

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

      # Render --------------------------------------------------------------
      renderHtml(table, self$results$tbl)

      # Notices -------------------------------------------------------------
      displayNotices(collector, self$options, self$results)

      # Hide status indicator -----------------------------------------------
      self$results$status$setVisible(FALSE)
    }
  )
)
