# Monkey-patch for cardx::ard_car_vif
#
# The current cardx version on the module does not pass `variable_names` to
# broom.helpers::.clean_backticks(), which causes non-syntactic variable names
# (e.g. names with spaces) to be silently dropped.
#
# This patch replaces the function in the cardx namespace at runtime so
# gtsummary::add_vif() picks up the fixed version automatically.
#
# TODO: Remove this file once cardx ships the fix and jamovi updates its dplyr
# version to use v1.2.0 as the new cardx version would require newer cards
# version which requires this version of dplyr which jamovi controls
# ---------------------------------------------------------------------------

.patchArdCarVif <- function() {
  # Only patch once per session — .patchEnv persists in the package namespace
  if (isTRUE(.patchEnv$vif_patched)) {
    return()
  }

  patched <- function(x, ...) {
    set_cli_abort_call()

    # check installed packages ---------------------------------------------------
    check_pkg_installed(c("car", "broom.helpers"))

    # check inputs ---------------------------------------------------------------
    check_not_missing(x)

    vif <- cards::eval_capture_conditions(car::vif(x, ...))

    # if vif failed, set result as NULL, error will be kept through eval_capture_conditions()
    if (is.null(vif$result)) {
      # try to capture variable names from `terms()`
      lst_terms <- cards::eval_capture_conditions(attr(
        stats::terms(x),
        "term.labels"
      ))
      # we cannot get variable names, error out
      if (!is.null(lst_terms[["error"]])) {
        cli::cli_abort(
          c(
            "There was an error running {.fun car::vif}. See below.",
            x = vif[["error"]]
          ),
          call = get_cli_abort_call()
        )
      }
      vif$result <- dplyr::tibble(
        variable = lst_terms[["result"]],
        VIF = list(NULL),
        GVIF = list(NULL),
        aGVIF = list(NULL),
        df = list(NULL)
      )
    } else if (!is.matrix(vif$result)) {
      # if VIF is returned
      vif$result <- dplyr::tibble(
        variable = names(vif$result),
        VIF = vif$result
      )
    } else if (is.matrix(vif$result)) {
      # if Generalized VIF is returned
      vif$result <-
        vif$result |>
        as.data.frame() %>%
        dplyr::mutate(., variable = rownames(.), .before = 1L) |>
        dplyr::rename(
          aGVIF = "GVIF^(1/(2*Df))",
          df = "Df"
        ) |>
        dplyr::tibble()
    }

    # Clean-up the result to fit the ard structure through pivot
    # FIX ADDED HERE: pass variable_names to .clean_backticks() so non-syntactic
    # names (e.g. with spaces) are matched correctly instead of dropped.
    vif$result <-
      vif$result |>
      dplyr::mutate(
        variable = broom.helpers::.clean_backticks(
          .data$variable,
          variable_names = broom.helpers::model_list_variables(
            x,
            only_variable = TRUE
          )
        )
      ) |>
      tidyr::pivot_longer(
        cols = -c("variable"),
        names_to = "stat_name",
        values_to = "stat"
      ) |>
      dplyr::mutate(
        context = "car_vif",
        stat = as.list(.data$stat),
        stat_label = ifelse(
          .data$stat_name == "aGVIF",
          "Adjusted GVIF",
          .data$stat_name
        ),
        fmt_fun = map(
          .data$stat,
          function(.x) {
            # styler: off
            if (is.integer(.x)) {
              return(0L)
            }
            if (is.numeric(.x)) {
              return(1L)
            }
            # styler: on
            NULL
          }
        )
      )

    # Bind the results and possible warning/errors together
    vif_return <- dplyr::tibble(
      vif$result,
      warning = vif["warning"],
      error = vif["error"]
    )

    # Clean up return object
    # FIX ADDED HERE: removed `check=FALSE` from cards::as_card() because the
    # older version of `cards` used in jamovi does not support this argument.
    vif_return |>
      cards::as_card() |>
      cards::tidy_ard_column_order()
  }

  # Set environment to cardx namespace so internal helpers resolve
  environment(patched) <- asNamespace("cardx")
  assignInNamespace("ard_car_vif", patched, ns = "cardx")
  .patchEnv$vif_patched <- TRUE
}

# Persists in package namespace across analysis runs (R process stays alive)
.patchEnv <- new.env(parent = emptyenv())
