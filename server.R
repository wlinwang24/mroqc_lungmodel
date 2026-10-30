library(shiny)
library(ggplot2)
library(ggpattern)
library(scales)

## ============================================================
## Self-contained model configuration (original visual design retained).
## The previous release failed when model_config.R was not copied
## beside server.R. This version embeds the same configuration,
## so only ui.R and server.R are required to START the Shiny app.
## Optional: fg_model_specs.R is loaded if present; it must contain
## FITTED approximate-dose Fine-Gray model specifications.
## ============================================================
## ============================================================
## MROQC SHINY MODEL CONFIGURATION
## ============================================================
## This file is intentionally explicit about unavailable parameters.
## Do not replace NA intercepts with an overall observed event rate.
## All doses entered into the app are PHYSICAL Gy.

## G2+ Model 1: SAS PROC LOGISTIC coefficients supplied 2026-10-15.
## Age is UNcentered; esophageal doses use PHYSICAL Gy.
## IMPORTANT: SAS CLASS effect coding (-1 for reference level 0,
## +1 for level 1) explains OR = exp(2 * beta) for concurrent
## chemotherapy and ECOG >=2. The model matrix MUST transform
## both binary indicators to 2*x - 1 before multiplying by the
## unchanged SAS coefficients and original SAS intercept.
## Verify the SAS Class Level Information confirms level 1 is +1.
## G3+ Model 3: original SAS PROC LOGISTIC parameter estimates,
## supplied 2026-10-15 (not rounded ORs from the published table).
## The ECOG odds ratio 2.605 is exp(2 * 0.4786), confirming effect coding.
## We assume level 1 -> +1 and level 0 -> -1; verify the SAS class
## design matrix if available. The original intercept is -4.3672.
ESO_SPECS <- list(
  g2 = list(
    label = "Grade >=2 acute esophagitis (Model 1, SAS coefficients)",
    intercept = -0.3568,
    beta = c(
      age = -0.0271,
      concurrent_chemo = 0.3959,
      ecog2 = 0.2356,
      esoph_mean_phys = 0.0278,
      esoph_d2cc_phys = 0.0271
    ),
    coefficients_are_rounded = TRUE,
    effect_coded_vars = c("concurrent_chemo", "ecog2")
  ),
  g3 = list(
    label = "Grade >=3 acute esophagitis (Model 3, SAS coefficients)",
    intercept = -4.3672,
    beta = c(
      age = -0.0499,
      ecog2 = 0.4786,
      esoph_d2cc_phys = 0.0696
    ),
    coefficients_are_rounded = TRUE,
    effect_coded_vars = "ecog2"
  )
)

## Manuscript's model-development population: 30-fraction RT.
## Do not generalize to >=30 fractions without supporting evidence.
esophagitis_is_applicable <- function(number_of_fractions) {
  length(number_of_fractions) == 1L &&
    is.finite(number_of_fractions) &&
    number_of_fractions == 30
}

## The conversion functions below reproduce the equations in the
## uploaded 02_supplement_approx_mld(6).Rmd and
## 02_supplement_approx_mhd(2).Rmd, alpha/beta = 3 Gy.
## They calculate DOSE metrics only, not event risks.
valid_dose_input <- function(dose, n_fx) {
  is.numeric(dose) && length(dose) == 1L && is.finite(dose) && dose >= 0 &&
    is.numeric(n_fx) && length(n_fx) == 1L && is.finite(n_fx) &&
    n_fx >= 1 && n_fx == round(n_fx)
}

approx_eqd2_mld <- function(physical_mld, n_fx) {
  if (!valid_dose_input(physical_mld, n_fx)) return(NA_real_)
  lq <- physical_mld * (1 + physical_mld / (3 * n_fx)) / (1 + 2 / 3)
  (if (n_fx <= 5) 2.0 else 1.2) * lq
}

approx_eqd2_mhd <- function(physical_mhd, n_fx) {
  if (!valid_dose_input(physical_mhd, n_fx)) return(NA_real_)
  lq <- physical_mhd * (1 + physical_mhd / (3 * n_fx)) / (1 + 2 / 3)
  if (n_fx <= 5) {
    -0.001931 + 1.1878 * lq
  } else if (n_fx <= 20) {
    0.152859 + 1.1015 * lq
  } else {
    0.218754 + 1.1069 * lq
  }
}

APP_CONFIG <- list(
  lung_eqd2_converter = approx_eqd2_mld,
  heart_eqd2_converter = approx_eqd2_mhd,

  ## Explicit mapping: exact coefficient name in fg_model_specs.R
  ##                  = canonical app predictor name.
  ## Unknown coefficient names fail CLOSED (do not silently use zero).
  predictor_map = c(
    age = "age",
    PO2 = "oxygen",
    PO2Yes = "oxygen",                # Cardiac model: factor Yes vs No
    ECOG_Base_G2 = "ecog2",
    ECOG_Base_G2Yes = "ecog2",        # Cardiac model: factor Yes vs No
    copd_base = "copd",
    comorbidity = "cardiac_comorbidity",
    comorbidityYes = "cardiac_comorbidity",  # Cardiac model: factor Yes vs No
    SBRT = "sbrt",
    MLD_EQD2_approx = "lung_eqd2",
    ## mean_heart intentionally unmapped: physical/EQD2 unit ambiguous.
    mean_heart_phys = "heart_phys",
    mean_heart_eqd2 = "heart_eqd2",
    MHD_EQD2_approx2 = "heart_eqd2",
    concurrent_chemo = "concurrent_chemo",
    concurrent_chemotherapy = "concurrent_chemo",
    ptv_heart_within_2cm = "ptv_near_heart",
    ptv_2cm_heartYes = "ptv_near_heart",    # Actual cardiac model.matrix column
    PTV_within_2cm_heart = "ptv_near_heart",
    PTV_2cm_Heart = "ptv_near_heart",
    GenderMale = "male",
    GenderM = "male",
    genderMale = "male",
    gender_male = "male",
    SexMale = "male",
    sexMale = "male"
  ),

  ## GenderMale aliases above assume Male=1 vs Female=0.
  ## Verify exact model.matrix() names and reference coding against fit.

  ## Source of FG specifications, required for actual lung/cardiac risks.
  fg_spec_filename = "fg_model_specs.R",
  lung_model_keys = c("anylung", "pneumonitis", "pneumonia"),
  cardiac_model_key = "anycardiac",
  ## Confirm clinical predictors: lung specifications remain unchanged,
  ## cardiac uses the revised six-variable clinical + dose model.
  ## These expected sets are from Tables in the uploaded MLD/MHD PDFs.
  expected_canonical_predictors = list(
    anylung = c("age", "oxygen", "ecog2", "copd", "male", "lung_eqd2"),
    pneumonitis = c("male", "lung_eqd2"),
    pneumonia = c("age", "oxygen", "ecog2", "copd", "male", "lung_eqd2"),
    ## COPD FINAL selected main model: clinical factors only; no MLD term.
    copd = c("age", "oxygen", "ecog2", "copd"),
    ## Updated cardiac model (Question 1, October 26): no PTV term.
    ## The App uses a new six-predictor model refit to CALIBRATED APPROXIMATE
    ## MHD so that dose input is consistent with the physical-MHD converter.
    anycardiac = c("age", "oxygen", "ecog2", "cardiac_comorbidity",
                   "male", "heart_eqd2")
  ),
  ## These model specs MUST be refitted with the selected variables.
  ## Fitted *approximate dose* models only. A fitted exact-EQD2 model
  ## must not be silently relabeled as an approximate-EQD2 model.
  expected_dose_terms = c(
    anylung = "MLD_EQD2_approx",
    pneumonitis = "MLD_EQD2_approx",
    pneumonia = "MLD_EQD2_approx",
    ## COPD uses a separate clinical-only fitted object and has no dose term.
    anycardiac = "MHD_EQD2_approx2"
  ),
  prediction_months = c(12, 24, 36)
)




## Keep loading failures visible, without preventing the app from opening.
load_fg_models <- function(filename) {
  if (!file.exists(filename)) {
    return(list(specs = list(), problem = paste0(
      "Missing ", filename,
      ". Copy the verified lung/cardiac model specifications into the app directory."
    )))
  }
  e <- new.env(parent = baseenv())
  problem <- tryCatch({
    sys.source(filename, envir = e)
    NULL
  }, error = function(err) conditionMessage(err))
  if (!is.null(problem)) {
    return(list(specs = list(), problem = paste("Unable to load FG models:", problem)))
  }
  if (!exists("fg_model_specs", envir = e, inherits = FALSE)) {
    return(list(specs = list(), problem = paste0(
      filename, " did not define fg_model_specs."
    )))
  }
  specs <- get("fg_model_specs", envir = e)
  if (!is.list(specs)) {
    return(list(specs = list(), problem = "fg_model_specs is not a list."))
  }
  list(specs = specs, problem = NULL)
}

FG <- load_fg_models(APP_CONFIG$fg_spec_filename)

## Main manuscript COPD model: clinical-factor-only fit (NOT the
## approximate-MLD sensitivity fit is excluded from fg_model_specs.R).
## Export model_copd_clinical_only.rds using export_copd_clinical.R.
COPD_CLINICAL_RDS <- "model_copd_clinical_only.rds"
load_copd_clinical_fit <- function(path = COPD_CLINICAL_RDS) {
  if (!file.exists(path)) return(list(spec = NULL, issue =
    "COPD clinical-only model has not been exported. Run export_copd_clinical.R from the main lung Rmd. The forced approximate-MLD sensitivity fit is intentionally not used for the main App."))
  obj <- tryCatch(readRDS(path), error = function(e) e)
  if (inherits(obj, "error")) return(list(spec = NULL, issue = conditionMessage(obj)))
  fit <- if (inherits(obj, "crr")) obj else obj$fit
  if (!inherits(fit, "crr") || !isTRUE(fit$converged) ||
      is.null(fit$uftime) || is.null(fit$bfitj)) {
    return(list(spec = NULL, issue = "Exported COPD clinical-only file does not contain a converged Fine-Gray fit with baseline hazards."))
  }
  if (!is.null(obj$final_dose_terms) && length(obj$final_dose_terms) > 0L) {
    return(list(spec = NULL, issue = "COPD export contains a selected dose term: expected the clinical-only model."))
  }
  list(spec = list(
    key = "copd", endpoint = "copd", dose_term = NULL,
    model_type = "clinical_only",
    coef_names = names(fit$coef), coef = unname(fit$coef),
    uftime = unname(fit$uftime), bfitj = unname(fit$bfitj)
  ), issue = NULL)
}
COPD_CLINICAL <- load_copd_clinical_fit()

safe_numeric <- function(x) {
  z <- suppressWarnings(as.numeric(x))
  if (length(z) != 1L || !is.finite(z)) return(NA_real_)
  z
}

valid_probability <- function(p) {
  length(p) == 1L && is.finite(p) && p >= 0 && p <= 1
}

## Display-only rounding; underlying risk probabilities are NOT rounded.
## sprintf avoids floating-point artifacts such as 1.4000000000000001%.
format_risk_pct <- function(p, digits = 0L) {
  if (!valid_probability(p)) return("—")
  if (!is.numeric(digits) || length(digits) != 1L || digits < 0L || digits > 3L) {
    stop("Invalid number of display digits")
  }
  sprintf(paste0("%.", as.integer(digits), "f%%"), 100 * p)
}

pct <- function(x) {
  if (!valid_probability(x)) return("Unavailable")
  format_risk_pct(x, 0L)
}

unavailable <- function(reason, status = "unavailable") {
  list(status = status, reason = reason,
       total = NA_real_, reference = NA_real_, difference = NA_real_)
}

available <- function(total, reference = NA_real_, type = "fg") {
  if (!valid_probability(total)) return(unavailable("Prediction is not a valid probability."))
  list(status = "ok", reason = NULL, total = total,
       reference = reference,
       difference = if (valid_probability(reference)) total - reference else NA_real_,
       type = type)
}

validate_esoph_specs <- function(model) {
  if (!is.list(model) || is.null(model$beta) || is.null(names(model$beta))) {
    return("Missing or unnamed logistic coefficients.")
  }
  if (!is.numeric(model$intercept) || length(model$intercept) != 1L ||
      !is.finite(model$intercept)) {
    return("Original fitted logistic intercept not supplied. The OR table alone cannot produce absolute risk.")
  }
  if (any(!is.finite(model$beta))) return("Nonfinite logistic coefficients.")
  ## Published rounded ORs are usable for preliminary predictions.
  ## Their rounding uncertainty is documented; it must not be silently treated as
  ## a fully validated original fitted logistic model.
  NULL
}

predict_esophagitis <- function(which_model, inputs) {
  if (!esophagitis_is_applicable(inputs$fx)) {
    return(unavailable("Published esophagitis models were developed for 30-fraction RT patients.",
                       status = "not_applicable"))
  }
  spec <- ESO_SPECS[[which_model]]
  issue <- validate_esoph_specs(spec)
  if (!is.null(issue)) return(unavailable(issue))
  keys <- names(spec$beta)
  missing <- setdiff(keys, names(inputs))
  if (length(missing)) {
    return(unavailable(paste("Missing esophagitis inputs:", paste(missing, collapse = ", "))))
  }
  x <- unlist(inputs[keys], use.names = FALSE)
  if (length(x) != length(keys) || any(!is.finite(x))) {
    return(unavailable("All required esophagitis predictor values must be finite."))
  }
  ## SAS CLASS effect coding: the binary indicator is transformed to
  ## -1 for level 0, +1 for level 1 before multiplying by the ORIGINAL
  ## SAS coefficient and ORIGINAL intercept. The model-specific
  ## effect_coded_vars set covers G2 chemotherapy/ECOG and G3 ECOG.
  effect_vars <- spec$effect_coded_vars
  if (length(effect_vars)) {
    effect_idx <- match(effect_vars, keys)
    if (anyNA(effect_idx) || !all(x[effect_idx] %in% c(0, 1))) {
      return(unavailable("Invalid CLASS coding for esophagitis predictors."))
    }
    x[effect_idx] <- 2 * x[effect_idx] - 1
  }
  eta <- as.numeric(spec$intercept + sum(x * spec$beta))
  x0 <- x
  dose_keys <- if (identical(which_model, "g2")) {
    c("esoph_mean_phys", "esoph_d2cc_phys")
  } else {
    "esoph_d2cc_phys"
  }
  x0[match(dose_keys, keys)] <- 0
  eta0 <- as.numeric(spec$intercept + sum(x0 * spec$beta))
  available(total = plogis(eta), reference = plogis(eta0), type = "logistic")
}

## Returns a list of fully validated canonical predictor values.
make_values <- function(input) {
  fx <- safe_numeric(input$total_fx)
  age <- safe_numeric(input$age)
  phys_lung <- safe_numeric(input$physical_mld)
  phys_heart <- safe_numeric(input$physical_mhd)
  med <- safe_numeric(input$esoph_mean_phys)
  d2cc <- safe_numeric(input$esoph_d2cc_phys)
  if (!is.finite(fx) || fx < 1 || fx != round(fx)) fx <- NA_real_
  if (!is.finite(age) || age < 18 || age > 110) age <- NA_real_
  if (!is.finite(phys_lung) || phys_lung < 0) phys_lung <- NA_real_
  if (!is.finite(phys_heart) || phys_heart < 0) phys_heart <- NA_real_
  if (!is.finite(med) || med < 0) med <- NA_real_
  if (!is.finite(d2cc) || d2cc < 0) d2cc <- NA_real_

  lung_eqd2 <- NA_real_
  if (is.function(APP_CONFIG$lung_eqd2_converter) &&
      is.finite(fx) && is.finite(phys_lung)) {
    lung_eqd2 <- tryCatch(
      safe_numeric(APP_CONFIG$lung_eqd2_converter(phys_lung, fx)),
      error = function(e) NA_real_)
  }
  heart_eqd2 <- NA_real_
  if (is.function(APP_CONFIG$heart_eqd2_converter) &&
      is.finite(fx) && is.finite(phys_heart)) {
    heart_eqd2 <- tryCatch(
      safe_numeric(APP_CONFIG$heart_eqd2_converter(phys_heart, fx)),
      error = function(e) NA_real_)
  }
  ## Empirical calibration has a small nonzero intercept. At physical
  ## MHD = 0, map to zero rather than negative/positive fitted intercept,
  ## for a coherent no-dose reference; equation unchanged for dose > 0.
  if (is.finite(phys_heart) && phys_heart == 0) heart_eqd2 <- 0

  list(
    fx = fx,
    age = age,
    ecog2 = as.integer(identical(input$ecogStatus, "2+")),
    oxygen = as.integer(isTRUE(input$PO2)),
    copd = as.integer(isTRUE(input$copd_base)),
    cardiac_comorbidity = as.integer(isTRUE(input$comorbidity)),
    concurrent_chemo = as.integer(isTRUE(input$concurrent_chemo)),
    ptv_near_heart = as.integer(isTRUE(input$ptv_near_heart)),
    sbrt = if (is.finite(fx)) as.integer(fx <= 5) else NA_integer_,
    male = as.integer(identical(input$gender, "Male")),
    female = as.integer(identical(input$gender, "Female")),
    esoph_mean_phys = med,
    esoph_d2cc_phys = d2cc,
    lung_eqd2 = lung_eqd2,
    heart_eqd2 = heart_eqd2,
    heart_phys = phys_heart,
    lung_phys = phys_lung
  )
}

## Strict mapping: an unknown or uncoded predictor makes the risk unavailable.
fg_predictor_vector <- function(spec, canonical) {
  if (is.null(spec$coef_names) || !length(spec$coef_names)) {
    stop("Missing spec$coef_names")
  }
  coef_names <- as.character(spec$coef_names)
  if (anyDuplicated(coef_names)) stop("Duplicate FG coefficient names")
  mp <- APP_CONFIG$predictor_map
  unknown <- setdiff(coef_names, names(mp))
  if (length(unknown)) stop(paste0(
    "Model predictors require explicit coding in server.R: ",
    paste(unknown, collapse = ", ")))
  canonical_keys <- unname(mp[coef_names])
  if (length(setdiff(canonical_keys, names(canonical)))) {
    stop("Internal predictor mapping references an unavailable input")
  }
  x <- vapply(canonical_keys, function(k) canonical[[k]], numeric(1))
  if (any(!is.finite(x))) {
    bad <- coef_names[!is.finite(x)]
    stop(paste("Dose conversion or required input not available:",
               paste(bad, collapse = ", ")))
  }
  setNames(x, coef_names)
}

validate_fg_spec <- function(spec) {
  required <- c("coef_names", "coef", "uftime", "bfitj")
  missing <- setdiff(required, names(spec))
  if (length(missing)) stop(paste("Incomplete FG spec:", paste(missing, collapse = ", ")))
  cn <- as.character(spec$coef_names)
  cf <- as.numeric(spec$coef)
  ut <- as.numeric(spec$uftime)
  bj <- as.numeric(spec$bfitj)
  if (length(cn) != length(cf) || any(!is.finite(cf))) {
    stop("FG coefficient names/values do not align")
  }
  if (!length(ut) || length(ut) != length(bj) || any(!is.finite(ut)) ||
      any(!is.finite(bj)) || any(bj < 0) ||
      is.unsorted(ut, strictly = TRUE)) {
    stop("Invalid FG baseline jump times/increments")
  }
  if (anyDuplicated(cn)) stop("Duplicated coefficient names")
  if (!is.null(names(spec$coef)) && all(cn %in% names(spec$coef))) {
    cf <- as.numeric(spec$coef[cn])
  }
  list(coef_names = cn, coef = cf, uftime = ut, bfitj = bj,
       dose_term = if (is.null(spec$dose_term)) NULL else as.character(spec$dose_term))
}

predict_fg_at <- function(vspec, x, month) {
  if (!is.finite(month) || month < 0) {
    stop(paste0("Requested prediction horizon ", month,
                " months is invalid"))
  }
  idx <- findInterval(month, vspec$uftime)
  if (idx == 0L) return(0)
  h0 <- cumsum(vspec$bfitj)[idx]
  eta <- sum(vspec$coef * x[vspec$coef_names])
  pred <- -expm1(-h0 * exp(eta))
  if (!valid_probability(pred)) stop("Fine-Gray prediction outside [0,1]")
  pred
}

predict_fg_result <- function(key, canonical, month) {
  ## COPD is deliberately handled separately because the manuscript-selected
  ## main model excludes MLD. Never silently use the sensitivity model instead.
  if (identical(key, "copd")) {
    if (!is.null(COPD_CLINICAL$issue)) return(unavailable(COPD_CLINICAL$issue))
    return(tryCatch({
      v <- validate_fg_spec(COPD_CLINICAL$spec)
      if (!is.null(v$dose_term) && length(v$dose_term)) {
        stop("COPD main model must not have a fitted dose term")
      }
      x <- fg_predictor_vector(v, canonical)
      mapped <- unname(APP_CONFIG$predictor_map[v$coef_names])
      expected <- APP_CONFIG$expected_canonical_predictors$copd
      if (!setequal(mapped, expected) || length(mapped) != length(expected)) {
        stop("COPD clinical-only model covariates are not age, oxygen, ECOG and baseline COPD")
      }
      ## Reference intentionally NA: no zero-dose counterfactual is estimated
      ## by a model without dose. Display the clinical-only risk as a solid bar.
      available(total = predict_fg_at(v, x, month),
                reference = NA_real_, type = "clinical_only")
    }, error = function(e) unavailable(conditionMessage(e))))
  }
  if (!is.null(FG$problem)) return(unavailable(FG$problem))
  if (!(key %in% names(FG$specs))) {
    return(unavailable(paste("FG specification key not found:", key,
                             "(needs fitted approximate-dose model).")))
  }
  tryCatch({
    v <- validate_fg_spec(FG$specs[[key]])
    expected <- APP_CONFIG$expected_dose_terms[[key]]
    if (is.null(expected) || length(expected) != 1L ||
        is.null(v$dose_term) || length(v$dose_term) != 1L ||
        !identical(v$dose_term, unname(expected)) ||
        !(unname(expected) %in% v$coef_names)) {
      stop(paste0("FG spec for ", key, " must be refit/extracted using ",
                  unname(expected), ". Exact-dose and other dose models",
                  " cannot be substituted."))
    }
    x <- fg_predictor_vector(v, canonical)
    fitted_predictors <- unname(APP_CONFIG$predictor_map[v$coef_names])
    expected_predictors <- APP_CONFIG$expected_canonical_predictors[[key]]
    if (!setequal(fitted_predictors, expected_predictors) ||
        length(fitted_predictors) != length(expected_predictors)) {
      stop(paste0("Fitted model covariates for ", key,
        " do not match the APPROVED current six-predictor model. ",
        "Verify gender, ECOG, COPD, cardiac factors, and the model version."))
    }
    total <- predict_fg_at(v, x, month)
    reference <- NA_real_
    if (!is.null(v$dose_term) && length(v$dose_term) == 1L && nzchar(v$dose_term)) {
      if (!v$dose_term %in% names(x)) {
        stop(paste("Invalid dose_term in spec:", v$dose_term))
      }
      x0 <- x
      x0[[v$dose_term]] <- 0
      reference <- predict_fg_at(v, x0, month)
    }
    available(total = total, reference = reference, type = "fg")
  }, error = function(e) unavailable(conditionMessage(e)))
}


## ================================================================
## ORIGINAL MROQC PLOT STYLE: stacked bar, same colors/stripe treatment
## ================================================================
MROQC_MAIN_LEVELS <- c("Any Lung", "Cardiac",
                       "G2+ Esophagitis", "G3+ Esophagitis")
MROQC_LUNG_LEVELS <- c("Any Lung", "Pneumonitis", "Pneumonia", "COPD")

## Keep labels centered on their own bars.  Split the long summary
## categories into two lines rather than reducing the font size with
## plotmath::scriptstyle(). The 2-year horizon is displayed separately
## in readable text in ui.R (only applies to lung/cardiac hospitalization).
MROQC_MAIN_TICK_LABELS <- c(
  "Lung\nHospitalization", "Cardiac\nHospitalization",
  "G2+\nEsophagitis", "G3+\nEsophagitis"
)
MROQC_LUNG_TICK_LABELS <- expression(
  bold("Any Lung"), bold("Pneumonitis"),
  bold("Pneumonia"), bold("COPD")
)

## Give the two narrow-screen summary plots exactly the SAME y-axis scale,
## so splitting four bars into two rows does not distort their comparison.
mroqc_risk_upper <- function(df) {
  ok <- df[is.finite(df$Total) & df$Total >= 0 & df$Total <= 1,
           , drop = FALSE]
  upper <- if (nrow(ok)) {
    max(c(ok$Total, ok$Reference[is.finite(ok$Reference)]), na.rm = TRUE)
  } else 0.10
  max(0.06, upper * 1.23)
}


## Pure bar-composition logic allows unit tests to assert that COPD
## is shown as fully hatched while its model does not claim a dose-zero CIF.
compute_bar_parts <- function(ok) {
  has_ref <- is.finite(ok$Reference)
  nonnegative <- has_ref & ok$Total >= ok$Reference
  clinical_only <- ok$Key == "copd" & !has_ref
  ok$base_part <- ifelse(clinical_only, ok$Total,
                         ifelse(nonnegative, ok$Reference, 0))
  ok$dose_part <- ifelse(clinical_only, 0,
                         ifelse(nonnegative, ok$Total - ok$Reference, ok$Total))
  ok
}

make_original_risk_plot <- function(df, plot_title, label_levels,
                                    hide_x_labels = FALSE,
                                    label_override = NULL,
                                    upper_override = NULL,
                                    mobile_summary = FALSE) {
  ## Keep all four categories on the horizontal axis, even when the
  ## underlying model is unavailable. Never plot an unverified 0% risk.
  df$Outcome <- factor(df$Outcome, levels = label_levels)
  ok <- df[is.finite(df$Total) & df$Total >= 0 & df$Total <= 1, , drop = FALSE]
  missing <- df[!is.finite(df$Total), , drop = FALSE]

  upper <- if (is.null(upper_override)) {
    mroqc_risk_upper(df)
  } else {
    stopifnot(is.numeric(upper_override), length(upper_override) == 1L,
              is.finite(upper_override), upper_override > 0)
    upper_override
  }

  axis_tick_labels <- if (!is.null(label_override)) {
    stopifnot(length(label_override) == length(label_levels))
    label_override
  } else if (identical(label_levels, MROQC_MAIN_LEVELS)) {
    MROQC_MAIN_TICK_LABELS
  } else if (identical(label_levels, MROQC_LUNG_LEVELS)) {
    MROQC_LUNG_TICK_LABELS
  } else {
    label_levels
  }
  base <- ggplot2::ggplot() +
    ggplot2::scale_x_discrete(limits = label_levels, drop = FALSE,
                              labels = axis_tick_labels) +
    ggplot2::scale_y_continuous(
      labels = function(z) sprintf("%.0f%%", 100 * z),
      limits = c(0, upper),
      expand = ggplot2::expansion(mult = c(0, .03))
    ) +
    ggplot2::labs(title = plot_title, x = NULL, y = "Risk (%)") +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      legend.position = "none",
      axis.text.y = ggplot2::element_text(size = 13, colour = "#404040"),
      axis.title.y = ggplot2::element_text(size = 15, colour = "#303030"),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = if (hide_x_labels) ggplot2::element_blank()
                    else ggplot2::element_text(
                      size = if (mobile_summary) 12.5 else 12,
                      family = "serif",
                      face = if (mobile_summary ||
                                 identical(label_levels, MROQC_MAIN_LEVELS))
                               "bold" else "plain",
                      lineheight = 1.10,
                      colour = "#303030", hjust = 0.5,
                      margin = ggplot2::margin(t = 10)
                    ),
      axis.ticks.x = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 8, r = 9, b = 6, l = 5),
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(size = 14, face = "plain")
    )

  if (nrow(ok)) {
    ok <- compute_bar_parts(ok)
    has_ref <- is.finite(ok$Reference)
    ok$total_label_y <- ifelse(
      has_ref & ok$Total < ok$Reference,
      pmax(ok$Total, ok$Reference) + upper * 0.015,
      ok$Total
    )
    plot_data <- rbind(
      data.frame(Outcome = ok$Outcome,
                 Component = "Background Risk",
                 Probability = ok$base_part,
                 stringsAsFactors = FALSE),
      data.frame(Outcome = ok$Outcome,
                 Component = "Additional Risk",
                 Probability = ok$dose_part,
                 stringsAsFactors = FALSE)
    )
    plot_data$Component <- factor(
      plot_data$Component, levels = c("Background Risk", "Additional Risk"))
    plot_data$Outcome <- factor(plot_data$Outcome, levels = label_levels)

    colors <- stats::setNames(df$Color, as.character(df$Outcome))
    base <- base +
      ggpattern::geom_col_pattern(
        data = plot_data,
        ggplot2::aes(x = Outcome, y = Probability, fill = Outcome,
                     pattern = Component),
        width = .70, color = "black",
        position = ggplot2::position_stack(reverse = TRUE),
        pattern_fill = "white", pattern_angle = 45,
        pattern_density = .08, pattern_spacing = .03,
        pattern_key_scale_factor = .6
      ) +
      ggplot2::scale_fill_manual(values = colors, guide = "none") +
      ggpattern::scale_pattern_manual(
        values = c("Background Risk" = "stripe", "Additional Risk" = "none"),
        guide = "none"
      ) +
      ggplot2::geom_text(
        data = ok,
        ggplot2::aes(x = Outcome, y = total_label_y,
                     label = vapply(Total, format_risk_pct, character(1), digits = 0L)),
        inherit.aes = FALSE, vjust = -.5, size = 5.0
      )

    ## Label positive excess risk inside the solid segment, as in old app.
    ## Matt: Do not draw a dose-associated number when the absolute
    ## difference is <2 percentage points. Also omit it when the
    ## segment is too short for readable type. The segment remains plotted.
    positive <- ok[is.finite(ok$Reference) &
                     ok$Total - ok$Reference >= 0.02 &
                     ok$dose_part >= 0.045 * upper, , drop = FALSE]
    if (nrow(positive)) {
      positive$mid <- positive$base_part + positive$dose_part / 2
      base <- base + ggplot2::geom_text(
        data = positive,
        ggplot2::aes(x = Outcome, y = mid,
                     label = vapply(Total - Reference, format_risk_pct,
                                    character(1), digits = 0L)),
        inherit.aes = FALSE, size = 4.2)
    }
    ## An open marker denotes the dose-zero reference when total < reference.
    ## We omit the white percentage-point box from the older plot.
    negative <- ok[is.finite(ok$Reference) &
                     ok$Total < ok$Reference - 1e-10, , drop = FALSE]
    if (nrow(negative)) {
      base <- base + ggplot2::geom_point(
        data = negative,
        ggplot2::aes(x = Outcome, y = Reference),
        inherit.aes = FALSE,
        shape = 21, size = 2.6, stroke = 0.8,
        fill = "white", color = "black"
      )
    }
  }
  if (nrow(missing)) {
    ## Nonapplicable G2/G3 categories are intentionally BLANK, not "N/A".
    ## A single plain-language note is shown beneath the plot.
    missing <- missing[missing$Status != "not_applicable", , drop = FALSE]
  }
  if (nrow(missing)) {
    missing$label_y <- rep(upper * .13, nrow(missing))
    missing$unavailable_label <- rep("Unavailable", nrow(missing))
    base <- base + ggplot2::geom_text(
      data = missing,
      ggplot2::aes(x = Outcome, y = label_y, label = unavailable_label),
      inherit.aes = FALSE, color = "grey45", size = 3.3)
  }
  base
}


## ================================================================
## EMPIRICAL DOSE-SUPPORT DIAGNOSTICS
## Derived from the actual ALL-PATIENTS model-development complete cases
## and stratified by fractionation; no guessed clinical dose cutoff.
## ================================================================
DOSE_SUPPORT_PATH <- "dose_support.csv"
DOSE_SUPPORT_REQUIRED <- c(
  "model", "fractionation", "n",
  "physical_min", "physical_p01", "physical_p99", "physical_max",
  "eqd2_min", "eqd2_p01", "eqd2_p99", "eqd2_max"
)

load_dose_support <- function(path = DOSE_SUPPORT_PATH) {
  if (!file.exists(path)) return(list(data = NULL, issue =
    "Training-dose ranges have not been loaded. Out-of-range checking is unavailable until dose_support.csv is generated from the analysis cohorts."))
  dat <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE),
                  error = function(e) NULL)
  if (is.null(dat) || !all(DOSE_SUPPORT_REQUIRED %in% names(dat))) {
    return(list(data = NULL, issue =
      "dose_support.csv is incomplete. Recreate it using export_dose_support.R."))
  }
  dat$model <- trimws(as.character(dat$model))
  dat$fractionation <- trimws(as.character(dat$fractionation))
  if (anyDuplicated(paste(dat$model, dat$fractionation, sep = "|"))) {
    return(list(data = NULL, issue =
      "dose_support.csv contains duplicate model/fractionation rows."))
  }
  list(data = dat, issue = NULL)
}

DOSE_SUPPORT <- load_dose_support()

dose_group <- function(n_fx) {
  if (!is.finite(n_fx) || n_fx < 1 || n_fx != round(n_fx)) return(NA_character_)
  if (n_fx <= 5) "SBRT" else if (n_fx <= 20) "Hypofx" else "Conventional"
}

dose_out_of_range_messages <- function(v) {
  if (!is.null(DOSE_SUPPORT$issue)) return(DOSE_SUPPORT$issue)
  grp <- dose_group(v$fx)
  if (is.na(grp)) return("Enter a valid integer number of fractions.")
  ## No dose-support check for COPD: main COPD model is clinical-only.
  keys <- c("anylung", "pneumonitis", "pneumonia", "anycardiac")
  pretty <- c(anylung = "Any Lung", pneumonitis = "Pneumonitis",
              pneumonia = "Pneumonia", copd = "COPD", anycardiac = "Cardiac")
  msgs <- character(0)
  for (key in keys) {
    d <- DOSE_SUPPORT$data
    dr <- d[d$model == key & d$fractionation == grp, , drop = FALSE]
    if (nrow(dr) != 1L) {
      msgs <- c(msgs, sprintf(
        "%s: no observed training-dose range for %s; extrapolation cannot be ruled out.",
        pretty[[key]], grp))
      next
    }
    n <- suppressWarnings(as.integer(dr$n[1]))
    if (!is.finite(n) || n < 1) {
      msgs <- c(msgs, sprintf("%s: no usable training patients in %s.", pretty[[key]], grp))
      next
    }
    dose <- if (key == "anycardiac") v$heart_phys else v$lung_phys
    eqd2 <- if (key == "anycardiac") v$heart_eqd2 else v$lung_eqd2
    tag <- if (key == "anycardiac") "MHD" else "MLD"
    if (!is.finite(dose) || !is.finite(eqd2)) {
      msgs <- c(msgs, sprintf("%s: enter a valid physical dose and fraction number.", pretty[[key]]))
      next
    }
    pairs <- list(
      list(value = dose, lo = dr$physical_min[1], hi = dr$physical_max[1],
           p01 = dr$physical_p01[1], p99 = dr$physical_p99[1],
           unit = paste0("physical ", tag)),
      list(value = eqd2, lo = dr$eqd2_min[1], hi = dr$eqd2_max[1],
           p01 = dr$eqd2_p01[1], p99 = dr$eqd2_p99[1],
           unit = paste0("approximate EQD2 ", tag))
    )
    extreme <- FALSE
    for (p in pairs) {
      xx <- suppressWarnings(as.numeric(c(
        p$value, p$lo, p$hi, p$p01, p$p99)))
      if (any(!is.finite(xx))) {
        msgs <- c(msgs, sprintf("%s: training-dose support is incomplete.", pretty[[key]]))
        break
      }
      if (xx[1] < xx[2] || xx[1] > xx[3]) {
        msgs <- c(msgs, sprintf(
          "%s: %s %.2f Gy lies OUTSIDE the observed %s training range [%.2f, %.2f] Gy (n=%d). The risk prediction is extrapolated.",
          pretty[[key]], p$unit, xx[1], grp, xx[2], xx[3], n))
        extreme <- TRUE
        ## Do not repeat same outcome warning for both units.
        break
      }
    }
    if (extreme) next
    ## Additional near-boundary alert only when a group has enough data to
    ## estimate extreme percentiles with some stability.
    if (n >= 100L) {
      p <- pairs[[2]]
      xx <- suppressWarnings(as.numeric(c(
        p$value, p$p01, p$p99)))
      if (all(is.finite(xx)) && (xx[1] < xx[2] || xx[1] > xx[3])) {
        msgs <- c(msgs, sprintf(
          "%s: approximate EQD2 %s %.2f Gy is within the observed %s range but outside its 1st–99th percentile interval [%.2f, %.2f] Gy (n=%d). Interpret cautiously.",
          pretty[[key]], tag, xx[1], grp, xx[2], xx[3], n))
      }
    }
  }
  unique(msgs)
}

shinyServer(function(input, output, session) {
  vals <- shiny::reactive(make_values(input))

  output$doseRangeWarning <- shiny::renderUI({
    warnings <- dose_out_of_range_messages(vals())
    if (!length(warnings)) return(NULL)
    tags$div(
      class = "mroqc-warning",
      tags$b("Prediction range warning"),
      tags$ul(lapply(warnings, tags$li))
    )
  })


  g2 <- shiny::reactive(predict_esophagitis("g2", vals()))
  g3 <- shiny::reactive(predict_esophagitis("g3", vals()))
  anylung <- shiny::reactive(predict_fg_result("anylung", vals(), 24))
  cardiac <- shiny::reactive(predict_fg_result(APP_CONFIG$cardiac_model_key, vals(), 24))

  build_risk_row <- function(key, label, color, result) {
    data.frame(
      Key = key,
      Outcome = label,
      Total = as.numeric(result$total),
      Reference = as.numeric(result$reference),
      Difference = as.numeric(result$difference),
      Status = as.character(result$status),
      Reason = if (is.null(result$reason)) "" else as.character(result$reason),
      Color = color,
      stringsAsFactors = FALSE
    )
  }

  summary_data <- shiny::reactive({
    rbind(
      build_risk_row("anylung", "Any Lung", "#67A9CF", anylung()),
      build_risk_row("anycardiac", "Cardiac", "#D6604D", cardiac()),
      build_risk_row("g2", "G2+ Esophagitis", "#FDB863", g2()),
      build_risk_row("g3", "G3+ Esophagitis", "#E66101", g3())
    )
  })

  output$summaryPlot <- shiny::renderPlot({
    ## Original four-bar layout is preserved on desktop.
    make_original_risk_plot(summary_data(),
                            NULL, MROQC_MAIN_LEVELS, hide_x_labels = FALSE)
  })

  ## Narrow screens use two bars per row. This is necessary because the full
  ## word "Hospitalization" cannot fit legibly in one of four phone-width
  ## axis slots, even after inserting a line break. The y scales are shared.
  output$summaryPlotMobileHosp <- shiny::renderPlot({
    all <- summary_data()
    idx <- match(MROQC_MAIN_LEVELS[1:2], as.character(all$Outcome))
    make_original_risk_plot(
      all[idx, , drop = FALSE], NULL, MROQC_MAIN_LEVELS[1:2],
      label_override = MROQC_MAIN_TICK_LABELS[1:2],
      upper_override = mroqc_risk_upper(all), mobile_summary = TRUE
    )
  })

  output$summaryPlotMobileEso <- shiny::renderPlot({
    all <- summary_data()
    idx <- match(MROQC_MAIN_LEVELS[3:4], as.character(all$Outcome))
    make_original_risk_plot(
      all[idx, , drop = FALSE], NULL, MROQC_MAIN_LEVELS[3:4],
      label_override = MROQC_MAIN_TICK_LABELS[3:4],
      upper_override = mroqc_risk_upper(all), mobile_summary = TRUE
    )
  })

  ## Matt's EXACT requested summary footnote. Keep status explanations
  ## accessible without occupying the main dashboard.
  output$summaryFootnote <- shiny::renderUI({
    x <- summary_data()
    bad <- x[x$Status != "ok" & x$Status != "not_applicable", , drop = FALSE]
    shiny::tagList(
      tags$p(class = "mroqc-footnote",
             "* Estimated risk in the absence of RT (due to factors other than RT dose)."),
      if (!esophagitis_is_applicable(vals()$fx))
        tags$p(class = "mroqc-eligibility-note",
               "* Esophagitis risk estimates are only available for patients treated with conventionally fractionated RT."),
      if (nrow(bad)) tags$details(class = "mroqc-model-detail",
        tags$summary("Model status (expand for details)"),
        shiny::tagList(lapply(seq_len(nrow(bad)), function(i) {
          tags$div(tags$b(paste0(bad$Outcome[i], ": ")),
                   bad$Reason[i])
        }))
      )
    )
  })

  ## Conversion is used IN THE BACKGROUND. No display of approx EQD2.
  output$modelInputNote <- shiny::renderUI({ NULL })

  lung_data <- shiny::reactive({
    rbind(
      build_risk_row("anylung", "Any Lung", "#67A9CF",
                     predict_fg_result("anylung", vals(), 24)),
      build_risk_row("pneumonitis", "Pneumonitis", "#4E79A7",
                     predict_fg_result("pneumonitis", vals(), 24)),
      build_risk_row("pneumonia", "Pneumonia", "#59A14F",
                     predict_fg_result("pneumonia", vals(), 24)),
      build_risk_row("copd", "COPD", "#9C755F",
                     predict_fg_result("copd", vals(), 24))
    )
  })

  output$lungDetailPlot <- shiny::renderPlot({
    ## Draw labels at the bar coordinates using the same typography as Summary.
    make_original_risk_plot(lung_data(), NULL,
                            MROQC_LUNG_LEVELS, hide_x_labels = FALSE)
  })

  output$lungDetailFootnote <- shiny::renderUI({
    failed <- lung_data()
    failed <- failed[failed$Status != "ok", , drop = FALSE]
    shiny::tagList(
      tags$p(class = "mroqc-footnote",
             "* Estimated risk in the absence of RT (due to factors other than RT dose)."),
      tags$p(class = "mroqc-input-note",
             "The competing events are death and hospice. Lung subtype predictions are",
             "separate outcomes and should not be added together."),
      tags$p(class = "mroqc-input-note",
             "The COPD bar is fully hatched because its model uses clinical factors only; no RT dose term was selected."),
      if (is.finite(lung_data()$Total[1]) &&
          any(lung_data()$Total[-1] > lung_data()$Total[1] + 1e-8, na.rm = TRUE))
        tags$div(class = "mroqc-warning",
          "Independent subtype models give a subtype risk higher than Any Lung for these inputs. This violates the nesting expected of these events; these predictions should not be interpreted clinically without review. Check whether the dose inputs fall within the model-development range.")
      ,
      if (nrow(failed)) tags$details(class = "mroqc-model-detail",
        tags$summary("Model status (expand for details)"),
        shiny::tagList(lapply(seq_len(nrow(failed)), function(i) {
          tags$div(tags$b(paste0(failed$Outcome[i], ": ")), failed$Reason[i])
        })))
    )
  })

  output$riskTable <- shiny::renderUI({
    keys <- c("anylung", "pneumonitis", "pneumonia", "copd")
    labels <- c("Any Lung", "Pneumonitis", "Pneumonia", "COPD")
    months <- c(12, 24, 36)
    label_pct <- function(p) {
      if (valid_probability(p)) format_risk_pct(p, 0L) else "—"
    }
    row_values <- Map(function(key, label) {
      z <- lapply(months, function(m) predict_fg_result(key, vals(), m))
      c(label, unlist(lapply(z, function(x) {
        c(label_pct(x$reference), label_pct(x$total))
      }), use.names = FALSE))
    }, keys, labels)
    tags$table(
      class = "table table-bordered table-condensed",
      style = "width:100%; text-align:center;",
      tags$thead(
        tags$tr(
          tags$th("Type of Hosp.", rowspan = 2,
                  style = "vertical-align:middle;"),
          tags$th("1 yr", colspan = 2, style = "text-align:center;"),
          tags$th("2 yr", colspan = 2, style = "text-align:center;"),
          tags$th("3 yr", colspan = 2, style = "text-align:center;")
        ),
        tags$tr(
          tags$th("Dose = 0"), tags$th("Entered dose"),
          tags$th("Dose = 0"), tags$th("Entered dose"),
          tags$th("Dose = 0"), tags$th("Entered dose")
        )
      ),
      tags$tbody(lapply(row_values, function(z) tags$tr(lapply(z, tags$td))))
    )
  })

  shiny::observeEvent(input$lung_more, {
    shiny::updateTabsetPanel(session, "mainTabs", selected = "Lung detail")
  })
  shiny::observeEvent(input$back_summary, {
    shiny::updateTabsetPanel(session, "mainTabs", selected = "Summary")
  })
})
