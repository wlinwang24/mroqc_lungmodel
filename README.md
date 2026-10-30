# MROQC Lung Models — Posit Connect Cloud deployment candidate

**Status: internal review only; not yet tested or approved for public GitHub or production.**

This folder is a minimal copy of the supplied October 2026 working files. It does not change the underlying statistical models, user interface, or inference code. The added `app.R` is a thin wrapper to supply a standard entry point for GitHub-to-Connect-Cloud deployment.

## 1. Files the live application reads

- `app.R`: Connect Cloud entry point, loads `ui.R` and `server.R`.
- `ui.R`: unchanged user interface; sources optional `google-analytics.html`.
- `server.R`: unchanged risk prediction and visualization functions.
- `fg_model_specs.R`: selected lung and cardiac Fine–Gray specifications, including baseline hazard jumps.
- `model_copd_clinical_only.rds`: COPD clinical-only Fine–Gray fit loaded at runtime. **This RDS must be reviewed by the MROQC data owner for patient-level or potentially sensitive content before it goes on any Git repository.**
- `dose_support.csv`: aggregate empirical dose support summary; get data owner approval for release.
- `www/`: images for the app; confirm any permission to republish branding/images.
- `google-analytics.html`: existing Google Analytics script, included unchanged to avoid unexpected behavior changes. Obtain organizational privacy/analytics approval before exposing it on a new host or domain. If omitted, the UI is coded to skip it.

## 2. Historical and analysis files deliberately omitted

Old `.git/` history, local `.RData`/`.Rhistory`/`.Rproj.user`, deployment records under `rsconnect/`, model-workspace files, private cardiac refits, derived analysis scripts and redundant individual lung RDS objects. In the supplied app, `server.R` directly reads `fg_model_specs.R`, `model_copd_clinical_only.rds`, and `dose_support.csv`; the other exported lung/cardinal model RDS files are not directly loaded by the UI/server.

The previous Git history tracked `model_workspace.RData`; **do not reuse/push the extracted original `.git` directory** without a specific security review.

## 3. First run locally (on a machine with R and RStudio)

1. Extract this review ZIP into a new folder, separate from the original project. Do not place its contents inside the old Git project.
2. Open RStudio and set the working directory to this folder (`Session > Set Working Directory > Choose Directory`).
3. Install missing packages. Run once in the R console:

```r
install.packages(c("shiny", "shinythemes", "ggplot2", "ggpattern", "scales", "rsconnect"))
```

4. Run the app locally:

```r
shiny::runApp(".")
```

5. Confirm the disclaimer, Summary, and Lung detail screens load; use different values for number of fractions, physical MLD/MHD, clinical factors, and 30-fraction esophagitis inputs. For every endpoint verify non-missing numerical predictions where expected. Compare several cases with the currently deployed shinyapps.io version, including exactly matched inputs (avoid comparing two different model versions).

## 4. Clinical and governance review before publishing

- Confirm with Matt / MROQC that this October 26 cardiac specification is the approved deployment version, and that the esophagitis logic is approved.
- Have an authorized MROQC data steward review `model_copd_clinical_only.rds`; models can carry training inputs or residuals even if file names say 'model'.
- Confirm `fg_model_specs.R`, `dose_support.csv`, the logo and third-party analytics script are suitable for the selected repository visibility.
- If the RDS cannot be shared through GitHub, ask the departmental hosting team about a secure deployment route or an approved minimized coefficient-only model specification instead. Do not simply omit the RDS: COPD estimates would then be unavailable.
- Agree on who will maintain the GitHub repo and who can approve model changes (e.g., Matt/Maggie); prefer a managed private lab or department repository.

## 5. Generate dependencies locally (do not invent or hand-edit package versions)

After the app runs locally, run in the R console with this folder as the working directory:

```r
rsconnect::writeManifest(appDir = ".")
```

This creates `manifest.json` using the packages actually installed in your R session. Commit that file alongside the approved source and model inputs. If the department prefers `renv.lock`, coordinate with them instead.

## 6. Connect Cloud deployment with the departmental team

1. Create a **new, clean** Git repository owned by Matt's lab or the Biostatistics Department. Decide private/public only after governance approval. Do not copy over the legacy `.git` directory.
2. Commit only the approved files in this folder, plus the locally generated `manifest.json`.
3. Ask the department to choose **Publish > Shiny > GitHub repository**, select the chosen branch (`main` if created that way), and select `app.R` as the primary file.
4. Initially deploy to an ordinary Connect Cloud test URL, not to `lungmodels.mroqc.org`.
5. Have the department configure custom domain DNS once the test app is accepted, with the `mroqc.org` domain administrator's cooperation. Confirm the repository and Connect Cloud account ownership and billing.
6. Enable auto-publish on push only after an agreed review process; prefer a protected `main` branch with pull requests.
7. Configure any usage analytics, access lists and resource/instance settings with IT.

Official documentation: https://docs.posit.co/connect-cloud/how-to/r/shiny-r.html
Custom domains: https://docs.posit.co/connect-cloud/user/share/custom-domains.html

## 7. Maintenance and validation

For a new **approved** model version, update the model specification or reviewed RDS file and any corresponding predictor mappings together. Record dates, model version, intended patient population, dose definitions, and change approver. Validate reference predictions before deploying. For display-only updates, keep model files fixed and check there is no change in prediction output. Maintain version-tagged release notes and an earlier version to roll back if needed.

### Known limits of this review package

- Not run: the code-review environment used here does not have R/RStudio, and no Posit Connect Cloud credentials or access to the current hosted app were provided.
- A `manifest.json` is therefore not included; generate it on your computer after checking package versions.
- The added `app.R` wrapper needs to pass the local RStudio smoke test.
- The package is **not a clinical or privacy approval**.

## Responsive label layout (October 29, 2026)

The Summary plot labels were revised to address two clinical reviewer requests:

- At desktop widths greater than 1050 CSS pixels, the original four-bar Summary layout is preserved. Lung/Cardiac hospitalization labels and both esophagitis labels wrap onto two lines. The former tiny `scriptstyle()` text for the 2-year horizon is now a single 15-pixel note under the plot.
- At viewport widths up to 1050 CSS pixels, the same four bars are presented as two groups of two (lung/cardiac and esophagitis) to avoid label overlap in portrait orientation. Text sizes are 12.5 points for these outcome labels and 15 CSS pixels for the time-horizon note.
- Both mobile plots use the **same** y-axis maximum calculated from all four outcomes to prevent visual miscomparison across the two rows. The risk values, fitted models, colors, and stratified reference bars are unchanged.
- The 2-year time horizon applies only to lung and cardiac hospitalization; esophagitis endpoints are acute and are not covered by that note.

Review on an actual phone in portrait/landscape orientation and on desktop before releasing. Confirm the labels and plots render correctly at several window widths (e.g., 320, 375, 430, 768, 1024, and 1200 pixels). The revised source has only been statically checked here; this environment does not have R.
