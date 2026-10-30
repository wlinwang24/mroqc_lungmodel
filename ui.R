## ================================================================
## MROQC SHINY APP - ORIGINAL-STYLE UI
## Retains the user-supplied original layout, title, colors, bar chart,
## summary tab, and lung detail tab.  Only requested changes are made.
## ================================================================
library(shiny)
library(shinythemes)

compact_number <- function(id, label, value, min = 0, max = NULL,
                           step = 0.1) {
  args <- list(inputId = id, label = NULL, value = value,
               min = min, step = step, width = "112px")
  if (!is.null(max)) args$max <- max
  div(class = "mroqc-inline-row",
      tags$label(`for` = id, label),
      do.call(numericInput, args))
}
compact_select <- function(id, label, choices, selected) {
  div(class = "mroqc-inline-row",
      tags$label(`for` = id, label),
      selectInput(id, label = NULL, choices = choices,
                  selected = selected, width = "112px"))
}

## Static clinician-facing documentation. The short display terms MLD and MHD
## refer to physical mean dose inputs; approximate EQD2 conversions remain in
## server.R. Changes here DO NOT change prediction model specifications.
mroqc_inputs_footer <- function(include_references = FALSE, lung_detail = FALSE) {
  row <- function(label, terms) {
    div(class = "mroqc-method-row",
        tags$strong(paste0(label, ": ")),
        span(terms))
  }

  rows <- if (isTRUE(lung_detail)) {
    list(
      row("Any Lung Hospitalization",
          "Age, ECOG, baseline COPD, supplemental oxygen (O2), gender, and MLD"),
      row("Pneumonitis", "Gender and MLD"),
      row("Pneumonia",
          "Age, ECOG, baseline COPD, supplemental oxygen (O2), gender, and MLD"),
      row("COPD",
          "Age, ECOG, baseline COPD, and supplemental oxygen (O2); no radiation dose term")
    )
  } else {
    list(
      row("Lung Hospitalization",
          "Age, ECOG, baseline COPD, supplemental oxygen (O2), gender, and MLD"),
      row("Cardiac Hospitalization",
          "Age, ECOG, supplemental oxygen (O2), any baseline cardiac comorbidity, gender, and MHD"),
      row("G2+ Esophagitis",
          "Age, ECOG, concurrent chemotherapy, mean esophageal dose, and esophageal D2cc"),
      row("G3+ Esophagitis",
          "Age, ECOG, and esophageal D2cc")
    )
  }

  references <- if (isTRUE(include_references)) {
    tagList(
      div(class = "mroqc-references-heading", "References:"),
      div(class = "mroqc-reference",
          tags$a(href = "https://doi.org/10.1016/j.radonc.2024.110349",
                 target = "_blank", rel = "noopener noreferrer",
                 "Herr DJ, et al. Factors associated with acute esophagitis during radiation therapy for lung cancer. Radiother Oncol. 2024;197:110349.")),
      div(class = "mroqc-reference",
          tags$a(href = "https://www.sciencedirect.com/science/article/pii/S0360301625043433",
                 target = "_blank", rel = "noopener noreferrer",
                 "Nieto JD, et al. Identification of dosimetric and clinical predictors for post-treatment pulmonary hospitalizations in NSCLC. Int J Radiat Oncol Biol Phys. 2025;123(1 Suppl):e198. Abstract 2448.")),
      div(class = "mroqc-reference",
          tags$a(href = "https://www.sciencedirect.com/science/article/abs/pii/S0360301626030695",
                 target = "_blank", rel = "noopener noreferrer",
                 "Nasser B, et al. Statewide Analysis of Clinical and Dosimetric Predictors of Cardiac Hospitalization after Thoracic RT for NSCLC. ASTRO 2026 conference abstract."))
    )
  } else NULL

  div(class = "mroqc-methods-footer",
      div(class = "mroqc-methods-heading", "Inputs to each model are as follows:"),
      do.call(tagList, rows),
      div(class = "mroqc-methods-note",
          "Lung and cardiac dose predictors use approximate EQD2 derived from physical mean dose and number of fractions. Esophagitis models use physical esophageal doses."),
      references)
}

fluidPage(
  theme = shinytheme("readable"),
  tags$head(
    if (file.exists("google-analytics.html")) includeHTML("google-analytics.html"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML("
      /* Retained from the original UI */
      .table .form-group { margin-bottom: 0; }
      .table input { text-align: center; }
      .table td, .table th { padding: 6px 8px; vertical-align: middle; }
      .app-title-row { display:flex; align-items:center; gap:14px; }
      .app-title-row img { height:78px; width:auto; max-width:225px; object-fit:contain; }
      .app-title-text { color:#2955C1; font-size:27px; font-weight:700; line-height:1.18; }
      .btn-blockish { width:100%; margin-bottom:6px; font-weight:600; }

      /* Limited changes requested by Matt: shorter, readable sidebar */
      .mroqc-inline-row { display:flex; align-items:center; gap:10px;
                          justify-content:space-between; margin:0 0 9px; }
      .mroqc-inline-row label { margin:0; line-height:1.35; font-weight:500;
                                flex:1; font-size:16px; }
      .mroqc-inline-row .form-group { margin:0 !important; flex:0 0 112px; }
      .mroqc-inline-row .form-control { height:38px; font-size:16px; }
      .mroqc-inline-row .selectize-input { min-height:38px; font-size:16px; }
      .well .checkbox { margin-top:7px; margin-bottom:7px; }
      .well .checkbox label { font-size:16px; line-height:1.45; }
      .mroqc-input-note { font-size:14px; line-height:1.45; margin:8px 0;
                          color:#464646; }
      .mroqc-gray-note { color:#777; background:#f2f2f2; padding:8px 10px;
                         border:1px solid #dedede; border-radius:3px; }
      .mroqc-footnote { font-size:15px; line-height:1.5; margin-top:9px;
                        margin-bottom:10px; color:#404040; }
      .mroqc-time-note { font-size:13px; line-height:1.4; color:#555;
                         margin:3px 0 10px; }
      /* Labels stay in ggplot at the bar coordinates (never a separate
         four-column HTML grid). The time horizon is a readable shared note. */
      .mroqc-plot-main .shiny-plot-output {
        display:block; width:100%; margin:0; padding:0;
      }
      .mroqc-summary-desktop { display:block; }
      .mroqc-summary-mobile { display:none; }
      .mroqc-chart-horizon {
        font-size:15px; line-height:1.45; color:#404040;
        text-align:center; margin:1px 4px 10px;
      }
      .mroqc-summary-mobile .mroqc-chart-horizon {
        margin:0 4px 13px;
      }
      .mroqc-summary-actions {
        display:grid;
        grid-template-columns:repeat(4,minmax(0,1fr));
        gap:3px;
        margin:0 6px 0 65px;
        align-items:start;
      }
      .mroqc-summary-actions > div { min-width:0; }
      .mroqc-summary-actions .btn {
        display:block;
        width:92%; max-width:190px;
        margin:0 auto 0;
        font-size:13px;
        font-weight:600;
      }
      /* Put the striped reference key to the right of each chart, rather
         than between the axis and category labels. */
      .mroqc-plot-row { display:grid; grid-template-columns:minmax(0,1fr) 160px;
                        column-gap:10px; align-items:start; }
      .mroqc-plot-main { min-width:0; }
      .mroqc-plot-legend { display:flex; align-items:flex-start;
                           justify-content:flex-start; padding-top:152px; }
      .mroqc-background-key { display:flex; align-items:center; gap:7px;
                              font-size:14px; font-weight:600; margin:0;
                              white-space:nowrap; }
      .mroqc-background-swatch { width:19px; height:14px; border:1px solid #666;
         background:repeating-linear-gradient(135deg,#777 0px,#777 2px,
                    #f5f5f5 2px,#f5f5f5 7px); display:inline-block; }
      .mroqc-eligibility-note { font-size:15px; line-height:1.5; margin:6px 0;
                                color:#555; }
      .mroqc-plain-button { display:block; margin:6px auto 0; max-width:210px; }
      .mroqc-title-logo { height:78px; width:auto; }
      .mroqc-model-detail { margin-top:8px; font-size:12px; color:#666; }
      .mroqc-model-detail summary { cursor:pointer; }
      .mroqc-model-detail div { padding:2px 0; }
      .mroqc-warning { font-size:13px; line-height:1.5; color:#654000;
                       background:#fff9eb; border-left:3px solid #d2a35d;
                       padding:7px 10px; margin:7px 0 12px; }
      /* Model transparency: compact footer, visually separate from predictions. */
      .mroqc-methods-footer {
        border-top:1px solid #ddd;
        margin:24px 0 20px;
        padding:15px 2px 8px;
        font-family:inherit;
        font-size:14px;
        line-height:1.5;
        color:#404040;
      }
      .mroqc-methods-footer .mroqc-methods-heading {
        font-size:15px;
        font-weight:700;
        margin:0 0 9px;
      }
      .mroqc-methods-footer .mroqc-method-row { margin:0 0 6px; }
      .mroqc-methods-footer .mroqc-method-row strong { font-weight:700; }
      .mroqc-methods-footer .mroqc-references-heading {
        font-size:15px;
        font-weight:700;
        margin:16px 0 8px;
      }
      .mroqc-methods-footer .mroqc-reference { margin:0 0 6px; }
      .mroqc-methods-footer .mroqc-methods-note {
        font-size:12.5px;
        color:#626262;
        margin:10px 0 0;
        line-height:1.45;
      }
      .mroqc-methods-footer a { text-decoration:underline; }
      .mroqc-input-section h3 { margin-top:16px; }
      .mroqc-detail-table { overflow-x:auto; }
      .mroqc-detail-table table { width:100%; font-size:13px; }
      .mroqc-detail-table th, .mroqc-detail-table td { white-space:nowrap; }
      .mroqc-detail-table .table { table-layout:auto; margin-bottom:0; }
      .mroqc-detail-table { width:100%; max-width:100%; }
      /* A phone cannot display four full 12-pt outcome labels side by side.
         Two rows of two bars give each label enough horizontal room. */
      @media(max-width:1050px) {
        .mroqc-summary-desktop { display:none; }
        .mroqc-summary-mobile { display:block; }
        .mroqc-summary-actions {
          grid-template-columns:1fr; margin:4px 0 10px;
        }
        .mroqc-summary-actions .btn { width:auto; min-width:130px; }
        .mroqc-plot-row { grid-template-columns:minmax(0,1fr); }
        .mroqc-plot-legend { padding:8px 0 5px 35px; }
      }
      @media(max-width:750px) {
        .app-title-row { flex-wrap:wrap; }
        .app-title-text { font-size:23px; }
      }
    "))
  ),

  ## The first (disclaimer) page shows the logo only at the top.
  ## Once Proceed is clicked, the full requested title appears alongside it.
  ## Optional official updated logo: place an approved image at
  ## www/mroqc_logo_updated.png; otherwise preserve the prior logo.
  titlePanel(
    title = div(class = "app-title-row",
                img(src = if (file.exists("www/mroqc_logo_updated.png"))
                             "mroqc_logo_updated.png" else "mroqc_logo.jpg",
                    alt = "MROQC logo", class = "mroqc-title-logo"),
                conditionalPanel(
                  condition = "input.goToApp > 0",
                  span("Toxicity and Hospitalization Risk Calculator for Lung Cancer Patients Treated with RT",
                       class = "app-title-text"))),
    windowTitle = "Toxicity and Hospitalization Risk Calculator for Lung Cancer Patients Treated with RT"
  ),

  ## Preserve the original disclaimer / Proceed screen.
  conditionalPanel(
    condition = "typeof input.goToApp === 'undefined' || input.goToApp === 0",
    br(),
    wellPanel(
      style = paste0("text-align:center;background:#BECACF;color:#FFFFFF;",
                     "border-color:#2955C1 #00274C;border-width:10px 0px"),
      tags$h1("Toxicity and Hospitalization Risk Calculator"),
      tags$h2("for Lung Cancer Patients Treated with RT")
    ),
    tags$h3("Disclaimer"),
    tags$p(paste(
      "The software designer and provider, including any collaborating",
      "institution(s), including the University of Michigan, shall have no",
      "liability to any patient or any other person. No such person or entity",
      "assumes any legal liability or responsibility for the accuracy,",
      "completeness, suitability, or usefulness of the Toxicity and",
      "Hospitalization Risk Calculator or related information. Any and all liability",
      "arising directly or indirectly from the use of this application is",
      "hereby disclaimed. The information herein is provided as is and",
      "without any warranty expressed or implied, including the warranties",
      "of merchantability and fitness for a particular purpose."
    )),
    tags$p("This app is a research prototype. Predictions are applicable only to",
           "the populations and dose metrics used in the underlying models.",
           "It is not a substitute for individualized clinical judgment."),
    fluidRow(column(12, style = "text-align:center;",
      checkboxInput("agree", "Click here if you have read the above disclaimer and agree to all statements.",
                    width = "100%"),
      conditionalPanel("input.agree === true",
                       actionButton("goToApp", "Proceed",
                                    style = "padding:6px 18px;font-size:120%;"))
    ))
  ),

  conditionalPanel(
    condition = "input.goToApp > 0",
    sidebarLayout(
      sidebarPanel(width = 4,
        div(class = "mroqc-input-section",
          h3("Clinical"),
          compact_number("age", "Patient Age (Years):", 55,
                         min = 40, max = 90, step = 1),
          compact_select("ecogStatus", "ECOG at baseline",
                         choices = c("0/1" = "0/1", "2+" = "2+"),
                         selected = "0/1"),
          checkboxInput("copd_base", "COPD at Baseline", FALSE),
          checkboxInput("PO2", "Oxygen", FALSE),
          ## SBRT checkbox removed: computed internally from fraction number.
          checkboxInput("comorbidity", "Any Baseline Cardiac Comorbidity", FALSE),
          tags$p(class = "mroqc-input-note",
                 "Included Cardiac Comorbidities: Hypertension, Cerebrovascular Disease,",
                 "Congestive Heart Failure, Myocardial Infarction, Peripheral Vascular Disease, Arrhythmia"),
          checkboxInput("concurrent_chemo", "Concurrent Chemotherapy", FALSE),
          ## Only used if a verified FG specification requires this coding.
          compact_select("gender", "Gender", c("Female" = "Female", "Male" = "Male"), "Female"),

          h3("RT Dose"),
          compact_number("total_fx", "Number of fractions", 30,
                         min = 1, max = 60, step = 1),
          compact_number("physical_mld", "Physical mean lung dose (Gy)", 8, min = 0),
          compact_number("physical_mhd", "Physical mean heart dose (Gy)", 5, min = 0),
          conditionalPanel(
            condition = "input.total_fx === 30",
            compact_number("esoph_mean_phys", "Physical mean esophageal dose (Gy)", 20, min = 0),
            compact_number("esoph_d2cc_phys", "Physical esophageal D2cc (Gy)", 50, min = 0)
          ),
          conditionalPanel(
            condition = "input.total_fx !== 30",
            div(class = "mroqc-input-note mroqc-gray-note",
                "Esophagitis estimates are not displayed for this fractionation schedule.")
          ),
          tags$p(class = "mroqc-input-note",
                 "We use the physical mean dose values combined with number of fractions to calculate an approximate EQD2 dose that is then used in the models.",
                 "Esophagitis models use physical esophageal dose metrics."),
          uiOutput("doseRangeWarning")
        )
      ),
      mainPanel(width = 8,
        tabsetPanel(id = "mainTabs", type = "hidden", selected = "Summary",
          tabPanel("Summary",
            div(class = "mroqc-plot-row",
              div(class = "mroqc-plot-main",
                div(class = "mroqc-summary-desktop",
                  plotOutput("summaryPlot", height = "390px"),
                  tags$p(class = "mroqc-chart-horizon",
                         "Lung and cardiac hospitalization: within 2 years of RT.")
                ),
                div(class = "mroqc-summary-mobile",
                  plotOutput("summaryPlotMobileHosp", height = "280px"),
                  tags$p(class = "mroqc-chart-horizon",
                         "Lung and cardiac hospitalization: within 2 years of RT."),
                  plotOutput("summaryPlotMobileEso", height = "280px")
                ),
                div(class = "mroqc-summary-actions",
                  div(actionButton("lung_more", "More detail",
                                   class = "btn-blockish mroqc-plain-button")),
                  div(), div(), div()
                )
              ),
              div(class = "mroqc-plot-legend",
                div(class = "mroqc-background-key",
                    span(class = "mroqc-background-swatch",
                         `aria-hidden` = "true"),
                    span("Background risk*"))
              )
            ),
            uiOutput("summaryFootnote"),
            mroqc_inputs_footer(include_references = TRUE)
          ),
          tabPanel("Lung detail",
            br(),
            fluidRow(column(12,
              div(style = "display:flex;justify-content:space-between;align-items:center;",
                  tags$h4("Lung hospitalization detail", style = "margin:0;"),
                  actionButton("back_summary", "Back to summary"))
            )),
            br(),
            tags$h5("Lung hospitalization within 2 years of RT, by subtype"),
            div(class = "mroqc-plot-row",
              div(class = "mroqc-plot-main",
                plotOutput("lungDetailPlot", height = "390px")
              ),
              div(class = "mroqc-plot-legend",
                div(class = "mroqc-background-key",
                    span(class = "mroqc-background-swatch",
                         `aria-hidden` = "true"),
                    span("Background risk*"))
              )
            ),
            uiOutput("lungDetailFootnote"),
            tags$h5("Predicted hospitalization risks by type and timing"),
            div(class = "mroqc-detail-table", uiOutput("riskTable")),
            mroqc_inputs_footer(lung_detail = TRUE)
          )
        )
      )
    )
  )
)
