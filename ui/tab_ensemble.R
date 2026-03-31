nav_panel(
  title = "Ensemble",
  includeMarkdown("www/content/ensemble.md"),
  layout_column_wrap(
    heights_equal = "row",
    style = css(grid_template_columns = "1fr 2fr"),
    card(
      actionButton(
        "run_ensemble",
        "Run Ensemble"
      ),
      strong("Settings"),
      selectizeInput(
        "ensemble_models",
        "Select at least two models to include in the ensemble:",
        width = "100%",
        multiple = TRUE,
        choices = NULL,
        options = list(
          placeholder = "Run at least two models to populate this input",
          plugins = list("remove_button")
        )
      ),
      hr(),
      strong("Upload Outside Model Forecast"),
      helpText(
        "Add a forecast from an external model to the ensemble. ",
        "Download the template below (pre-filled with the correct dates and",
        " quantile levels from your current forecasts), fill in the 'value'",
        " and 'model' columns, then upload."
      ),
      uiOutput("outside_model_template_ui"),
      fileInput(
        "outside_model_file",
        label = NULL,
        buttonLabel = "Browse...",
        placeholder = "Upload outside model (.csv)",
        accept = ".csv",
        width = "100%"
      ),
      uiOutput("outside_model_validation_ui"),
      uiOutput("outside_models_loaded_ui"),
    ), # end card
    card(
      plotOutput("ensemble_plots"),
      downloadButton(
        "ensemble_plot_download",
        "Download Ensemble Plot (.png)"
      )
    ) # end card
  ) # end layout_column_wrap
) # end nav_panel Ensemble
