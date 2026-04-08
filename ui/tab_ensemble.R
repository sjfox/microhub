nav_panel(
  title = "Ensemble",
  model_tab_shell(
    summary_text = "The Ensemble combines selected models into one shared forecast by taking the median prediction across them. It is useful when you want a more stable result that is less sensitive to the quirks of any single model.",
    methodology_link_id = "modal_ensemble_methodology",
    controls = tagList(
      control_section(
        "Run Model",
        actionButton(
          "run_ensemble",
          "Run Ensemble"
        )
      ),
      control_section(
        "Ensemble Members",
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
        )
      ),
      control_section(
        "Outside Model Upload",
        helpText(
          "Add a forecast from an external model to the ensemble. Download the template below, fill in the 'value' and 'model' columns, then upload it."
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
        uiOutput("outside_models_loaded_ui")
      )
    ),
    plot_output = plotOutput("ensemble_plots", height = "600px"),
    download_button = downloadButton(
      "ensemble_plot_download",
      "Download Ensemble Plot (.png)"
    )
  )
) # end nav_panel Ensemble
