nav_panel(
  title = "Ensemble",
  model_tab_shell(
    summary_text = ui_summary("ensemble"),
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
        "Ensemble Method",
        radioButtons(
          "ensemble_method",
          "How should member forecasts be combined?",
          choices = c(
            "Median (per quantile)"    = "median",
            "Mean (per quantile)"      = "mean",
            "Linear pool (distributional)" = "linear_pool"
          ),
          selected = "median"
        ),
        helpText(
          "Median/Mean combine models one quantile at a time. Linear pool instead ",
          "mixes each model's full predictive distribution before re-reading off the ",
          "same quantiles — often a more principled way to combine forecasts."
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
