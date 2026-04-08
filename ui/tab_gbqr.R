nav_panel(
  title = "GBQR",
  model_tab_shell(
    summary_text = "GBQR is a machine-learning model that learns patterns from past data, such as recent levels and trends, and uses them to predict a range of likely future values. It is helpful when the data may follow more complex relationships than a simple seasonal or historical-matching model can capture.",
    methodology_link_id = "modal_gbqr_methodology",
    controls = tagList(
      control_section(
        "Run Model",
        actionButton(
          "run_gbqr",
          "Run GBQR"
        )
      ),
      control_section(
        "Model Settings",
        radioButtons(
          "gbqr_model_type",
          label = tagList(
            "Model Fitting",
            actionLink(
              "modal_gbqr_model_type",
              icon("info-circle"),
              style = "margin-left: 5px;"
            )
          ),
          choices = c(
            "Individual (per group)" = "individual",
            "Global (all groups)"    = "global"
          ),
          selected = "individual"
        )
      )
    ),
    plot_output = plotOutput("gbqr_plots", height = "600px"),
    download_button = downloadButton(
      "gbqr_plot_download",
      "Download GBQR Plot (.png)"
    )
  )
) # end nav_panel GBQR
