nav_panel(
  title = "newGBQR",
  model_tab_shell(
    summary_text = "newGBQR is a year-round version of GBQR that removes fixed in-season windows, learns peak timing empirically from the uploaded data, and adds cyclic week-of-year features for forecasts in any week.",
    methodology_link_id = "modal_newgbqr_methodology",
    controls = tagList(
      control_section(
        "Run Model",
        actionButton(
          "run_newgbqr",
          "Run newGBQR"
        )
      ),
      control_section(
        "Model Settings",
        radioButtons(
          "newgbqr_model_type",
          label = tagList(
            "Model Fitting",
            actionLink(
              "modal_newgbqr_model_type",
              icon("info-circle"),
              style = "margin-left: 5px;"
            )
          ),
          choices = c(
            "Individual (per group)" = "individual",
            "Global (all groups)" = "global"
          ),
          selected = "global"
        )
      )
    ),
    plot_output = plotOutput("newgbqr_plots", height = "600px"),
    download_button = downloadButton(
      "newgbqr_plot_download",
      "Download newGBQR Plot (.png)"
    )
  )
) # end nav_panel newGBQR
