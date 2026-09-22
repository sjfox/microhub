nav_panel(
  title = "newGBQR",
  model_tab_shell(
    summary_text = ui_summary("newgbqr"),
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
            modal_info_link("modal_newgbqr_model_type")
          ),
          choices = c(
            "Individual (per group)" = "individual",
            "Global (all groups)" = "global"
          ),
          selected = "global"
        ),
        numericInput(
          "newgbqr_num_bags",
          label = "Bags",
          value = 50,
          min = 10,
          max = 100,
          step = 1
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
