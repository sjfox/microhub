nav_panel(
  title = "INFLAenza",
  model_tab_shell(
    summary_text = "INFLAenza combines recurring seasonal patterns with the most recent changes in the data to estimate where the series is headed next. It is a flexible forecasting model that works well when both long-term seasonality and short-term movement matter.",
    methodology_link_id = "modal_inla_methodology",
    controls = tagList(
      control_section(
        "Run Model",
        actionButton(
          "run_inla",
          "Run INFLAenza"
        )
      ),
      control_section(
        "Model Settings",
        selectizeInput(
          "forecast_uncertainty_parameter",
          label = tagList(
            "Forecast Uncertainty Parameter",
            actionLink(
              "modal_forecast_uncertainty",
              icon("info-circle"),
              style = "margin-left: 5px;"
            )
          ),
          choices = c("Default" = "default", "Smaller" = "small", "Tiny" = "tiny")
        ),
        radioButtons(
          "use_population_column",
          label = tagList(
            "Use population column?",
            actionLink(
              "modal_population",
              icon("info-circle"),
              style = "margin-left: 5px;"
            )
          ),
          choices = c("Yes", "No"),
          selected = "No",
          inline = TRUE
        )
      )
    ),
    plot_output = plotOutput("inla_plots", height = "600px"),
    download_button = downloadButton(
      "inla_plot_download",
      "Download INFLAenza Plot (.png)"
    )
  )
) # end nav_panel INFLAenza
