nav_panel(
  title = "INFLAenza",
  model_tab_shell(
    summary_text = ui_summary("inlaenza"),
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
            modal_info_link("modal_forecast_uncertainty")
          ),
          choices = c("Default" = "default", "Smaller" = "small", "Tiny" = "tiny")
        ),
        selectizeInput(
          "inla_interaction",
          label = tagList(
            "Group structure",
            modal_info_link("modal_inla_interaction")
          ),
          choices = c(
            "Exchangeable (default)" = "exchangeable",
            "Independent (iid)" = "iid",
            "None (shared trend)" = "none",
            "Exchangeable + main trend" = "exchangeable_main",
            "Independent + main trend" = "iid_main",
            "Spatial (neighbor graph)" = "besagproper"
          ),
          selected = "exchangeable"
        ),
        selectizeInput(
          "inla_seasonal",
          label = tagList(
            "Seasonality",
            modal_info_link("modal_inla_seasonal")
          ),
          choices = c(
            "Shared across all groups (default)" = "shared",
            "One curve per seasonal group" = "season_group",
            "One curve per target group" = "target_group"
          ),
          selected = "shared"
        ),
        radioButtons(
          "use_population_column",
          label = tagList(
            "Use population column?",
            modal_info_link("modal_population")
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
