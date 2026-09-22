nav_panel(
  title = "Copycat",
  model_tab_shell(
    summary_text = ui_summary("copycat"),
    methodology_link_id = "modal_copycat_methodology",
    controls = tagList(
      control_section(
        "Run Model",
        actionButton(
          "run_copycat",
          "Run Copycat"
        )
      ),
      control_section(
        "Model Settings",
        numericInput(
          "recent_weeks_touse",
          label = tagList(
            "Recent Weeks to Use",
            modal_info_link("modal_recent_weeks")
          ),
          value = 100,
          min = 3,
          max = 100
        ),
        numericInput(
          "resp_week_range",
          label = tagList(
            "Respiratory Week Range",
            modal_info_link("modal_resp_week_range")
          ),
          value = 2,
          min = 0,
          max = 10
        ),
        radioButtons(
          "copycat_share_groups",
          label = tagList(
            "Group Trajectories",
            modal_info_link("modal_copycat_share_groups")
          ),
          choices = c(
            "Shared (all groups)"    = "shared",
            "Individual (per group)" = "individual"
          ),
          selected = "shared"
        ),
        numericInput(
          "copycat_weight_exponent",
          label = tagList(
            "Weight Exponent",
            modal_info_link("modal_copycat_weight_exponent")
          ),
          value = 2,
          min = 1,
          max = 3
        ),
        radioButtons(
          "copycat_poisson_noise",
          label = tagList(
            "Add Observation Noise?",
            modal_info_link("modal_copycat_poisson_noise")
          ),
          choices = c("Yes", "No"),
          selected = "Yes",
          inline = TRUE
        ),
        numericInput(
          "copycat_noise_dispersion",
          label = tagList(
            "Noise Dispersion (Proportion data)",
            modal_info_link("modal_copycat_noise_dispersion")
          ),
          value = 100,
          min = 2,
          max = 2000
        ),
        numericInput(
          "copycat_points_per_knot",
          label = tagList(
            "Data Points per Knot",
            modal_info_link("modal_copycat_points_per_knot")
          ),
          value = 5,
          min = 3,
          max = 6
        ),
        numericInput(
          "copycat_max_matches",
          label = tagList(
            "Max Historical Matches",
            modal_info_link("modal_copycat_max_matches")
          ),
          value = NA,
          min = 1
        ),
        textOutput("copycat_max_matches_hint")
      )
    ),
    plot_output = plotOutput("copycat_plots", height = "600px"),
    download_button = downloadButton(
      "copycat_plot_download",
      "Download Copycat Plot (.png)"
    )
  )
) # end nav_panel Copycat
