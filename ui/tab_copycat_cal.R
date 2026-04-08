nav_panel(
  title = "Copycat (Cal.)",
  model_tab_shell(
    summary_text = "Copycat (Calibrated) starts with the same historical pattern-matching forecast as Copycat, then adjusts its uncertainty using how wrong similar forecasts were in past seasons. That makes its intervals more grounded in observed historical forecast error.",
    methodology_link_id = "modal_copycat_cal_methodology",
    controls = tagList(
      control_section(
        "Run Model",
        actionButton(
          "run_copycat_cal",
          "Run Copycat (Cal.)"
        )
      ),
      control_section(
        "Pattern Matching",
        numericInput(
          "recent_weeks_touse_cal",
          label = tagList(
            "Recent Weeks to Use",
            actionLink(
              "modal_recent_weeks",
              icon("info-circle"),
              style = "margin-left: 5px;"
            )
          ),
          value = 100,
          min   = 3,
          max   = 100
        ),
        numericInput(
          "resp_week_range_cal",
          label = tagList(
            "Respiratory Week Range",
            actionLink(
              "modal_resp_week_range",
              icon("info-circle"),
              style = "margin-left: 5px;"
            )
          ),
          value = 2,
          min   = 0,
          max   = 10
        ),
        radioButtons(
          "copycat_cal_share_groups",
          label = tagList(
            "Group Trajectories",
            actionLink(
              "modal_copycat_share_groups",
              icon("info-circle"),
              style = "margin-left: 5px;"
            )
          ),
          choices = c(
            "Shared (all groups)"    = "shared",
            "Individual (per group)" = "individual"
          ),
          selected = "shared"
        )
      ),
      control_section(
        "Calibration",
        numericInput(
          "ref_week_window",
          label = tagList(
            "Reference Week Window",
            actionLink(
              "modal_ref_week_window",
              icon("info-circle"),
              style = "margin-left: 5px;"
            )
          ),
          value = 0,
          min   = 0,
          max   = 5
        ),
        numericInput(
          "nsamps_cal",
          label = tagList(
            "Calibration Samples",
            actionLink(
              "modal_nsamps_cal",
              icon("info-circle"),
              style = "margin-left: 5px;"
            )
          ),
          value = 100,
          min   = 20,
          max   = 1000
        )
      )
    ),
    plot_output = plotOutput("copycat_cal_plots", height = "600px"),
    download_button = downloadButton(
      "copycat_cal_plot_download",
      "Download Copycat (Cal.) Plot (.png)"
    )
  )
) # end nav_panel Copycat (Cal.)
