nav_panel(
  title = "Copycat",
  model_tab_shell(
    summary_text = "Copycat looks for past seasons that behaved like the last few weeks of your current data, then projects those historical trajectories forward as possible futures. It is essentially a pattern-matching model built around the question: what happened next the last time things looked like this?",
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
            actionLink(
              "modal_recent_weeks",
              icon("info-circle"),
              style = "margin-left: 5px;"
            )
          ),
          value = 100,
          min = 3,
          max = 100
        ),
        numericInput(
          "resp_week_range",
          label = tagList(
            "Respiratory Week Range",
            actionLink(
              "modal_resp_week_range",
              icon("info-circle"),
              style = "margin-left: 5px;"
            )
          ),
          value = 2,
          min = 0,
          max = 10
        ),
        radioButtons(
          "copycat_share_groups",
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
      )
    ),
    plot_output = plotOutput("copycat_plots", height = "600px"),
    download_button = downloadButton(
      "copycat_plot_download",
      "Download Copycat Plot (.png)"
    )
  )
) # end nav_panel Copycat
