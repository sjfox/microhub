nav_panel(
  title = "Development",
  navset_tab(
    nav_panel(
      title = "CalCopycat",
      model_tab_shell(
        summary_text = "CalCopycat starts with the same historical pattern-matching forecast as Copycat, then calibrates its uncertainty using leave-one-out historical forecast errors from comparable weeks in the time series. That makes its intervals more grounded in observed forecast error.",
        methodology_link_id = "modal_calcopycat_methodology",
        controls = tagList(
          control_section(
            "Run Model",
            actionButton(
              "run_calcopycat",
              "Run CalCopycat"
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
              value = 12,
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
              "calcopycat_share_groups",
              label = tagList(
                "Group Trajectories",
                actionLink(
                  "modal_calcopycat_share_groups",
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
              value = 1,
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
        plot_output = plotOutput("calcopycat_plots", height = "600px"),
        download_button = downloadButton(
          "calcopycat_plot_download",
          "Download CalCopycat Plot (.png)"
        )
      )
    ),
    nav_panel(
      title = "FourCAT",
      includeMarkdown("www/content/fourcat.md"),
      layout_column_wrap(
        heights_equal = "row",
        style = css(grid_template_columns = "1fr 2fr"),
        card(
          actionButton(
            "run_fourcat",
            "Run FourCAT"
          )
        ),
        card(
          plotOutput("fourcat_plots"),
          downloadButton(
            "fourcat_plot_download",
            "Download FourCAT Plot (.png)"
          )
        )
      )
    )
  )
)
