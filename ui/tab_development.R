nav_panel(
  title = "Development",
  navset_card_underline(
    nav_panel(
      title = "CalCopycat",
      model_tab_shell(
        summary_text = ui_summary("calcopycat"),
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
            "Model Settings",
            numericInput(
              "recent_weeks_touse_cal",
              label = tagList(
                "Recent Weeks to Use",
                modal_info_link("modal_recent_weeks")
              ),
              value = 12,
              min = 3,
              max = 50
            ),
            numericInput(
              "resp_week_range_cal",
              label = tagList(
                "Respiratory Week Range",
                modal_info_link("modal_calcopycat_week_range")
              ),
              value = 2,
              min = 0,
              max = 10
            ),
            radioButtons(
              "calcopycat_share_groups",
              label = tagList(
                "Group Trajectories",
                modal_info_link("modal_calcopycat_share_groups")
              ),
              choices = c(
                "Shared (all groups)"    = "shared",
                "Individual (per group)" = "individual"
              ),
              selected = "shared"
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
      title = "parGBQR",
      model_tab_shell(
        summary_text = ui_summary("pargbqr"),
        methodology_link_id = "modal_pargbqr_methodology",
        controls = tagList(
          control_section(
            "Run Model",
            actionButton(
              "run_pargbqr",
              "Run parGBQR"
            )
          ),
          control_section(
            "Model Settings",
            radioButtons(
              "pargbqr_model_type",
              label = tagList(
                "Model Fitting",
                modal_info_link("modal_pargbqr_model_type")
              ),
              choices = c(
                "Individual (per group)" = "individual",
                "Global (all groups)" = "global"
              ),
              selected = "global"
            ),
            numericInput(
              "pargbqr_num_bags",
              label = "Bags",
              value = 50,
              min = 10,
              max = 100,
              step = 1
            )
          )
        ),
        plot_output = plotOutput("pargbqr_plots", height = "600px"),
        download_button = downloadButton(
          "pargbqr_plot_download",
          "Download parGBQR Plot (.png)"
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
