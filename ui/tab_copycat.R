nav_panel(
  title = "Copycat",
  includeMarkdown("www/content/copycat.md"),
  layout_column_wrap(
    heights_equal = "row",
    style = css(grid_template_columns = "1fr 2fr"),
    card(
      actionButton(
        "run_copycat",
        "Run Copycat"
      ),
      strong("Settings"),
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
      )
    ), # end card
    card(
      plotOutput("copycat_plots"),
      downloadButton(
        "copycat_plot_download",
        "Download Copycat Plot (.png)"
      )
    ) # end card
  ) # end layout_column_wrap
) # end nav_panel Copycat
