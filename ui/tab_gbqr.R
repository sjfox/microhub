nav_panel(
  title = "GBQR",
  includeMarkdown("www/content/gbqr.md"),
  layout_column_wrap(
    heights_equal = "row",
    style = css(grid_template_columns = "1fr 2fr"),
    card(
      actionButton(
        "run_gbqr",
        "Run GBQR"
      ),
      strong("Settings"),
    ), # end card
    card(
      plotOutput("gbqr_plots"),
      downloadButton(
        "gbqr_plot_download",
        "Download GBQR Plot (.png)"
      )
    ) # end card
  ) # end layout_column_wrap
) # end nav_panel GBQR
