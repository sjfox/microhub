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
    ), # end card
    card(
      plotOutput("fourcat_plots"),
      downloadButton(
        "fourcat_plot_download",
        "Download FourCAT Plot (.png)"
      )
    ) # end card
  ) # end layout_column_wrap
) # end nav_panel

