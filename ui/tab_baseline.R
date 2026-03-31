nav_panel(
  title = "Baseline",
  navset_card_underline(
    nav_panel(
      "Regular Baseline",
      includeMarkdown("www/content/baseline-regular.md"),
      layout_column_wrap(
        heights_equal = "row",
        style = css(grid_template_columns = "1fr 2fr"),
        card(
          actionButton(
            "run_baseline_regular",
            "Run Regular Baseline"
          )
        ),
        card(
          plotOutput("baseline_regular_plots"),
          downloadButton(
            "baseline_regular_plot_download",
            "Download Regular Baseline Plot (.png)"
          )
        ) # end card
      ) # end layout_column_wrap
    ), # end nav_panel Regular Baseline
    nav_panel(
      "Seasonal Baseline",
      includeMarkdown("www/content/baseline-seasonal.md"),
      layout_column_wrap(
        heights_equal = "row",
        style = css(grid_template_columns = "1fr 2fr"),
        card(
          actionButton(
            "run_baseline_seasonal",
            "Run Seasonal Baseline"
          )
        ),
        card(
          plotOutput("baseline_seasonal_plots"),
          downloadButton(
            "baseline_seasonal_plot_download",
            "Download Seasonal Baseline Plot (.png)"
          )
        ) # end card
      ) # end layout_column_wrap
    ), # end nav_panel Seasonal Baseline
    nav_panel(
      "Opt Baseline",
      includeMarkdown("www/content/baseline-opt.md"),
      layout_column_wrap(
        heights_equal = "row",
        style = css(grid_template_columns = "1fr 2fr"),
        card(
          actionButton(
            "run_baseline_opt",
            "Run Opt Baseline"
          )
        ),
        card(
          plotOutput("baseline_opt_plots"),
          downloadButton(
            "baseline_opt_plot_download",
            "Download Opt Baseline Plot (.png)"
          )
        ) # end card
      ) # end layout_column_wrap
    ) # end nav_panel Opt Baseline
  ) # end navset_card_underline
) # end nav_panel Baseline
