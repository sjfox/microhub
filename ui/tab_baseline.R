nav_panel(
  title = "Baseline",
  navset_card_underline(
    nav_panel(
      "Regular Baseline",
      model_tab_shell(
        summary_text = "The Regular Baseline assumes the next few weeks will look broadly like the usual week-to-week changes seen in your historical data. It is a simple benchmark that helps show whether a more complex model is really adding value.",
        methodology_link_id = "modal_baseline_regular_methodology",
        controls = control_section(
          "Run Model",
          actionButton(
            "run_baseline_regular",
            "Run Regular Baseline"
          )
        ),
        plot_output = plotOutput("baseline_regular_plots", height = "600px"),
        download_button = downloadButton(
          "baseline_regular_plot_download",
          "Download Regular Baseline Plot (.png)"
        )
      )
    ), # end nav_panel Regular Baseline
    nav_panel(
      "Seasonal Baseline",
      model_tab_shell(
        summary_text = "The Seasonal Baseline focuses on the repeating shape of past respiratory seasons and projects that typical seasonal pattern forward. It is useful when timing and rise-and-fall behavior tend to be fairly consistent from year to year.",
        methodology_link_id = "modal_baseline_seasonal_methodology",
        controls = control_section(
          "Run Model",
          actionButton(
            "run_baseline_seasonal",
            "Run Seasonal Baseline"
          )
        ),
        plot_output = plotOutput("baseline_seasonal_plots", height = "600px"),
        download_button = downloadButton(
          "baseline_seasonal_plot_download",
          "Download Seasonal Baseline Plot (.png)"
        )
      )
    ), # end nav_panel Seasonal Baseline
    nav_panel(
      "Opt Baseline",
      model_tab_shell(
        summary_text = "The Opt Baseline is a faster-reacting version of the baseline that pays more attention to the most recent part of the series instead of the full history. It is often useful when conditions are changing and older seasons may be less informative.",
        methodology_link_id = "modal_baseline_opt_methodology",
        controls = control_section(
          "Run Model",
          actionButton(
            "run_baseline_opt",
            "Run Opt Baseline"
          )
        ),
        plot_output = plotOutput("baseline_opt_plots", height = "600px"),
        download_button = downloadButton(
          "baseline_opt_plot_download",
          "Download Opt Baseline Plot (.png)"
        )
      )
    ) # end nav_panel Opt Baseline
  ) # end navset_card_underline
) # end nav_panel Baseline
