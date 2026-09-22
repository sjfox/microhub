nav_panel(
  title = "Baseline",
  navset_card_underline(
    nav_panel(
      "Regular Baseline",
      model_tab_shell(
        summary_text = ui_summary("baseline-regular"),
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
        summary_text = ui_summary("baseline-seasonal"),
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
        summary_text = ui_summary("baseline-opt"),
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
