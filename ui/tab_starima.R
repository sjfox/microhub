nav_panel(
  title = "STArima",
  model_tab_shell(
    summary_text = ui_summary("starima"),
    methodology_link_id = "modal_starima_methodology",
    controls = control_section(
      "Run Model",
      actionButton(
        "run_starima",
        "Run STArima"
      )
    ),
    plot_output = plotOutput("starima_plots", height = "600px"),
    download_button = downloadButton(
      "starima_plot_download",
      "Download STArima Plot (.png)"
    )
  )
)
