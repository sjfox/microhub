nav_panel(
  title = "Download",
  downloadButton(
    "download_results",
    "Download Results (.csv)"
  ),
  card(
    card_header("Results Preview"),
    DTOutput("results_preview")
  )
) # end nav_panel Download
