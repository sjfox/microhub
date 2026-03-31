nav_panel(
  title = "INFLAenza",
  includeMarkdown("www/content/inflaenza.md"),
  layout_column_wrap(
    heights_equal = "row",
    style = css(grid_template_columns = "1fr 2fr"),
    card(
      actionButton(
        "run_inla",
        "Run INFLAenza"
      ),
      strong("Settings"),
      # selectizeInput(
      #   "ar_order",
      #   label = tagList(
      #     "Order of AR",
      #     actionLink(
      #       "modal_ar",
      #       icon("info-circle"),
      #       style = "margin-left: 5px;"
      #     )
      #   ),
      #   choices = c(1, 2, 3),
      #   selected = 1
      # ),
      # selectizeInput(
      #   "rw_order",
      #   label = tagList(
      #     "Order of RW",
      #     actionLink(
      #       "modal_rw",
      #       icon("info-circle"),
      #       style = "margin-left: 5px;"
      #     )
      #   ),
      #   choices = c(1, 2),
      #   selected = 2
      # ),
      # selectizeInput(
      #   "seasonal_smoothness",
      #   label = tagList(
      #     "Seasonal Smoothness",
      #     actionLink(
      #       "modal_seasonal_smoothness",
      #       icon("info-circle"),
      #       style = "margin-left: 5px;"
      #     )
      #   ),
      #   choices = c("Default" = "default", "More" = "more", "Less" = "less")
      # ),
      selectizeInput(
        "forecast_uncertainty_parameter",
        label = tagList(
          "Forecast Uncertainty Parameter",
          actionLink(
            "modal_forecast_uncertainty",
            icon("info-circle"),
            style = "margin-left: 5px;"
          )
        ),
        choices = c("Default" = "default", "Smaller" = "small", "Tiny" = "tiny")
      ),
      radioButtons(
        "use_population_column",
        label = tagList(
          "Use population column?",
          actionLink(
            "modal_population",
            icon("info-circle"),
            style = "margin-left: 5px;"
          )
        ),
        choices = c("Yes", "No"),
        selected = "No",
        inline = TRUE
      ),
    ), # end card
    card(
      plotOutput("inla_plots"),
      downloadButton(
        "inla_plot_download",
        "Download INFLAenza Plot (.png)"
      ),
    ) # end card
  ) # end layout_column_wrap
) # end nav_panel INFLAenza
