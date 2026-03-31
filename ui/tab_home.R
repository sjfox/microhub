nav_panel(
  title = "Home",
  tagList(
    # Banner Section
    tags$div(
      class = "banner-bg",
      tags$div(class = "banner-overlay"),
      tags$div(
        class = "banner-content",
        h1("MicroHub Forecasting"),
        p("MicroHub Forecasting was developed to generate real-time probabilistic forecasts for respiratory illness surveillance data from public health programs around the world.")
      )
    ),
    # Tasks section
    tags$div(
      layout_columns(
        col_widths = breakpoints(
          sm = c(12, 12, 12),
          md = c(12, 12, 12),
          lg = c(4, 4, 4)
        ),
        card(
          card_header("Forecast", class = "bg-dark"),
          card_body("Produce weekly forecasts using a suite of complementary models — from simple baselines to machine learning approaches — without needing to modify any code.")
        ),
        card(
          card_header("Visualize", class = "bg-dark"),
          card_body("Visualize the data and forecasts to help tune the model parameters as needed.")
        ),
        card(
          card_header("Download", class = "bg-dark"),
          card_body("Download the forecasts in a standardized format that can seamlessly integrate with existing forecasting hub infrastructure such as the production of a weekly report.")
        )
      )
    ),
    # Goal Section
    tags$div(
      h5("MicroHub Forecasting empowers public health officials worldwide with timely, actionable insights to support effective responses to respiratory illness outbreaks — wherever they occur.", style = "background: #002454; color: white; padding: 2em")
    ),
    # Visualization Section
    tags$div(
      style = "background: #E1E0E0;",
      h3(
        "The Visualization",
        style = "text-align: center; margin: 0.8em;"
      ),
      layout_columns(
        tags$img(src = "images/plot-inla.png", style = "padding: 1em;"),
        tags$div(
          includeMarkdown("www/content/visualization.md"),
          style = "margin: 1em;"
        )
      )
    )
  )
) # end nav_panel Home
