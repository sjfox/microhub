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
      class = "page-section",
      layout_columns(
        col_widths = breakpoints(
          sm = c(12, 12, 12),
          md = c(12, 12, 12),
          lg = c(4, 4, 4)
        ),
        card(
          class = "app-card home-feature-card",
          card_header("Forecast"),
          card_body("Produce weekly forecasts using a suite of complementary models — from simple baselines to machine learning approaches — without needing to modify any code.")
        ),
        card(
          class = "app-card home-feature-card",
          card_header("Visualize"),
          card_body("Visualize the data and forecasts to help tune the model parameters as needed.")
        ),
        card(
          class = "app-card home-feature-card",
          card_header("Download"),
          card_body("Download the forecasts in a standardized format that can seamlessly integrate with existing forecasting hub infrastructure such as the production of a weekly report.")
        )
      )
    ),
    # Goal Section
    tags$div(
      class = "page-section home-highlight",
      h5("MicroHub Forecasting empowers public health officials worldwide with timely, actionable insights to support effective responses to respiratory illness outbreaks — wherever they occur.")
    ),
    # Visualization Section
    tags$div(
      class = "page-section home-visualization",
      h3(
        "The Visualization",
        class = "section-title"
      ),
      layout_columns(
        tags$img(src = "images/plot-inla.png", class = "home-visualization-image"),
        tags$div(
          includeMarkdown("www/content/visualization.md"),
          class = "home-visualization-copy"
        )
      )
    )
  )
) # end nav_panel Home
