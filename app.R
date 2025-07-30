# install.packages(c("shiny", "shinyjs", "bslib", "DT", "tidyverse", "gam", "splines", "INLA", "MMWRweek", "cmdstanr", "posterior", "cowplot"))

# Attach libraries =============================================================

library(shiny)
library(shinyjs)
library(bslib)
library(DT)
library(tidyverse)

# Source R scripts =============================================================

source("R/baseline-regular.R")
source("R/baseline-seasonal.R")
source("R/baseline-opt.R")
source("R/inla.R")
source("R/copycat.R")
source("R/plot.R")
source("R/utils.R")

# Set global options ===========================================================

options(
  # Set timeout to one hour
  shiny.timeout = 3600
)

# Define UI ====================================================================

ui <- page_navbar(
  header = tags$head(
    # Use styles.css
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "styles.css"
    ),
    # Initialize shinyjs globally
    useShinyjs(),
  ),
  title = "MicroHub Forecasting",
  theme = bs_theme(
    version = 5,
    bootswatch = "yeti",
    primary = "#BA0C2F",
    secondary = "#C8D8EB",
    header_font = font_google("Oswald"),
    base_font = font_google("Merriweather Sans")
  ),
  navbar_options = list(class = "bg-primary", theme = "dark"),


  ## Home Tab ------------------------------------------------------------------
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
          p("MicroHub Forecasting was developed to create real-time forecasts for Severe Acute Respiratory Illness (SARI) within the Paraguay sentinel surveillance system.")
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
            card_body("Produce weekly forecasts using the key models developed as part of the Paraguay Forecast Hub without needing to modify any code.")
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
        h5("MicroHub Forecasting empowers public health officials with timely, actionable forecasting insights to support effective responses to healthcare needs during the respiratory virus season in Paraguay.", style = "background: #333; color: white; padding: 2em")
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
  ), # end nav_panel
  ## Data upload and common inputs ---------------------------------------------

  nav_panel(
    title = "Data Upload & Settings",
    layout_column_wrap(
      style = css(grid_template_columns = "1fr 2fr"),
      card(
        strong("Download Data Template"),
        helpText(HTML("Download the template and replace the example data with your target data.")),
        actionLink(
          "modal_template",
          " See instructions for using the data template.",
          icon = icon("circle-info"),
          style = "font-size: .875em;"
        ),
        downloadButton(
          "download_template",
          label = "Download Template (.csv)"
        ),
        strong("Upload Data"),
        fileInput(
          "dataframe",
          "Choose CSV File",
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        ),
        div(id = "error_message"),
        strong("Settings for All Models"),
        dateInput(
          "forecast_date",
          label = tagList(
            "Forecast Date",
            actionLink(
              "modal_forecast_date",
              icon("info-circle"),
              style = "margin-left: 5px;"
            )
          ),
          value = closest_wednesday(Sys.Date())
        ),
        selectInput(
          "data_to_drop",
          label = tagList(
            "Data to Drop",
            actionLink(
              "modal_data_drop",
              icon("info-circle"),
              style = "margin-left: 5px;"
            )
          ),
          choices = c("0 weeks", "1 week" = "1 week", "2 weeks" = "2 week"),
          selected = "1 week"
        ),
        numericInput(
          "forecast_horizon",
          label = tagList(
            "Forecast Horizon (Weeks)",
            actionLink(
              "modal_forecast_horizon",
              icon("info-circle"),
              style = "margin-left: 5px;"
            )
          ),
          value = 4,
          min = 1,
          max = 8
        )
      ), # end card
      card(
        card_header("Data Preview"),
        DTOutput("data_preview")
      ) # end card
    ) # end layout_column_wrap
  ), # end nav_panel
  ## Baseline tab --------------------------------------------------------------

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
      ), # end nav_panel
      nav_panel(
        "Seasonal Baseline",
        includeMarkdown("www/content/baseline-seasonal.md"),
        layout_column_wrap(
          heights_equal = "row",
          style = css(grid_template_columns = "1fr 2fr"),
          card(
            div(
              class = "alert alert-warning d-flex align-items-center",
              style = "margin:20px 0px",
              role = "alert",
              shiny::icon("triangle-exclamation", class = "me-2"),
              tags$span(
                "This model is currently under development and will crash the app if run."
              )
            ),
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
      ), # end nav_panel
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
      ), # end nav_panel
    ) # end navset_card_pill
  ), # end nav_panel
  ## INFLAenza tab ------------------------------------------------------------------

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
        selectizeInput(
          "ar_order",
          label = tagList(
            "Order of AR",
            actionLink(
              "modal_ar",
              icon("info-circle"),
              style = "margin-left: 5px;"
            )
          ),
          choices = c(1, 2, 3),
          selected = 1
        ),
        selectizeInput(
          "rw_order",
          label = tagList(
            "Order of RW",
            actionLink(
              "modal_rw",
              icon("info-circle"),
              style = "margin-left: 5px;"
            )
          ),
          choices = c(1, 2),
          selected = 2
        ),
        selectizeInput(
          "seasonal_smoothness",
          label = tagList(
            "Seasonal Smoothness",
            actionLink(
              "modal_seasonal_smoothness",
              icon("info-circle"),
              style = "margin-left: 5px;"
            )
          ),
          choices = c("Default" = "default", "More" = "more", "Less" = "less")
        ),
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
        strong("Optional: Population Data"),
        helpText(HTML("Optionally, provide population data for your target groups to run INFLAenza with a population offset.")),
        actionLink(
          "modal_population",
          " See more information about population data.",
          icon = icon("info-circle"),
          style = "font-size: .875em;"
        ),
        downloadButton(
          "download_population",
          label = "Download Population Data Template (.csv)"
        ),
        strong("Upload Population Data"),
        fileInput(
          "population",
          "Choose CSV File",
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        ),
        div(id = "pop_error_message"),
        radioButtons(
          "use_population_data",
          "Use population offset?",
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
  ), # end nav_panel
  ## Copycat tab ---------------------------------------------------------------

  nav_panel(
    title = "Copycat",
    includeMarkdown("www/content/copycat.md"),
    layout_column_wrap(
      heights_equal = "row",
      style = css(grid_template_columns = "1fr 2fr"),
      card(
        actionButton(
          "run_copycat",
          "Run Copycat"
        ),
        strong("Settings"),
        numericInput(
          "recent_weeks_touse",
          label = tagList(
            "Recent Weeks to Use",
            actionLink(
              "modal_recent_weeks",
              icon("info-circle"),
              style = "margin-left: 5px;"
            )
          ),
          value = 100,
          min = 3,
          max = 100
        ),
        numericInput(
          "resp_week_range",
          label = tagList(
            "Respiratory Week Range",
            actionLink(
              "modal_resp_week_range",
              icon("info-circle"),
              style = "margin-left: 5px;"
            )
          ),
          value = 2,
          min = 0,
          max = 10
        )
      ), # end card
      card(
        plotOutput("copycat_plots"),
        downloadButton(
          "copycat_plot_download",
          "Download Copycat Plot (.png)"
        )
      ) # end card
    ) # end layout_column_wrap
  ), # end nav_panel
  ## Ensemble tab --------------------------------------------------------------

  nav_panel(
    title = "Ensemble",
    includeMarkdown("www/content/ensemble.md"),
    layout_column_wrap(
      heights_equal = "row",
      style = css(grid_template_columns = "1fr 2fr"),
      card(
        actionButton(
          "run_ensemble",
          "Run Ensemble"
        ),
        strong("Settings"),
        selectizeInput(
          "ensemble_models",
          "Select at least two models to include in the ensemble:",
          width = "100%",
          multiple = TRUE,
          choices = NULL,
          options = list(
            placeholder = "Run at least two models to populate this input",
            plugins = list("remove_button")
          )
        ),
      ), # end card
      card(
        plotOutput("ensemble_plots"),
        downloadButton(
          "ensemble_plot_download",
          "Download Ensemble Plot (.png)"
        )
      ) # end card
    ) # end layout_column_wrap
  ), # end nav_panel
  ## Download tab --------------------------------------------------------------

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
  ) # end nav_panel
) # end page_navbar

# Define server ================================================================

server <- function(input, output, session) {
  ## Initialize app ------------------------------------------------------------

  # Reactive values to store data and forecasts
  rv <- reactiveValues(
    data = NULL,
    valid_data = NULL,
    target_groups = NULL,
    overall = NULL,
    baseline_regular = NULL,
    baseline_seasonal = NULL,
    baseline_opt = NULL,
    inla = NULL,
    population = NULL,
    valid_pop = NULL,
    copycat = NULL,
    ensemble = NULL
  )

  # Disable action buttons initially
  disable("run_baseline_regular")
  disable("run_baseline_opt")
  disable("run_baseline_seasonal")
  disable("run_inla")
  disable("inla_plot_download")
  disable("run_copycat")
  disable("copycat_plot_download")
  disable("run_ensemble")
  disable("ensemble_plot_download")
  disable("download_results")

  # Disable use_population_data button initially
  disable("use_population_data")

  ## Download/Upload -----------------------------------------------------------

  # Modal for template instructions
  observeEvent(input$modal_template, {
    show_modal(
      title = "Target Data",
      id = "modal-template",
      md = "modal-template"
    )
  })

  # Modal for forecast date
  observeEvent(input$modal_forecast_date, {
    show_modal(
      title = "Forecast Date",
      id = "modal-forecast-date",
      md = "modal-forecast-date"
    )
  })

  # Modal for data to drop
  observeEvent(input$modal_data_drop, {
    show_modal(
      title = "Data to Drop",
      id = "modal-data-drop",
      md = "modal-data-drop"
    )
  })

  # Modal for forecast horizon
  observeEvent(input$modal_forecast_horizon, {
    show_modal(
      title = "Forecast Horizon (Weeks)",
      id = "modal-forecast-horizon",
      md = "modal-forecast-horizon"
    )
  })

  # Download template
  output$download_template <- downloadHandler(
    filename = function() {
      paste0("microhub-template_", Sys.Date(), ".csv")
    },
    content = function(file) {
      file.copy("data/microhub-data-template.csv", file)
    }
  )

  # Read uploaded data
  observeEvent(input$dataframe, {
    # Remove previous validation messages
    removeUI(
      selector = "#error_message > *",
      immediate = TRUE
    )

    # Remove previously uploaded data
    rv$data <- NULL

    # Run validation check (see validate_data() in utils.R)

    validation_results <- tryCatch(
      validate_data(input$dataframe$datapath),
      error = function(e) {
        insertUI(
          selector = "#error_message",
          where = "beforeEnd",
          ui = div(
            class = "alert alert-danger",
            paste("Error:", e$message)
          )
        )
        return(NULL)
      }
    )

    # ✅ All checks passed
    if (length(validation_results) == 0) {
      rv$valid_data <- TRUE

      insertUI(
        selector = "#error_message",
        where = "beforeEnd",
        ui = div(
          class = "alert alert-success",
          icon("circle-check", style = "margin-right:2px"),
          "All data validation checks passed!"
        )
      )

      # Read in data
      rv$data <- read.csv(input$dataframe$datapath) |>
        mutate(date = as.Date(
          date,
          tryFormats = c("%m/%d/%Y", "%m-%d-%Y", "%Y-%m-%d")
        ))

      # Get vector of target groups
      rv$target_groups <- rv$data |>
        distinct(target_group) |>
        pull()

      # Check if "overall" category equals the sum of individual components
      overall <- rv$data |>
        filter(!target_group == "Overall") |>
        summarize(target_sum = sum(value), .by = c("year", "week", "date")) |>
        left_join(
          rv$data |> filter(target_group == "Overall"),
          by = join_by(year, week, date)
        ) |>
        mutate(overall_equal_sum = ifelse(target_sum == value, TRUE, FALSE))

      # Set `overall` reactive var to single_target or aggregate
      rv$overall <- ifelse(
        any(overall$overall_equal_sum == FALSE),
        "single_target",
        "aggregate"
      )

      # Update the date input to the wednesday nearest last tdate from data
      updateDateInput(
        session,
        "forecast_date",
        value = closest_wednesday(max(as.Date(rv$data$date), na.rm = TRUE) + 3)
      )

      # ❌ Validation failed
    } else {
      rv$valid_data <- FALSE

      # Flatten nested error lists for display
      all_errors <- unlist(validation_results, recursive = TRUE)

      insertUI(
        selector = "#error_message",
        where = "beforeEnd",
        ui = div(
          class = "alert alert-danger",
          icon("circle-exclamation", style = "margin-right:2px"),
          tags$strong("Please review the following errors, correct them in your data, and re-upload."),
          tags$br(),
          tags$br(),
          tags$strong("Validation Errors:"),
          tags$ul(lapply(all_errors, tags$li))
        )
      )
    }
  })

  # Data preview
  output$data_preview <- renderDT(
    datatable(rv$data, rownames = FALSE, filter = "top", selection = "none")
  )

  # Enable run model buttons once data uploaded and validated
  observe({
    if (!is.null(input$dataframe) & isTRUE(rv$valid_data)) {
      enable("run_baseline_regular")
      enable("run_baseline_opt")
      enable("run_baseline_seasonal")
      enable("run_inla")
      enable("run_copycat")
    } else {
      disable("run_baseline_regular")
      disable("run_baseline_opt")
      disable("run_baseline_seasonal")
      disable("run_inla")
      disable("run_copycat")
    }
  })

  ## Regular Baseline ----------------------------------------------------------

  observeEvent(input$run_baseline_regular, {
    req(rv$data)
    withProgress(message = "Regular Baseline", value = 0, {
      incProgress(0.1, detail = "Wrangling data...")

      # Wrangle
      baseline_regular_input <- wrangle_baseline_regular(
        dataframe = rv$data,
        forecast_date = input$forecast_date,
        data_to_drop = input$data_to_drop
      )

      incProgress(0.3, detail = "Fitting model...")

      # Fit
      baseline_regular_results <- fit_process_baseline_regular(
        target_edf = baseline_regular_input$target_edf,
        forecast_date = input$forecast_date,
        desired_max_time_value = baseline_regular_input$desired_max_time_value,
        base_weeks_ahead = baseline_regular_input$base_weeks_ahead,
        forecast_horizons = input$forecast_horizon
      )

      # Save to reactive values
      rv$baseline_regular <- baseline_regular_results |>
        mutate(model = "Regular Baseline", .before = 1)

      incProgress(0.8, detail = "Plotting results...")

      # Plot
      baseline_regular_plot_df <- prepare_historic_data(
        rv$data,
        baseline_regular_results,
        input$forecast_date
      )

      baseline_regular_plots <- rv$target_groups |>
        map(
          plot_state_forecast_try,
          forecast_date = input$forecast_date,
          curr_season_data = baseline_regular_plot_df$curr_season_data,
          forecast_df = baseline_regular_plot_df$forecast_df,
          historic_data = baseline_regular_plot_df$historic_data,
          data_to_drop = input$data_to_drop
        )

      baseline_regular_grid <- plot_grid(
        plotlist = baseline_regular_plots,
        ncol = 1
      )
      baseline_regular_grid <- ggdraw(add_sub(
        baseline_regular_grid,
        "Forecast with the Regular Baseline model.",
        x = 1,
        hjust = 1,
        size = 11,
        color = "gray20"
      ))

      baseline_regular_plot_path <- paste0(
        "figures/plot-baseline_regular_",
        Sys.Date(),
        ".png"
      )

      output$baseline_regular_plots <- renderPlot({
        ggsave(
          baseline_regular_plot_path,
          width = 8,
          height = 8,
          dpi = 300,
          bg = "white"
        )

        # Enable plot download button once plot is saved
        enable("baseline_regular_plot_download")

        # Render the plot
        baseline_regular_grid
      })

      incProgress(1)
    })

    # Download plot
    output$baseline_regular_plot_download <- downloadHandler(
      filename = function() {
        baseline_regular_plot_path
      },
      content = function(file) {
        file.copy(
          baseline_regular_plot_path,
          file,
          overwrite = TRUE
        )
      }
    )
  })

  ## Seasonal Baseline ---------------------------------------------------------

  observeEvent(input$run_baseline_seasonal, {
    req(rv$data)
    withProgress(message = "Seasonal Baseline", value = 0, {
      incProgress(0.1, detail = "Wrangling data...")

      # Wrangle
      baseline_seasonal_input <- wrangle_baseline_seasonal(
        data = rv$data,
        forecast_date = input$forecast_date,
        data_to_drop = input$data_to_drop
      )

      incProgress(0.3, detail = "Fitting model...")

      # Fit
      baseline_seasonal_results <- fit_process_baseline_seasonal(
        clean_data = baseline_seasonal_input,
        forecast_date = input$forecast_date,
        data_to_drop = input$data_to_drop
      )

      # Save to reactive values
      rv$baseline_seasonal <- baseline_seasonal_results |>
        mutate(model = "Seasonal Baseline", .before = 1)

      incProgress(0.8, detail = "Plotting results...")

      # Plot
      baseline_seasonal_plot_df <- prepare_historic_data(
        rv$data,
        baseline_seasonal_results,
        input$forecast_date
      )

      baseline_seasonal_plots <- rv$target_groups |>
        map(
          plot_state_forecast_try,
          forecast_date = input$forecast_date,
          curr_season_data = baseline_seasonal_plot_df$curr_season_data,
          forecast_df = baseline_seasonal_plot_df$forecast_df,
          historic_data = baseline_seasonal_plot_df$historic_data,
          data_to_drop = input$data_to_drop
        )

      baseline_seasonal_grid <- plot_grid(plotlist = baseline_seasonal_plots, ncol = 1)
      baseline_seasonal_grid <- ggdraw(add_sub(
        baseline_seasonal_grid,
        "Forecast with the Seasonal Baseline model.",
        x = 1,
        hjust = 1,
        size = 11,
        color = "gray20"
      ))

      baseline_seasonal_plot_path <- paste0(
        "figures/plot-baseline_seasonal_",
        Sys.Date(),
        ".png"
      )

      output$baseline_seasonal_plots <- renderPlot({
        ggsave(
          baseline_seasonal_plot_path,
          width = 8,
          height = 8,
          dpi = 300,
          bg = "white"
        )

        # Enable plot download button once plot is saved
        enable("baseline_seasonal_plot_download")

        # Render the plot
        baseline_seasonal_grid
      })

      incProgress(1)
    })

    # Download plot
    output$baseline_seasonal_plot_download <- downloadHandler(
      filename = function() {
        baseline_seasonal_plot_path
      },
      content = function(file) {
        file.copy(
          baseline_seasonal_plot_path,
          file,
          overwrite = TRUE
        )
      }
    )
  })


  ## Opt Baseline --------------------------------------------------------------

  observeEvent(input$run_baseline_opt, {
    req(rv$data)
    withProgress(message = "Opt Baseline", value = 0, {
      incProgress(0.1, detail = "Wrangling data...")

      # Wrangle
      baseline_opt_input <- wrangle_baseline_opt(
        dataframe = rv$data,
        forecast_date = input$forecast_date,
        data_to_drop = input$data_to_drop
      )

      incProgress(0.3, detail = "Fitting model...")

      # Fit
      baseline_opt_results <- fit_process_baseline_opt(
        target_edf = baseline_opt_input$target_edf,
        forecast_date = input$forecast_date,
        desired_max_time_value = baseline_opt_input$desired_max_time_value,
        base_weeks_ahead = baseline_opt_input$base_weeks_ahead,
        forecast_horizons = input$forecast_horizon
      )

      # Save to reactive values
      rv$baseline_opt <- baseline_opt_results |>
        mutate(model = "Opt Baseline", .before = 1)

      incProgress(0.8, detail = "Plotting results...")

      # Plot
      baseline_opt_plot_df <- prepare_historic_data(
        rv$data,
        baseline_opt_results,
        input$forecast_date
      )

      baseline_opt_plots <- rv$target_groups |>
        map(
          plot_state_forecast_try,
          forecast_date = input$forecast_date,
          curr_season_data = baseline_opt_plot_df$curr_season_data,
          forecast_df = baseline_opt_plot_df$forecast_df,
          historic_data = baseline_opt_plot_df$historic_data,
          data_to_drop = input$data_to_drop
        )

      baseline_opt_grid <- plot_grid(plotlist = baseline_opt_plots, ncol = 1)
      baseline_opt_grid <- ggdraw(add_sub(
        baseline_opt_grid,
        "Forecast with the Opt Baseline model.",
        x = 1,
        hjust = 1,
        size = 11,
        color = "gray20"
      ))

      baseline_opt_plot_path <- paste0(
        "figures/plot-baseline_opt_",
        Sys.Date(),
        ".png"
      )

      output$baseline_opt_plots <- renderPlot({
        ggsave(
          baseline_opt_plot_path,
          width = 8,
          height = 8,
          dpi = 300,
          bg = "white"
        )

        # Enable plot download button once plot is saved
        enable("baseline_opt_plot_download")

        # Render the plot
        baseline_opt_grid
      })

      incProgress(1)
    })

    # Download plot
    output$baseline_opt_plot_download <- downloadHandler(
      filename = function() {
        baseline_opt_plot_path
      },
      content = function(file) {
        file.copy(
          baseline_opt_plot_path,
          file,
          overwrite = TRUE
        )
      }
    )
  })

  ## INFLAenza -----------------------------------------------------------------

  # Modal for INFLAenza methodology
  # Edit content in www/content/modal-inflaenza.md
  observe({
    onclick("inla_methodology", {
      show_modal(
        title = "INFLAenza Methodology",
        id = "modal-inflaenza",
        md = "modal-inflaenza"
      )
    })
  })

  # Modal for AR
  observeEvent(input$modal_ar, {
    show_modal(
      title = "Order of Autoregression",
      id = "modal-ar",
      md = "modal-ar"
    )
  })

  # Modal for RW
  observeEvent(input$modal_rw, {
    show_modal(
      title = "Order of Random Walk",
      id = "modal-rw",
      md = "modal-rw"
    )
  })

  # Modal for Seasonal Smoothness
  observeEvent(input$modal_seasonal_smoothness, {
    show_modal(
      title = "Seasonal Smoothness",
      id = "modal-seasonal-smoothness",
      md = "modal-seasonal-smoothness"
    )
  })

  # Modal for Forecast Uncertainty
  observeEvent(input$modal_forecast_uncertainty, {
    show_modal(
      title = "Forecast Uncertainty",
      id = "modal-forecast-uncertainty",
      md = "modal-forecast-uncertainty"
    )
  })

  # Modal for population data
  # Edit content in www/content/modal-population.md
  observeEvent(input$modal_population, {
    show_modal(
      title = "Population Data",
      id = "modal-population",
      md = "modal-population"
    )
  })

  # Download population data
  output$download_population <- downloadHandler(
    filename = function() {
      paste0("microhub-population-template_", Sys.Date(), ".csv")
    },
    content = function(file) {
      file.copy("data/population-data-template.csv", file)
    }
  )

  # Read uploaded population data
  observeEvent(input$population, {
    # Remove previous validation messages
    removeUI(
      selector = "#pop_error_message > *",
      immediate = TRUE
    )

    # Remove previously uploaded data
    rv$population <- NULL

    # Run validation check (see validate_data() in utils.R)

    validation_results <- tryCatch(
      validate_population(input$population$datapath),
      error = function(e) {
        insertUI(
          selector = "#pop_error_message",
          where = "beforeEnd",
          ui = div(
            class = "alert alert-danger",
            paste("Error:", e$message)
          )
        )
        return(NULL)
      }
    )

    # ✅ All checks passed
    if (length(validation_results) == 0) {
      rv$valid_pop <- TRUE

      insertUI(
        selector = "#pop_error_message",
        where = "beforeEnd",
        ui = div(
          class = "alert alert-success",
          icon("circle-check", style = "margin-right:2px"),
          "All data validation checks passed!"
        )
      )

      rv$population <- read.csv(input$population$datapath)

      # If user uploads valid population data, enable button and update to "Yes"
      enable("use_population_data")
      updateRadioButtons(
        session,
        "use_population_data",
        selected = "Yes"
      )

      # ❌ Validation failed
    } else {
      rv$valid_pop <- FALSE

      # Flatten nested error lists for display
      all_errors <- unlist(validation_results, recursive = TRUE)

      insertUI(
        selector = "#pop_error_message",
        where = "beforeEnd",
        ui = div(
          class = "alert alert-danger",
          icon("circle-exclamation", style = "margin-right:2px"),
          tags$strong("Please review the following errors, correct them in your data, and re-upload."),
          tags$br(),
          tags$br(),
          tags$strong("Validation Errors:"),
          tags$ul(lapply(all_errors, tags$li))
        )
      )

      # If user uploads invalid population data, disable and update to "No"
      disable("use_population_data")
      updateRadioButtons(
        session,
        "use_population_data",
        selected = "No"
      )
    }
  })

  observeEvent(input$run_inla, {
    req(rv$data)

    withProgress(message = "INFLAenza", value = 0, {
      incProgress(0.1, detail = "Wrangling data...")

      # Wrangle
      if (input$use_population_data == "Yes") {
        fitted_data <- wrangle_inla_population(
          dataframe = rv$data,
          forecast_date = input$forecast_date,
          data_to_drop = input$data_to_drop,
          forecast_horizons = input$forecast_horizon,
          pop_table = rv$population
        )
      } else {
        fitted_data <- wrangle_inla_no_population(
          dataframe = rv$data,
          forecast_date = input$forecast_date,
          data_to_drop = input$data_to_drop,
          forecast_horizons = input$forecast_horizon
        )
      }

      incProgress(0.3, detail = "Fitting model...")

      # Fit
      if (input$use_population_data == "Yes" & rv$overall == "aggregate") {
        inla_results <- fit_process_inla_offset_aggregate(
          fit_df = fitted_data,
          forecast_date = input$forecast_date,
          ar_order = input$ar_order,
          rw_order = input$rw_order,
          seasonal_smoothness = input$seasonal_smoothness,
          forecast_uncertainty_parameter = input$forecast_uncertainty_parameter
        )
        print("INFLAenza using fit_process_inla_offset_aggregate()")
      }

      if (input$use_population_data == "Yes" & rv$overall == "single_target") {
        inla_results <- fit_process_inla_offset_single_target(
          fit_df = fitted_data,
          forecast_date = input$forecast_date,
          ar_order = input$ar_order,
          rw_order = input$rw_order,
          seasonal_smoothness = input$seasonal_smoothness,
          forecast_uncertainty_parameter = input$forecast_uncertainty_parameter
        )
        print("INFLAenza using fit_process_inla_offset_single_target()")
      }

      if (input$use_population_data == "No" & rv$overall == "aggregate") {
        inla_results <- fit_process_inla_no_offset_aggregate(
          fit_df = fitted_data,
          forecast_date = input$forecast_date,
          ar_order = input$ar_order,
          rw_order = input$rw_order,
          seasonal_smoothness = input$seasonal_smoothness,
          forecast_uncertainty_parameter = input$forecast_uncertainty_parameter
        )
        print("INFLAenza using fit_process_inla_no_offset_aggregate()")
      }

      if (input$use_population_data == "No" & rv$overall == "single_target") {
        inla_results <- fit_process_inla_no_offset_single_target(
          fit_df = fitted_data,
          forecast_date = input$forecast_date,
          ar_order = input$ar_order,
          rw_order = input$rw_order,
          seasonal_smoothness = input$seasonal_smoothness,
          forecast_uncertainty_parameter = input$forecast_uncertainty_parameter
        )
        print("INFLAenza using fit_process_inla_no_offset_single_target()")
      }

      # Save to reactive values
      rv$inla <- inla_results |>
        mutate(model = "INFLAenza", .before = 1)

      incProgress(0.8, detail = "Plotting results...")

      # Plot
      inla_plot_df <- prepare_historic_data(
        rv$data,
        inla_results,
        input$forecast_date
      )

      inla_plots <- rv$target_groups |>
        map(
          plot_state_forecast_try,
          forecast_date = input$forecast_date,
          curr_season_data = inla_plot_df$curr_season_data,
          forecast_df = inla_plot_df$forecast_df,
          historic_data = inla_plot_df$historic_data,
          data_to_drop = input$data_to_drop
        )

      inla_grid <- plot_grid(plotlist = inla_plots, ncol = 1)
      inla_grid <- ggdraw(add_sub(
        inla_grid,
        "Forecast with the INFLAenza model.",
        x = 1,
        hjust = 1,
        size = 11,
        color = "gray20"
      ))

      inla_plot_path <- paste0("figures/plot-inla_", Sys.Date(), ".png")

      output$inla_plots <- renderPlot({
        ggsave(
          inla_plot_path,
          width = 8,
          height = 8,
          dpi = 300,
          bg = "white"
        )

        # Enable plot download button once plot is saved
        enable("inla_plot_download")

        # Render the plot
        inla_grid
      })

      incProgress(1)
    })

    # Download plot
    output$inla_plot_download <- downloadHandler(
      filename = function() {
        inla_plot_path
      },
      content = function(file) {
        file.copy(
          inla_plot_path,
          file,
          overwrite = TRUE
        )
      }
    )
  })

  ## Copycat -------------------------------------------------------------------

  # Modal for copycat methodology
  # Edit content in www/content/copycat-methodology.md
  observe({
    onclick("copycat_methodology", {
      show_modal(
        title = "Copycat Methodology",
        id = "modal-copycat",
        md = "modal-copycat"
      )
    })
  })

  # Modal for Recent Weeks to Use
  observeEvent(input$modal_recent_weeks, {
    show_modal(
      title = "Recent Weeks to Use",
      id = "modal-recent-weeks",
      md = "modal-recent-weeks"
    )
  })

  # Modal for Resp Week Range
  observeEvent(input$modal_resp_week_range, {
    show_modal(
      title = "Respiratory Week Range",
      id = "modal-resp-week-range",
      md = "modal-resp-week-range"
    )
  })

  observeEvent(input$run_copycat, {
    req(rv$data)
    withProgress(message = "Copycat", value = 0, {
      incProgress(0.1, detail = "Wrangling data...")

      # Wrangle
      copycat_input <- wrangle_copycat(
        dataframe = rv$data,
        forecast_date = input$forecast_date
      )

      incProgress(0.3, detail = "Fitting model...")

      # Fit
      copycat_results <- fit_process_copycat(
        fit_df = copycat_input$recent_sari,
        historic_df = copycat_input$historic_sari,
        forecast_date = input$forecast_date,
        data_to_drop = input$data_to_drop,
        forecast_horizon = input$forecast_horizon,
        recent_weeks_touse = input$recent_weeks_touse,
        resp_week_range = input$resp_week_range
      ) |>
        mutate(output_type_id = as.numeric(output_type_id))

      # Save to reactive values
      rv$copycat <- copycat_results |>
        mutate(model = "Copycat", .before = 1)

      incProgress(0.8, detail = "Plotting results...")

      # Plot
      copycat_plot_df <- prepare_historic_data(
        rv$data,
        copycat_results,
        input$forecast_date
      )

      copycat_plots <- rv$target_groups |>
        map(
          plot_state_forecast_try,
          forecast_date = input$forecast_date,
          curr_season_data = copycat_plot_df$curr_season_data,
          forecast_df = copycat_plot_df$forecast_df,
          historic_data = copycat_plot_df$historic_data,
          data_to_drop = input$data_to_drop
        )

      copycat_grid <- plot_grid(plotlist = copycat_plots, ncol = 1)
      copycat_grid <- ggdraw(add_sub(
        copycat_grid,
        "Forecast with the Copycat model.",
        x = 1,
        hjust = 1,
        size = 11,
        color = "gray20"
      ))

      copycat_plot_path <- paste0("figures/plot-copycat_", Sys.Date(), ".png")

      output$copycat_plots <- renderPlot({
        ggsave(
          copycat_plot_path,
          width = 8,
          height = 8,
          dpi = 300,
          bg = "white"
        )

        # Enable plot download button once plot is saved
        enable("copycat_plot_download")

        # Render the plot
        copycat_grid
      })

      incProgress(1)
    })

    # Download plot
    output$copycat_plot_download <- downloadHandler(
      filename = function() {
        copycat_plot_path
      },
      content = function(file) {
        file.copy(
          copycat_plot_path,
          file,
          overwrite = TRUE
        )
      }
    )
  })


  ## Ensemble ------------------------------------------------------------------

  # Update selectizeInput with models that have been run
  observe({
    req(combined_results())

    model_choices <- combined_results() |>
      distinct(model) |>
      filter(!model == "Ensemble") |>
      pull(model)

    if (length(model_choices) >= 2) {
      updateSelectizeInput(
        session,
        "ensemble_models",
        choices = model_choices,
        selected = model_choices,
        server = TRUE
      )
    } else {
      updateSelectizeInput(
        session,
        "ensemble_models",
        choices = NULL,
        server = TRUE
      )
    }
  })

  # Enable run_ensemble button once two models have been selected
  observe({
    selected_models <- input$ensemble_models
    toggleState("run_ensemble", condition = length(selected_models) >= 2)
  })

  observeEvent(input$run_ensemble, {
    req(rv$data)
    req(length(combined_results()) >= 2)
    withProgress(message = "Ensemble", value = 0, {
      incProgress(0.1, detail = "Processing...")

      ensemble_results <- combined_results() |>
        filter(model %in% input$ensemble_models) |>
        summarize(
          value = round(median(value), 0),
          .by = c(
            reference_date,
            horizon,
            target_end_date,
            target_group,
            output_type,
            output_type_id
          )
        ) |>
        mutate(
          reference_date = as.Date(reference_date),
          target_end_date = as.Date(target_end_date)
        )

      # Save to reactive values
      rv$ensemble <- ensemble_results |>
        mutate(model = "Ensemble", .before = 1)

      incProgress(0.8, detail = "Plotting results...")

      # Plot
      ensemble_plot_df <- prepare_historic_data(
        rv$data,
        ensemble_results,
        input$forecast_date
      )

      ensemble_plots <- rv$target_groups |>
        map(
          plot_state_forecast_try,
          forecast_date = input$forecast_date,
          curr_season_data = ensemble_plot_df$curr_season_data,
          forecast_df = ensemble_plot_df$forecast_df,
          historic_data = ensemble_plot_df$historic_data,
          data_to_drop = input$data_to_drop
        )

      ensemble_grid <- plot_grid(plotlist = ensemble_plots, ncol = 1)
      ensemble_grid <- ggdraw(add_sub(
        ensemble_grid,
        "Forecast with the Ensemble model.",
        x = 1,
        hjust = 1,
        size = 11,
        color = "gray20"
      ))

      ensemble_plot_path <- paste0("figures/plot-ensemble_", Sys.Date(), ".png")

      output$ensemble_plots <- renderPlot({
        ggsave(
          ensemble_plot_path,
          width = 8,
          height = 8,
          dpi = 300,
          bg = "white"
        )

        # Enable plot download button once plot is saved
        enable("ensemble_plot_download")

        # Render the plot
        ensemble_grid
      })

      incProgress(1)
    })

    # Download plot
    output$ensemble_plot_download <- downloadHandler(
      filename = function() {
        ensemble_plot_path
      },
      content = function(file) {
        file.copy(
          ensemble_plot_path,
          file,
          overwrite = TRUE
        )
      }
    )
  })

  # Download tab ---------------------------------------------------------------

  # Data preview
  combined_results <- reactive({
    req(rv$data)
    req(input$run_baseline_regular > 0 |
      input$run_baseline_seasonal > 0 |
      input$run_baseline_opt > 0 |
      input$run_inla > 0 |
      input$run_copycat > 0)

    combined_results <- bind_rows(
      rv$baseline_regular,
      rv$baseline_seasonal,
      rv$baseline_opt,
      rv$inla,
      rv$copycat,
      rv$ensemble
    ) |>
      mutate(
        model = factor(model),
        reference_date = format(reference_date, "%Y-%m-%d"),
        horizon = round(horizon, 0),
        target_end_date = format(target_end_date, "%Y-%m-%d"),
        value = round(value, 0)
      )
    combined_results
  })

  # Enable download if the dataframe has at least one row
  observe({
    if (!is.null(combined_results()) && nrow(combined_results()) > 0) {
      enable("download_results")
    } else {
      disable("download_results")
    }
  })

  # Show preview table if dataframe has at least one row
  output$results_preview <- renderDT({
    req(nrow(combined_results()) > 0)
    datatable(
      combined_results(),
      rownames = FALSE,
      filter = "top",
      selection = "none",
      options = list(
        columnDefs = list(
          list(targets = 0, width = "150px")
        )
      )
    )
  })

  # Download button
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("microhub-model-output_", Sys.Date(), ".csv")
    },
    content = function(filename) {
      write.csv(x = combined_results(), file = filename, row.names = FALSE)
    }
  )
} # end server

shinyApp(ui, server)
