# install.packages(c("shiny", "shinyjs", "bslib", "DT", "tidyverse", "gam", "splines", "INLA", "MMWRweek", "cmdstanr", "posterior", "cowplot"))

# Attach libraries =============================================================

library(shiny)
library(shinyjs)
library(bslib)
library(DT)
library(tidyverse)

# Source R scripts =============================================================

source("R/inla.R")
source("R/sirsea.R")
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
  # Initialize shinyjs globally
  header = useShinyjs(),
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


  # ## Instructions for using tool ---------------------------------------------
  #
  # nav_panel(
  #   title = "Instructions",
  #   page(
  #     card(
  #       card_header("Getting started guide!")
  #     )
  #   ) # end page_sidebar
  # ), # end nav_panel

  ## Data upload and common inputs ---------------------------------------------

  nav_panel(
    title = "Data Upload & Settings",
    page_sidebar(
      sidebar = sidebar(
        open = "always",
        width = 500,
        strong("Download Data Template"),
        helpText(HTML("Download the template and replace the example data with your target data.")),
        actionLink(
          "modal_template",
          "See instructions for using the data template.",
          icon = icon("triangle-exclamation"),
          style = "font-size: .875em"
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
        tags$hr(),
        strong("Settings for All Models"),
        dateInput(
          "forecast_date",
          "Forecast Date",
          value = closest_wednesday(Sys.Date())
        ),
        selectInput(
          "data_to_drop",
          "Data to Drop",
          choices = c("0 weeks", "1 week" = "1 week", "2 weeks" = "2 week"),
          selected = "1 week"
        ),
        numericInput(
          "forecast_horizon",
          "Forecast Horizon (Weeks)",
          value = 4,
          min = 1,
          max = 8
        )
      ), # end sidebar
      card(
        card_header("Data Preview"),
        DTOutput("data_preview")
      )
    ) # end page_sidebar
  ), # end nav_panel
  ## INLA tab ------------------------------------------------------------------

  nav_panel(
    title = "INFLAenza",
    page_sidebar(
      sidebar = sidebar(
        open = "always",
        width = 400,
        strong("Configure INFLAenza Model"),
        selectInput(
          "ar_order",
          "Order of AR",
          choices = c(1, 2, 3),
          selected = 1
        ),
        selectInput(
          "rw_order",
          "Order of RW",
          choices = c(1, 2),
          selected = 2
        ),
        selectInput(
          "seasonal_smoothness",
          "Seasonal Smoothness",
          choices = c("Default" = "default", "More" = "more", "Less" = "less")
        ),
        selectInput(
          "forecast_uncertainty_parameter",
          "Forecast Uncertainty Parameter",
          choices = c("Default" = "default", "Smaller" = "small", "Tiny" = "tiny")
        ),
        tags$hr(),
        actionButton(
          "run_inla",
          "Run INFLAenza"
        ),
        downloadButton(
          "inla_plot_download",
          "Download INFLAenza Plot (.png)"
        )
      ), # end sidebar
      card(
        plotOutput("inla_plots")
      )
    ) # end page_sidebar
  ), # end nav_panel
  ## SIRsea tab ----------------------------------------------------------------

  # Comment out to hide SIRsea tab

  # nav_panel(
  #   title = "SIRsea",
  #   page_sidebar(
  #     sidebar = sidebar(
  #       open = "always",
  #       width = 400,
  #       helpText(HTML("SIRsea uses CmdStan. Please see the <a href='https://mc-stan.org/cmdstanr/articles/cmdstanr.html' target='_blank'>Getting started with CmdStan article</a> for installation instructions. Once installed, run cmdstan_path() in your R console to get the path to your local CmdStan installation to paste in the below text box.")),
  #       textInput(
  #         "cmdstan_path",
  #         "Path to CmdStan",
  #         value = "/Users/spencerfox/.cmdstan/cmdstan-2.36.0"
  #       ),
  #       tags$hr(),
  #       actionButton(
  #         "run_sirsea",
  #         "Run SIRsea"
  #       ),
  #       downloadButton(
  #         "sirsea_plot_download",
  #         "Download SIRsea Plot (.png)"
  #       )
  #     ), # end sidebar
  #     card(
  #       plotOutput("sirsea_plots")
  #     ),
  #   ) # end page_sidebar
  # ), # end nav_panel

  ## Copycat tab ---------------------------------------------------------------

  nav_panel(
    title = "Copycat",
    page_sidebar(
      sidebar = sidebar(
        open = "always",
        width = 400,
        strong("Configure Copycat Model"),
        # selectInput(
        #   "recent_weeks_touse",
        #   "Recent Weeks to Use",
        #   choices = c(3, 5, 7, 10, 12, 15, 20, 100),
        #   selected = 100
        # ),
        # selectInput(
        #   "resp_week_range",
        #   "Resp Week Range",
        #   choices = 0:10,
        #   selected = 2
        # ),
        numericInput(
          "recent_weeks_touse",
          "Recent Weeks to Use",
          value = 100,
          min = 3,
          max = 100
        ),
        numericInput(
          "resp_week_range",
          "Resp Week Range",
          value = 2,
          min = 0,
          max = 10
        ),
        tags$hr(),
        actionButton(
          "run_copycat",
          "Run Copycat"
        ),
        downloadButton(
          "copycat_plot_download",
          "Download Copycat Plot (.png)"
        )
      ), # end sidebar
      card(
        plotOutput("copycat_plots")
      )
    ) # end page_sidebar
  ), # end nav_panel
  ## Download tab --------------------------------------------------------------

  nav_panel(
    title = "Download",
    page_sidebar(
      sidebar = sidebar(
        open = "always",
        width = 200,
        downloadButton(
          "download_results",
          "Download Results (.csv)"
        )
      ), # end sidebar
      card(
        card_header("Results Preview"),
        DTOutput("results_preview")
      )
    ) # end page_sidebar
  ) # end nav_panel
) # end page_navbar

# Define server ================================================================

server <- function(input, output, session) {
  ## Initialize app ------------------------------------------------------------

  # Reactive values to store data and forecasts
  rv <- reactiveValues(
    data = NULL,
    valid = NULL,
    target_groups = NULL,
    inla = NULL,
    sirsea = NULL,
    copycat = NULL
  )

  # Disable action buttons initially
  disable("run_inla")
  disable("inla_plot_download")
  disable("run_sirsea")
  disable("sirsea_plot_download")
  disable("run_copycat")
  disable("copycat_plot_download")
  disable("download_results")

  ## Download/Upload -----------------------------------------------------------

  # Modal for template instructions
  observeEvent(input$modal_template, {
    show_modal(
      title = "Target Data",
      id = "modal-template",
      md = "modal-template"
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

      rv$valid <- TRUE

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
        mutate(date = mdy(date))

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
      rv$valid <- FALSE

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
    if (!is.null(input$dataframe) & isTRUE(rv$valid)) {
      enable("run_inla")
      enable("run_sirsea")
      enable("run_copycat")
    } else {
      disable("run_inla")
      disable("run_sirsea")
      disable("run_copycat")
    }
  })

  ## INLA ----------------------------------------------------------------------

  observeEvent(input$run_inla, {
    req(rv$data)

    withProgress(message = "INFLAenza", value = 0, {
      incProgress(0.1, detail = "Wrangling data...")

      # Wrangle
      fitted_data <- wrangle_inla(
        dataframe = rv$data,
        forecast_date = input$forecast_date,
        data_to_drop = input$data_to_drop,
        forecast_horizons = input$forecast_horizon
      )

      incProgress(0.3, detail = "Fitting model...")
      # Fit
      inla_results <- fit_process_inla(
        fit_df = fitted_data,
        forecast_date = input$forecast_date,
        ar_order = input$ar_order,
        rw_order = input$rw_order,
        seasonal_smoothness = input$seasonal_smoothness,
        forecast_uncertainty_parameter = input$forecast_uncertainty_parameter
      )

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

  ## SIRsea --------------------------------------------------------------------

  observeEvent(input$run_sirsea, {
    req(rv$data)
    req(input$cmdstan_path)

    withProgress(message = "SIRsea", value = 0, {
      incProgress(0.1, detail = "Wrangling data...")

      # Wrangle
      stan_input <- wrangle_sirsea(
        dataframe = rv$data,
        forecast_date = input$forecast_date,
        data_to_drop = input$data_to_drop,
        forecast_horizon = input$forecast_horizon
      )

      incProgress(0.3, detail = "Fitting model...")

      # Fit
      sirsea_results <- fit_process_sirsea(
        dataframe = stan_input$subset_data,
        stan_dat = stan_input$stan_dat,
        forecast_date = input$forecast_date,
        data_to_drop = input$data_to_drop,
        forecast_horizon = input$forecast_horizon,
        cmdstan_path = input$cmdstan_path
      )

      # Save to reactive values
      rv$sirsea <- sirsea_results |>
        mutate(model = "SIRsea", .before = 1)

      incProgress(0.8, detail = "Plotting results...")

      # Plot
      sirsea_plot_df <- prepare_historic_data(
        rv$data,
        sirsea_results,
        input$forecast_date
      )

      sirsea_plots <- rv$target_groups |>
        map(
          plot_state_forecast_try,
          forecast_date = input$forecast_date,
          curr_season_data = sirsea_plot_df$curr_season_data,
          forecast_df = sirsea_plot_df$forecast_df,
          historic_data = sirsea_plot_df$historic_data,
          data_to_drop = input$data_to_drop
        )

      sirsea_grid <- plot_grid(plotlist = sirsea_plots, ncol = 1)
      sirsea_grid <- ggdraw(add_sub(
        sirsea_grid,
        "Forecast with the SIRsea model.",
        x = 1,
        hjust = 1,
        size = 11,
        color = "gray20"
      ))

      sirsea_plot_path <- paste0("figures/plot-sirsea_", Sys.Date(), ".png")

      output$sirsea_plots <- renderPlot({
        ggsave(
          sirsea_plot_path,
          width = 8,
          height = 8,
          dpi = 300,
          bg = "white"
        )

        # Enable plot download button once plot is saved
        enable("sirsea_plot_download")

        # Render the plot
        sirsea_grid
      })

      incProgress(1)
    })

    # Download plot
    output$sirsea_plot_download <- downloadHandler(
      filename = function() {
        sirsea_plot_path
      },
      content = function(file) {
        file.copy(
          sirsea_plot_path,
          file,
          overwrite = TRUE
        )
      }
    )
  })

  ## Copycat -------------------------------------------------------------------

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
      )

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

  # Download tab ---------------------------------------------------------------

  # Data preview
  combined_results <- reactive({
    req(rv$data)
    req(input$run_inla > 0 | input$run_sirsea > 0 | input$run_copycat > 0)
    bind_rows(rv$inla, rv$sirsea, rv$copycat) |>
      mutate(
        model = factor(model),
        reference_date = format(reference_date, "%Y-%m-%d"),
        target_end_date = format(target_end_date, "%Y-%m-%d"),
        horizon = round(horizon, 0),
        value = round(value, 0)
      )
  })

  # Enable download if the dataframe has at least one row
  observe({
    if (nrow(combined_results()) > 0) {
      enable("download_results")
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
      paste0("model-output_", Sys.Date(), ".csv")
    },
    content = function(filename) {
      write.csv(x = combined_results(), file = filename, row.names = FALSE)
    }
  )
} # end server

shinyApp(ui, server)
