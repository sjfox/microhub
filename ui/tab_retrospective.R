nav_panel(
  title = "Retrospective",
  layout_columns(
    col_widths = c(4, 8),
    card(
      # Mirrors .data-tab-scroll-panel on the results column: this column is
      # taller than the viewport once a few configurations exist, and without
      # its own scroll region the overflow is simply unreachable.
      class = "retrospective-controls-card",
      strong("Data"),
      fileInput(
        "retrospective_file",
        "Choose CSV File",
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv"
        )
      ),
      uiOutput("retrospective_upload_status_ui"),
      tags$div(
        style = "margin-top:8px;",
        helpText(
          "Or pick up where you left off: load a previously downloaded (and extracted) retrospective output folder to view its results and add more models without starting over."
        ),
        shinyFiles::shinyDirButton(
          "retrospective_load_run_dir",
          "Load Previous Run",
          "Select a retrospective output folder",
          class = "btn-outline-secondary btn-sm"
        )
      ),
      tags$hr(),
      strong("Retrospective Settings"),
      # A "retrospective_group" column in the uploaded CSV (e.g. "country")
      # runs every model independently for each of its values. Since
      # different groups can be genuinely different data sources (one
      # location reporting counts, another already reporting a proportion),
      # the single selector below is swapped for one manual Data Type picker
      # per group -- see retrospective_group_seasonality_ui in
      # server/retrospective.R -- the same way the single Local Seasonality
      # selector is swapped for one zone picker per group.
      tags$div(
        id = "retrospective_single_data_type_block",
        radioButtons(
          "retrospective_data_type",
          label = tagList(
            "Data Type",
            modal_info_link("modal_data_type")
          ),
          choices = c("Counts" = "count", "Proportion (0-1)" = "proportion"),
          selected = "count"
        )
      ),
      tags$div(
        id = "retrospective_single_seasonality_block",
        selectizeInput(
          inputId = "retrospective_country_select",
          label = "Local Seasonality",
          choices = epizone_choices,
          selected = "Paraguay",
          width = "100%",
          options = list(
            placeholder = "Type to search countries...",
            maxOptions = length(epizone_choices)
          )
        ),
        shinyjs::hidden(
          radioButtons(
            inputId = "retrospective_seasonality",
            label = NULL,
            choices = list("A" = "A", "B" = "B", "C" = "C", "D" = "D", "E" = "E"),
            selected = "E"
          )
        ),
        uiOutput("retrospective_zone_badge_ui")
      ),
      uiOutput("retrospective_group_seasonality_ui"),
      selectInput(
        "retrospective_start_week",
        "First Reference Week",
        choices = NULL
      ),
      selectInput(
        "retrospective_end_week",
        "Last Reference Week",
        choices = NULL
      ),
      numericInput(
        "retrospective_horizon",
        "Forecast Horizon (Weeks)",
        value = 4,
        min = 1,
        max = 6
      ),
      navset_tab(
        nav_panel(
          "Models",
          # Development models sit at the end of retrospective_model_choices
          # (R/retrospective.R), so a plain list already shows them last.
          # checkboxGroupInput has no real option-group support; the heading
          # used to be folded into the first development model's label, which
          # did not render dependably.
          checkboxGroupInput(
            "retrospective_models",
            "Models",
            choices = retrospective_model_choices,
            selected = retrospective_default_model_choices
          ),
          div(
            style = "display:flex; gap:8px; margin:6px 0 14px 0;",
            actionButton(
              "select_all_retrospective_models",
              "Select All",
              style = "flex:1;"
            ),
            actionButton(
              "clear_retrospective_models",
              "Clear All",
              style = "flex:1;"
            )
          )
        ),
        nav_panel(
          "Advanced Setup",
          selectInput(
            "retrospective_config_model",
            "Model",
            choices = retrospective_model_choices
          ),
          uiOutput("retrospective_parameter_inputs_ui"),
          textInput(
            "retrospective_config_label",
            "Run Label",
            value = ""
          ),
          div(
            style = "display:flex; gap:8px; margin:6px 0 14px 0;",
            actionButton(
              "add_retrospective_config",
              "Add Combination",
              style = "flex:1;"
            ),
            actionButton(
              "reset_retrospective_configs",
              "Reset",
              style = "flex:1;"
            )
          ),
          uiOutput("retrospective_remove_config_ui"),
          # Bounded and scrollable: without this the table grows without limit
          # as configurations are added, stretching the control column past
          # the bottom of the viewport and pushing "Run Retrospective" out of
          # reach.
          tags$div(
            class = "retrospective-config-scroll",
            DTOutput("retrospective_config_table")
          )
        )
      ),
      textInput(
        "retrospective_run_name",
        "Session Name (optional)",
        value = "",
        placeholder = "e.g. copycat-noise-test"
      ),
      helpText(
        "A short label for this whole retrospective run (not to be confused ",
        "with a model's own Run Label above) so you can tell runs apart ",
        "later -- shown in the run summary below and baked into the saved ",
        "output folder's name, so it's visible right in the ",
        "\"Load Previous Run\" browser too."
      ),
      actionButton(
        "run_retrospective",
        "Run Retrospective"
      ),
      uiOutput("retrospective_run_blockers_ui"),
      uiOutput("retrospective_run_size_ui"),
      downloadButton(
        "download_retrospective_zip",
        "Download Retrospective ZIP"
      ),
      tags$hr(),
      # Optional model inputs. Kept below the run/download controls because
      # they apply only to INFLAenza configurations, so most runs never touch
      # them and they shouldn't push the primary controls down the column.
      strong("Optional Model Inputs"),
      helpText(
        "Both are validated against the target groups in the retrospective ",
        "dataset above, and are saved with the run so a loaded run can be ",
        "extended without re-uploading them."
      ),
      helpText(
        "A neighbor graph, in the same two-column format as the Data tab, makes the spatial structure available to INFLAenza's Besag-proper group structure."
      ),
      fileInput(
        "retrospective_neighbor_graph_file",
        label = NULL,
        buttonLabel = "Browse...",
        placeholder = "Upload neighbor graph (.csv)",
        accept = ".csv",
        width = "100%"
      ),
      uiOutput("retrospective_neighbor_graph_status_ui"),
      helpText(
        "A seasonal grouping, in the same two-column format as the Data tab, gives target groups whose seasonality differs from the majority their own seasonal curve."
      ),
      fileInput(
        "retrospective_season_groups_file",
        label = NULL,
        buttonLabel = "Browse...",
        placeholder = "Upload seasonal groups (.csv)",
        accept = ".csv",
        width = "100%"
      ),
      uiOutput("retrospective_season_groups_status_ui"),
      tags$hr(),
      strong("Download Data Template"),
      helpText(HTML("Download the template and replace the example data with your target data.")),
      modal_info_link(
        "modal_retrospective_template",
        label = " See instructions for using the retrospective data template.",
        icon = icon("circle-info"),
        style = "font-size: .875em;"
      ),
      downloadButton(
        "download_retrospective_template",
        label = "Download Template (.csv)"
      )
    ),
    tags$div(
      class = "data-tab-scroll-panel",
      # 1. Interpret with caution ------------------------------------------
      div(
        class = "alert alert-warning",
        style = "padding:10px 12px; margin-bottom:1rem;",
        tags$strong("Interpret with caution. "),
        "These retrospective results are calculated using the finalized uploaded dataset, not archived data snapshots as they were available in real time. Scores are useful for comparing models on this dataset, but they should not be interpreted as expected real-time forecast performance."
      ),
      # 2. Retrospective Summary -- covers every group at once ------------
      card(
        card_header("Retrospective Summary"),
        uiOutput("retrospective_run_summary_ui")
      ),
      # 3. Further Specification -- baseline model + ensemble, applied to
      # every group at once ------------------------------------------------
      card(
        card_header("Further Specification"),
        tags$p(
          class = "plot-helper-text",
          "Choose the baseline model scores are compared against, and which models to combine into an ensemble. Both apply to every retrospective group at once."
        ),
        uiOutput("retrospective_ensemble_controls_ui")
      ),
      # 4. Overall Score Summary across every group (hidden for an
      # ungrouped/single-country run -- see retrospective_overall_pooled_
      # card_ui) ------------------------------------------------------------
      uiOutput("retrospective_overall_pooled_card_ui"),
      # 5. Individual Group Detail -- forecast visualization and this one
      # group's own scoring summary -----------------------------------------
      card(
        card_header("Individual Group Detail"),
        uiOutput("retrospective_group_select_ui"),
        tags$h6("Forecast Visualization", style = "margin-top:12px;"),
        tags$p(
          class = "plot-helper-text",
          "Observed target data with thinned forecast medians and prediction intervals across the retrospective evaluation period."
        ),
        uiOutput("retrospective_plot_model_ui"),
        uiOutput("retrospective_forecast_plot_message_ui"),
        plotOutput("retrospective_ensemble_forecast_plot", height = "460px"),
        tags$hr(),
        tags$h6("Scoring Summary"),
        tags$p(
          class = "plot-helper-text",
          "Weighted interval score (WIS) is lower when forecasts are sharper and better calibrated. Relative WIS compares each model to the selected scoring reference for the same forecast targets."
        ),
        uiOutput("retrospective_group_scoring_reference_note_ui"),
        # navset_underline (NOT navset_card_underline) -- this whole section
        # is already nested inside the "Individual Group Detail" card()
        # above; navset_card_underline() wraps its tabs in its own .card,
        # which under the yeti bootswatch theme rendered as a visibly
        # double-bordered/double-shadowed card within a card.
        # navset_underline() renders the same underlined tabs without that
        # extra wrapper, matching what a single card should look like.
        navset_underline(
          nav_panel(
            "Overall",
            DTOutput("retrospective_score_overall_table")
          ),
          nav_panel(
            "Target Groups",
            downloadButton(
              "download_retrospective_score_target_group_plot",
              "Download Plot"
            ),
            plotOutput("retrospective_score_target_group_plot", height = "420px"),
            DTOutput("retrospective_score_target_group_table")
          ),
          nav_panel(
            "Forecast Dates",
            downloadButton(
              "download_retrospective_score_forecast_date_plot",
              "Download Plot"
            ),
            plotOutput("retrospective_score_forecast_date_plot", height = "420px"),
            DTOutput("retrospective_score_forecast_date_table")
          )
        )
      )
    )
  )
)
