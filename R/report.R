# Forecast report (PDF) =======================================================
# Builds a styled, single-country, ensemble-centric PDF report from a completed
# MicroHub run. Pages: (1) cover — run configuration & data handling + a "data
# used" plot (no forecast) arguing the configuration; (2..G+1) one page per
# target group — narrative + forecast plot + a 2x2 grid of KPI cards (projected
# 4-week change, cumulative SARI over 4 weeks, vs last season, season peak). Pure
# base-R pdf() device + ggplot2 + gridExtra + ggtext (no LaTeX/pandoc).
#
# The report is fully internationalised: every reader-facing string lives in
# .report_tr (English/Spanish/Portuguese), and build_forecast_report(language=)
# assembles the localised report. Numbers and dates are localised too.
#
# Conventions (deliberate, for this self-contained module): external calls are
# namespaced (pkg::fn) and dplyr/ggplot2 refs use the .data pronoun, so the module's
# dependencies are explicit and it can be sourced standalone for testing. A local
# `%||%` is defined for the same reason (it matches shiny's exported version).

# ---- palette (Northern Arizona University brand: True Blue navy + Gold) -----
# Primary brand colours applied to the banner/accent/median; semantic data colours
# (red dropped, orange last-season) are kept, plus a green->red KPI severity gradient.
.report_palette <- list(
  banner   = "#003466", accent = "#F1B300",
  zebra = "#EEF2F7", this_obs = "#1A1A1A", last_obs = "#C2792B", median = "#1F4E8C",
  band50 = "#7FA3CC", band90 = "#C7D6E8", dropped = "#C8102E", footer = "grey38",
  cumulative = "#5E4B8B")  # standalone violet for the cumulative KPI tile (distinct from all others)

`%||%` <- function(a, b) if (is.null(a)) b else a  # matches shiny's; for standalone sourcing

# ---- translations ----------------------------------------------------------
# en holds every key (the fallback); es/pt override per key. SARI is rendered
# with the official PAHO/WHO term per language (SRAG in pt, IRAG in es).
# Templates use sprintf placeholders; literal % is escaped as %% in sprintf'd
# strings only. Filled/verified by the translation pass.
.report_tr <- list(
  en = list(
    thousands = ",", decimal = ".", de_dates = FALSE,
    month_abbr = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
    month_full = c("January","February","March","April","May","June","July","August","September","October","November","December"),
    na_value = "n/a",
    title = "MicroHub Forecast Report",
    error_title = "Report could not be generated",
    error_body = "MicroHub could not build the forecast report from the current run. The application is still running; check that an ensemble forecast has been produced, then try downloading again.",
    subtitle_mid = "SARI ensemble forecast",
    subtitle_fdate = "forecast date",
    subtitle_generated = "report generated",
    subbanner_right = "SARI ensemble",
    sec1 = "1. Run configuration & data handling",
    sec2 = "2. Observed data used (no forecast shown)",
    tbl_setting = "Setting", tbl_value = "Value",
    cfg_forecast_date = "Forecast date",
    cfg_first_week = "First forecast week",
    cfg_country_zone = "Country (zone)",
    cfg_models_generated = "Models generated",
    cfg_ensemble_combines = "Ensemble combines",
    cfg_weeks_ahead = "Weeks ahead",
    cfg_target_groups = "Target groups",
    cfg_output_type = "Output type",
    cfg_latest_week = "Latest data week",
    cfg_weeks_dropped = "Weeks dropped",
    cfg_last_week_used = "Last week used",
    cfg_recent_estimated = "Recent weeks estimated",
    val_weeks_ahead = "%d weeks ahead",
    val_quantile = "Quantile (%d levels, %s to %s)",
    val_none = "none", val_to = "to",
    zone_southern = "southern", zone_northern = "northern", zone_na = "n/a",
    minihead = "Key indicators",
    card1_label = "PROJECTED 4-WEEK CHANGE",
    card2_label = "PROJECTED VS LAST SEASON",
    card3_label = "SEASON PEAK SO FAR",
    card4_label = "ESTIMATED SARI · NEXT 4 WEEKS",
    card1_sub = "forecast median vs latest observed",
    card1_sub_na = "no recent data",
    card2_sub = "vs the same week last season",
    na_no_prior = "no prior season",
    card3_sub = "weekly SARI · last season: %s",
    card3_sub_na = "weekly SARI count",
    card4_sub = "sum of weekly medians",
    axis_x = "Week (target-end date)",
    axis_y = "Weekly SARI count",
    legend_median = "Ensemble median",
    legend_excluded = "Excluded (recent)",
    legend_50 = "50% range",
    legend_90 = "90% range",
    legend_last_week = "Last week used",
    season_fmt = "%d season",
    season_fmt2 = "%d-%02d season",
    footer_issued = "forecast issued",
    footer_page = "Page %d of %d",
    trend_large_increase = "rise sharply",
    trend_increase = "rise",
    trend_decrease = "fall",
    trend_large_decrease = "fall sharply",
    trend_stable = "remain roughly stable",
    vs_above = "above", vs_below = "below",
    vs_txt = " That is **%d%% %s** the same week last season.",
    rng_txt = " (90%% range **%s to %s**)",
    chgw_txt = " (about **%s** from the latest observed week)",
    narrative = "The ensemble projects SARI in **%s** (%s) to **%s**%s through the week of **%s**, with a median near **%s/week**%s.%s The highest weekly count so far this season is **%s**.",
    caption_main = "**Ensemble forecast for %s, %s.** Shaded bands are the **50%% and 90%% prediction intervals** around the ensemble median; the dashed line marks the **last week used**.",
    caption_drop_sing = " The %d most recent week was set aside by the data cutoff (red ×) and nowcast by the model, so the forecast is anchored %d week behind the latest data.",
    caption_drop_plur = " The %d most recent weeks were set aside by the data cutoff (red ×) and nowcast by the model, so the forecast is anchored %d weeks behind the latest data.",
    outlook = "%s through the week of %s; central forecast near %s/week (4-week change %s)",
    cover_outlook = "**%s outlook:** projected to %s.",
    cover_caption = "**Observed weekly SARI counts (%s).** This season (black) vs the same weeks last season (orange, faded). The dashed vertical line marks the **last week used**.%s This is the data the forecast is built on.",
    cover_caption_drop = " The red segment and × after it are **recent weeks set aside** by the data cutoff.",
    groups = list(Overall = "Overall", Adults = "Adults", Pediatrics = "Pediatrics")
  ),
  es = list(
    thousands = ".", decimal = ",", de_dates = TRUE,
    month_abbr = c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic"),
    month_full = c("enero","febrero","marzo","abril","mayo","junio","julio","agosto","septiembre","octubre","noviembre","diciembre"),
    na_value = "n/d",
    title = "Informe de pronóstico de MicroHub",
    error_title = "No se pudo generar el informe",
    error_body = "MicroHub no pudo generar el informe de pronóstico a partir de la ejecución actual. La aplicación sigue en funcionamiento; verifique que se haya producido un pronóstico de conjunto e intente descargarlo de nuevo.",
    subtitle_mid = "Pronóstico de conjunto de IRAG",
    subtitle_fdate = "fecha del pronóstico",
    subtitle_generated = "informe generado",
    subbanner_right = "Conjunto de IRAG",
    sec1 = "1. Configuración de la ejecución y manejo de datos",
    sec2 = "2. Datos observados utilizados (no se muestra el pronóstico)",
    tbl_setting = "Parámetro", tbl_value = "Valor",
    cfg_forecast_date = "Fecha del pronóstico",
    cfg_first_week = "Primera semana del pronóstico",
    cfg_country_zone = "País (zona)",
    cfg_models_generated = "Modelos generados",
    cfg_ensemble_combines = "El conjunto combina",
    cfg_weeks_ahead = "Semanas a futuro",
    cfg_target_groups = "Grupos objetivo",
    cfg_output_type = "Tipo de resultado",
    cfg_latest_week = "Última semana de datos",
    cfg_weeks_dropped = "Semanas descartadas",
    cfg_last_week_used = "Última semana utilizada",
    cfg_recent_estimated = "Semanas recientes estimadas",
    val_weeks_ahead = "%d semanas a futuro",
    val_quantile = "Cuantil (%d niveles, %s a %s)",
    val_none = "ninguno", val_to = "a",
    zone_southern = "sur", zone_northern = "norte", zone_na = "n/d",
    minihead = "Indicadores clave",
    card1_label = "CAMBIO PROYECTADO A 4 SEMANAS",
    card2_label = "PROYECTADO VS. TEMPORADA ANTERIOR",
    card3_label = "PICO DE LA TEMPORADA HASTA AHORA",
    card4_label = "IRAG ESTIMADA · PRÓX. 4 SEMANAS",
    card1_sub = "mediana del pronóstico vs. último valor observado",
    card1_sub_na = "sin datos recientes",
    card2_sub = "vs. la misma semana de la temporada anterior",
    na_no_prior = "sin temporada anterior",
    card3_sub = "IRAG semanal · temporada anterior: %s",
    card3_sub_na = "casos semanales de IRAG",
    card4_sub = "suma de medianas semanales",
    axis_x = "Semana (fecha de fin del objetivo)",
    axis_y = "Casos semanales de IRAG",
    legend_median = "Mediana del conjunto",
    legend_excluded = "Excluidas (recientes)",
    legend_50 = "intervalo 50%",
    legend_90 = "intervalo 90%",
    legend_last_week = "Última semana utilizada",
    season_fmt = "temporada %d",
    season_fmt2 = "temporada %d-%02d",
    footer_issued = "pronóstico emitido",
    footer_page = "Página %d de %d",
    trend_large_increase = "aumentar marcadamente",
    trend_increase = "aumentar",
    trend_decrease = "disminuir",
    trend_large_decrease = "disminuir marcadamente",
    trend_stable = "mantenerse aproximadamente estable",
    vs_above = "por encima de", vs_below = "por debajo de",
    vs_txt = " Eso es **%d%% %s** la misma semana de la temporada anterior.",
    rng_txt = " (intervalo de predicción del 90%% **de %s a %s**)",
    chgw_txt = " (cerca de **%s** frente al último valor observado)",
    narrative = "El pronóstico de conjunto proyecta que la IRAG en **%s** (%s) va a **%s**%s hasta la semana del **%s**, con una mediana cercana a **%s/semana**%s.%s El mayor número semanal de casos en lo que va de la temporada es **%s**.",
    caption_main = "**Pronóstico de conjunto para %s, %s.** Las bandas sombreadas son los **intervalos de predicción del 50%% y del 90%%** en torno a la mediana del conjunto; la línea discontinua marca la **última semana utilizada**.",
    caption_drop_sing = " La %d semana más reciente se excluyó debido al rezago de datos (× roja) y fue estimada por el modelo (nowcast), por lo que el pronóstico queda anclado %d semana por detrás de los datos más recientes.",
    caption_drop_plur = " Las %d semanas más recientes se excluyeron debido al rezago de datos (× roja) y fueron estimadas por el modelo (nowcast), por lo que el pronóstico queda anclado %d semanas por detrás de los datos más recientes.",
    outlook = "%s hasta la semana del %s; pronóstico central cercano a %s/semana (cambio a 4 semanas %s)",
    cover_outlook = "**Perspectiva (%s):** se proyecta que va a %s.",
    cover_caption = "**Casos semanales de IRAG observados (%s).** Esta temporada (negro) vs. las mismas semanas de la temporada anterior (naranja, atenuado). La línea vertical discontinua marca la **última semana utilizada**.%s Estos son los datos sobre los que se construye el pronóstico.",
    cover_caption_drop = " El segmento rojo y la × que le sigue son **semanas recientes excluidas** debido al rezago de datos.",
    groups = list(Overall = "Totales", Adults = "Adultos", Pediatrics = "Pediátricos")
  ),
  pt = list(
    thousands = ".", decimal = ",", de_dates = TRUE,
    month_abbr = c("jan","fev","mar","abr","mai","jun","jul","ago","set","out","nov","dez"),
    month_full = c("janeiro","fevereiro","março","abril","maio","junho","julho","agosto","setembro","outubro","novembro","dezembro"),
    na_value = "n/d",
    title = "Relatório de Previsão MicroHub",
    error_title = "Não foi possível gerar o relatório",
    error_body = "O MicroHub não conseguiu gerar o relatório de previsão a partir da execução atual. O aplicativo continua em funcionamento; verifique se uma previsão de conjunto foi gerada e tente baixá-lo novamente.",
    subtitle_mid = "Previsão de conjunto de SRAG",
    subtitle_fdate = "data da previsão",
    subtitle_generated = "relatório gerado em",
    subbanner_right = "conjunto de SRAG",
    sec1 = "1. Configuração da execução e tratamento dos dados",
    sec2 = "2. Dados observados utilizados (sem previsão exibida)",
    tbl_setting = "Parâmetro", tbl_value = "Valor",
    cfg_forecast_date = "Data da previsão",
    cfg_first_week = "Primeira semana prevista",
    cfg_country_zone = "País (zona)",
    cfg_models_generated = "Modelos gerados",
    cfg_ensemble_combines = "O conjunto combina",
    cfg_weeks_ahead = "Semanas à frente",
    cfg_target_groups = "Grupos-alvo",
    cfg_output_type = "Tipo de saída",
    cfg_latest_week = "Semana mais recente dos dados",
    cfg_weeks_dropped = "Semanas descartadas",
    cfg_last_week_used = "Última semana utilizada",
    cfg_recent_estimated = "Semanas recentes estimadas",
    val_weeks_ahead = "%d semanas à frente",
    val_quantile = "Quantil (%d níveis, de %s a %s)",
    val_none = "nenhuma", val_to = "a",
    zone_southern = "sul", zone_northern = "norte", zone_na = "n/d",
    minihead = "Indicadores-chave",
    card1_label = "VARIAÇÃO PROJETADA EM 4 SEMANAS",
    card2_label = "PROJEÇÃO VS. TEMPORADA ANTERIOR",
    card3_label = "PICO DA TEMPORADA ATÉ AGORA",
    card4_label = "SRAG ESTIMADA · PRÓX. 4 SEMANAS",
    card1_sub = "mediana da previsão vs. último valor observado",
    card1_sub_na = "sem dados recentes",
    card2_sub = "vs. a mesma semana da temporada anterior",
    na_no_prior = "sem temporada anterior",
    card3_sub = "SRAG semanal · temporada anterior: %s",
    card3_sub_na = "contagem semanal de SRAG",
    card4_sub = "soma das medianas semanais",
    axis_x = "Semana (data final do alvo)",
    axis_y = "Contagem semanal de SRAG",
    legend_median = "Mediana do conjunto",
    legend_excluded = "Excluídas (recentes)",
    legend_50 = "faixa de 50%",
    legend_90 = "faixa de 90%",
    legend_last_week = "Última semana utilizada",
    season_fmt = "temporada %d",
    season_fmt2 = "temporada %d-%02d",
    footer_issued = "previsão emitida em",
    footer_page = "Página %d de %d",
    trend_large_increase = "aumentar acentuadamente",
    trend_increase = "aumentar",
    trend_decrease = "diminuir",
    trend_large_decrease = "diminuir acentuadamente",
    trend_stable = "permanecer aproximadamente estável",
    vs_above = "acima", vs_below = "abaixo",
    vs_txt = " Isso está **%d%% %s** da mesma semana da temporada anterior.",
    rng_txt = " (faixa de 90%% **de %s a %s**)",
    chgw_txt = " (cerca de **%s** em relação ao último valor observado)",
    narrative = "A previsão de conjunto projeta que a SRAG em **%s** (%s) deve **%s**%s até a semana de **%s**, com mediana próxima a **%s/semana**%s.%s A maior contagem semanal até agora nesta temporada é **%s**.",
    caption_main = "**Previsão de conjunto para %s, %s.** As faixas sombreadas são os **intervalos de predição de 50%% e 90%%** em torno da mediana do conjunto; a linha tracejada marca a **última semana utilizada**.",
    caption_drop_sing = " A %d semana mais recente foi excluída devido ao atraso dos dados (× vermelho) e estimada pelo modelo (nowcast), de modo que a previsão fica ancorada %d semana atrás dos dados mais recentes.",
    caption_drop_plur = " As %d semanas mais recentes foram excluídas devido ao atraso dos dados (× vermelho) e estimadas pelo modelo (nowcast), de modo que a previsão fica ancorada %d semanas atrás dos dados mais recentes.",
    outlook = "%s até a semana de %s; previsão central próxima a %s/semana (variação em 4 semanas %s)",
    cover_outlook = "**Perspectiva para %s:** projeta-se que deve %s.",
    cover_caption = "**Contagens semanais observadas de SRAG (%s).** Esta temporada (preto) vs. as mesmas semanas da temporada anterior (laranja, esmaecido). A linha vertical tracejada marca a **última semana utilizada**.%s Estes são os dados sobre os quais a previsão é construída.",
    cover_caption_drop = " O segmento vermelho e o × após ele são as **semanas recentes excluídas** devido ao atraso dos dados.",
    groups = list(Overall = "Totais", Adults = "Adultos", Pediatrics = "Pediátricos")
  )
)

# resolve a language to a complete dictionary (per-key fallback to English)
.resolve_lang <- function(language) {
  language <- if (is.null(language)) "en" else tolower(as.character(language))
  if (!language %in% names(.report_tr)) language <- "en"
  L <- utils::modifyList(.report_tr$en, .report_tr[[language]])
  L$groups <- utils::modifyList(.report_tr$en$groups, .report_tr[[language]]$groups %||% list())
  L
}

# ---- localised number / date / label helpers -------------------------------
.fmtL <- function(x, L) if (length(x) == 0 || is.na(x)) L$na_value else format(round(x), big.mark = L$thousands, decimal.mark = L$decimal)
.pctlabL <- function(x, L) if (is.na(x)) L$na_value else if (round(x) == 0) "~0%" else sprintf("%+d%%", x)
.date_long <- function(d, L) {
  d <- as.Date(d); dd <- as.integer(format(d, "%d")); m <- as.integer(format(d, "%m")); y <- format(d, "%Y")
  if (isTRUE(L$de_dates)) sprintf("%d de %s de %s", dd, L$month_full[m], y) else sprintf("%d %s %s", dd, L$month_abbr[m], y)
}
.date_md <- function(d, L) {
  d <- as.Date(d); dd <- as.integer(format(d, "%d")); m <- as.integer(format(d, "%m"))
  if (isTRUE(L$de_dates)) sprintf("%d de %s", dd, L$month_full[m]) else sprintf("%s %d", L$month_abbr[m], dd)
}
# compact numeric date in locale order: es/pt day/month (25/04), en month-day (04-25)
.date_short <- function(d, L) if (isTRUE(L$de_dates)) format(as.Date(d), "%d/%m") else format(as.Date(d), "%m-%d")
# compact day + abbreviated-month + year (e.g. "2 may 2026") for the space-constrained
# banner subtitle, where the full "2 de mayo de 2026" form would overflow
.date_compact <- function(d, L) {
  d <- as.Date(d); sprintf("%d %s %s", as.integer(format(d, "%d")), L$month_abbr[as.integer(format(d, "%m"))], format(d, "%Y"))
}
.grp_name <- function(g, L) { v <- L$groups[[g]]; if (is.null(v)) g else v }

# categorical trend label from a percent change (FluSight-style thresholds)
.cat_of <- function(pct) {
  ifelse(is.na(pct), "n/a",
    ifelse(pct >= 20, "Large increase",
      ifelse(pct >= 5, "Increase",
        ifelse(pct > -5, "Stable",
          ifelse(pct > -20, "Decrease", "Large decrease")))))
}
# trend phrase (localised) for a trend category
.trend_phrase <- function(cat, L) switch(cat,
  "Large increase" = L$trend_large_increase, "Increase" = L$trend_increase,
  "Decrease" = L$trend_decrease, "Large decrease" = L$trend_large_decrease, L$trend_stable)

# map the app's WHO transmission-zone code (input$seasonality is "A".."E", set from
# the epizone assignment) to a season hemisphere: A/B/C span the year boundary
# (northern-style), D/E are calendar-year (southern-style) — matching the A/B/C vs
# D/E split in baseline-seasonal.R. Also passes through "northern"/"southern".
.zone_hemisphere <- function(s) {
  if (is.null(s) || length(s) != 1 || is.na(s)) return(NA_character_)
  s <- as.character(s)
  if (toupper(s) %in% c("A", "B", "C")) "northern"
  else if (toupper(s) %in% c("D", "E")) "southern"
  else if (s %in% c("northern", "southern")) s
  else "southern"
}
# the season containing date d, by hemisphere. Southern (default): a season is a
# calendar year (winter peak mid-year). Northern: a season spans the year boundary
# (~MMWR week 40 / Oct start, matching baseline-seasonal.R). Label localised via L.
.season_of <- function(d, hemisphere = "southern", L = .report_tr$en) {
  d <- as.Date(d); y <- lubridate::year(d); m <- lubridate::month(d)
  if (identical(hemisphere, "northern")) {
    sy <- if (m >= 10) y else y - 1
    list(start = as.Date(sprintf("%d-10-01", sy)), end = as.Date(sprintf("%d-09-30", sy + 1)),
         label = sprintf(L$season_fmt2, sy, (sy + 1) %% 100))
  } else {
    list(start = as.Date(sprintf("%d-01-01", y)), end = as.Date(sprintf("%d-12-31", y)),
         label = sprintf(L$season_fmt, y))
  }
}
# wrap a single table cell onto multiple lines so long values don't overflow
.wrap_cell <- function(s, w = 74) paste(strwrap(s, width = w), collapse = "\n")

# ---- styled table grob -----------------------------------------------------
.report_table_grob <- function(df, header_fill, col_labels = names(df), base_size = 9) {
  df <- as.data.frame(lapply(df, function(col) {
    if (is.numeric(col)) format(col, big.mark = ",", trim = TRUE) else as.character(col)
  }), stringsAsFactors = FALSE, check.names = FALSE)
  names(df) <- col_labels
  zebra <- rep(c("white", .report_palette$zebra), length.out = max(nrow(df), 1))
  theme <- gridExtra::ttheme_minimal(
    base_size = base_size,
    core = list(fg_params = list(hjust = 0, x = 0.04, col = "grey15"),
                bg_params = list(fill = zebra, col = "grey85", lwd = 0.5)),
    colhead = list(fg_params = list(col = "white", fontface = "bold", hjust = 0, x = 0.04),
                   bg_params = list(fill = header_fill, col = header_fill)))
  # stretch the columns to fill the page width (avoids a narrow table floating in gutters)
  gridExtra::tableGrob(df, rows = NULL, theme = theme,
                       widths = grid::unit(c(0.33, 0.67), "npc"))
}

.report_banner_grob <- function(title, subtitle) {
  # shrink the title font for long "Report - Country" strings (some country names run
  # to 40+ chars) so the title never overflows the fixed-width banner
  fs <- max(10L, min(20L, 980L %/% max(1L, nchar(title))))
  grid::grobTree(
    grid::rectGrob(gp = grid::gpar(fill = .report_palette$banner, col = NA)),
    grid::rectGrob(y = 0.04, height = 0.06, gp = grid::gpar(fill = .report_palette$accent, col = NA)),
    grid::textGrob(title, x = 0.025, hjust = 0, y = 0.62, gp = grid::gpar(col = "white", fontsize = fs, fontface = "bold")),
    grid::textGrob(subtitle, x = 0.025, hjust = 0, y = 0.26, gp = grid::gpar(col = "white", fontsize = 9)))
}

.report_subbanner <- function(group, right_text) {
  grid::grobTree(
    grid::rectGrob(gp = grid::gpar(fill = .report_palette$banner, col = NA)),
    grid::rectGrob(y = 0.07, height = 0.09, gp = grid::gpar(fill = .report_palette$accent, col = NA)),
    grid::textGrob(group, x = 0.022, hjust = 0, y = 0.58, gp = grid::gpar(col = "white", fontsize = 15, fontface = "bold")),
    grid::textGrob(right_text, x = 0.978, hjust = 1, y = 0.58, gp = grid::gpar(col = "white", fontsize = 9.5)))
}

.section_title <- function(text) {
  cowplot::ggdraw() + cowplot::draw_label(text, x = 0.01, hjust = 0, fontface = "bold", size = 12.5,
                                          colour = .report_palette$banner)
}
.minihead <- function(text) {
  cowplot::ggdraw() + cowplot::draw_label(toupper(text), x = 0.006, hjust = 0, fontface = "bold", size = 8, colour = "#4A5560")
}
# full-width boxed text via ggtext: auto-wraps to the box width + supports **bold**
.boxed_text <- function(text, size = 2.85, txt = "#262626", fill = "#F4F6F8", border = "grey78") {
  ggplot2::ggplot(data.frame(x = 0, y = 0, label = text)) +
    ggtext::geom_textbox(ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      width = grid::unit(1, "npc"), hjust = 0, vjust = 0.5, halign = 0, valign = 0.5,
      size = size, colour = txt, fill = fill, box.colour = border, box.size = 0.5,
      box.r = grid::unit(2.5, "pt"), box.padding = grid::unit(c(4, 8, 4, 8), "pt"), lineheight = 1.2) +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(-0.5, 0.5), expand = c(0, 0)) +
    ggplot2::theme_void() + ggplot2::theme(plot.margin = ggplot2::margin(1, 1, 1, 1))
}
.caption_bar <- function(text) .boxed_text(text, size = 2.75, txt = "#333333", fill = "#F4F6F8", border = "grey78")
.narrative_box <- function(text) .boxed_text(text, size = 3.2, txt = "#1A1A1A", fill = "#EAF0F6", border = "#C5D5E6")

# ---- page margins + footer -------------------------------------------------
.with_margins <- function(content, lr = 0.05, top = 0.015, bottom = 0.02) {
  inner <- cowplot::plot_grid(NULL, content, NULL, ncol = 1, rel_heights = c(top, 1, bottom))
  cowplot::plot_grid(NULL, inner, NULL, ncol = 3, rel_widths = c(lr, 1, lr))
}
.report_footer <- function(page_num, total, report) {
  cowplot::ggdraw() +
    cowplot::draw_line(x = c(0.05, 0.95), y = c(0.88, 0.88), colour = "grey75", linewidth = 0.5) +
    cowplot::draw_label(report$footer_left, x = 0.05, y = 0.5, hjust = 0, size = 8, colour = .report_palette$footer) +
    cowplot::draw_label(sprintf(report$L$footer_page, page_num, total), x = 0.95, y = 0.5, hjust = 1, size = 8, colour = .report_palette$footer)
}
.report_page <- function(content, page_num, total, report) {
  cowplot::plot_grid(.with_margins(content), .report_footer(page_num, total, report), NULL,
                     ncol = 1, rel_heights = c(1, 0.034, 0.016))
}

# ---- forecast plot (this season vs last; show_forecast=FALSE for the cover) -
.report_forecast_plot <- function(grp, L, show_forecast = TRUE, xlim = grp$x_lim) {
  pal <- .report_palette
  used <- dplyr::filter(grp$obs, !.data$dropped_week, !is.na(.data$value))
  dropped <- dplyr::filter(grp$obs, .data$dropped_week, !is.na(.data$value))
  q <- grp$q
  has_q <- show_forecast && !is.null(q) && nrow(q) > 0
  has_last <- !is.null(grp$last) && nrow(grp$last) > 0
  has_drop <- nrow(dropped) > 0
  this_lab <- grp$this_lab; prev_lab <- grp$prev_lab
  med_lab <- L$legend_median; drop_lab <- L$legend_excluded
  r50 <- L$legend_50; r90 <- L$legend_90; lw_lab <- L$legend_last_week
  # connect the last used point through the dropped weeks so the line has no gap
  red_seg <- if (has_drop && nrow(used) > 0) dplyr::arrange(dplyr::bind_rows(utils::tail(used, 1), dropped), .data$date) else dropped
  col_vals <- stats::setNames(c(pal$this_obs, pal$last_obs, pal$median, pal$dropped),
                              c(this_lab, prev_lab, med_lab, drop_lab))
  col_breaks <- c(if (has_last) prev_lab, this_lab, if (has_q) med_lab, if (has_drop) drop_lab)
  shp <- stats::setNames(c(16, 16, 16, 4), c(this_lab, prev_lab, med_lab, drop_lab))
  lty <- stats::setNames(c(1, 1, 1, 1), c(this_lab, prev_lab, med_lab, drop_lab))
  # only declare the PI fill scale when bands are actually drawn (cover plot has none)
  fill_scale <- if (has_q) ggplot2::scale_fill_manual(name = NULL,
    values = stats::setNames(c(pal$band50, pal$band90), c(r50, r90)),
    breaks = c(r50, r90), guide = ggplot2::guide_legend(order = 2)) else NULL
  xlab_fn <- function(x) { m <- as.integer(format(x, "%m")); out <- paste(L$month_abbr[m], format(x, "%Y")); out[is.na(m)] <- ""; out }
  p <- ggplot2::ggplot()
  if (has_q) p <- p +
    ggplot2::geom_ribbon(data = q, ggplot2::aes(.data$target_end_date, ymin = .data$lo90, ymax = .data$hi90, fill = r90)) +
    ggplot2::geom_ribbon(data = q, ggplot2::aes(.data$target_end_date, ymin = .data$lo50, ymax = .data$hi50, fill = r50))
  if (has_last) p <- p +
    ggplot2::geom_line(data = grp$last, ggplot2::aes(.data$date_aligned, .data$value, colour = prev_lab), linewidth = 0.7, alpha = 0.5) +
    ggplot2::geom_point(data = grp$last, ggplot2::aes(.data$date_aligned, .data$value, colour = prev_lab), size = 0.7, alpha = 0.5)
  if (has_q) p <- p +
    ggplot2::geom_line(data = q, ggplot2::aes(.data$target_end_date, .data$med, colour = med_lab), linewidth = 1) +
    ggplot2::geom_point(data = q, ggplot2::aes(.data$target_end_date, .data$med, colour = med_lab), size = 1.5)
  p <- p +
    ggplot2::geom_line(data = used, ggplot2::aes(.data$date, .data$value, colour = this_lab), linewidth = 0.55) +
    ggplot2::geom_point(data = used, ggplot2::aes(.data$date, .data$value, colour = this_lab), size = 1.3)
  if (has_drop) p <- p +
    ggplot2::geom_line(data = red_seg, ggplot2::aes(.data$date, .data$value, colour = drop_lab), linewidth = 0.7) +
    ggplot2::geom_point(data = dropped, ggplot2::aes(.data$date, .data$value, colour = drop_lab), shape = 4, size = 2.6, stroke = 1.2)
  p +
    ggplot2::geom_vline(data = data.frame(xi = grp$anchor),
      ggplot2::aes(xintercept = .data$xi, linetype = lw_lab), colour = "#3A3A3A", linewidth = 0.8,
      key_glyph = "path") +
    ggplot2::scale_colour_manual(name = NULL, values = col_vals, breaks = col_breaks,
      guide = ggplot2::guide_legend(order = 1, override.aes = list(shape = unname(shp[col_breaks]),
        linetype = unname(lty[col_breaks]), linewidth = 1.1, size = 2.7))) +
    fill_scale +
    ggplot2::scale_linetype_manual(name = NULL, values = stats::setNames("42", lw_lab),
      guide = ggplot2::guide_legend(order = 3, override.aes = list(colour = "#3A3A3A"))) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(big.mark = L$thousands, decimal.mark = L$decimal), breaks = scales::breaks_pretty(n = 5),
      expand = ggplot2::expansion(mult = c(0.02, 0.17))) +
    ggplot2::scale_x_date(date_breaks = "2 months", labels = xlab_fn) +
    ggplot2::coord_cartesian(xlim = xlim) +
    ggplot2::labs(x = L$axis_x, y = L$axis_y) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      # open L-axes: left + bottom only, light horizontal guides, no box/frame
      axis.line = ggplot2::element_line(colour = "grey45", linewidth = 0.5),
      panel.border = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "grey92", linewidth = 0.4),
      axis.ticks = ggplot2::element_line(colour = "grey55", linewidth = 0.4),
      # legend INSIDE the panel, centred in the reserved whitespace band at the top
      # (even margins all round; larger keys that read at a glance against the plot)
      legend.position = c(0.5, 0.99), legend.justification = c(0.5, 1),
      legend.box = "horizontal", legend.direction = "horizontal",
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_rect(fill = "white", colour = "grey70", linewidth = 0.5),
      legend.box.margin = ggplot2::margin(0, 0, 0, 0),
      legend.margin = ggplot2::margin(3, 6, 3, 6), legend.box.spacing = grid::unit(0, "pt"),
      legend.spacing.x = grid::unit(3, "pt"), legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 7.1, colour = "#2B2B2B"), legend.key.size = grid::unit(0.45, "cm"),
      legend.key = ggplot2::element_rect(fill = "white", colour = NA),
      axis.title.x = ggplot2::element_text(size = 9, colour = "grey20", margin = ggplot2::margin(t = 4)),
      axis.title.y = ggplot2::element_text(size = 9, colour = "grey20"),
      plot.margin = ggplot2::margin(6, 12, 4, 8))
}

# ---- KPI cards: four white tiles in a 2x2 grid, coloured accent + value ------
# Diverging colour for the two %-change tiles (4-week change, vs last season):
#   < 0 (fewer cases = better) -> shades of green (deeper with magnitude)
#   ~ 0 (rounds to 0%)          -> neutral grey  [matches the "~0%" value label]
#   > 0 (more cases = worse)    -> yellow -> orange -> red (deeper with magnitude)
# The darkest shades (|change| >= 20%) line up with the "large increase/decrease"
# narrative category. NOTE: the magnitude cut-points (8%, 20%) are a transparent
# heuristic in the spirit of the FluSight/PAHO 5-level trend framing; they are NOT
# empirically derived per zone (PAHO sets those from each zone's historical change
# distribution). Tunable here if science-grounded thresholds are wired in later.
# The cumulative tile uses its own standalone colour (.report_palette$cumulative);
# the season-peak tile is neutral brand navy.
.chg_col <- function(pct) {
  if (is.na(pct)) return("#9AA7B2")
  if (round(pct) == 0) return("#8B97A3")
  if (pct < 0) { if (pct <= -20) "#176B3A" else if (pct <= -8) "#2E8B57" else "#4FA06B" }
  else         { if (pct >=  20) "#B23B2E" else if (pct >=   8) "#DB7B2B" else "#D4A017" }
}
# renders the pre-built grp$cards tibble (labels/values/subs/colours from build)
.report_stat_cards <- function(grp) {
  cards <- grp$cards
  ggplot2::ggplot(cards, ggplot2::aes(x = .data$col, y = .data$row)) +
    ggplot2::geom_tile(fill = "white", width = 0.96, height = 0.9, colour = "grey78", linewidth = 0.6) +
    ggplot2::geom_tile(ggplot2::aes(y = .data$row + 0.38, fill = .data$accent), width = 0.96, height = 0.07) +
    ggplot2::geom_text(ggplot2::aes(y = .data$row + 0.2, label = .data$label), size = 2.55, colour = "#3A4550", fontface = "bold") +
    ggplot2::geom_text(ggplot2::aes(y = .data$row - 0.02, label = .data$value, colour = .data$valcol), size = 6.6, fontface = "bold") +
    ggplot2::geom_text(ggplot2::aes(y = .data$row - 0.24, label = .data$sub), size = 2.6, colour = "#55606B") +
    ggplot2::scale_fill_identity() + ggplot2::scale_colour_identity() +
    ggplot2::coord_cartesian(xlim = c(0.45, 2.55), ylim = c(0.45, 2.55), expand = FALSE) +
    ggplot2::theme_void() + ggplot2::theme(plot.margin = ggplot2::margin(2, 6, 2, 6))
}

# ---- per-group page --------------------------------------------------------
.report_group_page <- function(grp, page_num, total, report) {
  L <- report$L
  right <- sprintf("%s  ·  %s", report$country, L$subbanner_right)
  body <- cowplot::plot_grid(
    .report_subbanner(grp$group_disp, right),
    .narrative_box(grp$narrative),
    .report_forecast_plot(grp, L),
    .caption_bar(grp$caption),
    .minihead(L$minihead),
    .report_stat_cards(grp),
    NULL,
    ncol = 1, rel_heights = c(0.07, 0.10, 0.40, 0.075, 0.028, 0.30, 0.022)
  )
  .report_page(body, page_num, total, report)
}

# ---- main entry ------------------------------------------------------------
write_forecast_report_pdf <- function(report, file, width = 8.5, height = 11) {
  if (is.null(report$groups) || length(report$groups) == 0) stop("No ensemble forecast is available to report.")
  L <- report$L
  total <- 1L + length(report$groups)
  grDevices::pdf(file = file, width = width, height = height, onefile = TRUE)
  # close only OUR device on exit (never a Shiny/render device that might be current)
  dev_num <- grDevices::dev.cur()
  on.exit(if (dev_num %in% grDevices::dev.list()) grDevices::dev.off(dev_num), add = TRUE)

  # ---- Page 1: cover ------------------------------------------------------
  # country sits in the title ("... Report - Chile"); the subtitle no longer repeats it
  banner <- .report_banner_grob(sprintf("%s - %s", L$title, report$country),
    sprintf("%s   ·   %s %s   ·   %s %s",
            L$subtitle_mid, L$subtitle_fdate, .date_compact(report$forecast_date, L),
            L$subtitle_generated, report$generated))
  overall <- report$groups[["Overall"]]; if (is.null(overall)) overall <- report$groups[[1]]
  outlook <- .boxed_text(sprintf(L$cover_outlook, overall$group_disp, overall$outlook),
                         size = 3.0, txt = "#1A1A1A", fill = "#EAF0F6", border = "#C5D5E6")
  cfg_plot <- .report_forecast_plot(overall, L, show_forecast = FALSE, xlim = c(overall$x_lim[1], overall$anchor + 21))
  cover_drop <- if (sum(overall$obs$dropped_week) > 0) L$cover_caption_drop else ""
  page1 <- cowplot::plot_grid(
    banner,
    outlook,
    .section_title(L$sec1),
    .report_table_grob(report$config_all, .report_palette$banner, c(L$tbl_setting, L$tbl_value)),
    .section_title(L$sec2),
    cfg_plot,
    .caption_bar(sprintf(L$cover_caption, overall$group_disp, cover_drop)),
    ncol = 1, rel_heights = c(0.13, 0.06, 0.04, 0.33, 0.05, 0.33, 0.05))
  print(.report_page(page1, 1L, total, report))

  # ---- Pages 2..G+1: per group (Overall first) ----------------------------
  for (i in seq_along(report$groups)) print(.report_group_page(report$groups[[i]], i + 1L, total, report))
  invisible(file)
}

# Minimal one-page fallback PDF, written when report generation fails so the download
# still yields a valid file and no error escapes the Shiny download handler (an escaping
# error can tear down the session -> grey "disconnected" overlay needing a restart).
# Deliberately plain (banner + message, no ggtext/tableGrob) so it cannot itself fail.
write_report_error_pdf <- function(file, language = "en", width = 8.5, height = 11) {
  L <- .resolve_lang(language)
  grDevices::pdf(file = file, width = width, height = height, onefile = TRUE)
  dev_num <- grDevices::dev.cur()
  on.exit(if (dev_num %in% grDevices::dev.list()) grDevices::dev.off(dev_num), add = TRUE)
  page <- cowplot::plot_grid(
    .report_banner_grob(L$title, L$subtitle_mid),
    cowplot::ggdraw() +
      cowplot::draw_label(L$error_title, x = 0.05, y = 0.72, hjust = 0, fontface = "bold",
                          size = 15, colour = .report_palette$banner) +
      cowplot::draw_label(paste(strwrap(L$error_body, width = 88), collapse = "\n"),
                          x = 0.05, y = 0.5, hjust = 0, vjust = 1, size = 11, colour = "#333333"),
    NULL, ncol = 1, rel_heights = c(0.13, 0.82, 0.05))
  print(.with_margins(page))
  invisible(file)
}

# ---- assemble the report object from a completed run -----------------------
build_forecast_report <- function(country, raw_data, ensemble, forecast_date,
                                  data_to_drop, seasonality = NA, ensemble_models = NULL,
                                  output_models = NULL, quantiles = NULL, generated = NULL,
                                  language = "en") {
  L <- .resolve_lang(language)
  fd <- as.Date(forecast_date)
  raw_data <- dplyr::mutate(raw_data, date = as.Date(.data$date), value = as.numeric(.data$value))
  ens <- ensemble |> dplyr::mutate(date = as.Date(.data$target_end_date), horizon = as.integer(round(.data$horizon)),
                                   q = as.character(.data$output_type_id), value = as.numeric(.data$value))
  pdat <- get_plot_data(raw_data, fd, data_to_drop)
  used <- get_fcast_data(raw_data, fd, data_to_drop)
  if (!nrow(used)) stop("No usable observed weeks remain after applying the data cutoff.")
  anchor <- max(used$date, na.rm = TRUE)
  H <- max(ens$horizon, na.rm = TRUE)
  groups <- sort(unique(ens$target_group))
  groups <- c(intersect("Overall", groups), setdiff(groups, "Overall"))  # Overall first
  # season-scoped windows (for "peak so far" / "last season peak" and the line labels).
  # seasonality arrives as a WHO zone code (A-E); map it to a hemisphere first.
  hemi <- .zone_hemisphere(seasonality)
  cur_season <- .season_of(anchor, hemi, L)
  prev_season <- .season_of(cur_season$start - 1, hemi, L)
  # plot the current season (from its start), keeping a 12-week minimum width so the
  # black line is a single, honestly-labelled season rather than spanning the prior one
  x_min <- min(cur_season$start, anchor - 84); x_max <- max(ens$date, na.rm = TRUE)

  # pivot the ensemble quantiles to wide PI columns. Tolerant of an ensemble that
  # is missing one of the expected levels (e.g. a member that only emitted an inner
  # interval): any absent level is filled with NA so the rename never errors and the
  # plot simply omits that band. Empty groups stay 0-row and are handled downstream.
  qlvls <- c(lo90 = "0.05", lo50 = "0.25", med = "0.5", hi50 = "0.75", hi90 = "0.95")
  wide_q <- function(g) {
    w <- ens |>
      dplyr::filter(.data$target_group == g, .data$q %in% qlvls) |>
      dplyr::select("horizon", "date", "q", "value") |>
      tidyr::pivot_wider(names_from = "q", values_from = "value")
    for (lvl in qlvls) if (!lvl %in% names(w)) w[[lvl]] <- NA_real_
    w |>
      dplyr::rename(dplyr::all_of(c(target_end_date = "date", qlvls))) |>
      dplyr::arrange(.data$target_end_date)
  }

  group_objs <- lapply(groups, function(g) {
    g_disp <- .grp_name(g, L)
    qg <- wide_q(g)
    obs <- pdat |> dplyr::filter(.data$target_group == g, .data$date >= x_min, .data$date <= x_max) |>
      dplyr::select("date", "value", "dropped_week")
    last <- raw_data |> dplyr::filter(.data$target_group == g, !is.na(.data$value)) |>
      dplyr::transmute(date_aligned = .data$date + 364, value = .data$value) |>
      dplyr::filter(.data$date_aligned >= x_min, .data$date_aligned <= x_max)
    used_obs <- dplyr::filter(obs, !.data$dropped_week)
    anchor_val <- if (nrow(used_obs)) tail(used_obs$value, 1) else NA_real_
    # guard a group whose ensemble lacks the expected quantile levels (empty qg):
    # fall back to NA forecast stats + an observed-only plot rather than crashing.
    qf <- if (nrow(qg) > 0) dplyr::filter(qg, .data$horizon >= 0) else qg
    if (nrow(qf) == 0) qf <- qg
    has_fc <- nrow(qf) > 0
    medH <- if (has_fc) qf$med[nrow(qf)] else NA_real_
    loH  <- if (has_fc) qf$lo90[nrow(qf)] else NA_real_
    hiH  <- if (has_fc) qf$hi90[nrow(qf)] else NA_real_
    # projected change over the forecast window, measured against the latest observed week
    chg4 <- if (has_fc && is.finite(anchor_val) && anchor_val > 0) round((medH - anchor_val) / anchor_val * 100) else NA
    rg_all <- dplyr::arrange(dplyr::filter(raw_data, .data$target_group == g), .data$date)
    # vs last season: forecast endpoint vs a 3-week mean centred on the matching
    # calendar week a year earlier (full history). Require the match within ~10 days,
    # so a single-season upload yields no spurious comparison (falls back to NA).
    end_wk <- if (has_fc) max(qf$target_end_date) else NA
    target_back <- if (!is.na(end_wk)) end_wk - 364 else NA
    last_ref <- if (!is.na(target_back) && nrow(rg_all) > 0 && min(abs(as.numeric(rg_all$date - target_back))) <= 10) {
      li <- which.min(abs(as.numeric(rg_all$date - target_back)))
      mean(rg_all$value[intersect((li - 1):(li + 1), seq_len(nrow(rg_all)))], na.rm = TRUE)
    } else NA_real_
    vs_last <- if (is.finite(last_ref) && last_ref > 0) round((medH - last_ref) / last_ref * 100) else NA
    # peaks are scoped to the actual season, not the plot window: this season so far =
    # season start .. anchor; last season = the full previous season.
    pk <- function(lo, hi) { v <- rg_all$value[rg_all$date >= lo & rg_all$date <= hi]; if (length(v)) max(v, na.rm = TRUE) else NA }
    peak_this <- pk(cur_season$start, anchor)
    peak_last <- pk(prev_season$start, prev_season$end)
    trend_cat <- .cat_of(chg4)
    phrase <- .trend_phrase(trend_cat, L)
    end_lab <- if (has_fc) .date_md(max(qf$target_end_date), L) else L$na_value
    ndg <- sum(obs$dropped_week)
    # caption: the in-plot legend carries the colour mapping. We do NOT claim the
    # excluded weeks are "incomplete"/"will revise" — unknowable from one snapshot;
    # they were set aside by the configured cutoff and are nowcast by the model.
    drop_txt <- if (ndg > 0) sprintf(if (ndg == 1) L$caption_drop_sing else L$caption_drop_plur, ndg, ndg) else ""
    caption <- paste0(sprintf(L$caption_main, country, g_disp), drop_txt)
    rng_txt <- if (is.finite(loH) && is.finite(hiH) && hiH > loH) sprintf(L$rng_txt, .fmtL(loH, L), .fmtL(hiH, L)) else ""
    vs_txt <- if (is.na(vs_last)) "" else sprintf(L$vs_txt, abs(vs_last), if (vs_last >= 0) L$vs_above else L$vs_below)
    # state the week-over-week projected change in prose for clearly rising/falling
    # forecasts (omit when ~flat, where it would just read "about ~0%")
    chgw_txt <- if (trend_cat %in% c("Stable", "n/a") || is.na(chg4)) "" else sprintf(L$chgw_txt, .pctlabL(chg4, L))
    narrative <- sprintf(L$narrative, country, g_disp, phrase, chgw_txt, end_lab, .fmtL(medH, L), rng_txt, vs_txt, .fmtL(peak_this, L))
    outlook <- sprintf(L$outlook, phrase, end_lab, .fmtL(medH, L), .pctlabL(chg4, L))
    # KPI cards (2x2). Top row = the two forecast tiles (projected 4-week change +
    # cumulative SARI over the 4-week horizon); bottom row = the reference tiles
    # (vs last season + season peak). The three severity tiles share one green->red
    # gradient, each on its own metric; the season-peak tile is a neutral navy ref.
    cum_med <- if (has_fc) sum(qf$med, na.rm = TRUE) else NA_real_  # sum of forward weekly medians
    col_chg <- .chg_col(chg4); col_vs <- .chg_col(vs_last)
    col_cum <- .report_palette$cumulative   # standalone violet, distinct from the other tiles
    cards <- tibble::tibble(
      col = c(1, 2, 1, 2), row = c(2, 2, 1, 1),
      label = c(L$card1_label, L$card4_label, L$card2_label, L$card3_label),
      value = c(.pctlabL(chg4, L), .fmtL(cum_med, L), .pctlabL(vs_last, L), .fmtL(peak_this, L)),
      sub = c(if (is.na(chg4)) L$card1_sub_na else L$card1_sub,
              if (is.na(cum_med)) L$card1_sub_na else L$card4_sub,
              if (is.na(vs_last)) L$na_no_prior else L$card2_sub,
              if (is.na(peak_last)) L$card3_sub_na else sprintf(L$card3_sub, .fmtL(peak_last, L))),
      accent = c(col_chg, col_cum, col_vs, .report_palette$banner),
      valcol = c(col_chg, col_cum, col_vs, "#1A1A1A"))
    list(group_disp = g_disp, obs = obs, last = last, q = qg,
         caption = caption, narrative = narrative, outlook = outlook, cards = cards,
         anchor = anchor, x_lim = c(x_min, x_max),
         this_lab = cur_season$label, prev_lab = prev_season$label,
         # analytical summary (rendered via the fields above; also asserted by the validation harness)
         trend_cat = trend_cat, anchor_val = anchor_val, chg4 = chg4,
         vs_last = vs_last, peak_this = peak_this, peak_last = peak_last)
  })
  names(group_objs) <- groups

  latest_raw <- max(raw_data$date[raw_data$date <= fd], na.rm = TRUE)
  # show the actual WHO zone code (A-E) when supplied, plus the derived hemisphere
  hemi_word <- if (is.na(hemi)) NA_character_ else if (identical(hemi, "northern")) L$zone_northern else L$zone_southern
  zone_code <- if (length(seasonality) == 1 && !is.na(seasonality) && toupper(as.character(seasonality)) %in% c("A","B","C","D","E")) toupper(as.character(seasonality)) else NA_character_
  zone <- if (is.na(hemi_word)) L$zone_na else if (!is.na(zone_code)) sprintf("%s, %s", zone_code, hemi_word) else hemi_word
  groups_disp <- vapply(groups, function(g) .grp_name(g, L), character(1))
  # first FORWARD forecast week (horizon >= 0), so nowcast/bridge weeks don't show
  fwd_dates <- ens$date[ens$horizon >= 0]
  first_fc <- if (length(fwd_dates)) min(fwd_dates, na.rm = TRUE) else min(ens$date, na.rm = TRUE)
  config <- tibble::tibble(
    Setting = c(L$cfg_forecast_date, L$cfg_first_week, L$cfg_country_zone, L$cfg_models_generated,
                L$cfg_ensemble_combines, L$cfg_weeks_ahead, L$cfg_target_groups, L$cfg_output_type),
    Value = c(format(fd, "%Y-%m-%d"), format(first_fc, "%Y-%m-%d"),
              sprintf("%s (%s)", country, zone),
              if (length(output_models)) .wrap_cell(paste(sort(output_models), collapse = ", ")) else "—",
              if (length(ensemble_models)) .wrap_cell(paste(sort(ensemble_models), collapse = ", ")) else "—",
              sprintf(L$val_weeks_ahead, H + 1), .wrap_cell(paste(groups_disp, collapse = ", ")),
              if (length(quantiles)) sprintf(L$val_quantile, length(quantiles),
                formatC(min(quantiles), format = "f", digits = 2, decimal.mark = L$decimal),
                formatC(max(quantiles), format = "f", digits = 2, decimal.mark = L$decimal)) else L$cfg_output_type))
  data_handling <- tibble::tibble(
    Setting = c(L$cfg_latest_week, L$cfg_weeks_dropped, L$cfg_last_week_used, L$cfg_recent_estimated),
    Value = c(format(latest_raw, "%Y-%m-%d"), as.character(get_weeks_to_drop(data_to_drop)), format(anchor, "%Y-%m-%d"),
              if (latest_raw > anchor) sprintf("%s %s %s", .date_short(anchor + 7, L), L$val_to, .date_short(latest_raw, L)) else L$val_none))

  gen <- generated %||% paste0(.date_compact(Sys.Date(), L), ", ", format(Sys.time(), "%H:%M"))
  list(country = country, forecast_date = fd, target_end_date = min(ens$date), generated = gen,
       L = L, footer_left = sprintf("MicroHub  ·  %s  ·  %s %s", country, L$footer_issued, .date_long(fd, L)),
       config = config, data_handling = data_handling, config_all = dplyr::bind_rows(config, data_handling),
       groups = group_objs)
}
