# Forecast report (PDF) =======================================================
# Builds a styled, single-country, ensemble-centric PDF report from a completed
# MicroHub run. Pages: (1) cover — run configuration & data handling + a "data
# used" plot (no forecast) arguing the configuration; (2..G+1) one page per
# target group — forecast plot + caption, then a one-row strip of an outlook tile (the
# categorical 4-week call) beside reference stats (cumulative SARI, vs last season,
# season peak), and a "weekly trend & certainty" card. Pure base-R pdf() device +
# ggplot2 + cowplot + gridExtra + ggtext (no LaTeX/pandoc).
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

# ---- categorical (5-level) trend palette -----------------------------------
# Asymmetric green->red ramp used by the categorical outlook hero and the weekly
# trend strip. Falling and stable are greens (a stable respiratory season is
# reassuring, not neutral); rises go warm. Keys are ordered low->high, so a
# category's position also encodes its direction.
.report_cat_levels <- c("large_decrease", "decrease", "stable", "increase", "large_increase")
.report_cat_colors <- c(
  large_decrease = "#176B3A",  # deep green
  decrease       = "#2E8B57",  # green
  stable         = "#B7E0A0",  # light green ("not bad, and that's the point")
  increase       = "#E0922E",  # orange
  large_increase = "#C0392B")  # red

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
    card2_label = "PROJECTED VS LAST SEASON",
    card3_label = "SEASON PEAK SO FAR",
    card4_label = "CUMULATIVE SARI",
    card4_sub_na = "no forecast weeks",
    card2_sub = "vs the same week last season",
    na_no_prior = "no prior season",
    card3_sub = "weekly SARI · last season: %s",
    card3_sub_na = "weekly SARI count",
    card4_sub = "next %d weeks · sum of medians",
    cat_title = "Weekly trend and certainty",
    cat_large_decrease = "Large decrease", cat_decrease = "Decrease", cat_stable = "Stable",
    cat_increase = "Increase", cat_large_increase = "Large increase",
    cat_region_recent = "recent weeks (estimated)", cat_region_forecast = "forecast",
    cat_outlook = "OUTLOOK",
    cat_more = "more certain", cat_less = "less certain",
    cat_caption = "One cell per week. Color shows the direction; fill height shows the certainty of the categorical forecast.",
    hero_up = "%d%% chance of an increase by %s",
    hero_down = "%d%% chance of a decrease by %s",
    hero_level = "%d%% chance of staying stable by %s",
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
    caption_main = "**Ensemble forecast for %s, %s.** Shaded bands are the **50%% and 90%% prediction intervals** around the ensemble median; the dashed line marks the **last week used**.",
    caption_drop_sing = " The %d most recent week was set aside by the data cutoff (red ×) and nowcast by the model, so the forecast is anchored %d week behind the latest data.",
    caption_drop_plur = " The %d most recent weeks were set aside by the data cutoff (red ×) and nowcast by the model, so the forecast is anchored %d weeks behind the latest data.",
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
    card2_label = "PROYECTADO VS. TEMPORADA ANTERIOR",
    card3_label = "PICO DE LA TEMPORADA HASTA AHORA",
    card4_label = "IRAG ACUMULADA",
    card4_sub_na = "sin semanas de pronóstico",
    card2_sub = "vs. la misma semana de la temporada anterior",
    na_no_prior = "sin temporada anterior",
    card3_sub = "IRAG semanal · temporada anterior: %s",
    card3_sub_na = "casos semanales de IRAG",
    card4_sub = "próx. %d semanas · suma de medianas",
    cat_title = "Tendencia semanal y certeza",
    cat_large_decrease = "Gran disminución", cat_decrease = "Disminución", cat_stable = "Estable",
    cat_increase = "Aumento", cat_large_increase = "Gran aumento",
    cat_region_recent = "semanas recientes (estimadas)", cat_region_forecast = "pronóstico",
    cat_outlook = "PERSPECTIVA",
    cat_more = "más certeza", cat_less = "menos certeza",
    cat_caption = "Una celda por semana. El color indica la dirección; la altura de la barra indica la certeza del pronóstico categórico.",
    hero_up = "%d%% prob. de un aumento para %s",
    hero_down = "%d%% prob. de una disminución para %s",
    hero_level = "%d%% prob. de mantenerse estable para %s",
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
    caption_main = "**Pronóstico de conjunto para %s, %s.** Las bandas sombreadas son los **intervalos de predicción del 50%% y del 90%%** en torno a la mediana del conjunto; la línea discontinua marca la **última semana utilizada**.",
    caption_drop_sing = " La %d semana más reciente se excluyó debido al rezago de datos (× roja) y fue estimada por el modelo (nowcast), por lo que el pronóstico queda anclado %d semana por detrás de los datos más recientes.",
    caption_drop_plur = " Las %d semanas más recientes se excluyeron debido al rezago de datos (× roja) y fueron estimadas por el modelo (nowcast), por lo que el pronóstico queda anclado %d semanas por detrás de los datos más recientes.",
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
    card2_label = "PROJEÇÃO VS. TEMPORADA ANTERIOR",
    card3_label = "PICO DA TEMPORADA ATÉ AGORA",
    card4_label = "SRAG ACUMULADA",
    card4_sub_na = "sem semanas de previsão",
    card2_sub = "vs. a mesma semana da temporada anterior",
    na_no_prior = "sem temporada anterior",
    card3_sub = "SRAG semanal · temporada anterior: %s",
    card3_sub_na = "contagem semanal de SRAG",
    card4_sub = "próx. %d semanas · soma das medianas",
    cat_title = "Tendência semanal e certeza",
    cat_large_decrease = "Grande queda", cat_decrease = "Queda", cat_stable = "Estável",
    cat_increase = "Aumento", cat_large_increase = "Grande aumento",
    cat_region_recent = "semanas recentes (estimadas)", cat_region_forecast = "previsão",
    cat_outlook = "PERSPECTIVA",
    cat_more = "mais certeza", cat_less = "menos certeza",
    cat_caption = "Uma célula por semana. A cor indica a direção; a altura da barra indica a certeza da previsão categórica.",
    hero_up = "%d%% chance de um aumento até %s",
    hero_down = "%d%% chance de uma queda até %s",
    hero_level = "%d%% chance de permanecer estável até %s",
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
    caption_main = "**Previsão de conjunto para %s, %s.** As faixas sombreadas são os **intervalos de predição de 50%% e 90%%** em torno da mediana do conjunto; a linha tracejada marca a **última semana utilizada**.",
    caption_drop_sing = " A %d semana mais recente foi excluída devido ao atraso dos dados (× vermelho) e estimada pelo modelo (nowcast), de modo que a previsão fica ancorada %d semana atrás dos dados mais recentes.",
    caption_drop_plur = " As %d semanas mais recentes foram excluídas devido ao atraso dos dados (× vermelho) e estimadas pelo modelo (nowcast), de modo que a previsão fica ancorada %d semanas atrás dos dados mais recentes.",
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

# ---- quantile forecast -> 5-level categorical PMF --------------------------
# A category's probability is simply the share of the predictive distribution that
# falls between two count thresholds. We rebuild a CDF from the ensemble quantiles,
# read it at the four thresholds, and difference adjacent values to get the five
# probabilities. (Deliberately a plain interpolation so there is no new dependency.)

# Build an approximate CDF, P(X <= x), from quantile (level, value) pairs. Values are
# forced strictly increasing first (approx() needs unique x) and the tails are held
# flat (rule = 2). Returns a function of x, or NULL if there are too few points.
.cdf_from_quantiles <- function(levels, values) {
  ok <- is.finite(levels) & is.finite(values)
  levels <- levels[ok]; values <- values[ok]
  if (length(values) < 2) return(NULL)
  ord <- order(values)
  values <- values[ord]; levels <- levels[ord]
  eps <- max(1e-6, diff(range(values)) * 1e-6)            # nudge flat spots apart
  values <- cummax(values) + cumsum(rep(eps, length(values)))
  function(x) pmin(pmax(stats::approx(values, levels, xout = x, rule = 2)$y, 0), 1)
}

# Five-category probabilities for one forecast week. `thresholds` are the four count
# cut-points (ascending): below t1 = large decrease ... above t4 = large increase.
# Returns a named vector over .report_cat_levels that sums to 1, or NULL.
.quantile_to_pmf <- function(levels, values, thresholds) {
  cdf <- .cdf_from_quantiles(levels, values)
  if (is.null(cdf)) return(NULL)
  f <- cdf(thresholds)                                     # F at the four thresholds
  probs <- c(
    large_decrease = f[1],
    decrease       = f[2] - f[1],
    stable         = f[3] - f[2],
    increase       = f[4] - f[3],
    large_increase = 1 - f[4])
  probs[probs < 0] <- 0                                    # clip tiny interpolation noise
  if (sum(probs) > 0) probs <- probs / sum(probs)
  probs
}

# Empirical change thresholds for a horizon: the requested percentiles of how much
# this group's count has historically moved over `horizon` weeks. Prior seasons only
# (no current-season leakage). Returns length(probs) change amounts, or NULL when there is
# too little history (the caller then falls back to fixed percentages).
.empirical_change_thresholds <- function(history, horizon, probs, current_epiyear, min_obs = 8) {
  h <- max(1L, as.integer(round(horizon)))
  train <- history[lubridate::epiyear(history$date) < current_epiyear, ]
  train <- train[!duplicated(train$date), , drop = FALSE]  # one row per week: a duplicate date would explode the date-merge into a cartesian product
  if (nrow(train) <= h) return(NULL)
  # match each week to the value h*7 days earlier by DATE, not by row position, so gaps in
  # the weekly history never compare non-adjacent weeks (which would corrupt the percentiles)
  prior  <- data.frame(date = train$date + 7L * h, prev = train$value)
  merged <- merge(train[, c("date", "value")], prior, by = "date", all.x = TRUE)
  diffs  <- merged$value - merged$prev
  diffs  <- diffs[is.finite(diffs)]
  if (length(diffs) < min_obs) return(NULL)
  as.numeric(stats::quantile(diffs, probs = probs, na.rm = TRUE, names = FALSE))
}

# Category thresholds (as count changes) for one horizon: a TIGHT central "stable" band
# (a small fraction of the current level, so genuinely-flat reads as stable) bracketed by
# the empirical "large increase/decrease" boundaries -- this country/horizon's own history,
# the severity knob. The large boundaries are floored at twice the stable band, so a plain
# decrease/increase zone always remains. Falls back to +/-20% for the large boundaries if history
# is too thin. This is the fix for "visible downtrend shown as stable": the old empirical
# IQR was wide enough (it captures whole seasonal swings) to swallow real trends. The stable
# band is deliberately horizon-constant (a flat fraction of the current level) so "stable"
# means the same thing at every horizon; only the large-move boundaries widen with horizon.
.change_thresholds <- function(history, horizon, anchor_val, probs, stable_frac, current_epiyear) {
  if (!is.finite(anchor_val)) return(NULL)
  sb  <- max(stable_frac * abs(anchor_val), 1)               # stable half-width (>= 1 count, so a near-zero-count flat series still reads "stable")
  emp <- .empirical_change_thresholds(history, horizon, probs, current_epiyear)
  if (is.null(emp)) { lo <- -0.20 * anchor_val; hi <- 0.20 * anchor_val }
  else              { lo <- emp[1]; hi <- emp[length(emp)] } # outer (p05 / p95) severe bounds
  # Use an empirical bound only when it clears the stable band; otherwise the band would
  # swallow it -- collapsing the plain decrease/increase zone and silently degenerating the
  # 5-level scale -- so fall back to a bound at least twice the band (>= the +/-20% default).
  lo_b <- if (is.finite(lo) && lo < -sb) lo else min(-2 * sb, -0.20 * abs(anchor_val))
  hi_b <- if (is.finite(hi) && hi >  sb) hi else max( 2 * sb,  0.20 * abs(anchor_val))
  c(lo_b, -sb, sb, hi_b)
}

# Build the per-week categorical strip for one group: one row per ensemble week that
# lands after the last observed value (the nowcast/bridge weeks plus the forecast
# weeks). Each row's category follows the median forecast; fill height = its certainty.
.build_trajectory <- function(ens_g, anchor_val, anchor_date, forecast_date,
                              history, probs, stable_frac, current_epiyear) {
  if (is.null(ens_g) || nrow(ens_g) == 0 || !is.finite(anchor_val)) return(NULL)
  weeks <- sort(unique(ens_g$date[ens_g$date > anchor_date]))
  if (length(weeks) == 0) return(NULL)
  rows <- lapply(weeks, function(d) {
    wk <- ens_g[ens_g$date == d, ]
    horizon <- as.integer(round(as.numeric(d - anchor_date) / 7))
    thr <- .change_thresholds(history, horizon, anchor_val, probs, stable_frac, current_epiyear)
    if (is.null(thr)) return(NULL)
    pmf <- .quantile_to_pmf(as.numeric(wk$q), wk$value, anchor_val + thr)
    if (is.null(pmf)) return(NULL)
    p_up   <- unname(pmf["increase"]) + unname(pmf["large_increase"])
    p_down <- unname(pmf["decrease"]) + unname(pmf["large_decrease"])
    p_stb  <- unname(pmf["stable"])
    # Category follows the MEDIAN forecast, so the card always agrees with the plotted
    # ensemble line a forecaster sees. (Keying off the PMF mode instead let a heavy tail
    # disagree with the median -- the "downtrend shown as stable" and "flat shown as large
    # increase" gripes.) The probability of the median's direction becomes the certainty
    # that drives the fill height.
    # median = the exact 0.5 quantile (matching the nowcast anchor); skip the week if it is
    # missing/NA rather than letting which.min pick a non-median level and NA-crash the if-chain
    med_q   <- wk$value[abs(as.numeric(wk$q) - 0.5) < 1e-9]
    med_val <- if (length(med_q) >= 1) med_q[1] else NA_real_
    if (!is.finite(med_val)) return(NULL)
    med_change <- med_val - anchor_val
    category <- if (med_change <= thr[1]) "large_decrease"
                else if (med_change <= thr[2]) "decrease"
                else if (med_change <  thr[3]) "stable"
                else if (med_change <  thr[4]) "increase"
                else "large_increase"
    dir <- if (category %in% c("decrease", "large_decrease")) "down"
           else if (category %in% c("increase", "large_increase")) "up" else "level"
    confidence <- switch(dir, down = p_down, up = p_up, p_stb)
    tibble::tibble(
      date = d, horizon = horizon, is_forecast = d > forecast_date,
      category = category, dir = dir, confidence = confidence, med = med_val)
  })
  rows <- dplyr::bind_rows(rows)
  if (nrow(rows) == 0) return(NULL)
  dplyr::arrange(rows, .data$date)
}

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
      expand = ggplot2::expansion(mult = c(0.02, 0.06))) +
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
      # legend ABOVE the panel: it used to sit inside at top-centre, where it overlapped
      # the dashed "last week used" line; placing it outside avoids any overlap
      legend.position = "top", legend.justification = "center",
      legend.box = "horizontal", legend.direction = "horizontal",
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_blank(),
      legend.box.margin = ggplot2::margin(0, 0, 1, 0),
      legend.margin = ggplot2::margin(0, 6, 0, 6), legend.box.spacing = grid::unit(2, "pt"),
      legend.spacing.x = grid::unit(3, "pt"), legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 7.1, colour = "#2B2B2B"), legend.key.size = grid::unit(0.45, "cm"),
      legend.key = ggplot2::element_rect(fill = "white", colour = NA),
      axis.title.x = ggplot2::element_text(size = 9, colour = "grey20", margin = ggplot2::margin(t = 4)),
      axis.title.y = ggplot2::element_text(size = 9, colour = "grey20"),
      plot.margin = ggplot2::margin(6, 12, 4, 8))
}

# ---- card colours ----------------------------------------------------------
# .chg_col is used for the one %-change tile (vs last season): a diverging ramp by
# magnitude --
#   < 0 (fewer cases = better) -> shades of green (deeper with magnitude)
#   ~ 0 (rounds to 0%)          -> neutral grey  [matches the "~0%" value label]
#   > 0 (more cases = worse)    -> yellow -> orange -> red (deeper with magnitude)
# The magnitude cut-points (8%, 20%) are a transparent heuristic. The cumulative tile
# uses its own violet (.report_palette$cumulative); the season-peak tile is brand navy.
.chg_col <- function(pct) {
  if (is.na(pct)) return("#9AA7B2")
  if (round(pct) == 0) return("#8B97A3")
  if (pct < 0) { if (pct <= -20) "#176B3A" else if (pct <= -8) "#2E8B57" else "#4FA06B" }
  else         { if (pct >=  20) "#B23B2E" else if (pct >=   8) "#DB7B2B" else "#D4A017" }
}
# ---- bottom-of-page cards: outlook + reference stats (one row), categorical strip --

# Mix a hex toward white -- used for the tinted backgrounds of the outlook / vs-last-season
# tiles (so the colour is the card; the value text can then be plain dark for legibility).
.tint <- function(hex, amt = 0.85) {
  rgb <- grDevices::col2rgb(hex)[, 1]
  m <- round(rgb + (255 - rgb) * amt)
  grDevices::rgb(m[1], m[2], m[3], maxColorValue = 255)
}

# One horizontal row of cards: the categorical OUTLOOK tile (its category word coloured)
# beside the reference stats (cumulative / vs last season / season peak), on a shared axis.
# `valsize` lets the word-valued outlook tile use a smaller font than the number tiles.
.report_top_row <- function(cards) {
  wrap <- function(s, w) vapply(s, .wrap_cell, character(1), w = w)
  cards$label <- wrap(cards$label, 20)
  cards$value <- wrap(cards$value, 11)        # wraps the long outlook word; numbers stay one line
  cards$sub   <- wrap(cards$sub, 30)
  ncards <- nrow(cards)
  ggplot2::ggplot(cards, ggplot2::aes(x = .data$col, y = 1)) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data$bg), width = 0.95, height = 0.92, colour = "grey78", linewidth = 0.6) +
    ggplot2::geom_tile(ggplot2::aes(y = 1.41, fill = .data$accent), width = 0.95, height = 0.06) +
    ggplot2::geom_text(ggplot2::aes(y = 1.31, label = .data$label), size = 2.1, colour = "#3A4550", fontface = "bold", lineheight = 0.9, vjust = 1) +
    ggplot2::geom_text(ggplot2::aes(y = 1.04, label = .data$value, colour = .data$valcol, size = .data$valsize), fontface = "bold", lineheight = 0.85) +
    ggplot2::geom_text(ggplot2::aes(y = 0.80, label = .data$sub), size = 2.0, colour = "#55606B", lineheight = 0.9, vjust = 1) +
    ggplot2::scale_fill_identity() + ggplot2::scale_colour_identity() + ggplot2::scale_size_identity() +
    ggplot2::coord_cartesian(xlim = c(0.45, ncards + 0.55), ylim = c(0.5, 1.5), expand = FALSE) +
    ggplot2::theme_void() + ggplot2::theme(plot.margin = ggplot2::margin(2, 6, 2, 6))
}

# Legend for the categorical strip, in one bounded box: row 1 = a single diverging bar split
# into the five direction colours (adjacent, no gaps), labels centred under each segment;
# row 2 = a centred fill-height = certainty key (rising-fill cells flanked by less/more).
.report_cat_legend <- function(L) {
  cols <- .report_cat_colors[.report_cat_levels]
  labs <- c(L$cat_large_decrease, L$cat_decrease, L$cat_stable, L$cat_increase, L$cat_large_increase)
  # row 1: one bar, five adjacent segments, labels centred under each
  x0 <- 0.04; x1 <- 0.96; seg_w <- (x1 - x0) / 5
  bar <- data.frame(xmin = x0 + (0:4) * seg_w, xmax = x0 + (1:5) * seg_w, fill = unname(cols))
  lab <- data.frame(x = x0 + (0:4 + 0.5) * seg_w, label = labs)
  # row 2: the certainty key, sized to its content and centred in the box
  cw <- 0.018; cg <- 0.006; pad <- 0.014
  cells_w <- 4 * cw + 3 * cg
  cells_x0 <- (1 - cells_w) / 2   # cells centred; both labels then anchored `pad` from them
  cells <- data.frame(
    xmin   = cells_x0 + (0:3) * (cw + cg),
    xmax   = cells_x0 + (0:3) * (cw + cg) + cw,
    fill_h = c(0.25, 0.5, 0.75, 1.0))
  cy_lo <- 0.16; cy_hi <- 0.34
  ggplot2::ggplot() +
    # row 1: the diverging bar + centred labels
    ggplot2::geom_rect(data = bar, ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = 0.66, ymax = 0.82, fill = .data$fill)) +
    ggplot2::geom_text(data = lab, ggplot2::aes(x = .data$x, y = 0.555, label = .data$label), size = 2.0, colour = "#3A4550") +
    # row 2: neutral cells with rising fill, flanked by the less/more labels (centred group)
    ggplot2::annotate("text", x = cells_x0 - pad, y = 0.25, hjust = 1, size = 2.2, colour = "#55606B", label = L$cat_less) +
    ggplot2::geom_rect(data = cells, ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = cy_lo, ymax = cy_hi), fill = "#E2E0D8") +
    ggplot2::geom_rect(data = cells, ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = cy_lo, ymax = cy_lo + (cy_hi - cy_lo) * .data$fill_h), fill = "#6F6E69") +
    ggplot2::annotate("text", x = cells_x0 + cells_w + pad, y = 0.25, hjust = 0, size = 2.2, colour = "#55606B", label = L$cat_more) +
    ggplot2::scale_fill_identity() +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    ggplot2::theme_void()
}

# The strip itself: one gauge per week. Colour = category, fill height = certainty.
# A dashed "today" divider separates the nowcast (recent, estimated) weeks from the
# forecast weeks, and each region is labelled.
.report_cat_strip <- function(traj, L, forecast_date) {
  n <- nrow(traj)
  df <- data.frame(
    idx = seq_len(n),
    date_lab = vapply(traj$date, function(d) .date_short(d, L), character(1)),
    col = unname(.report_cat_colors[traj$category]),
    prob = traj$confidence,
    is_forecast = traj$is_forecast)
  n_now <- sum(!df$is_forecast)
  hw <- 0.42
  p <- ggplot2::ggplot(df) +
    ggplot2::geom_rect(ggplot2::aes(xmin = .data$idx - hw, xmax = .data$idx + hw, ymin = 0, ymax = 1, fill = .data$col), alpha = 0.22) +
    ggplot2::geom_rect(ggplot2::aes(xmin = .data$idx - hw, xmax = .data$idx + hw, ymin = 0, ymax = .data$prob, fill = .data$col)) +
    ggplot2::geom_rect(ggplot2::aes(xmin = .data$idx - hw, xmax = .data$idx + hw, ymin = 0, ymax = 1), fill = NA, colour = "grey82", linewidth = 0.3) +
    ggplot2::scale_fill_identity()
  if (n_now > 0) {
    p <- p +
      ggplot2::annotate("segment", x = 1 - hw, xend = n_now + hw, y = 1.03, yend = 1.03, colour = "grey70", linewidth = 0.4) +
      ggplot2::annotate("text", x = (1 + n_now) / 2, y = 1.10, label = L$cat_region_recent, size = 2.5, colour = "#55606B")
  }
  if (n_now < n) {
    p <- p +
      ggplot2::annotate("segment", x = (n_now + 1) - hw, xend = n + hw, y = 1.03, yend = 1.03, colour = "grey70", linewidth = 0.4) +
      ggplot2::annotate("text", x = (n_now + 1 + n) / 2, y = 1.10, label = L$cat_region_forecast, size = 2.5, colour = "#55606B")
  }
  if (n_now > 0 && n_now < n) {
    bnd <- n_now + 0.5
    p <- p +
      ggplot2::annotate("segment", x = bnd, xend = bnd, y = 0, yend = 1.15, colour = "#1A1A1A", linewidth = 0.7, linetype = "22") +
      ggplot2::annotate("text", x = bnd, y = 1.22, label = .date_md(forecast_date, L), size = 2.5, colour = "#1A1A1A")
  }
  p +
    ggplot2::scale_x_continuous(breaks = df$idx, labels = df$date_lab) +
    ggplot2::coord_cartesian(xlim = c(0.5, n + 0.5), ylim = c(0, 1.26), expand = FALSE, clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 7, colour = "grey25", margin = ggplot2::margin(t = 3)),
      plot.margin = ggplot2::margin(2, 10, 2, 10))
}

# The categorical outlook gets its own bordered card: title, legend, strip, caption.
.report_cat_card <- function(traj, L, forecast_date) {
  border <- function(inner) cowplot::ggdraw() +
    cowplot::draw_grob(grid::roundrectGrob(r = grid::unit(8, "pt"),
      gp = grid::gpar(fill = "white", col = "grey78", lwd = 1))) +
    cowplot::draw_plot(inner, x = 0.012, y = 0.04, width = 0.976, height = 0.92)
  if (is.null(traj) || nrow(traj) == 0) {
    return(border(cowplot::ggdraw() +
      cowplot::draw_label(L$na_value, x = 0.5, y = 0.5, size = 11, colour = "#55606B")))
  }
  title <- cowplot::ggdraw() +
    cowplot::draw_label(L$cat_title, x = 0.012, hjust = 0, fontface = "bold", size = 9.5, colour = .report_palette$banner)
  # boxless caption (a muted grey line) so the card carries a single outer frame, not
  # a frame-within-a-frame; the reclaimed height goes to the per-week strip cells
  caption <- cowplot::ggdraw() +
    cowplot::draw_label(L$cat_caption, x = 0.012, hjust = 0, size = 6.5, colour = "#7A828B")
  inner <- cowplot::plot_grid(
    title, .report_cat_legend(L), .report_cat_strip(traj, L, forecast_date), caption,
    ncol = 1, rel_heights = c(0.12, 0.18, 0.60, 0.10))
  border(inner)
}

# ---- per-group page --------------------------------------------------------
.report_group_page <- function(grp, page_num, total, report) {
  L <- report$L
  right <- sprintf("%s  ·  %s", report$country, L$subbanner_right)
  body <- cowplot::plot_grid(
    .report_subbanner(grp$group_disp, right),
    .report_forecast_plot(grp, L),
    .caption_bar(grp$caption),
    .report_top_row(grp$cards),
    .report_cat_card(grp$traj, L, report$forecast_date),
    NULL,
    ncol = 1, rel_heights = c(0.06, 0.45, 0.05, 0.145, 0.27, 0.025)
  )
  .report_page(body, page_num, total, report)
}

# ---- cover run-metadata card -----------------------------------------------
# A compact horizontal strip of three tiles under the banner: each tile is a small
# UPPERCASE grey label over a bold dark value, so the label-over-value hierarchy is
# unmistakable and the run dates read start-to-finish. Pulls the dated facts off the
# report object so the banner subtitle can stay a single clean product line.
.report_meta_card <- function(items) {
  tile <- function(label, value) {
    cowplot::ggdraw() +
      cowplot::draw_grob(grid::roundrectGrob(r = grid::unit(5, "pt"),
        gp = grid::gpar(fill = "#F4F6F8", col = "grey80", lwd = 0.8))) +
      # a thin brand-gold rule across the top, inset so it clears the rounded corners
      cowplot::draw_grob(grid::rectGrob(x = 0.5, width = 0.94, y = 0.955, height = 0.05,
        gp = grid::gpar(fill = .report_palette$accent, col = NA))) +
      cowplot::draw_label(toupper(label), x = 0.07, y = 0.66, hjust = 0, size = 7.3,
                          fontface = "bold", colour = "#5B6770") +
      cowplot::draw_label(value, x = 0.07, y = 0.34, hjust = 0, size = 12.5,
                          fontface = "bold", colour = .report_palette$banner)
  }
  tiles <- lapply(items, function(it) tile(it$label, it$value))
  n <- length(tiles)
  cells <- vector("list", 2L * n - 1L)            # tiles on odd slots, NULL spacers on even
  for (i in seq_len(n)) cells[[2L * i - 1L]] <- tiles[[i]]
  rel <- rep(1, 2L * n - 1L); rel[seq(2L, 2L * n - 1L, by = 2L)] <- 0.04
  cowplot::plot_grid(plotlist = cells, nrow = 1, rel_widths = rel)
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
  banner <- .report_banner_grob(sprintf("%s - %s", L$title, report$country), L$subtitle_mid)
  overall <- report$groups[["Overall"]]; if (is.null(overall)) overall <- report$groups[[1]]
  cfg_plot <- .report_forecast_plot(overall, L, show_forecast = FALSE, xlim = c(overall$x_lim[1], overall$anchor + 21))
  cover_drop <- if (sum(overall$obs$dropped_week) > 0) L$cover_caption_drop else ""
  lw <- report$latest_week
  lw_disp <- if (length(lw) == 1 && is.finite(as.numeric(lw))) .date_compact(lw, L) else L$na_value
  meta_card <- .report_meta_card(list(
    list(label = L$subtitle_fdate,     value = .date_compact(report$forecast_date, L)),
    list(label = L$subtitle_generated, value = report$generated),
    list(label = L$cfg_latest_week,    value = lw_disp)))
  # the meta card now owns these two dates, so drop them from the config table below
  cover_tbl <- report$config_all[!report$config_all$Setting %in% c(L$cfg_forecast_date, L$cfg_latest_week), ]
  page1 <- cowplot::plot_grid(
    banner,
    NULL,                                   # breathing room between the banner and the date cards
    meta_card,
    .section_title(L$sec1),
    .report_table_grob(cover_tbl, .report_palette$banner, c(L$tbl_setting, L$tbl_value)),
    .section_title(L$sec2),
    cfg_plot,
    .caption_bar(sprintf(L$cover_caption, overall$group_disp, cover_drop)),
    ncol = 1, rel_heights = c(0.115, 0.03, 0.075, 0.04, 0.30, 0.045, 0.35, 0.045))
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
                                  language = "en",
                                  severity_percentiles = c(0.05, 0.95),
                                  stable_band = 0.10) {
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
    # ---- categorical outlook: per-week trend + certainty strip --------------
    # Reuses the ensemble quantiles (no new model). Each week after the last observed
    # value gets a 5-level category from where its MEDIAN sits versus this group's own
    # historical change, with the probability of that direction as the certainty.
    ens_g <- ens |>
      dplyr::filter(.data$target_group == g) |>
      dplyr::select("date", "q", "value", "horizon")
    # Anchor the categorical to the model's NOWCAST of the latest used week (its own
    # estimate of "now"), not the raw observation -- robust when that week is incomplete
    # (a late report makes the raw value spuriously low). Falls back to the raw value when
    # the ensemble doesn't re-estimate that week (e.g. weeks were dropped before it).
    nc_anchor  <- ens_g$value[ens_g$date == anchor & abs(as.numeric(ens_g$q) - 0.5) < 1e-9]
    cat_anchor <- if (length(nc_anchor) >= 1 && is.finite(nc_anchor[1])) nc_anchor[1] else anchor_val
    traj <- .build_trajectory(ens_g, cat_anchor, anchor, fd, rg_all,
                              severity_percentiles, stable_band, lubridate::epiyear(anchor))
    # the hero summarises the furthest forecast week. We show its actual date (not a fixed
    # "4 weeks out"): data lag + dropped weeks make the real span longer than four weeks.
    if (!is.null(traj) && nrow(traj) > 0) {
      fc_rows  <- traj[traj$is_forecast, , drop = FALSE]
      pick     <- if (nrow(fc_rows) > 0) fc_rows else traj
      hero_row <- pick[which.max(pick$horizon), , drop = FALSE]
      hc       <- hero_row$category
      hdir     <- hero_row$dir
      hsub_tpl <- switch(hdir, up = L$hero_up, down = L$hero_down, L$hero_level)
      hero <- list(dir = hdir, category = hc,
                   col = unname(.report_cat_colors[hc]),
                   label = L[[paste0("cat_", hc)]],
                   sub = sprintf(hsub_tpl, round(100 * hero_row$confidence), .date_short(hero_row$date, L)))
    } else {
      hero <- NULL
    }
    ndg <- sum(obs$dropped_week)
    # caption: the in-plot legend carries the colour mapping. We do NOT claim the
    # excluded weeks are "incomplete"/"will revise" — unknowable from one snapshot;
    # they were set aside by the configured cutoff and are nowcast by the model.
    drop_txt <- if (ndg > 0) sprintf(if (ndg == 1) L$caption_drop_sing else L$caption_drop_plur, ndg, ndg) else ""
    caption <- paste0(sprintf(L$caption_main, country, g_disp), drop_txt)
    # one shared row: the categorical OUTLOOK tile beside three reference cards
    # (cumulative burden, vs last season, season peak). The outlook word is coloured by its
    # category; word tiles use a smaller value font than the number tiles (valsize).
    # cumulative burden = sum of the FORWARD weekly medians (horizon >= 0) that are finite, so
    # the "next N weeks" label and the sum always agree and never count nowcast/NA-median weeks
    fwd_med <- qf$med[is.finite(qf$horizon) & qf$horizon >= 0 & is.finite(qf$med)]
    cum_med <- if (length(fwd_med) > 0) sum(fwd_med) else NA_real_
    n_fwd   <- if (length(fwd_med) > 0) length(fwd_med) else NA_integer_
    out_col <- if (is.null(hero)) "#9AA7B2" else hero$col
    vs_col  <- .chg_col(vs_last)
    cards <- tibble::tibble(
      col = 1:4,
      label = c(L$cat_outlook, L$card4_label, L$card2_label, L$card3_label),
      value = c(if (is.null(hero)) L$na_value else hero$label,
                if (is.na(cum_med)) .fmtL(cum_med, L) else paste0("~", .fmtL(cum_med, L)),
                .pctlabL(vs_last, L), .fmtL(peak_this, L)),
      sub = c(if (is.null(hero)) "" else hero$sub,
              if (is.na(cum_med)) L$card4_sub_na else sprintf(L$card4_sub, n_fwd),
              if (is.na(vs_last)) L$na_no_prior else L$card2_sub,
              if (is.na(peak_last)) L$card3_sub_na else sprintf(L$card3_sub, .fmtL(peak_last, L))),
      # colour-coded tiles (outlook, vs last season) show their colour as a tinted
      # background with a plain dark value; the other two stay white with a coloured value.
      bg = c(.tint(out_col), "white", .tint(vs_col), "white"),
      accent = c(out_col, .report_palette$cumulative, vs_col, .report_palette$banner),
      valcol = c("#1A1A1A", .report_palette$cumulative, "#1A1A1A", "#1A1A1A"),
      valsize = c(3.8, 5.4, 5.4, 5.4))
    list(group_disp = g_disp, obs = obs, last = last, q = qg,
         caption = caption, cards = cards,
         hero = hero, traj = traj,
         anchor = anchor, x_lim = c(x_min, x_max),
         this_lab = cur_season$label, prev_lab = prev_season$label,
         # analytical summary fields backing the cards + strip above
         anchor_val = anchor_val, cat_anchor = cat_anchor, chg4 = chg4,
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
       latest_week = latest_raw,
       L = L, footer_left = sprintf("MicroHub  ·  %s  ·  %s %s", country, L$footer_issued, .date_long(fd, L)),
       config = config, data_handling = data_handling, config_all = dplyr::bind_rows(config, data_handling),
       groups = group_objs)
}
