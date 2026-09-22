# CalCopycat ====================================================================
#
# A method-of-analogues forecaster, like Copycat, but matched directly against
# real historical dates instead of a manufactured per-season trajectory
# database. There is no get_full_year_df()-style season reconstruction, no
# resp_season_year/resp_season_week bucketing, and no per-season GAM spline --
# a "season" is never defined at all. Instead:
#
#   1. The data is assumed to be weekly. "Today" gets compared against the
#      same calendar position 1 year back, 2 years back, 3 years back, and so
#      on -- anchors at exact 52-week multiples -- rather than scoring every
#      historical date in the record. This is plain calendar-week arithmetic,
#      not epiweek, and only a small ring of weeks around each yearly anchor
#      (widened by the "Respiratory Week Range" buffer) is ever scored.
#   2. A candidate is only scored if it has a FULL, real, gap-free trailing
#      window the same length as the current one -- no partial-overlap
#      fallback. Comparing a 4-week estimate to a 12-week estimate on the same
#      raw-error footing is comparing quantities with different sampling
#      noise, so a candidate near the start of its recorded history (or whose
#      window straddles a data gap) is simply not scored, the same way a
#      candidate whose real data doesn't reach the full forecast horizon is
#      never scored (no fabricated growth rate, ever, on either side).
#   3. Eligible candidates are resampled with replacement, weighted by match
#      quality. The score for candidate i is exp(-weight_i / h), where h is
#      set by the BEST weight actually found this time, floored at a
#      data-derived "noise floor" (2x the variance of the current window --
#      the error you'd expect from pure coincidence). Flooring h this way
#      means a single suspiciously-perfect match can't collapse the reference
#      scale down to itself and swamp every other candidate; it still gets
#      rewarded for being genuinely better than chance, just not treated as
#      infinitely certain.
#   4. Each sampled trajectory gets its own random perturbation added at
#      EVERY horizon step (a fresh draw each week, not one draw reused for
#      the whole path), from Normal(0, sd = sqrt(max(that candidate's own
#      weight, the same noise floor) / n_current)). This is what keeps a
#      thin candidate pool (even a single analog) from collapsing into a
#      handful of rigid, repeated trajectories: a candidate that matched
#      poorly gets shaken around more, one that matched well gets barely
#      perturbed, and every trajectory still gets at least the noise floor's
#      worth of shake regardless, since a good historical match is never a
#      guarantee about the future. Dividing by n_current turns the raw,
#      single-week noise level (noise_floor) into the standard error of the
#      WINDOW'S AVERAGE growth rate -- our uncertainty about the underlying
#      trend, which is what this perturbation should reflect, not the raw
#      week-to-week noisiness of individual observations (that's already
#      captured by resampling across real historical weeks, and again by the
#      observation-noise layer below). Drawing a fresh shift each horizon
#      step, rather than reusing one shift for the whole path, makes
#      uncertainty compound the way a random walk should -- spread grows
#      like sqrt(horizon), not linearly in horizon inside an exponential --
#      so it still widens naturally the further out you forecast, without
#      the runaway tails a single reused shift produces.
#   5. There is no separate observation-noise layer -- the growth-rate
#      perturbation above already reflects the data's own volatility, and
#      adding Poisson/Beta sampling noise on top of it turned out to be
#      redundant at best (real historical data already provides plenty of
#      spread once enough analogs exist) and actively broken for small
#      proportions at worst (see the removed code's history). The one thing
#      that layer used to also do -- turning a continuous simulated
#      trajectory into a realistic value -- is instead handled directly by
#      rounding: count forecasts are rounded to the nearest non-negative
#      integer (a real hospitalization count is a whole number), while
#      proportions are left as continuous values in [0, 1].
#   6. For proportion data specifically, the noise floor in point 3 also has
#      a resolution-aware floor of its own. A rounded percentage can report
#      the exact same value for many consecutive weeks once true prevalence
#      falls below the reporting resolution -- a real seasonal trough, not
#      missing data -- which would otherwise make the recent window's
#      empirical variance (and the noise floor derived from it) collapse
#      toward zero exactly when there's the most uncertainty about when the
#      season will turn. calcopycat_fxn()'s resolution_q argument (set by
#      fit_process_calcopycat() from the smallest positive value seen in
#      the data) prevents this: the noise floor is never allowed to fall
#      below the level of noise the data's own rounding resolution would
#      produce on the growth-rate scale. It's a no-op for count data
#      (resolution_q is NA) and mostly a no-op for a proportion series
#      whose recent window still has real volatility of its own -- it only
#      kicks in when that volatility has been rounded away.


# Core matcher/simulator for one target group ===================================

calcopycat_fxn <- function(curr_data,           ## this group's full history (date, value, growth); value already shifted (see fit_process_calcopycat())
                            db,                  ## candidate pool with the same columns (may be curr_data's own group only, or every group, depending on share_groups)
                            forecast_horizon   = 5,
                            recent_weeks_touse = 12,
                            nsamps             = 1000,
                            resp_week_range    = 2,  ## calendar-week buffer around each 52-week-multiple anchor
                            resolution_q       = NA_real_,  ## the data's own reporting resolution (e.g. a percentage's rounding step); NA disables the resolution-aware noise floor (used for count data)
                            group_label        = NULL) {

  most_recent_date  <- max(curr_data$date)
  current_row       <- curr_data[curr_data$date == most_recent_date, ]
  most_recent_value <- current_row$value[[1]]

  current_window <- curr_data |>
    filter(!is.na(growth), date < most_recent_date) |>
    arrange(date) |>
    slice_tail(n = recent_weeks_touse) |>
    pull(growth)

  n_current <- length(current_window)
  if (n_current == 0) {
    stop(
      "CalCopycat: not enough recent real growth data",
      if (!is.null(group_label)) paste0(" for group '", group_label, "'") else "",
      " to build a matching window."
    )
  }

  ## The error you'd expect from pure coincidence: if a candidate's window
  ## were independent, unrelated noise, the expected squared difference
  ## between it and the current window is Var(current) + Var(candidate) --
  ## approximated here using only the current window's own recent volatility,
  ## since that's all we know before we've looked at any candidates yet.
  var_current <- if (n_current >= 2) stats::var(current_window) else 0
  noise_floor <- max(2 * var_current, 1e-6)

  ## A rounded percentage can report the exact same value for many
  ## consecutive weeks once true prevalence falls below the reporting
  ## resolution -- a real seasonal trough, not missing data or "nothing is
  ## changing." That flatness would otherwise make var_current (and the
  ## noise floor above) collapse toward zero right when there's the most
  ## uncertainty about when the season will turn. Guard against that by
  ## never letting the noise floor fall below the level of noise the data's
  ## own rounding resolution would produce on the growth-rate scale: each
  ## reported value is treated as a rounded version of some true value
  ## uniformly distributed within +/- resolution_q / 2 of what was reported
  ## (quantization error variance = resolution_q^2 / 12), propagated
  ## through the growth-rate transform via the delta method at the current
  ## operating point (most_recent_value).
  if (!is.na(resolution_q)) {
    sd_quant_per_point <- resolution_q / sqrt(12)
    quant_floor <- 2 * (sd_quant_per_point / most_recent_value)^2
    noise_floor <- max(noise_floor, quant_floor)
  }

  ## Score a candidate's own trailing growth window against the current one.
  ## Only ever called on a FULL-length window (see the eligibility filter
  ## below) -- no partial, truncated comparison.
  match_weight <- function(hist_window) {
    n <- min(length(hist_window), n_current)
    if (n == 0) return(list(weight = NA_real_, n_overlap = 0))
    h <- utils::tail(hist_window, n)
    c_now <- utils::tail(current_window, n)
    valid <- !is.na(h) & !is.na(c_now)
    if (sum(valid) == 0) return(list(weight = NA_real_, n_overlap = 0))
    list(
      weight    = sum((h[valid] - c_now[valid])^2) / sum(valid),
      n_overlap = sum(valid)
    )
  }

  ## Build the candidate pool one group at a time: instead of scoring every
  ## row in `db`, only the weeks that fall within `resp_week_range` weeks of
  ## a 52-week-multiple anchor ("this same week N years ago") are ever
  ## scored. That keeps the expensive part of this function -- computing a
  ## trailing-window match score -- limited to a handful of weeks per year of
  ## history, not the whole historical record.
  score_group <- function(g) {
    n <- nrow(g)
    if (n == 0) return(g[0, , drop = FALSE])

    growth_vec <- g$growth
    for (h in seq_len(forecast_horizon)) {
      g[[paste0("lead_", h)]] <- dplyr::lead(growth_vec, h)
    }

    earliest_date <- g$date[[1]]
    max_k <- floor(as.numeric(difftime(most_recent_date, earliest_date, units = "weeks")) / 52)
    if (!is.finite(max_k) || max_k < 1) return(g[0, , drop = FALSE])

    idx <- integer(0)
    for (k in seq_len(max_k)) {
      anchor    <- most_recent_date - as.difftime(52 * k * 7, units = "days")
      week_dist <- abs(as.numeric(difftime(g$date, anchor, units = "weeks")))
      idx <- c(idx, which(week_dist <= resp_week_range))
    }
    idx <- sort(unique(idx))
    if (length(idx) == 0) return(g[0, , drop = FALSE])

    match_stats <- lapply(idx, function(i) {
      window_start <- max(1, i - recent_weeks_touse + 1)
      match_weight(growth_vec[window_start:i])
    })

    out <- g[idx, , drop = FALSE]
    out$weight    <- purrr::map_dbl(match_stats, "weight")
    out$n_overlap <- purrr::map_dbl(match_stats, "n_overlap")
    out$gap_weeks <- as.numeric(difftime(most_recent_date, out$date, units = "weeks"))
    out
  }

  db_prepped <- db |>
    group_by(target_group) |>
    arrange(date, .by_group = TRUE) |>
    group_modify(~ score_group(.x)) |>
    ungroup()

  lead_cols <- paste0("lead_", seq_len(forecast_horizon))

  db_prepped <- db_prepped |>
    mutate(future_ok = rowSums(is.na(across(all_of(lead_cols)))) == 0)

  ## Eligibility -- no fabricated data, ever, on either side of the match:
  ##  - a FULL, real, gap-free trailing window -- the same length as the
  ##    current one, not a shorter partial-overlap stand-in -- so every
  ##    candidate's weight is comparable on the same footing
  ##  - far enough from "now" that this candidate's own window and forecast
  ##    range can't just be re-matching against itself (a defensive guard --
  ##    normally moot, since the anchor ring starts ~52 weeks back, but it
  ##    still matters for large Recent-Weeks-to-Use / horizon combinations)
  ##  - real data actually reaches every week the horizon needs
  candidates <- db_prepped |>
    filter(
      !is.na(weight),
      n_overlap == n_current,
      gap_weeks >= (recent_weeks_touse + forecast_horizon),
      future_ok
    )

  if (nrow(candidates) == 0) {
    stop(
      "CalCopycat: no historical analogs",
      if (!is.null(group_label)) paste0(" for group '", group_label, "'") else "",
      " have a full real matching window and real data spanning the full",
      " forecast horizon within the requested Respiratory Week Range. Try a",
      " shorter forecast horizon, fewer Recent Weeks to Use, a larger",
      " Respiratory Week Range, or check that enough historical years of",
      " data exist."
    )
  }

  ## h is set by the best (lowest-error) candidate actually found this time,
  ## but never below the noise floor -- so a single suspiciously-perfect
  ## match can't shrink the reference scale down to itself and make every
  ## other candidate look worthless by comparison.
  h <- max(min(candidates$weight), noise_floor)
  candidates <- candidates |> mutate(score = exp(-weight / h))

  sampled <- candidates |>
    sample_n(size = nsamps, replace = TRUE, weight = score) |>
    mutate(id = seq_along(weight))

  ## A fresh random shift at EVERY horizon step -- see point 4 in the header
  ## comment. Its size is the candidate's own error (or the noise floor)
  ## scaled down by n_current, i.e. the standard error of the window's
  ## average growth rate, not the raw single-week noise level. Drawing
  ## independently per step (rather than reusing one shift across the whole
  ## path) makes the compounding through cumprod() widen like a random walk
  ## (spread grows with sqrt(horizon)) instead of blowing up linearly in
  ## horizon inside an exponential.
  trajectories <- sampled |>
    select(id, weight, all_of(lead_cols)) |>
    tidyr::pivot_longer(all_of(lead_cols), names_to = "horizon", values_to = "growth_rate") |>
    mutate(
      horizon      = as.integer(sub("lead_", "", horizon)),
      growth_shift = rnorm(n(), mean = 0, sd = sqrt(pmax(weight, noise_floor) / n_current))
    ) |>
    arrange(id, horizon) |>
    group_by(id) |>
    mutate(mult_factor = cumprod(exp(growth_rate + growth_shift))) |>
    ungroup() |>
    mutate(forecast = most_recent_value * mult_factor)

  trajectories |> select(id, horizon, forecast)
}


# Fit and process CalCopycat ====================================================

fit_process_calcopycat <- function(df,
                                    fcast_horizon,
                                    quantiles_needed,   ## the desired quantiles for the output
                                    recent_weeks_touse = 12, ## how many recent real weeks to match on
                                    nsamps             = 1000,
                                    resp_week_range    = 2,  ## calendar-week buffer around each 52-week-multiple anchor
                                    share_groups       = TRUE,
                                    data_type          = "count") {

  ## Proportions are left as continuous values in [0, 1]. Counts are real,
  ## whole-number observations, so their simulated trajectories are rounded
  ## to the nearest non-negative integer -- there's no such thing as 1.03
  ## hospitalizations -- rather than left as raw multiplicative output.
  clip_forecast <- function(x) {
    if (identical(data_type, "proportion")) pmin(pmax(x, 0), 1) else round(pmax(x, 0))
  }

  ## The additive shift used to keep log(growth) finite when a value is (or
  ## is near) zero. +1 is the right size for count data -- negligible next
  ## to typical counts -- but proportions live in [0, 1] to begin with, so
  ## shifting by a whole 1 would swamp the real signal entirely (every
  ## shifted value would land within [1, 2], compressing all real relative
  ## change down to near-zero and making the growth-rate matching all but
  ## meaningless). 1e-4 matches this data's own reporting floor while
  ## barely disturbing real proportions.
  shift <- if (identical(data_type, "proportion")) 1e-4 else 1

  ## The reporting resolution of a rounded proportion: the smallest
  ## representable value above zero. Below this, a state's growth-rate
  ## window can go artificially flat for many consecutive weeks purely from
  ## rounding, not because nothing is really happening (see
  ## calcopycat_fxn()'s resolution-aware noise floor, header point 6).
  ## Counts are already true, unrounded observations, so this doesn't apply
  ## to them.
  resolution_q <- if (identical(data_type, "proportion")) {
    positive_values <- df$value[!is.na(df$value) & df$value > 0]
    if (length(positive_values) > 0) min(positive_values) else NA_real_
  } else {
    NA_real_
  }

  ## One continuous, dated growth series per target group -- no season
  ## buckets, no synthetic index, no epiweek lookup needed.
  history <- df |>
    group_by(target_group) |>
    arrange(date, .by_group = TRUE) |>
    mutate(
      value  = value + shift,
      growth = log(lead(value) / value)
    ) |>
    ungroup()

  groups <- unique(history$target_group)
  group_forecasts <- vector("list", length = length(groups))

  for (curr_group in groups) {
    curr_series <- history |> filter(target_group == curr_group)
    candidate_pool <- if (isTRUE(share_groups)) history else curr_series

    sim <- calcopycat_fxn(
      curr_data          = curr_series,
      db                 = candidate_pool,
      forecast_horizon   = fcast_horizon,
      recent_weeks_touse = recent_weeks_touse,
      nsamps             = nsamps,
      resp_week_range    = resp_week_range,
      resolution_q       = resolution_q,
      group_label        = curr_group
    )

    forecast_trajectories <- sim |>
      mutate(
        forecast = forecast - shift,
        forecast = clip_forecast(forecast)
      )

    cleaned_forecasts_quantiles <- forecast_trajectories |>
      group_by(horizon) |>
      summarize(qs = list(
        value = quantile(forecast, probs = quantiles_needed)
      ), .groups = "drop") |>
      unnest_wider(qs) |>
      gather(quantile, value, -horizon) |>
      mutate(
        quantile       = as.numeric(gsub("[\\%,]", "", quantile)) / 100,
        target_group   = curr_group,
        output_type_id = as.numeric(quantile),
        output_type    = "quantile",
        value          = value
      ) |>
      select(
        horizon,
        target_group,
        output_type,
        output_type_id,
        value
      )

    group_forecasts[[match(curr_group, groups)]] <- cleaned_forecasts_quantiles |>
      mutate(output_type_id = as.character(output_type_id))
  }

  final_forecasts <- bind_rows(group_forecasts) |>
    arrange(target_group, horizon, output_type_id)

  return(final_forecasts)
}
