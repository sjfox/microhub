inla.setOption(inla.mode = "classic")
inla.setOption(num.threads = "1:1")

# Wrangle data for INLA model ==================================================

wrangle_inla_population <- function(
  dataframe,
  forecast_date,
  data_to_drop,
  forecast_horizons,
  pop_table # new argument
) {
  forecast_date <- as.Date(forecast_date)

  # Preprocess the data
  data_preprocessed <- dataframe |>
    rename(
      epiweek = week,
      count = value
    ) |>
    filter(year > 2021)

  # Add population column
  data_with_population <- add_population_column(
    pop_table,
    data_preprocessed
  )

  # Determine configuration based on selected data to drop option
  config <- switch(
    data_to_drop,
    "0 weeks" = list(days_before = 0, weeks_ahead = 4),
    "1 week" = list(days_before = 4, weeks_ahead = 5),
    "2 week" = list(days_before = 11, weeks_ahead = 6),
    stop("Invalid data_to_drop option")
  )

  days_before <- config$days_before
  base_weeks_ahead <- config$weeks_ahead

  # Adjust weeks_ahead based on forecast horizons
  weeks_ahead <- base_weeks_ahead + (as.numeric(forecast_horizons) - 4)

  # Final data wrangling
  wrangled_data <- data_with_population |>
    mutate(date = as.Date(date)) |>
    filter(date < ymd(forecast_date) - days_before)

  fit_df <- prep_fit_data_population(wrangled_data, weeks_ahead)

  return(fit_df)
}

wrangle_inla_no_population <- function(
  dataframe,
  forecast_date,
  data_to_drop,
  forecast_horizons
) {
  forecast_date <- as.Date(forecast_date)

  # Preprocess the data
  data_preprocessed <- dataframe |>
    rename(
      epiweek = week,
      count = value
    ) |>
    filter(year > 2021)

  # Determine configuration based on selected data to drop option
  config <- switch(
    data_to_drop,
    "0 weeks" = list(days_before = 0, weeks_ahead = 4),
    "1 week" = list(days_before = 4, weeks_ahead = 5),
    "2 week" = list(days_before = 11, weeks_ahead = 6),
    stop("Invalid data_to_drop option")
  )

  days_before <- config$days_before
  base_weeks_ahead <- config$weeks_ahead

  # Adjust weeks_ahead based on forecast horizons
  weeks_ahead <- base_weeks_ahead + (as.numeric(forecast_horizons) - 4)

  # Final data wrangling
  wrangled_data <- data_preprocessed |>
    mutate(date = as.Date(date)) |>
    filter(date < ymd(forecast_date) - days_before)

  fit_df <- prep_fit_data_no_population(wrangled_data, weeks_ahead)

  return(fit_df)
}
# Fit and process INLA =========================================================
# trim_data_inla_paraguay <- function(df) {
#   filter(
#     df,
#     age_group != "Overall",
#     date >= "2021-09-01",
#     epiweek != 53
#   )
# }

# `group_levels` fixes the mapping between target group NAME and the integer
# `group_idx` that INLA actually sees. When NULL it is derived exactly as
# before (order of first appearance), so behaviour is unchanged for callers
# that don't pass it. The derived vector is attached to the result as the
# "group_levels" attribute, because the neighbor adjacency matrix must be built
# against this same vector -- INLA's f(group_idx, ..., graph=W) sees only an
# integer index and an unnamed matrix, so if W's row order disagreed with this
# mapping the model would fit cleanly and forecast wrongly with no error.
prep_data_inla <- function(df, weeks_ahead, group_levels=NULL, season_groups=NULL) {
  if (is.null(group_levels)) {
    group_levels <- levels(fct_inorder(as.character(df$target_group)))
  }

  # Seasonal grouping is derived from the SAME group_levels as group_idx, so a
  # target group's seasonal curve and its spatial position can never refer to
  # different regions. Groups absent from `season_groups` share one default
  # index, so an uploaded file only has to name the exceptions.
  season_by_level <- season_group_index(season_groups, group_levels)

  ret <- df |>
    mutate(
      epiweek=lubridate::epiweek(date),
      group_idx=as.numeric(factor(as.character(target_group), levels=group_levels)),
      season_idx=season_by_level[group_idx]
    )

  pred_df <- expand_grid( # makes pairs of new weeks X groups
    tibble(
      date=max(ret$date) + weeks(1:weeks_ahead),
      # t=1:weeks_ahead + max(ret$t),
      epiweek=epiweek(date)
    ),
    distinct(ret, target_group, group_idx, season_idx)
  )

  # for forecasts, assume the offset is to be the most recent offset term for
  # each group. This only matters if the offset changes over time, e.g.
  # between years:
  group_pred_offset <- ret |>
    slice_max(date) |>
    distinct(target_group, offset)

  pred_df <- left_join(
    pred_df, group_pred_offset,
    by=c("target_group"), unmatched="error", relationship="many-to-one"
  )

  ret <- bind_rows(ret, pred_df) # add to data for counts to be NAs

  # index dates with a sequential time index, accounting for holes in the
  # data and spacing indices properly
  date_seq <- seq.Date(min(ret$date), max(ret$date), "1 week")

  date_ind <- tibble(t=seq_along(date_seq), date=date_seq) |>
    filter(date %in% unique(ret$date))

  out <- ret |>
    left_join(date_ind, by=c("date"), relationship="many-to-one") |>
    # `t2` is a duplicate time index. INLA requires a distinct column name for
    # each f() term, so a model carrying both a main temporal effect and a
    # space-time interaction needs two copies of the same index.
    mutate(t2=t) |>
    arrange(t, group_idx)

  attr(out, "group_levels") <- group_levels
  attr(out, "season_by_level") <- season_by_level
  out
}

# Valid values for the `interaction` argument of inla_model_formula() /
# fit_process_inla(), i.e. how short-term deviations are shared across target
# groups. "besagproper" additionally requires an uploaded neighbor graph.
INLA_INTERACTION_CHOICES <- c(
  "exchangeable", "iid", "none",
  # "_main" variants add a separate main temporal effect alongside the group
  # interaction, which is the shape the upstream reference always uses. They
  # exist so that group structure can be compared against besagproper with the
  # main term held constant -- besagproper necessarily carries one, so a
  # comparison against plain "exchangeable" (which does not) would vary two
  # things at once and could not attribute a WIS difference to adjacency.
  "exchangeable_main", "iid_main",
  "besagproper"
)

# Build the model formula.
#
# `interaction` controls the group structure on the short-term (weekly) effect:
#
#   exchangeable  one common correlation between every pair of groups. The
#                 historical microhub default.
#   iid           group-level dynamics independent of each other, pooled only
#                 through the shared prior.
#   none          a single shared temporal trend; groups differ by intercept.
#   besagproper   neighbor-based spatial structure from an uploaded graph, so
#                 adjacent groups are correlated and distant ones are not.
#
# NOTE on structure: the "exchangeable" and "iid" forms carry the group
# interaction on the SAME f(t, ...) term that provides the temporal effect,
# which is what microhub has always done. The upstream reference
# (brendandaisy/inla-forecasting-paper, src/model-formulas.R) instead always
# carries a separate main f(t, ...) plus an interaction on f(t2, ...). That is
# a real structural difference worth testing, but adopting it here would
# silently change every existing forecast, so the established forms are kept
# byte-identical and only besagproper -- which genuinely requires both terms --
# uses the main-plus-interaction shape.
# How the seasonal curve is shared across target groups.
#
#   shared        one cyclic RW2 for every group (the historical default)
#   season_group  one curve per uploaded season group, drawn from a common
#                 prior via control.group="iid" -- partial pooling, so a curve
#                 is free to take any shape but is still regularised by a
#                 precision estimated across all groups
#   target_group  one curve per target group; no pooling of shape at all
#
# "season_group" is the reference's approach (scripts/flusight-25-26, which
# gives AK, HI and PR their own curves via a hand-built season_group).
INLA_SEASONAL_CHOICES <- c("shared", "season_group", "target_group")

inla_model_formula <- function(single_group, interaction="exchangeable",
                               seasonal="shared") {
  interaction <- match.arg(interaction, INLA_INTERACTION_CHOICES)
  seasonal <- match.arg(seasonal, INLA_SEASONAL_CHOICES)

  seasonal_group_idx <- switch(
    seasonal,
    "shared" = NULL,
    "season_group" = "season_idx",
    "target_group" = "group_idx"
  )

  seasonal <- if (is.null(seasonal_group_idx)) {
    'f(epiweek, model="rw2", cyclic=TRUE, hyper=hyper_epwk, scale.model=TRUE)'
  } else {
    paste0(
      'f(epiweek, model="rw2", cyclic=TRUE, hyper=hyper_epwk, scale.model=TRUE, ',
      'group=', seasonal_group_idx, ', control.group=list(model="iid"))'
    )
  }

  # With one group there is no group structure to specify -- and no seasonal
  # grouping either, since there is only one curve to estimate.
  if (single_group) {
    seasonal <- 'f(epiweek, model="rw2", cyclic=TRUE, hyper=hyper_epwk, scale.model=TRUE)'
    return(paste0(
      'value ~ 1 + ', seasonal, ' +\n   f(t, model="ar1", hyper=hyper_wk)'
    ))
  }

  # A separate main temporal effect, grouped interaction on the duplicate index
  # t2 -- the upstream reference's shape (src/model-formulas.R).
  with_main <- function(group_model) {
    paste0(
      'f(t, model="ar1", hyper=hyper_wk) +\n   ',
      'f(t2, model="ar1", hyper=hyper_wk, group=group_idx, ',
      'control.group=list(model="', group_model, '"))'
    )
  }

  temporal <- switch(
    interaction,
    "none" = 'f(t, model="ar1", hyper=hyper_wk)',
    "iid" = 'f(t, model="ar1", hyper=hyper_wk, group=group_idx, control.group=list(model="iid"))',
    "exchangeable" = 'f(t, model="ar1", hyper=hyper_wk, group=group_idx, control.group=list(model="exchangeable"))',
    "iid_main" = with_main("iid"),
    "exchangeable_main" = with_main("exchangeable"),
    # `graph` is resolved from the calling environment of as.formula(), the same
    # way hyper_epwk and hyper_wk already are.
    "besagproper" = paste0(
      'f(t, model="ar1", hyper=hyper_wk) +\n   ',
      'f(group_idx, model="besagproper", hyper=hyper_wk, graph=graph, ',
      'group=t2, control.group=list(model="ar1"))'
    )
  )

  paste0('value ~ 1 + target_group +\n   ', seasonal, ' +\n   ', temporal)
}

# Censoring threshold for a Beta likelihood, derived from the data.
#
# The Beta density is defined on the OPEN interval (0, 1), so an observation of
# exactly 0 has no likelihood and INLA will not accept it. There are two ways
# to deal with that:
#
#   clamp    rewrite the 0 as some small positive number, asserting a value
#            that was never observed
#   censor   tell INLA the observation lies somewhere in (0, c) and let it
#            integrate the likelihood over that interval
#
# Censoring is the honest treatment -- a zero genuinely means "below what this
# surveillance system can resolve", not "equal to 0.0001" -- and it is what the
# upstream production pipelines use (inla-forecasting-paper,
# scripts/flusight-25-26/inflaenza-forecast.R and scripts/metrocast-25-26/
# run-metrocast.R, both `control.family=list(beta.censor.value=cens)`).
#
# Half the smallest positive observation is their choice and a good one: it is
# a proxy for the measurement resolution, and because it is strictly below
# every positive value, no genuine observation is ever censored -- only true
# zeros are. Crucially it scales with the data, unlike a hardcoded constant,
# which on a series whose median is 8e-4 would otherwise land inside the bulk
# of the distribution rather than below it.
beta_censor_value <- function(values, fallback = 1e-6) {
  values <- suppressWarnings(as.numeric(values))
  positive <- values[!is.na(values) & is.finite(values) & values > 0]

  if (length(positive) == 0) {
    # Degenerate (every observation zero or missing); nothing to scale to.
    return(fallback)
  }

  cens <- min(positive) / 2

  # INLA censors the Beta response SYMMETRICALLY: y <= c is left-censored and
  # y >= 1 - c is right-censored. min(positive)/2 only reasons about the lower
  # tail, so on a series whose smallest positive value is large it produces a
  # threshold big enough to swallow genuine high observations -- c(0, 0.5, 0.999)
  # gives c = 0.25, which would right-censor the 0.999. Bound it by the upper
  # headroom too.
  #
  # Skipped when the data actually reaches 1: a series that saturates SHOULD be
  # right-censored there, and (1 - 1)/2 would be a degenerate threshold anyway.
  finite_values <- values[!is.na(values) & is.finite(values)]
  max_value <- max(finite_values)
  if (max_value < 1) {
    cens <- min(cens, (1 - max_value) / 2)
  }

  if (!is.finite(cens) || cens <= 0) {
    return(fallback)
  }

  cens
}

# INLA censors the Beta response symmetrically: y <= c is left-censored and
# y >= 1 - c is right-censored. That upper rule is harmless for the small
# proportions this is normally used on, but would silently swallow real data if
# a series sat close to 1, so say so rather than let it pass.
warn_if_beta_censor_touches_upper_tail <- function(values, cens) {
  values <- suppressWarnings(as.numeric(values))
  values <- values[!is.na(values) & is.finite(values)]

  n_upper <- sum(values >= 1 - cens)
  if (n_upper > 0) {
    warning(
      "INFLAenza: ", n_upper, " observation(s) are at or above ", 1 - cens,
      " and will be treated as right-censored at 1 by the Beta likelihood. ",
      "If those are real values rather than saturation, the censoring ",
      "threshold derived from this data (", signif(cens, 3), ") is too coarse.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

# The Beta precision is a FAMILY hyperparameter, which INLA lists before the
# latent-field ones -- but looking it up by name rather than by position keeps
# this correct if that ever stops being true, or if a second family
# hyperparameter appears.
beta_precision_mean <- function(fit) {
  hp <- fit$summary.hyperpar
  idx <- grep("beta observations", rownames(hp), ignore.case = TRUE)
  if (length(idx) == 0) {
    idx <- 1L
  }
  hp[idx[1], "mean"]
}

forecast_samples_inla <- function(fit_df, fit, nsamp=1000, response=count, family="poisson") {
  pred_idx <- parse_number(fit$selection$names)

  ret_df <- fit_df[pred_idx,]

  t_forecast <- max(filter(fit_df, !is.na({{response}}))$t) # find the t corresponding to the forecast date
  ret_df$horizon <- ret_df$t - t_forecast # works for hindcasting too!

  eta_samp <- inla.rjmarginal(nsamp, fit$selection)$samples

  # control.predictor=list(link=1) in fit_process_inla() reports fitted values
  # on the *linear predictor* scale for every family, so the inverse link has
  # to match the family: Poisson's default link is log, Beta's is logit.
  mu_samp <- switch(
    family,
    poisson = exp(eta_samp),
    beta = plogis(eta_samp),
    stop("Unsupported family for forecast_samples_inla(): ", family)
  )

  offset <- ret_df$offset
  pred_dim <- length(offset)

  rfun <- switch(
    family,
    # Poisson exposure (population offset, or 1 when unused) scales the mean
    # directly; Beta has no such exposure concept, so `mu` is used as-is.
    poisson = function(mu) rpois(pred_dim, mu * offset),
    beta = function(mu) {
      phi <- beta_precision_mean(fit)
      rbeta(pred_dim, mu * phi, (1 - mu) * phi)
    }
  )

  pred_samp <- list_transpose(map(1:nsamp, \(samp) { # invert list so have sampled values for each row
    rfun(mu_samp[, samp])
  }))

  mutate(ret_df, predicted=pred_samp)
}

aggregate_forecast_inla <- function(pred_samp, ..., fun=sum, tags=tibble(location="All")) {
  pred_samp |>
    mutate(predicted=map(predicted, \(p) tibble(sample_id=seq_along(p), predicted=p))) |>
    unnest(predicted) |>
    group_by(date, horizon, ..., sample_id) |>
    summarise(predicted=fun(predicted), .groups="drop") |>
    bind_cols(tags) |>
    select(-sample_id) |>
    nest(predicted=predicted) |>
    mutate(predicted=map(predicted, ~.$predicted))
}

summarize_quantiles_inla <- function(pred_samples, agg_samps=NULL, q=c(0.025, 0.25, 0.5, 0.75, 0.975)) {
  pred_samples |>
    bind_rows(agg_samps) |>
    unnest(predicted) |>
    group_by(date, target_group, horizon) |>
    summarize(
      mean=mean(predicted),
      qs=list(value=quantile(predicted, probs=q)),
      .groups="drop"
    ) |>
    unnest_wider(qs) |>
    pivot_longer(contains("%"), names_to="quantile") |>
    mutate(
      output_type="quantile",
      output_type_id=as.character(parse_number(quantile)/100)
    ) |>
    select(horizon, target_group, output_type, output_type_id, value)
}

fit_process_inla <- function(
    df,
    weeks_ahead,
    quantiles_needed,
    agg_group="Overall",
    data_type="count",
    # seasonal_smoothness,
    forecast_uncertainty="default",
    use_offset=FALSE,
    interaction="exchangeable",
    neighbor_graph=NULL,
    seasonal="shared",
    season_groups=NULL
) {
  interaction <- match.arg(interaction, INLA_INTERACTION_CHOICES)
  seasonal <- match.arg(seasonal, INLA_SEASONAL_CHOICES)
  hyper_epwk <- list(prec=list(prior="pc.prec", param=c(1, 0.01)))

  hyper_wk <- switch(
    forecast_uncertainty,
    "default" = list(theta = list(prior = "pc.prec", param = c(1, 0.01))),
    "small" = list(theta = list(prior = "pc.prec", param = c(0.2, 0.01))),
    "tiny" = list(theta = list(prior = "pc.prec", param = c(0.05, 0.01))),
    stop("Invalid selection for forecast_uncertainty_parameter")
  )

  # Proportion data is fit with a Beta likelihood (response strictly inside
  # (0, 1)) instead of Poisson; a population offset/exposure term has no
  # meaning for Beta, so it's forced off below regardless of `use_offset`.
  family <- if (identical(data_type, "proportion")) "beta" else "poisson"
  use_offset <- use_offset & identical(family, "poisson")

  single_group <- length(unique(df$target_group)) == 1
  pred_agg_group <- !single_group & (agg_group %in% unique(df$target_group))
  # Summing per-group *proportions* to build an "Overall" series isn't
  # meaningful the way summing per-group counts is, so aggregation is
  # Poisson-only.
  pred_agg_group <- pred_agg_group & identical(family, "poisson")

  df_no_agg <- if (pred_agg_group) filter(df, target_group != {{agg_group}}) else df

  suppressWarnings({
    if (use_offset & !is.null(df_no_agg$population))
      df_no_agg$offset <- df_no_agg$population
    else
      df_no_agg$offset <- 1
  })

  # Zeros in a Beta response are handled by CENSORING rather than by clamping
  # them to a small constant -- see beta_censor_value(). The observed values are
  # deliberately left untouched here; the threshold is passed to INLA via
  # control.family below, and INLA integrates the likelihood over (0, cens) for
  # any observation at or below it.
  beta_cens <- NULL
  if (identical(family, "beta")) {
    beta_cens <- beta_censor_value(df_no_agg$value)
    warn_if_beta_censor_touches_upper_tail(df_no_agg$value, beta_cens)
  }

  # Resolve the seasonal structure before building fit_df, since "season_group"
  # is only meaningful when an uploaded grouping actually distinguishes the
  # groups being fit. Falling back loudly rather than silently, for the same
  # reason as the besagproper fallback below: a retrospective row labelled
  # "season_group" that was in fact fit with one shared curve is worse than an
  # error.
  season_levels <- levels(fct_inorder(as.character(df_no_agg$target_group)))
  if (identical(seasonal, "season_group") &&
      !season_groups_are_usable(season_groups, season_levels)) {
    warning(
      "INFLAenza: per-season-group seasonality was requested but the uploaded ",
      "grouping does not distinguish the target groups being fit (or none was ",
      "uploaded); falling back to a single shared seasonal curve.",
      call. = FALSE
    )
    seasonal <- "shared"
  }

  fit_df <- prep_data_inla(df_no_agg, weeks_ahead, season_groups = season_groups)
  group_levels <- attr(fit_df, "group_levels")

  # Build the adjacency matrix against the SAME level vector that produced
  # group_idx, so the row order of `graph` and the integer INLA indexes by
  # cannot disagree. Edges naming groups not being fit (the aggregate group,
  # typically) are dropped by build_neighbor_matrix().
  graph <- NULL
  graph_coverage <- NULL
  if (identical(interaction, "besagproper")) {
    if (single_group) {
      # No group structure to place on a single series.
      interaction <- "none"
    } else if (neighbor_graph_is_usable(neighbor_graph, group_levels)) {
      graph <- build_neighbor_matrix(neighbor_graph, group_levels)

      # The graph was validated when uploaded, against whatever target groups
      # were loaded THEN. The groups actually being fit here can differ (a
      # different dataset, or a retrospective group's own slice), so re-check
      # coverage now. Total non-coverage is caught by the branch above; this
      # catches the partial case, which would otherwise fit a half-populated
      # adjacency without comment.
      coverage <- neighbor_graph_coverage(neighbor_graph, group_levels)
      if (length(coverage$missing) > 0) {
        warning(
          "INFLAenza: ", length(coverage$missing), " of ", length(group_levels),
          " target group(s) have no neighbors in the uploaded graph and will be ",
          "modelled as isolated: ",
          paste(utils::head(coverage$missing, 10), collapse = ", "),
          if (length(coverage$missing) > 10) ", ..." else "",
          call. = FALSE
        )
      }
      graph_coverage <- coverage
    } else {
      # Fall back loudly rather than silently fitting a different model than
      # the one that was asked for -- a silent downgrade in a retrospective run
      # would produce a row labelled besagproper that wasn't one.
      warning(
        "INFLAenza: 'besagproper' was requested but no usable neighbor graph ",
        "covers the target groups being fit; falling back to 'exchangeable'. ",
        "Upload a neighbor graph on the Data tab to use the spatial structure.",
        call. = FALSE
      )
      interaction <- "exchangeable"
    }
  }

  model <- inla_model_formula(single_group, interaction, seasonal)
  mod <- as.formula(model)

  # TODO: currently assumes value has no NAs in the input df
  pred_idx <- which(is.na(fit_df$value))

  inla_args <- list(
    mod, family=family, data=fit_df,
    selection=list(Predictor=pred_idx),
    control.fixed=list(prec=1, expand.factor.strategy="inla"),
    control.compute=list(dic=FALSE, mlik=FALSE, return.marginals.predictor=TRUE, config=FALSE),
    control.predictor=list(link=1), # produce marginal fitted values with default link function
    verbose = isTRUE(as.logical(Sys.getenv("INLA_VERBOSE", "FALSE")))
  )
  # E (Poisson exposure) is meaningless for Beta -- omit it entirely rather
  # than pass a vector of 1s, so INLA never has to reason about it.
  if (identical(family, "poisson")) {
    inla_args$E <- fit_df$offset
  }

  # Left-censor zeros (and right-censor anything at 1) instead of relocating
  # them, so a below-detection observation contributes an integral over
  # (0, cens) rather than a point density at an invented value.
  if (identical(family, "beta") && !is.null(beta_cens)) {
    inla_args$control.family <- list(beta.censor.value = beta_cens)
  }

  fit <- do.call(inla, inla_args)

  pred_samp <- forecast_samples_inla(
    fit_df, fit,
    nsamp=5000, response=value, family=family
  )

  pred_samp_agg <- if (pred_agg_group)
    aggregate_forecast_inla(pred_samp, tags=tibble_row(target_group={{agg_group}}))
  else NULL


  out <- summarize_quantiles_inla(pred_samp, pred_samp_agg, q=quantiles_needed)

  # Report the structure ACTUALLY fit, which differs from the one requested
  # whenever the besagproper fallback above fired. Callers compare this against
  # what they asked for; run_inla_model() turns a mismatch into a user-visible
  # notification, since the warning() raised above only reaches the console when
  # this runs inside a Shiny reactive.
  attr(out, "interaction_used") <- interaction
  attr(out, "seasonal_used") <- seasonal
  attr(out, "n_season_groups") <- length(unique(attr(fit_df, "season_by_level")))
  # Names the groups fit as isolated, so a caller can surface it. NULL unless a
  # spatial fit actually ran with partial coverage.
  attr(out, "neighbor_graph_missing") <- graph_coverage$missing
  out
}

# fit_process_inla_offset_aggregate <- function(
#   fit_df,
#   forecast_date,
#   ar_order,
#   rw_order,
#   seasonal_smoothness,
#   forecast_uncertainty_parameter,
#   q = c(0.025, 0.25, 0.5, 0.75, 0.975),
#   joint = TRUE
# ) {
#   forecast_date <- as.Date(forecast_date)
#
#   fit_df <- fit_df |>
#     filter(target_group != "Overall")
#
#   # Fit the current model
#   fit <- fit_current_model1(
#     fit_df,
#     forecast_date,
#     ar_order,
#     rw_order,
#     seasonal_smoothness,
#     forecast_uncertainty_parameter,
#     q,
#     joint
#   )
#
#   # Sample national-level predictions
#   nat_samps <- sample_national(fit_df, fit, forecast_date)
#
#   # Sample count predictions
#   pred_samples <- sample_count_predictions(fit_df, fit)
#
#   # Summarize quantiles
#   forecast_quantiles <- summarize_quantiles_aggregate(
#     pred_samples,
#     nat_samps,
#     forecast_date,
#     q = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
#   )
#
#   return(forecast_quantiles)
# }
#
# fit_process_inla_no_offset_aggregate <- function(
#   fit_df,
#   forecast_date,
#   ar_order,
#   rw_order,
#   seasonal_smoothness,
#   forecast_uncertainty_parameter,
#   q = c(0.025, 0.25, 0.5, 0.75, 0.975),
#   joint = TRUE
# ) {
#   forecast_date <- as.Date(forecast_date)
#
#   fit_df <- fit_df |>
#     filter(target_group != "Overall")
#
#   # Fit the current model
#   fit <- fit_current_model1_no_offset(
#     fit_df,
#     forecast_date,
#     ar_order,
#     rw_order,
#     seasonal_smoothness,
#     forecast_uncertainty_parameter,
#     q,
#     joint
#   )
#
#   # Sample national-level predictions
#   nat_samps <- sample_national_no_offset(fit_df, fit, forecast_date)
#
#   # Sample count predictions
#   pred_samples <- sample_count_predictions_no_offset(fit_df, fit)
#
#   # Summarize quantiles
#   forecast_quantiles <- summarize_quantiles_aggregate(
#     pred_samples,
#     nat_samps,
#     forecast_date,
#     q = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
#   )
#
#   return(forecast_quantiles)
# }
#
# fit_process_inla_offset_single_target <- function(
#   fit_df,
#   forecast_date,
#   ar_order,
#   rw_order,
#   seasonal_smoothness,
#   forecast_uncertainty_parameter,
#   q = c(0.025, 0.25, 0.5, 0.75, 0.975),
#   joint = TRUE
# ) {
#   forecast_date <- as.Date(forecast_date)
#
#   # Fit the current model
#   fit <- fit_current_model1(
#     fit_df,
#     forecast_date,
#     ar_order,
#     rw_order,
#     seasonal_smoothness,
#     forecast_uncertainty_parameter,
#     q,
#     joint
#   )
#
#   # Sample count predictions
#   pred_samples <- sample_count_predictions(fit_df, fit)
#
#   # Summarize quantiles
#   forecast_quantiles <- summarize_quantiles_single_target(
#     pred_samples,
#     nat_samps,
#     forecast_date,
#     q = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
#   )
#
#   return(forecast_quantiles)
# }
#
# fit_process_inla_no_offset_single_target <- function(
#   fit_df,
#   forecast_date,
#   ar_order,
#   rw_order,
#   seasonal_smoothness,
#   forecast_uncertainty_parameter,
#   q = c(0.025, 0.25, 0.5, 0.75, 0.975),
#   joint = TRUE
# ) {
#   forecast_date <- as.Date(forecast_date)
#
#   # Fit the current model
#   fit <- fit_current_model1_no_offset(
#     fit_df,
#     forecast_date,
#     ar_order,
#     rw_order,
#     seasonal_smoothness,
#     forecast_uncertainty_parameter,
#     q,
#     joint
#   )
#
#   # Sample count predictions
#   pred_samples <- sample_count_predictions_no_offset(fit_df, fit)
#
#   # Summarize quantiles
#   forecast_quantiles <- summarize_quantiles_single_target(
#     pred_samples,
#     nat_samps,
#     forecast_date,
#     q = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
#   )
#
#   return(forecast_quantiles)
# }
#
#
# # INLA helper functions ========================================================
#
# add_population_column <- function(pop_table, data_frame) {
#   # Add a year column based on date
#   data_frame <- data_frame |>
#     mutate(year = year(date))
#
#   # If `year` is in population table, do year-based join
#   if ("year" %in% colnames(pop_table)) {
#     min_year <- min(pop_table$year, na.rm = TRUE)
#     max_year <- max(pop_table$year, na.rm = TRUE)
#
#     data_frame <- data_frame |>
#       mutate(year_capped = case_when(
#         year < min_year ~ min_year,
#         year > max_year ~ max_year,
#         TRUE ~ year
#       )) |>
#       left_join(
#         pop_table,
#         by = c("year_capped" = "year", "target_group")
#       ) |>
#       select(-year_capped)
#   } else {
#     # No year in population table → join by age group only
#     data_frame <- data_frame |>
#       left_join(
#         pop_table,
#         by = "target_group"
#       )
#   }
#
#   return(data_frame)
# }
#
# ########################## this is the version for population data
# prep_fit_data_population <- function(input_data, weeks_ahead = weeks_ahead) {
#   input_data$date <- as.Date(input_data$date)
#
#   ret <- input_data |>
#     group_by(date) |>
#     mutate(t = cur_group_id(), .after = date) |> # add a time counter starting from 1 for earliest week
#     ungroup() |>
#     mutate(
#       snum = as.numeric(fct_inorder(target_group)), # INLA needs groups as ints starting from 1, so add numeric state code
#       ex_lam = population
#     )
#
#   # make a dataframe to hold group info for forecasting
#   pred_df <- expand_grid(
#     tibble(
#       date = duration(1:weeks_ahead, "week") + max(ret$date),
#       t = 1:weeks_ahead + max(ret$t),
#       epiweek = epiweek(date)
#     ),
#     distinct(ret, target_group, snum, population) # makes pairs of new times X each state
#   ) |>
#     left_join(
#       distinct(ret, target_group, epiweek, ex_lam),
#       by = join_by(epiweek, target_group)
#     ) # go and find `ex_lam` values for each state and epiweek
#
#   bind_rows(ret, pred_df) |> # add to data for counts to be NAs
#     arrange(t)
# }
#
# ################################## this is the version without population data
# prep_fit_data_no_population <- function(input_data, weeks_ahead = weeks_ahead) {
#   input_data$date <- as.Date(input_data$date)
#
#   ret <- input_data |>
#     group_by(date) |>
#     mutate(t = cur_group_id(), .after = date) |> # add a time counter starting from 1 for earliest week
#     ungroup() |>
#     mutate(
#       snum = as.numeric(fct_inorder(target_group)) # INLA needs groups as ints starting from 1, so add numeric state code
#     )
#
#   # make a dataframe to hold group info for forecasting
#   pred_df <- expand_grid(
#     tibble(
#       date = duration(1:weeks_ahead, "week") + max(ret$date),
#       t = 1:weeks_ahead + max(ret$t),
#       epiweek = epiweek(date)
#     ),
#     distinct(ret, target_group, snum) # makes pairs of new times X each state
#   ) |>
#     left_join(
#       distinct(ret, target_group, epiweek),
#       by = join_by(epiweek, target_group)
#     ) # go and find `ex_lam` values for each state and epiweek
#
#   bind_rows(ret, pred_df) |> # add to data for counts to be NAs
#     arrange(t)
# }
# ######################## this is the version with the offset
# fit_current_model1 <- function(
#   fit_df,
#   forecast_date,
#   ar_order,
#   rw_order,
#   seasonal_smoothness,
#   forecast_uncertainty_parameter,
#   q = c(0.025, 0.25, 0.5, 0.75, 0.975),
#   joint = TRUE
# ) {
#   # Set hyperparameters for seasonal effect
#   hyper_epwk <- switch(
#     seasonal_smoothness,
#     "default" = list(theta = list(prior = "pc.prec", param = c(0.5, 0.01))),
#     "less" = list(theta = list(prior = "pc.prec", param = c(0.25, 0.01))),
#     "more" = list(theta = list(prior = "pc.prec", param = c(1, 0.01))),
#     stop("Invalid selection for seasonal_smoothness")
#   )
#
#   # Set hyperparameters for weekly effect
#   # hyper_wk <- switch(weekly_effect,
#   # "default" = list(theta=list(prior="pc.prec", param=c(1, 0.01))),
#   # "less" = list(theta=list(prior="pc.prec", param=c(0.5, 0.01))),
#   # "more" = list(theta=list(prior="pc.prec", param=c(2, 0.01))),
#   # stop("Invalid selection for weekly effect"))
#
#   hyper_wk <- switch(
#     forecast_uncertainty_parameter,
#     "default" = list(theta = list(prior = "pc.prec", param = c(1, 0.01))),
#     "small" = list(theta = list(prior = "pc.prec", param = c(0.2, 0.01))),
#     "tiny" = list(theta = list(prior = "pc.prec", param = c(0.05, 0.01))),
#     stop("Invalid selection for forecast_uncertainty_parameter")
#   )
#
#   # Create the rw_mod part of the formula dynamically
#   rw_mod <- switch(
#     rw_order,
#     "1" = "f(epiweek, model='rw1', cyclic=TRUE, hyper=hyper_epwk)",
#     "2" = "f(epiweek, model='rw2', cyclic=TRUE, hyper=hyper_epwk)", ### this is default
#     stop("Invalid selection for RW order")
#   )
#
#   # Create the complete formula as a string
#   formula_string <- paste(
#     "count ~ 1 +",
#     rw_mod,
#     "+ f(t, model='ar', order=",
#     ar_order,
#     ", group=snum, hyper=hyper_wk, control.group=list(model='exchangeable'))"
#   )
#
#   # Convert the formula string to a formula object
#   mod <- as.formula(formula_string)
#
#   pred_idx <- which(fit_df$date >= forecast_date)
#
#   fit <- inla(
#     mod,
#     family = "poisson",
#     data = fit_df,
#     E = fit_df$ex_lam,
#     quantiles = q,
#     selection = if (!joint) NULL else list(Predictor = pred_idx),
#     control.compute = list(
#       dic = FALSE,
#       mlik = FALSE,
#       return.marginals.predictor = TRUE
#     ),
#     control.predictor = list(link = 1, compute = TRUE)
#   )
#   return(fit)
# }
#
# sample_count_predictions <- function(
#   fit_df,
#   fit,
#   nsamp = 10000
# ) {
#   samp_counts <- map2_dfr(
#     fit$marginals.fitted.values,
#     fit_df$ex_lam,
#     \(m, ex) {
#       msamp <- pmax(0, inla.rmarginal(nsamp, m)) # sampling sometimes produces very small neg. numbers
#       ct_samp <- rpois(nsamp, msamp * ex)
#       tibble_row(count_samp = list(ct_samp))
#     }
#   )
#
#   return(bind_cols(fit_df, samp_counts))
# }
#
#
# sample_national <- function(fit_df, fit, forecast_date, nsamp = 10000) {
#   state_info <- distinct(fit_df, target_group, ex_lam)
#   nstate <- nrow(state_info)
#
#   ret_df <- fit_df |>
#     filter(date >= forecast_date) |>
#     # filter(date >= temp_date) |>
#     group_by(date, t, epiweek) |>
#     summarise(population = sum(population), .groups = "drop")
#
#   jsamp_fvals <- exp(inla.rjmarginal(nsamp, fit$selection)$samples)
#   # ex_lam <- filter(fit_df, date >= forecast_date)$ex_lam
#   ex_lam <- filter(fit_df, date >= forecast_date)$ex_lam
#
#
#   tslice <- map(1:nrow(ret_df), ~ nstate * (.x - 1) + 1:nstate) # produce sequence [1:nstate, nstate+1:2nstate, ...]
#
#   imap_dfr(tslice, \(idx, t) {
#     nat_sum_per_t <- map_dbl(1:nsamp, \(samp) {
#       lambda <- jsamp_fvals[idx, samp] * ex_lam
#       samp <- rpois(nstate, lambda)
#       sum(samp)
#     })
#     # qs <- quantile(nat_sum_per_t, q)
#     # names(qs) <- str_c("q", names(qs))
#     tibble_row(target_group = "Overall", count_samp = list(nat_sum_per_t))
#   }) |>
#     bind_cols(ret_df) |>
#     select(date:epiweek, target_group, population, count_samp)
# }
#
# ################################### this is the no offset version
# fit_current_model1_no_offset <- function(
#   fit_df,
#   forecast_date,
#   ar_order,
#   rw_order,
#   seasonal_smoothness,
#   forecast_uncertainty_parameter,
#   q = c(0.025, 0.25, 0.5, 0.75, 0.975),
#   joint = TRUE
# ) {
#   hyper_epwk <- switch(
#     seasonal_smoothness,
#     "default" = list(theta = list(prior = "pc.prec", param = c(0.5, 0.01))),
#     "less"    = list(theta = list(prior = "pc.prec", param = c(0.25, 0.01))),
#     "more"    = list(theta = list(prior = "pc.prec", param = c(1, 0.01))),
#     stop("Invalid seasonal_smoothness")
#   )
#
#   hyper_wk <- switch(
#     forecast_uncertainty_parameter,
#     "default" = list(theta = list(prior = "pc.prec", param = c(1, 0.01))),
#     "small"   = list(theta = list(prior = "pc.prec", param = c(0.2, 0.01))),
#     "tiny"    = list(theta = list(prior = "pc.prec", param = c(0.05, 0.01))),
#     stop("Invalid forecast_uncertainty_parameter")
#   )
#
#   rw_mod <- switch(
#     rw_order,
#     "1" = "f(epiweek, model='rw1', cyclic=TRUE, hyper=hyper_epwk)",
#     "2" = "f(epiweek, model='rw2', cyclic=TRUE, hyper=hyper_epwk)",
#     stop("Invalid RW order")
#   )
#
#   formula_string <- paste(
#     "count ~ 1 +",
#     rw_mod,
#     "+ f(t, model='ar', order=",
#     ar_order,
#     ", group=snum, hyper=hyper_wk, control.group=list(model='exchangeable'))"
#   )
#
#   mod <- as.formula(formula_string)
#   pred_idx <- which(fit_df$date >= forecast_date)
#
#   fit <- inla(
#     mod,
#     family = "poisson",
#     data = fit_df,
#     quantiles = q,
#     selection = if (!joint) NULL else list(Predictor = pred_idx),
#     control.compute = list(
#       dic = FALSE,
#       mlik = FALSE,
#       return.marginals.predictor = TRUE
#     ),
#     control.predictor = list(link = 1, compute = TRUE)
#   )
#
#   return(fit)
# }
#
# sample_count_predictions_no_offset <- function(fit_df, fit, nsamp = 10000) {
#   samp_counts <- map(fit$marginals.fitted.values, \(m) {
#     msamp <- pmax(0, inla.rmarginal(nsamp, m))
#     rpois(nsamp, msamp)
#   })
#
#   count_df <- tibble(count_samp = samp_counts)
#   return(bind_cols(fit_df, count_df))
# }
#
# sample_national_no_offset <- function(fit_df, fit, forecast_date, nsamp = 10000) {
#   state_info <- distinct(fit_df, target_group)
#   nstate <- nrow(state_info)
#
#   ret_df <- fit_df |>
#     filter(date >= forecast_date) |>
#     group_by(date, t, epiweek) |>
#     summarise(.groups = "drop")
#
#   jsamp_fvals <- exp(inla.rjmarginal(nsamp, fit$selection)$samples)
#
#   tslice <- map(1:nrow(ret_df), ~ nstate * (.x - 1) + 1:nstate)
#
#   imap_dfr(tslice, \(idx, t) {
#     nat_sum_per_t <- map_dbl(1:nsamp, \(samp) {
#       lambda <- jsamp_fvals[idx, samp]
#       sum(rpois(nstate, lambda))
#     })
#     tibble_row(target_group = "Overall", count_samp = list(nat_sum_per_t))
#   }) |>
#     bind_cols(ret_df) |>
#     select(date, t, epiweek, target_group, count_samp)
# }
#
# ######################################### works for aggregate
#
# summarize_quantiles_aggregate <- function(
#   pred_samples,
#   nat_samps,
#   forecast_date,
#   q
# ) {
#   pred_samples |>
#     filter(date >= forecast_date) |>
#     bind_rows(nat_samps) |>
#     unnest(count_samp) |>
#     group_by(date, target_group) |>
#     summarize(
#       qs = list(
#         value = quantile(count_samp, probs = q)
#       ),
#       .groups = "drop"
#     ) |>
#     unnest_wider(qs) |>
#     pivot_longer(contains("%"), names_to = "quantile") |>
#     mutate(quantile = as.numeric(gsub("[\\%,]", "", quantile)) / 100) |>
#     mutate(
#       horizon = as.numeric(as.factor(date)) - 1,
#       reference_date = as.Date(forecast_date) + 3,
#       # Use lubridate syntax to add horizon to forecast_date
#       target_end_date = (as.Date(forecast_date) %m+% weeks(horizon)) + 3,
#       # Below gave error (non-numeric argument to binary operator)
#       # target_end_date = forecast_date + horizon * 7,
#       output_type_id = as.numeric(quantile),
#       output_type = "quantile",
#       value = round(value)
#     ) |>
#     arrange(target_group, horizon, quantile) |>
#     dplyr::select(
#       reference_date,
#       horizon,
#       target_end_date,
#       target_group,
#       output_type,
#       output_type_id,
#       value
#     )
# }
#
# ####################### works for single target
# summarize_quantiles_single_target <- function(
#   pred_samples,
#   nat_samps,
#   forecast_date,
#   q
# ) {
#   pred_samples |>
#     filter(date >= forecast_date) |>
#     # bind_rows(nat_samps) |>
#     unnest(count_samp) |>
#     group_by(date, target_group) |>
#     summarize(
#       qs = list(
#         value = quantile(count_samp, probs = q)
#       ),
#       .groups = "drop"
#     ) |>
#     unnest_wider(qs) |>
#     pivot_longer(contains("%"), names_to = "quantile") |>
#     mutate(quantile = as.numeric(gsub("[\\%,]", "", quantile)) / 100) |>
#     mutate(
#       horizon = as.numeric(as.factor(date)) - 1,
#       reference_date = as.Date(forecast_date) + 3,
#       # Use lubridate syntax to add horizon to forecast_date
#       target_end_date = (as.Date(forecast_date) %m+% weeks(horizon)) + 3,
#       # Below gave error (non-numeric argument to binary operator)
#       # target_end_date = forecast_date + horizon * 7,
#       output_type_id = as.numeric(quantile),
#       output_type = "quantile",
#       value = round(value)
#     ) |>
#     arrange(target_group, horizon, quantile) |>
#     dplyr::select(
#       reference_date,
#       horizon,
#       target_end_date,
#       target_group,
#       output_type,
#       output_type_id,
#       value
#     )
# }
