
# =====================================
# Helper: Compute Quantiles
# =====================================
get_quantiles_df <- function(
  sim_matrix,
  taus = c(0.01, 0.025, seq(0.05, 0.95, 0.05), 0.975, 0.99)
) {
  if (!is.matrix(sim_matrix)) stop("`sim_matrix` must be a matrix.")

  h <- ncol(sim_matrix)
  quantile_list <- lapply(1:h, function(i) {
    quantile(sim_matrix[, i], probs = taus, na.rm = TRUE)
  })

  df <- do.call(rbind, quantile_list)
  df <- as.data.frame(df)
  df$h <- 1:h

  df_long <- tidyr::pivot_longer(df, -h, names_to = "name", values_to = "value")
  df_long$quantile <- as.numeric(gsub("%", "", df_long$name)) / 100
  df_long <- df_long[, c("h", "quantile", "value")]

  return(df_long)
}

# Fit and process regular baseline =============================================
fit_process_baseline_flat <- function(
  df,
  weeks_ahead,
  quantiles_needed,
  window_size=NULL,
  data_type = "count"
) {

  is_proportion <- identical(data_type, "proportion")
  logit_eps <- 1e-4

  preds <- map_dfr(unique(df$target_group), \(grp) {

    df_grp <- df |> filter(target_group == grp)

    wsize <- if (is.null(window_size)) nrow(df_grp)-1 else window_size

    # For proportions, do the bounded transform ourselves and hand `simplets`
    # an already-transformed series (transformation = "none"): a plain sqrt
    # transform has no notion of an upper bound and can simulate values above
    # 1, while a logit transform structurally can't.
    fit_y <- if (is_proportion) {
      qlogis(pmin(pmax(df_grp$value, logit_eps), 1 - logit_eps))
    } else {
      df_grp$value
    }

    baseline_fit <- fit_simple_ts(
      y = fit_y,
      ts_frequency = 1,
      model = "quantile_baseline",
      transformation = if (is_proportion) "none" else "sqrt",
      transform_offset = 1,
      d = 0,
      D = 0,
      symmetrize = TRUE,
      window_size = wsize
    )

    sim_matrix <- predict(
      baseline_fit,
      nsim = 10000,
      horizon = weeks_ahead,
      quantiles = quantiles_needed,
      origin = "obs", # predict forward from the most recent value
      # force_nonneg clamps on the *transformed* scale (see note below), which
      # would be wrong for logit values (legitimately negative), so only use
      # it for the unbounded count/sqrt path.
      force_nonneg = !is_proportion
    )

    if (is_proportion) {
      # plogis() is already bounded to (0, 1); no separate floor/ceiling needed.
      sim_matrix <- plogis(sim_matrix)
    } else {
      # `simplets` applies `force_nonneg` on the transformed scale. With
      # sqrt(value + 1), transformed values below 1 invert to negative values,
      # so enforce the outcome's lower bound after returning to its original scale.
      sim_matrix <- pmax(sim_matrix, 0)
    }

    get_quantiles_df(sim_matrix,
                     taus = quantiles_needed) |>
      mutate(
        target_group = grp,
        value = ifelse(quantile == 0.5, tail(df_grp$value, 1), value)
      )
  })

  preds |>
    mutate(horizon=h, output_type="quantile", output_type_id=as.character(quantile)) |>
    select(horizon, target_group, output_type, output_type_id, value)
}
