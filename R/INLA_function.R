library(INLA)
library(tidyverse)
library(MMWRweek)
library(lubridate)
library(readr)

inla.setOption(inla.mode="classic")


################
#Overall fit and process function 
################
fit_process_INLA <- function(fit_df, forecast_date, ar_order, rw_order, seasonal_smoothness, forecast_uncertainty_parameter, 
                             q = c(0.025, 0.25, 0.5, 0.75, 0.975), joint = TRUE) {
  # Fit the current model
  fit <- fit_current_model1(fit_df, forecast_date, ar_order, rw_order, seasonal_effect, weekly_effect, q, joint)
  
  # Sample national-level predictions
  nat_samps <- sample_national(fit_df, fit, forecast_date)
  
  # Sample count predictions
  pred_samples <- sample_count_predictions(fit_df, fit)
  
  # Summarize quantiles
  forecast_quantiles <- summarize_quantiles(pred_samples, nat_samps, forecast_date, 
                                            q = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99))
  
  return(forecast_quantiles)
}



################
#fit model
###############
fit_current_model1 <- function(fit_df, forecast_date, ar_order, rw_order, seasonal_smoothness, forecast_uncertainty_parameter, q=c(0.025, 0.25, 0.5, 0.75, 0.975), joint=TRUE) {
  
  # Set hyperparameters for seasonal effect
  hyper_epwk <- switch(seasonal_smoothness,
                       "default" = list(theta=list(prior="pc.prec", param=c(0.5, 0.01))),
                       "less" = list(theta=list(prior="pc.prec", param=c(0.25, 0.01))),
                       "more" = list(theta=list(prior="pc.prec", param=c(1, 0.01))),
                       stop("Invalid selection for seasonal_smoothness"))
  
  # Set hyperparameters for weekly effect
  #hyper_wk <- switch(weekly_effect,
  #"default" = list(theta=list(prior="pc.prec", param=c(1, 0.01))),
  #"less" = list(theta=list(prior="pc.prec", param=c(0.5, 0.01))),
  #"more" = list(theta=list(prior="pc.prec", param=c(2, 0.01))),
  #stop("Invalid selection for weekly effect"))
  
  hyper_wk <- switch(forecast_uncertainty_parameter,
                     "default" = list(theta=list(prior="pc.prec", param=c(1, 0.01))),
                     "small" = list(theta=list(prior="pc.prec", param=c(0.2, 0.01))),
                     "tiny" = list(theta=list(prior="pc.prec", param=c(0.05, 0.01))),
                     stop("Invalid selection for forecast_uncertainty_parameter"))
  
  # Create the rw_mod part of the formula dynamically
  rw_mod <- switch(rw_order,
                   "1" = "f(epiweek, model='rw1', cyclic=TRUE, hyper=hyper_epwk)",
                   "2" = "f(epiweek, model='rw2', cyclic=TRUE, hyper=hyper_epwk)",   ###this is default
                   stop("Invalid selection for RW order"))
  
  # Create the complete formula as a string
  formula_string <- paste("count ~ 1 +", rw_mod, "+ f(t, model='ar', order=", ar_order, ", group=snum, hyper=hyper_wk, control.group=list(model='exchangeable'))")
  
  # Convert the formula string to a formula object
  mod <- as.formula(formula_string)
  
  pred_idx <- which(fit_df$date >= forecast_date)
  
  fit <- inla(
    mod, family="poisson", data=fit_df, 
    E=fit_df$ex_lam,
    quantiles=q,
    selection=if (!joint) NULL else list(Predictor=pred_idx),
    control.compute=list(dic=FALSE, mlik=FALSE, return.marginals.predictor=TRUE), 
    control.predictor=list(link=1, compute=TRUE) 
  )
  return(fit)
}


#######################
#process fit functions 
#######################

sample_count_predictions <- function(fit_df, fit, nsamp=10000) {
  samp_counts <- map2_dfr(fit$marginals.fitted.values, fit_df$ex_lam, \(m, ex) {
    msamp <- pmax(0, inla.rmarginal(nsamp, m)) # sampling sometimes produces very small neg. numbers
    ct_samp <- rpois(nsamp, msamp * ex)
    tibble_row(count_samp=list(ct_samp))
  })
  
  return(bind_cols(fit_df, samp_counts))
}


sample_national <- function(fit_df, fit, forecast_date, nsamp=10000) {
  
  state_info <- distinct(fit_df, age_group, ex_lam)
  nstate <- nrow(state_info)
  
  ret_df <- fit_df |> 
    filter(date >= forecast_date) |> 
    #filter(date >= temp_date) |> 
    group_by(date, t, epiweek) |> 
    summarise(population=sum(population), .groups="drop")
  
  jsamp_fvals <- exp(inla.rjmarginal(nsamp, fit$selection)$samples)
  #ex_lam <- filter(fit_df, date >= forecast_date)$ex_lam
  ex_lam <- filter(fit_df, date >= forecast_date)$ex_lam
  
  
  tslice <- map(1:nrow(ret_df), ~nstate*(.x-1) + 1:nstate) # produce sequence [1:nstate, nstate+1:2nstate, ...]
  
  imap_dfr(tslice, \(idx, t) {
    nat_sum_per_t <- map_dbl(1:nsamp, \(samp) {
      lambda <- jsamp_fvals[idx, samp] * ex_lam
      samp <- rpois(nstate, lambda)
      sum(samp)
    })
    # qs <- quantile(nat_sum_per_t, q)
    # names(qs) <- str_c("q", names(qs))
    tibble_row(age_group="Overall", count_samp=list(nat_sum_per_t))
  }) |> 
    bind_cols(ret_df) |> 
    select(date:epiweek, age_group, population, count_samp)
}

summarize_quantiles <- function(pred_samples, nat_samps, forecast_date, q) {
  pred_samples |> 
    filter(date >= forecast_date) |> 
    bind_rows(nat_samps) |> 
    unnest(count_samp) |> 
    group_by(date, age_group) |> 
    summarize(qs = list(value = quantile(count_samp, probs=q)), .groups="drop") |> 
    unnest_wider(qs) |>
    pivot_longer(contains("%"), names_to="quantile") |> 
    mutate(quantile = as.numeric(gsub("[\\%,]", "", quantile))/100) |> 
    mutate(target = paste0("inc sari hosp"),
           horizon=as.numeric(as.factor(date)) - 1,
           reference_date = forecast_date,
           target_end_date = forecast_date + horizon*7,
           output_type_id = quantile,
           output_type = 'quantile',
           value = round(value)) %>%
    arrange(age_group, horizon, quantile) |>
    dplyr::select(reference_date, target, horizon, target_end_date, age_group, output_type, output_type_id, value)
}


