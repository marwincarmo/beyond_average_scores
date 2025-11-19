library(dplyr)
library(tidyr)
library(ggplot2)

## Bias with correlated random effects

results_raw <- rbind(readRDS("data/output/df_6.rds"),
                     readRDS("data/output/df_18.rds"))

str(results_raw)

results_df <- results_raw |>
  dplyr::mutate(
         bias_ivd = ivd_sd_scl_Intc - sd_slab,
         bias_hlm = hlm_sd_scl_Intc - sd_slab,
         rmse_ivd = sqrt((ivd_sd_scl_Intc - sd_slab)^2),
         rmse_hlm = sqrt((hlm_sd_scl_Intc - sd_slab)^2)) |>
  tidyr::pivot_longer(
    cols = matches("^(tp|fp|fn|tn|bias|rmse|r)_"),  # Metrics prefixed columns
    names_to = c("metric", "model"),
    names_pattern = "^(tp|fp|fn|tn|bias|rmse|r)_(.+)$",
    values_to = "value"
    ) |>
  # fix model names
  ## dplyr::mutate(model = dplyr::case_when(
  ##                              model == "rb" ~ "hlm",
  ##                              model == "jags" ~ "ivd",
  ##                              TRUE ~ model)) |>
  tidyr::pivot_wider(
         names_from = metric,
         values_from = value) |>
  tidyr::replace_na(list(fn = 0)) |>
  dplyr::mutate(
         tpr = tp / (tp + fn),
         fpr = fp / (fp + tn),
         tnr = tn / (tn + fp),
         precision = tp / (tp + fp),
         f1 = 2 * (precision * tpr) / (precision + tpr)
         )

str(results_df)

results_df |>
  dplyr::filter(#p_slab != 0,
                #n_students == 75
                n_schools == 150
                ) |>
  dplyr::mutate(sd_slab = factor(round(sd_slab,2))) |>
  ggplot(aes(y = r, x = sd_slab, color = model, fill = model)) +
  geom_boxplot(position = position_dodge(preserve = "single"), alpha = 0.5 )+
  theme_bw(16) +
  scale_color_manual(
      values = c("coral", "cornflowerblue")
    , labels = c("Two-stage HLM", "SS-MELSM")
    ) +
   scale_fill_manual(
      values = c("coral", "cornflowerblue")
    , labels = c("Two-stage HLM", "SS-MELSM")
    ) +
  theme(legend.position = "bottom") +
  facet_grid(n_students
             #n_schools
             ~ p_slab, labeller = label_both) +
  labs(x = "True random intercept SD",
       y = "Bias",
       title = "Biasfor 75 students")

## Direction of classification

results2_raw <- rbind(readRDS("data/output/03_simulation_main_results_r2_19.rds"),
                      readRDS("data/output/03_simulation_main_results_r2_20.rds"))

results2_raw |>
  dplyr::select(condition_id, replication, n_students, n_schools,
         p_slab, sd_slab,
         tp_high_hlm, tp_low_hlm, fp_high_hlm, fp_low_hlm,
         tp_high_ivd, tp_low_ivd, fp_high_ivd, fp_low_ivd) |>
  tidyr::pivot_longer(
    cols = starts_with("tp_") | starts_with("fp_"),
    names_to = c("type", "direction", "model"),
    names_sep = "_",
    values_to = "value"
    ) |>
  tidyr::pivot_wider(
         names_from = type,
         values_from = value) |>
  tidyr::replace_na(list(fn = 0)) |>
  dplyr::mutate(
         tpr = tp / (tp + fn),
         fpr = fp / (fp + tn),
         precision = tp / (tp + fp),
         f1 = 2 * (precision * tpr) / (precision + tpr)
         ) |>
  head()

results_long <- results2_raw |>
  ## pivot_longer(
  ##   cols = matches("^(tp|fp)_(high|low)_(hlm|ivd)$"),
  ##   names_to = c("type", "direction", "model"),
  ##   names_sep = "_",
  ##   values_to = "count"
  ## )
  dplyr::select(condition_id, replication, n_students, n_schools,
                p_slab, sd_slab, dplyr::starts_with(c("tp", "fp", "tn", "fn"))) |>
  tidyr::pivot_longer(
    cols = matches("^(tp|fp|fn|tn)_"),  # Metrics prefixed columns
    names_to = c("metric", "model"),
    names_pattern = "(.+)_(hlm|ivd)$",
    values_to = "value"
    )

rates_by_condition <- results_long |>
   tidyr::pivot_wider(
         names_from = metric,
         values_from = value) |>
  dplyr::mutate(
           tpr = tp / (tp + fn),
           fpr = fp / (fp + tn),
           tpr_high = tp_high / (tp + fn),
           tpr_low = tp_low / (tp + fn),
           fpr_high = fp_high / (fp + tn),
           fpr_low = fp_low / (fp + tn)) |>
  tidyr::replace_na(list(tpr = 0, tpr_high = 0,
                         tpr_low = 0)) |>
  dplyr::group_by(n_students, p_slab, sd_slab, model) |>
  dplyr::summarise(
           tpr_high = mean(tpr_high, na.rm = TRUE),
           tpr_low  = mean(tpr_low, na.rm = TRUE),
           fpr_high = mean(fpr_high, na.rm = TRUE),
           fpr_low  = mean(fpr_low, na.rm = TRUE),
           .groups = "drop") |>
  tidyr::pivot_longer(
           cols = dplyr::matches("^(tpr|fpr)_"),
           names_to = c("component", "direction"),
           values_to = "mean_rate",
           names_sep = "_") |>
  dplyr::group_by(n_students, p_slab, sd_slab, model, component) |>
  dplyr::mutate(
    total_rate = sum(mean_rate),
    percentage = mean_rate / total_rate,
    .groups = "drop"
    ) |>
  dplyr::mutate(percentage = if_else(is.nan(percentage), 0, percentage))


## TPR
rates_by_condition |>
  dplyr::mutate(sd_slab = factor(round(sd_slab, 2))) |>
  dplyr::filter(component == "tpr", p_slab != 0) |>
  ggplot(aes(x=sd_slab, y = mean_rate, fill = direction)) +
  geom_col(position = "stack", color = "black", alpha = 0.8) +
  geom_text(
    # We use 'percentage' for the label, formatted nicely
    aes(label = if_else(percentage > 0.02, # Only label if > 2%
                        scales::percent(percentage, accuracy = 1L),
                        "")),
    position = position_stack(vjust = 0.5), # This centers text in each segment
    color = "white", # "white" or "black" often works best
    size = 3.5       # Adjust size as needed
  ) +
  facet_grid(n_students ~ p_slab + model, labeller = label_both) +
  theme_bw(16) +
  scale_fill_brewer(palette = "Set1")

## FPR
rates_by_condition |>
  dplyr::mutate(sd_slab = factor(round(sd_slab, 2))) |>
  dplyr::filter(component == "fpr") |>
  ggplot(aes(x=sd_slab, y = mean_rate, fill = direction)) +
  geom_col(position = "stack", color = "black", alpha = 0.8) +
  geom_text(
    # We use 'percentage' for the label, formatted nicely
    aes(label = if_else(percentage > 0.02, # Only label if > 2%
                        scales::percent(percentage, accuracy = 1L),
                        "")),
    position = position_stack(vjust = 0.5), # This centers text in each segment
    color = "white", # "white" or "black" often works best
    size = 3.5       # Adjust size as needed
  ) +
  facet_grid(n_students ~ p_slab + model, labeller = label_both,
             scales = "free_x") +
  theme_bw(16) +
  scale_fill_brewer(palette = "Set1")


## TPR - low variance

results_tpr|>
  dplyr::filter(n_schools == 150) %>%
  dplyr::mutate(sd_slab = factor(round(sd_slab, 2))) %>%
  ggplot(aes(
    y = rate,
    x = sd_slab,
    color = model,
    fill = model
  )) +
  geom_boxplot(position = position_dodge(preserve = "single"), alpha = 0.5) +
  theme_bw(base_size = 16) +
  scale_color_manual(
    values = c("coral", "cornflowerblue"),
    labels = c("Two-stage HLM", "SS-MELSM")
  ) +
  scale_fill_manual(
    values = c("coral", "cornflowerblue"),
    labels = c("Two-stage HLM", "SS-MELSM")
  ) +
  facet_grid(n_students ~ p_slab, labeller = label_both) +
  theme(legend.position = "bottom") +
  labs(
    x = "True random intercept SD",
    y = "True Positive (proxy) Rate",
    title = "TPR by tail direction (150 schools)"
  )

## Sensitivity for p_slab = 0

results2_raw |>
  dplyr::filter(p_slab == 0) |>
  tidyr::pivot_longer(
    cols = matches("^(tp|fp|fn|tn)_"),  # Metrics prefixed columns
    names_to = c("metric", "model"),
    names_pattern = "^(tp|fp|fn|tn)_(.+)$",
    values_to = "value"
    ) |>
  tidyr::pivot_wider(
           names_from = metric,
           values_from = value) |>
  tidyr::replace_na(list(fn = 0)) |>
  dplyr::mutate(
           tpr = tp / (tp + fn),
           fpr = fp / (fp + tn),
           tnr = tn / (tn + fp),
           precision = tp / (tp + fp),
           f1 = 2 * (precision * tpr) / (precision + tpr)
         ) |>
  dplyr::group_by(n_students, model) |>
  dplyr::summarise(avg_tnr = mean(tnr, na.rm = TRUE))

results_df <- results_raw |>
  dplyr::mutate(
         bias_ivd = ivd_sd_scl_Intc - sd_slab,
         bias_hlm = hlm_sd_scl_Intc - sd_slab,
         rmse_ivd = sqrt((ivd_sd_scl_Intc - sd_slab)^2),
         rmse_hlm = sqrt((hlm_sd_scl_Intc - sd_slab)^2)) |>
  tidyr::pivot_longer(
    cols = matches("^(tp|fp|fn|tn|bias|rmse|r)_"),  # Metrics prefixed columns
    names_to = c("metric", "model"),
    names_pattern = "^(tp|fp|fn|tn|bias|rmse|r)_(.+)$",
    values_to = "value"
    ) |>
  # fix model names
  ## dplyr::mutate(model = dplyr::case_when(
  ##                              model == "rb" ~ "hlm",
  ##                              model == "jags" ~ "ivd",
  ##                              TRUE ~ model)) |>
  tidyr::pivot_wider(
         names_from = metric,
         values_from = value) |>
  tidyr::replace_na(list(fn = 0)) |>
  dplyr::mutate(
         tpr = tp / (tp + fn),
         fpr = fp / (fp + tn),
         tnr = tn / (tn + fp),
         precision = tp / (tp + fp),
         f1 = 2 * (precision * tpr) / (precision + tpr)
         )
