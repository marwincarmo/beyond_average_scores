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

results2_raw <- readRDS("data/output//03_simulation_main_results_r2_19.rds")

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

results_long <- results2_raw %>%
  pivot_longer(
    cols = matches("^(tp|fp)_(high|low)_(hlm|ivd)$"),
    names_to = c("type", "direction", "model"),
    names_sep = "_",
    values_to = "count"
  )

rates <- results2_raw %>%
  summarise(
    across(c(tp_high_hlm, tp_low_hlm, fp_high_hlm, fp_low_hlm,
             tp_high_ivd, tp_low_ivd, fp_high_ivd, fp_low_ivd),
           sum)
  ) %>%
  pivot_longer(
    everything(),
    names_to = c("type", "direction", "model"),
    names_sep = "_",
    values_to = "count"
  ) %>%
  group_by(model, direction) %>%
  summarise(
    tp = sum(count[type == "tp"]),
    fp = sum(count[type == "fp"]),
    # Denominators: number of true or false cases
    # You might have the number of true signals known per simulation (e.g., n_true_high, etc.)
    # Replace  totals_below with your actual numbers:
    tpr = tp / (tp + fp + 1e-9),  # placeholder formula — adjust below
    fpr = fp / (tp + fp + 1e-9)
  )

library(dplyr)
library(tidyr)

rates_by_condition <- results2_raw %>%
  # Reshape to long format
  pivot_longer(
    cols = matches("^(tp|fp)_(high|low)_(hlm|ivd)$"),
    names_to = c("type", "direction", "model"),
    names_sep = "_",
    values_to = "count"
  ) %>%
  group_by(condition_id, n_students, n_schools, p_slab, sd_slab,  model, direction, type) %>%
  summarise(
    mean_count = mean(count, na.rm = TRUE),
    sd_count   = sd(count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = type,
    values_from = c(mean_count, sd_count),
    names_glue = "{.value}_{type}"
  ) |>
  mutate(
    fpr_proxy = mean_count_fp / (mean_count_tp + mean_count_fp + 1e-9),
    tpr_proxy = mean_count_tp / (mean_count_tp + mean_count_fp + 1e-9)
  )

## FPR
rates_by_condition %>%
  dplyr::filter(n_schools == 150) %>%
  dplyr::mutate(sd_slab = factor(round(sd_slab, 2))) %>%
  ggplot(aes(
    y = fpr_proxy,
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
  facet_grid(direction ~ p_slab, labeller = label_both) +
  theme(legend.position = "bottom") +
  labs(
    x = "True random intercept SD",
    y = "False Positive (proxy) Rate",
    title = "FPR by tail direction (150 schools)"
  )

## TPR

rates_by_condition %>%
  dplyr::filter(n_schools == 150) %>%
  dplyr::mutate(sd_slab = factor(round(sd_slab, 2))) %>%
  ggplot(aes(
    y = tpr_proxy,
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
  facet_grid(direction ~ p_slab, labeller = label_both) +
  theme(legend.position = "bottom") +
  labs(
    x = "True random intercept SD",
    y = "True Positive (proxy) Rate",
    title = "TPR by tail direction (150 schools)"
  )
