head(results_raw)
head(results_spike)
library(stringr)

results_spike <- readRDS("../beyond_average_scores/data/output/01_simulation_main_results0.rds")

results_renamed <- results_spike |>
  rename_with(
    # The function to apply: replace all instances of "ivd" with "jags"
    ~ str_replace_all(.x, "ivd", "jags"),
    # The columns to apply it to: any column that contains "ivd"
    .cols = contains("ivd")
  )

# First, isolate the part of results_raw you want to keep
results_to_keep <- results_raw |>
  filter(!(p_slab == 0 & sd_slab == 0 & n_students == 75))
# The line above reads: "Filter for rows where it is NOT the case that
# p_slab is 0 AND n_students is 75."

# Now, bind it with your renamed data
dat <- bind_rows(results_renamed, results_to_keep)

dat |>
  filter(n_students == 75, p_slab == 0) |>
  tally()

dat <- bind_rows(
  results_renamed,
  results_raw[results_raw$p_slab != 0 | results_raw$n_students != 75, ]
)

dat |> 
  dplyr::filter(n_students == 75, p_slab != 0)

results_raw = dat

results_clean <- results_spike |>
  mutate(
    # For every column that starts with "jags_"
    across(
      starts_with("jags_"),
      # Apply this function:
      ~ coalesce(
        .x, # Take the original value from the jags_* column (.x)
        # And coalesce it with the value from the corresponding ivd_* column
        get(str_replace(cur_column(), "^jags_", "ivd_"))
      )
    )
  ) |>
  # Finally, remove all the original ivd_* columns
  select(-starts_with("ivd_"))

dat |> 
  dplyr::filter(n_students == 75, p_slab != 0)
