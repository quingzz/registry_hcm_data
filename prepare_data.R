library(tidyverse)
library(plotly)
library(rlang)

# ====== Prepare birth data =====
# ----- Load and create new week variable
birth_data <- readRDS(file.path("data", "count_dangky_week.rds")) %>% ungroup()
birth_data <- birth_data %>%
  mutate(
    # create another variable for week cumulative week instead of week within a year
    cumulative_week = birth_week*(birth_year - min(birth_year) + 1)
)

# --- Compute valid HCM districts
valid_district <- birth_data %>%
  group_by(district_reg) %>%
  summarize(total_birth = sum(n)) %>%
  filter(
    # filter out districts with low birth count from 2014-2021 as those are typos (not HCM district)
    total_birth > 1000
  ) %>% select(district_reg) %>% ungroup()

# ---- only keep records with valid district
birth_data <- birth_data %>%
  inner_join(valid_district, by = join_by(district_reg == district_reg))

# ---- Save preprocessed data
write_rds(birth_data, "data/hcm_birth_data.rds")

# ====== Prepare coverage data ======
measles_vac <- readRDS("data/summarize_measles_vac.rds")
birth_data <- readRDS("data/hcm_birth_data.rds")
aggregated_birth_data <- birth_data %>%
  group_by(
    district_reg, birth_year
  ) %>%
  summarize(
    birth_count = sum(n)
  ) %>%
  ungroup()

# ----- Helper to compute coverage
compute_coverage <- function(birth_data, vacdata, coverage_of = "any_shot", vac_age = "[0-1)"){
  if (vac_age == "[0-1)"){
    # ==== Only consider vaccinations within 1 year of birth =====
    shot_count <- vacdata %>%
      filter(age_group == "[0-1)")
  }else{
    # ==== If compute number of shots at all vaccination age_group -> concatenate shots =====
    shot_count <- vacdata %>%
      group_by(pid, birth_year, district) %>%
      summarize(
        total_shots = sum(total_shots)
      ) %>%
      ungroup()
  }


  # ===== Compute total number of vaccination shots ====
  shot_count <- shot_count %>%
    group_by(birth_year, district) %>%
    summarize(
      any_shot = sum(total_shots > 0),
      shots_1 = sum(total_shots == 1),
      shots_2 =  sum(total_shots == 2),
      shots_3 = sum(total_shots == 3)
    ) %>%
    ungroup()

  # ===== Compute coverage =======
  compute_cov_expr <- glue::glue("shot_count %>%
    inner_join(
      aggregated_birth_data,
      by = join_by(district == district_reg, birth_year == birth_year)
    ) %>%
    mutate(coverage = {coverage_of} /birth_count)")

  eval(rlang::parse_expr(compute_cov_expr))
}

# compute coverage for vaccination within one year of birth only
one_year_cov <- compute_coverage(aggregated_birth_data, measles_vac, vac_age = "[0-1)") %>% mutate(age_group = "[0-1)")
# compute overall coverage
all_age_cov <- compute_coverage(aggregated_birth_data, measles_vac, vac_age = "all") %>% mutate(age_group = "all")

full_cov <- bind_rows(one_year_cov, all_age_cov) %>% arrange(district, birth_year, age_group)

write_csv(full_cov, "data/measles_coverage_data.csv")
