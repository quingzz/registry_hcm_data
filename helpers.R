# ===== helper to compute birth at weekly, district level ====
compute_birth <- function(birth_data, concatenate_by = "weekly"){
  selected_group_var <- if (concatenate_by == "weekly") "cumulative_week" else "birth_year"
  x_label <- if (concatenate_by == "weekly") "Cumulative week" else "Year of birth"

  plot_data <- birth_data %>%
    group_by(!!! rlang::syms(selected_group_var), district_reg) %>%
    summarize(
      no_birth = sum(n)
    )  %>%
    ungroup()

  rename_expr <- glue::glue("plot_data <- plot_data %>% rename(x = {selected_group_var})")
  eval(parse_expr(rename_expr))

  plot <- ggplot() +
    geom_line(
      aes(
        x = x, y = no_birth,
        color = factor(district_reg)
      ),
      data = plot_data
    ) +
    labs(
      x = x_label,
      y = "Number of births"
    )

  return(list("data"=data, "plot"=plot))
}


