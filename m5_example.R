library(m5)
library(fabletools)
library(fable)
devtools::load_all()
set.seed(42)
h = 28

# Import the
m5_ca1 <- tiny_m5 |>
  filter(store_id == "CA_1") |>
  tsibble(index = date, key = c(item_id, dept_id, cat_id)) |>
  aggregate_key( cat_id / dept_id / item_id, value = sum(value))

# Print the unique combinations of cat_id, dept_id, and item_id in m5_ca1
m5_ca1 |> as_tibble() |> select(cat_id, dept_id, item_id) |> 
  distinct() |> print(n = Inf)


# Build a column to identify aggregated time series
m5_ca1 <- m5_ca1 |>
  mutate(is_agg = fabletools::is_aggregated(cat_id) |
           fabletools::is_aggregated(dept_id) |
           fabletools::is_aggregated(item_id))

# Identify the last date in the dataset
end_date <- max(m5_ca1$date) - h + 1

# Fit ETS on upper time series
upper_fit <- m5_ca1 |>
  filter(date < end_date) |>
  filter(is_agg) |>
  model(base = ETS(value ~ trend("A")))

# Fit STATICNB on bottom time series
bottom_fit <- m5_ca1 |>
  filter(date < end_date) |>
  filter(!is_agg) |>
  model(base = SNAIVE(value, period = 7))


fit <- bind_rows(upper_fit, bottom_fit) |> reconcile(
  buis = bayesRecon_BUIS(base),
  t = bayesRecon_t(base),
  mixcond = bayesRecon_MixCond(base),
  tdcond = bayesRecon_tdcond(base)
  )

