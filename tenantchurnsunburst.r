# 0. Libraries ------------------------------------------------------------------
library(dplyr)
library(tidyr)          # crossing()
library(ggplot2)
library(scales)         # percent_format()
library(RColorBrewer)
library(plotly)
library(readr)          # fast CSV

setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)  # turn off 60-s IDE limit

# 1. Parameters you may tune ----------------------------------------------------
look_ahead <- 2   # quarters near the panel end treated as "still active"

# 2. Load leases ----------------------------------------------------------------
leases <- read_csv("Leases.csv", col_types = cols()) %>%    # faster than read.csv
  mutate(year_quarter = paste0(year, "_Q", quarter)) %>%    # e.g. 2021_Q3
  filter(
    market %in% c("Chicago", "Chicago Suburbs"),
    transaction_type %in% c("New", "Renewal"),
    internal_class %in% c("A", "O")
  )

# 3. Build complete tenant × quarter panel (market-level) -----------------------
tenant_presence <- leases %>%
  group_by(company_name, market, year_quarter) %>%          # NO submarket/class
  summarise(leasedSF_total = sum(leasedSF, na.rm = TRUE), .groups = "drop") %>%
  mutate(present = 1L)

all_quarters <- sort(unique(leases$year_quarter))

tenant_grid <- crossing(
  tenant_presence %>% distinct(company_name, market),       # one row per tenant-market
  year_quarter = all_quarters
)

tenant_presence_full <- tenant_grid %>%
  left_join(tenant_presence,
            by = c("company_name","market","year_quarter")) %>%
  mutate(
    present        = ifelse(is.na(present), 0L, 1L),
    leasedSF_total = ifelse(is.na(leasedSF_total), 0,  leasedSF_total)
  ) %>%
  arrange(company_name, market, year_quarter)

# 4. Flag permanent exits (requires a long-enough trailing gap) ---------------
gap_needed <- 3   # ← change this: 3  means “must be absent ≥ 3 quarters”

tenant_churn_status <- tenant_presence_full %>%
  group_by(company_name, market) %>%
  summarise(
    # index positions (1 … length(all_quarters)) where the tenant is present
    idx_present    = list(which(present == 1L)),
    total_sqft     = sum(leasedSF_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    last_present_idx = ifelse(length(idx_present[[1]]) > 0,
                              max(idx_present[[1]]),   # most recent quarter with presence
                              0L),
    
    # how many *consecutive* quarters at the end of the panel show no presence
    trailing_zeros  = length(all_quarters) - last_present_idx,
    
    churned = if_else(
      trailing_zeros >= gap_needed + look_ahead,  # long enough gap *and* not censored
      1L, 0L
    ),
    sqft_churned = if_else(churned == 1L, total_sqft, 0)
  ) %>%
  select(-idx_present)   # drop the list column


# 4. Flag permanent exits (one per company×market) ------------------------------
tenant_churn_status <- tenant_presence_full %>%
  group_by(company_name, market) %>%
  summarise(
    last_present_idx = ifelse(any(present == 1L),
                              max(which(present == 1L)),
                              0L),
    total_sqft       = sum(leasedSF_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    churned      = if_else(last_present_idx <= length(all_quarters) - look_ahead,
                           1L, 0L),
    sqft_churned = if_else(churned == 1L, total_sqft, 0)
  )

# 5. Attach latest sub-market & class (for reporting only) ----------------------
latest_tag <- leases %>%                               # use raw data for latest tag
  arrange(year_quarter) %>%
  group_by(company_name, market) %>%
  slice_tail(n = 1) %>%                                # latest row
  ungroup() %>%
  select(company_name, market, internal_submarket, internal_class)

tenant_churn_status <- tenant_churn_status %>%
  left_join(latest_tag, by = c("company_name","market"))

# 6. Summaries ------------------------------------------------------------------
churn_summary <- tenant_churn_status %>%
  group_by(market, internal_submarket, internal_class) %>%
  summarise(
    total_tenants      = n(),
    tenants_churned    = sum(churned),
    churn_rate_tenants = tenants_churned / total_tenants,
    total_sqft         = sum(total_sqft),
    sqft_churned       = sum(sqft_churned),
    churn_rate_sqft    = sqft_churned / total_sqft,
    .groups = "drop"
  )

# 7. Bar charts -----------------------------------------------------------------
ggplot(churn_summary,
       aes(x = market, y = churn_rate_tenants, fill = internal_class)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Tenant Churn Rate by Market & Class (Count)",
       x = "Market", y = "Churn Rate (Tenants)", fill = "Class") +
  theme_minimal()

ggplot(churn_summary,
       aes(x = market, y = churn_rate_sqft, fill = internal_class)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Tenant Churn Rate by Market & Class (Square Footage)",
       x = "Market", y = "Churn Rate (SF)", fill = "Class") +
  theme_minimal()

# 5. Column plots – churn by tenant count and by square footage
ggplot(churn_summary,
       aes(x = market, y = churn_rate_tenants, fill = internal_class)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = percent_format(1)) +
  labs(title = "Tenant Churn Rate by Market & Class (Count)",
       x = "Market", y = "Churn Rate (Tenants)", fill = "Class") +
  theme_minimal()

ggplot(churn_summary,
       aes(x = market, y = churn_rate_sqft, fill = internal_class)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = percent_format(1)) +
  labs(title = "Tenant Churn Rate by Market & Class (Square Footage)",
       x = "Market", y = "Churn Rate (SF)", fill = "Class") +
  theme_minimal()

# 6. Chicago-only drill-down for the sunburst visual
churn_chicago <- tenant_churn_status %>%            # <- NOT tenant_presence_full
  filter(market == "Chicago") %>%
  mutate(churn_status = if_else(churned == 1, "Churned", "Stayed")) %>%
  group_by(internal_submarket, internal_class, churn_status) %>%
  summarise(total_tenants = n(), .groups = "drop") %>%
  group_by(internal_submarket, internal_class) %>%
  mutate(prop = total_tenants / sum(total_tenants)) %>%
  ungroup()


# Sunburst hierarchy (root → submarket → class → churn status)
root <- tibble(ids = "Chicago", parents = "", total_tenants = sum(churn_chicago$total_tenants))

level2 <- churn_chicago %>%
  group_by(internal_submarket) %>%
  summarise(total_tenants = sum(total_tenants), .groups = "drop") %>%
  mutate(ids = internal_submarket, parents = "Chicago")

level3 <- churn_chicago %>%
  group_by(internal_submarket, internal_class) %>%
  summarise(total_tenants = sum(total_tenants), .groups = "drop") %>%
  mutate(ids = paste(internal_submarket, internal_class, sep = " – "),
         parents = internal_submarket)

level4 <- churn_chicago %>%
  mutate(ids = paste(internal_submarket, internal_class, churn_status, sep = " – "),
         parents = paste(internal_submarket, internal_class, sep = " – ")) %>%
  select(ids, parents, total_tenants)

sunburst_df <- bind_rows(root, level2, level3, level4)

print(plot_ly(
  sunburst_df,
  ids     = ~ids,
  labels  = ~ids,
  parents = ~parents,
  values  = ~total_tenants,
  type    = "sunburst",
  branchvalues = "total"
) %>%
  layout(title = "Chicago Tenant Churn Hierarchy (Sunburst)"))








