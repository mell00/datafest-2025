# ──────────────────────────────────────────────────────────────────────────────
# Chicago & Chicago-Suburbs Tenant Churn • 2018-2024
# Updated to align with Leases.csv formatting (uses monthsigned)
# ──────────────────────────────────────────────────────────────────────────────

# 0. Libraries
library(readr)      # fast CSV import
library(dplyr)
library(tidyr)      # crossing(), unnest()
library(lubridate)  # make_date(), %m+% months(), year(), quarter()
library(zoo)        # as.yearqtr()
library(ggplot2)
library(scales)     # percent_format()
library(plotly)
library(RColorBrewer)

# Remove interactive time limit
setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)

# 1. Parameters ---------------------------------------------------------------
months_new  <- 60   # months assumed term for NEW leases
months_ren  <- 36   # months assumed term for RENEWAL leases
gap_needed  <- 2    # quarters absent to count as churn
look_ahead  <- 2    # last N quarters are right-censored

# 2. Load data & build lease start/end using actual signing month --------------
leases <- read_csv("Leases.csv", col_types = cols()) %>%
  mutate(
    year         = suppressWarnings(as.integer(year)),
    monthsigned  = suppressWarnings(as.integer(monthsigned)),
    # lease_start from exact signing month (first day)
    lease_start = make_date(year, monthsigned, 1),        # NA if invalid
    months_long = if_else(transaction_type == "New", months_new, months_ren),
    lease_end   = lease_start %m+% months(months_long)
  ) %>%
  filter(
    !is.na(lease_start),
    !is.na(lease_end),
    ## Keep only those leases that began in 2020 or later:
    lease_start >= as.Date("2020-01-01"),
    market %in% c("Chicago", "Chicago Suburbs"),
    internal_class %in% c("A", "O")
  )

# 3. Expand leases to all covered quarters ------------------------------------
leases_expanded <- leases %>%
  rowwise() %>%
  mutate(
    qtr_dates = list(seq(from = lease_start, to = lease_end, by = "quarter"))
  ) %>%
  unnest(qtr_dates) %>%
  ungroup() %>%
  mutate(
    year_quarter = paste0(year(qtr_dates), "_Q", quarter(qtr_dates))
  )

# 4. Presence panel -----------------------------------------------------------
tenant_presence <- leases_expanded %>%
  group_by(company_name, market, year_quarter) %>%
  summarise(leasedSF_total = sum(leasedSF, na.rm = TRUE), .groups = "drop") %>%
  mutate(present = 1L)

all_quarters <- sort(unique(leases_expanded$year_quarter))

tenant_grid <- crossing(
  tenant_presence %>% distinct(company_name, market),
  year_quarter = all_quarters
)

tenant_full <- tenant_grid %>%
  left_join(tenant_presence, by = c("company_name","market","year_quarter")) %>%
  mutate(
    present        = replace_na(present, 0L),
    leasedSF_total = replace_na(leasedSF_total, 0)
  ) %>%
  arrange(company_name, market, year_quarter)

# 5. Churn flag ---------------------------------------------------------------
tenant_churn <- tenant_full %>%
  group_by(company_name, market) %>%
  summarise(
    last_present_idx = max(which(present == 1L)),
    total_sqft       = sum(leasedSF_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    trailing_zeros = length(all_quarters) - last_present_idx,
    churned        = if_else(trailing_zeros >= gap_needed + look_ahead, 1L, 0L),
    sqft_churned   = if_else(churned == 1L, total_sqft, 0)
  )

# 6. Latest submarket & class for visualization -------------------------------
tag_latest <- leases_expanded %>%
  arrange(year_quarter) %>%
  group_by(company_name, market) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(company_name, market, internal_submarket, internal_class)

tenant_churn <- tenant_churn %>%
  left_join(tag_latest, by = c("company_name","market"))


# 7. Quarterly churn rates ------------------------------------------------------

# 7.1 Add next-quarter churn flag to the full panel
tenant_quarterly <- tenant_full %>%
  group_by(company_name, market) %>%
  arrange(year_quarter) %>%
  mutate(
    next_present      = lead(present, default = 1L),                          # assume stays beyond end
    churned_qtr       = if_else(present == 1L & next_present == 0L, 1L, 0L),
    sqft_churned_qtr  = if_else(churned_qtr == 1L, leasedSF_total, 0)
  ) %>%
  ungroup()

# 7. Quarterly churn rates ------------------------------------------------------

# 7.0 Tag each tenant×quarter with its submarket & class (for grouping)
tenant_quarterly_tagged <- tenant_quarterly %>%
  left_join(tag_latest, by = c("company_name","market"))

# 7.1 Summarize churn per quarter × market × submarket × class
churn_by_quarter <- tenant_quarterly_tagged %>%
  group_by(year_quarter, market, internal_submarket, internal_class) %>%
  summarise(
    tenants_present   = sum(present,        na.rm = TRUE),
    tenants_churned   = sum(churned_qtr,    na.rm = TRUE),
    churn_rate        = tenants_churned / tenants_present,
    sqft_present      = sum(leasedSF_total, na.rm = TRUE),
    sqft_churned      = sum(sqft_churned_qtr, na.rm = TRUE),
    churn_rate_sqft   = sqft_churned / sqft_present,
    .groups = "drop"
  )

# 7.2 Compute average churn per quarter
# a) simple (equal‐weight) mean
avg_churn_equal <- churn_by_quarter %>%
  group_by(market, internal_submarket, internal_class) %>%
  summarise(
    avg_churn_rate      = mean(churn_rate,      na.rm = TRUE),
    avg_churn_rate_sqft = mean(churn_rate_sqft, na.rm = TRUE),
    .groups = "drop"
  )

# b) tenant-weighted mean
avg_churn_weighted <- churn_by_quarter %>%
  group_by(market, internal_submarket, internal_class) %>%
  summarise(
    avg_churn_rate      = sum(churn_rate   * tenants_present, na.rm = TRUE) /
      sum(tenants_present,      na.rm = TRUE),
    avg_churn_rate_sqft = sum(churn_rate_sqft * sqft_present, na.rm = TRUE) /
      sum(sqft_present,              na.rm = TRUE),
    .groups = "drop"
  )

# (Optional) inspect the new tables
print(churn_by_quarter)
print(avg_churn_equal)
print(avg_churn_weighted)



# 7. Summaries by market / submarket / class (avg quarterly churn) ------------
churn_summary <- avg_churn_equal %>% 
  rename(
    churn_rate_tenants = avg_churn_rate,
    churn_rate_sqft   = avg_churn_rate_sqft
  )

# 8. Bar plots (average churn per quarter) -------------------------------------
ggplot(churn_summary,
       aes(x = market, y = churn_rate_tenants, fill = internal_class)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Average Quarterly Tenant Churn Rate by Market & Class",
    x     = "Market",
    y     = "Avg Churn Rate (Tenants per Quarter)",
    fill  = "Class"
  ) +
  theme_minimal()

ggplot(churn_summary,
       aes(x = market, y = churn_rate_sqft, fill = internal_class)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Average Quarterly Tenant Churn Rate by Market & Class (SF-weighted)",
    x     = "Market",
    y     = "Avg Churn Rate (SF per Quarter)",
    fill  = "Class"
  ) +
  theme_minimal()

# 9. Chicago sunburst (full-period churn) --------------------------------------
# ——————————————————————————————————————————————————————————————————————
# Sunburst of Average Quarterly Churn Rate by Submarket & Class (Chicago)
# ——————————————————————————————————————————————————————————————————————

# 1. Filter to Chicago, grab the avg churn rate per submarket × class
chicago_avg <- avg_churn_equal %>%
  filter(market == "Chicago") %>%
  select(internal_submarket, internal_class, avg_churn_rate)

library(dplyr)
library(plotly)

# avg_churn_equal must have: market, internal_submarket, internal_class, avg_churn_rate

# 1. Market level (root’s children)
level1 <- avg_churn_equal %>%
  group_by(market) %>%
  summarise(value = mean(avg_churn_rate), .groups = "drop") %>%
  mutate(
    ids     = market,
    parents = ""         # top level
  )

# 2. Submarket level
level2 <- avg_churn_equal %>%
  group_by(market, internal_submarket) %>%
  summarise(value = mean(avg_churn_rate), .groups = "drop") %>%
  mutate(
    ids     = paste(market, internal_submarket, sep = " – "),
    parents = market
  )

# 3. Class level
level3 <- avg_churn_equal %>%
  mutate(
    value   = avg_churn_rate,
    ids     = paste(market, internal_submarket, internal_class, sep = " – "),
    parents = paste(market, internal_submarket,      sep = " – ")
  ) %>%
  select(ids, parents, value)

# 4. Combine
sunburst_df <- bind_rows(level1, level2, level3)

# 5. Plot with each slice labeled by name + rate, 
#    and branchvalues="remainder" so values needn't sum to parents
print(plot_ly(
  sunburst_df,
  ids     = ~ids,
  labels  = ~ids,
  parents = ~parents,
  values  = ~value,
  type    = "sunburst",
  branchvalues = "remainder",   # each slice sized by its own value
  textinfo     = "text",        # use only our text
  texttemplate = "%{label}<br>%{value:.2%}",  # name + churn rate
  insidetextorientation = "horizontal",
  textfont       = list(size = 16, color = "black"),
  insidetextfont = list(size = 16, color = "black"),
  outsidetextfont= list(size = 16, color = "black"),
  insidetextfont        = list(size = 10)      # ← increase font size here
) %>%
  layout(
    uniformtext = list(minsize = 8, mode = "show"),
    title = "Avg Quarterly Tenant Churn Rate by Market → Submarket → Class",
    margin = list(l=0, r=0, b=0, t=50)
  )


)


library(stringr)

# 1. Tighter wrap at ~12 chars
sunburst_df <- sunburst_df %>%
  mutate(
    ids_wrapped = str_replace_all(
      str_wrap(ids, width = 12),
      "\n", "<br>"
    )
  )

# 2. Plot with padding + hide‐overlap
print(plot_ly(
  sunburst_df,
  ids                    = ~ids,
  labels                 = ~ids_wrapped,
  parents                = ~parents,
  values                 = ~value,
  type                   = "sunburst",
  branchvalues           = "remainder",
  textinfo               = "text",
  texttemplate           = "%{label}<br>%{value:.2%}",
  insidetextorientation  = "horizontal",
  
  # ↑ keep your font settings here ↑
  textfont       = list(size = 12, color = "black"),
  insidetextfont = list(size = 12, color = "black"),
  outsidetextfont= list(size = 12, color = "black"),
  
  # add padding between slices (bigger number → more gap)
  tiling         = list(pad = 4),
  
  # draw a thin white border around each slice
  marker         = list(line = list(color = "white", width = 2))
) %>%
  layout(
    uniformtext = list(minsize = 10, mode = "show"),
    title       = "Avg Quarterly Tenant Churn Rate by Market → Submarket → Class",
    margin      = list(l = 0, r = 0, b = 0, t = 30)
  )

)





















library(plotly)
library(dplyr)
library(RColorBrewer)

# 1. build a palette keyed by submarket
submkts <- unique(sunburst_df$internal_submarket)
pal      <- brewer.pal(length(submkts), "Set3")
names(pal) <- submkts

# 2. add a color column to your data.frame
sunburst_df <- sunburst_df %>%
  mutate(color = pal[internal_submarket])

# 3. plot the sunburst, coloring by submarket
p <- plot_ly(
  sunburst_df,
  ids                   = ~ids,
  labels                = ~ids_wrapped,
  parents               = ~parents,
  values                = ~value,
  type                  = "sunburst",
  branchvalues          = "remainder",
  marker                = list(
    colors = ~color,
    line   = list(color = "white", width = 2)
  ),
  textinfo              = "text",
  texttemplate          = "%{label}<br>%{value:.2%}",
  insidetextorientation = "horizontal",
  showlegend            = FALSE     # we'll add our own below
) %>%
  layout(
    title  = "Avg Quarterly Tenant Churn Rate by Submarket",
    margin = list(l=0, r=200, b=0, t=30)  
    # note: we leave room on the right (r=200) for our legend
  )

# 4. tack on one invisible scatter‐trace per submarket to build the legend
for(sub in submkts) {
  p <- p %>% add_trace(
    x        = c(NA),
    y        = c(NA),
    type     = "scatter",
    mode     = "markers",
    marker   = list(size = 10, color = pal[sub]),
    showlegend = TRUE,
    name        = sub
  )
}

# … build p with plot_ly() and add_trace() for the legend …

p %>%
  layout(
      title = list(
        text   = "Avg Quarterly Tenant Churn Rate by Submarket",
        x      = 0.4,           # 0 = left, 0.5 = center, 1 = right
        xanchor= "center"       # align title at its center
      ),
    xaxis = list(visible = FALSE, showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    yaxis = list(visible = FALSE, showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    plot_bgcolor  = "rgba(0,0,0,0)",
    paper_bgcolor = "rgba(0,0,0,0)",
    margin = list(l = 0, r = 200, b = 0, t = 30)
  )















library(plotly)
library(dplyr)
library(viridis)   # for magma()

# 1. build a magma palette keyed by market + submarket
#    include the root‐level market so it gets its own magma color
all_levels <- c(unique(sunburst_df$market), unique(sunburst_df$internal_submarket))
pal_all    <- magma(length(all_levels))
names(pal_all) <- all_levels

# 2. assign a color to *every* row: market‐level or submarket‐level
sunburst_df <- sunburst_df %>%
  mutate(
    colour = if_else(
      parents == "",        # root slice (market)
      pal_all[market],
      pal_all[internal_submarket]
    )
  )

# 3. plot the sunburst, coloring by our new 'colour' column
p <- plot_ly(
  sunburst_df,
  ids                   = ~ids,
  labels                = ~ids_wrapped,
  parents               = ~parents,
  values                = ~value,
  type                  = "sunburst",
  branchvalues          = "remainder",
  marker                = list(
    colors = ~colour,
    line   = list(color = "white", width = 2)
  ),
  textinfo              = "text",
  texttemplate          = "%{label}<br>%{value:.2%}",
  insidetextorientation = "horizontal",
  showlegend            = FALSE
) %>%
  layout(
    title = list(
      text    = "Avg Quarterly Tenant Churn Rate by Submarket",
      x       = 0.5,
      xanchor = "center"
    ),
    xaxis = list(visible = FALSE),
    yaxis = list(visible = FALSE),
    plot_bgcolor  = "rgba(0,0,0,0)",
    paper_bgcolor = "rgba(0,0,0,0)",
    margin = list(l = 0, r = 200, b = 0, t = 50)
  )

# 4. rebuild legend for submarkets only
for(sub in unique(sunburst_df$internal_submarket)) {
  p <- p %>% add_trace(
    x         = NA, y = NA,
    type      = "scatter", mode = "markers",
    marker    = list(size = 10, color = pal_all[sub]),
    showlegend= TRUE, name = sub
  )
}

p







library(plotly)
library(dplyr)
library(viridis)   # for magma()

# 1. build a magma palette keyed by market + submarket
all_levels <- c(unique(sunburst_df$market), unique(sunburst_df$internal_submarket))
pal_all    <- magma(length(all_levels))
names(pal_all) <- all_levels

# 2. assign a color to *every* row: market‐level or submarket‐level
sunburst_df <- sunburst_df %>%
  mutate(
    colour = if_else(
      parents == "",        # root slice (market)
      pal_all[market],
      pal_all[internal_submarket]
    )
  )

# 3. plot the sunburst with white text and black background
p <- plot_ly(
  sunburst_df,
  ids                    = ~ids,
  labels                 = ~ids_wrapped,
  parents                = ~parents,
  values                 = ~value,
  type                   = "sunburst",
  branchvalues           = "remainder",
  marker                 = list(
    colors = ~colour,
    line   = list(color = "white", width = 2)
  ),
  textinfo               = "text",
  texttemplate           = "%{label}<br>%{value:.2%}",
  insidetextorientation  = "horizontal",
  textfont               = list(color = "white"),      # all label text white
  insidetextfont         = list(color = "white"),
  outsidetextfont        = list(color = "white"),
  showlegend             = FALSE
) %>%
  layout(
    # center title
    title = list(
      text    = "Avg Quarterly Tenant Churn Rate by Submarket",
      x       = 0.45,
      xanchor = "center"
    ),
    # hide axes
    xaxis = list(visible = FALSE),
    yaxis = list(visible = FALSE),
    # black background
    plot_bgcolor  = "black",
    paper_bgcolor = "black",
    # white legend text
    font = list(color = "white"),
    # room for legend
    margin = list(l = 0, r = 200, b = 0, t = 50)
  )

# 4. rebuild legend traces (submarkets only)
for(sub in unique(sunburst_df$internal_submarket)) {
  p <- p %>% add_trace(
    x         = NA, y = NA,
    type      = "scatter", mode = "markers",
    marker    = list(size = 10, color = pal_all[sub]),
    showlegend= TRUE, name = sub
  )
}

p


