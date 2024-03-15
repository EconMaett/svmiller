# 01 - How to Adjust for Economic Indicators for Inflation (and Index Them) ----
# Blogpost: http://svmiller.com/blog/2023/01/index-economic-data-adjust-inflation/
# RMarkdown: https://github.com/svmiller/svmiller.github.io/blob/master/_rmd/2023-01-19-index-economic-data-adjust-inflation.Rmd

## The Data ----
# Access the `commodity_prices` data set from the `stevedata` R package.
# - `{stevedata}`: http://svmiller.com/stevedata/
# - `commodity_prices`: http://svmiller.com/stevedata/reference/commodity_prices.html
library(tidyverse)
library(fredr)
library(stevedata)
library(stevethemes)
fig_path <- "figures/01_inflation-adjustment/"

head(commodity_prices)
# Frequency: Monthly
# Range: Jan 1960 to Dec 2022
# Source: World Bank "pink sheet"


## First Things First: Getting Inflation Adjustment Data ----

# The variables in the set `commodity_prices` are all demarcated nominal US dollars,
# i.e., recorded at that particular moment in time.

# There are instances when it makes sense to look at nominal values,
# prominently in the debt/GDP comparison.

# Over-time comparisons should use real, i.e. inflation-adjusted values.

# The Bureau of Labor Statistics publishes price indexes for different baskets of goods and services.

# The most commonly cited index is the consumer price index for all urban consumers,
# which covers about 93% of the US population.
# FRED: https://fred.stlouisfed.org/series/CPIAUCSL

# Use the `{fredr}` R package to access the FRED API.
CPI <- fredr(series_id = "CPIAUCSL", observation_start = as.Date("1960-01-01")) |> 
  as_tibble() |> 
  rename(cpiu = value) |> 
  filter(year(date) <= 2022)

CPI

# Get recession dates
Recessions <- fredr(series_id = "USREC", observation_start = as.Date("1960-01-01")) |> 
  select(date, value) |> 
  mutate(episode = case_when(
    lag(value, 1)  == 0 & value == 1 ~ "Peak",
    lead(value, 1) == 0 & value == 1 ~ "Trough",
    lag(value, 1)  == 1 & value == 1 ~ "Ongoing"
    )
  ) |> 
  na.omit() |> 
  tibble() |> 
  filter(episode %in% c("Peak", "Trough")) |> 
  select(-value) |> 
  pivot_wider(names_from = episode, values_from = date, values_fn = list) |> 
  unnest(c(Peak, Trough))

Recessions

# Note that the `{stevedata}` R package includes a `recessions` data frame
recessions

# Plot the CPI
CPI |> 
  ggplot(mapping = aes(date, cpiu)) + 
  geom_rect(
    data = Recessions, inherit.aes = FALSE, 
    mapping = aes(xmin = Peak, xmax = Trough, ymin = 0, ymax = +Inf),  alpha=0.6) +
  geom_line(linewidth = 1.2) +
  theme_steve(style = "web") +
  scale_x_date(
    breaks = function(x) { seq.Date(from = as.Date("1960-01-01"), to = as.Date("2025-01-01"), by = "5 years") },
    date_labels = "%Y") +
  geom_ribbon(mapping = aes(ymin = 0, ymax = cpiu), alpha = 0.3, fill = "#619cff") +
  labs(
    y = "Consumer Price Index for All Urban Consumers: All Items in U.S. City Average",
    x = "",
    caption = "Data: Bureau of Labor Statistics, by way of St. Louis Fed and {fredr}. Indexed to 1982-84. Recessions shaded for context.",
    title = "The (Monthly) Consumer Price Index for All Urban Consumers, U.S. City Average (1960-2022)",
    subtitle = "The time series is long, but you can tease out the major inflation episodes of the mid-1970s and after the pandemic."
    )

ggsave("01_CPIAUCSL.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()


## Choosing a benchmark for inflation adjustment ----

# The consumer price index data seems to be benchmarked against an
# amalgam of all months from 1982 to 1984.

# That decision was made sometime in 1988 based on experimental weights 
# the BLS had deployed across 1982 to 1984.

# The formula for benchmarking is always
# `\frac{x_{t}}{x_{b}} \times 100`

# If we like thinking in terms of current dollars,
# we can take Dec 2022 as our benchmark.
# Because we restricted our sample Dec 2022, this is the last value.

# Alternatively, we can take the average of 1990 as the benchmark.

# Creating a benchmark against which to adjust for inflation
# means dividing the CPI over the benchmark and multiplying by 100.

mean1990 <- CPI |> 
  filter(year(date) == 1990) |> 
  summarise(mean = mean(cpiu)) |> 
  pull(mean)

CPI <- CPI |> 
  select(date, cpiu) |> 
  mutate(
    last = last(cpiu),
    first = first(cpiu),
    mean1990 = mean1990,
    bench_first = (cpiu / first) * 100,
    bench_last = (cpiu / last) * 100,
    bench_1990 = (cpiu / mean1990) * 100
  )

CPI
# We now have three benchmarks.

# Use `left_join()` to join by the exact year-month dates.
commodity_prices <- commodity_prices |> 
  left_join(
    y = CPI |> 
      select(date, bench_first:bench_1990),
    by = join_by(date)
  )

commodity_prices
# Now we can start adjusting for inflation.


## The price of coffee, nominal and real ----

# Coffee prices are an interesting case of worsening terms of trade for developing economies.

# In short, the world is drinking more and more coffee, but consumers in advanced economies 
# end up paying cheaper prices in light of increased demand.

# Coffee markets are competitive and the machinery required to mass produce coffee
# cluster on oligopolies, which raises prices for developing countries that export 
# coffee to the same markets from which they buy expensive machinery to harvest coffee.

# Have a look at the nominal price of arabica and robustas coffee expressed in USD/kg averages
# as reported by the International Coffee Organization.

# Arabica is considered the superior good but it is more sensitive to pests and weather issues.

# Robustas is more robuts to pests and weather issues, hence the name.

# We therefore expect the prices for Arabica and Robustas to move in tandem,
# but the price for Arabica generally to be both higher and more sensitive 
# to distruptions due to pests and weather effects.
commodity_prices |> 
  select(date, coffee_arabica, coffee_robustas) |> 
  pivot_longer(cols = -date, names_to = "var", values_to = "val")

# Plot the prices for Arabica and Robusta
commodity_prices |> 
  select(date, coffee_arabica, coffee_robustas) |> 
  pivot_longer(cols = -date, names_to = "var", values_to = "val") |> 
  mutate(var = if_else(condition = var == "coffee_arabica", true = "Arabica", false = "Robustas")) |> 
  ggplot(mapping = aes(x = date, y = val, linetype = var)) +
  geom_line(lwd = 1.2, color = "#5d4037") +
  theme_steve(style = "web") +
  scale_x_date(
    breaks = function(x) { seq.Date(from = as.Date("1960-01-01"), to = as.Date("2025-01-01"), by = "5 years") },
    date_labels = "%Y") +
  labs(
    x = "", y = "Price of coffee, nominal USD/kg",
    linetype = "",
    title = "The (Nominal!) Price of Arabica and Robustas Coffee, 1960-2022",
    subtitle = "The nominal price for both has risen substantially over time. Arabica, in particular, is now over 400% more expensive in nominal terms.",
    caption = "Data: ?commodity_prices in {stevedata}, by way of World Bank 'Pink Sheet' data and the International Coffee Organization."
    )

ggsave("02_coffee-nominal.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()

# The data shows large fluctuations.

# The visible spike in the mid-1970s is a function of "the big frost" in Brazil
# that wiped out as much as half of all coffee harvests in the country.

# In nominal terms, coffee seems much more expensive today than it was in Jan 1960.

# The price of Robustas has risen about 194% and the price of Arabica over 400%.


# Let's see how the picture changes if we adjust the prices to 
# Dec 2022 dollars
coffee_data <- commodity_prices |> 
  select(date, coffee_arabica, coffee_robustas, bench_last) |> 
  mutate(
    real_arabica = (coffee_arabica / bench_last) * 100,
    real_robusta = (coffee_robustas / bench_last) * 100
  )

coffee_data

# Plot the prices for Arabica and Robustas coffee in real Dec 2022 USD
coffee_data |> 
  select(-bench_last) |> 
  pivot_longer(cols = -date, names_to = "var", values_to = "val") |> 
  mutate(
    type = case_when(
      var %in% c("coffee_arabica", "real_arabica") ~ "Arabica",
      TRUE ~ "Robustas"
      ),
    cat = case_when(
      var %in% c("coffee_arabica", "coffee_robustas") ~ "Nominal $/kg",
      TRUE ~ "Dec. 2022 $/kg"
      )
    ) |> 
  ggplot(mapping = aes(x = date, y = val, linetype = type)) +
  geom_line(lwd = 1.2, color = "#5d4037") +
  theme_steve(style = "web") +
  scale_x_date(
    breaks = function(x) { seq.Date(from = as.Date("1960-01-01"),  to = as.Date("2025-01-01"), by = "5 years") },
    date_labels = "%Y"
    ) +
  facet_wrap(facets = vars(cat), nrow = 2, scales = "free_y") +
  labs(
    x = "", y = "Price of coffee ($/kg)", linetype ="",
    title = "The Price of Arabica and Robustas Coffee, 1960-2022, in Nominal and Dec. 2022 Dollars",
    subtitle = "Coffee has actually become a lot cheaper over time. Don't be misled by the fact general inflation can mask the true over-time value of commodities.",
    caption = "Data: ?commodity_prices in {stevedata}, by way of World Bank 'Pink Sheet' data and the International Coffee Organization."
    )

ggsave("03_coffee-real.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()

# You see that the y-axis has changed as we adjusted for the changing value of the dollar,
# but now the plot tells a fundamentally different story about the price of coffee over time.

# In fact, coffee has become a lot cheaper over time!

# In Dec 2022 dollars, a kilogram of Arabica coffee cost about $9.55 in Jan 1960.
# In Dec 2022 it cost about $4.63 to get a kilogram of coffee.

# The price has basically been cut in half over the past 60+ years even as the world is
# consuming more and more coffee.


## Converting a real price to an index ----

# Depending on the audience and our narrative, it can make sense to convert an
# inflation-adjusted price to an index as it pertains to some benchmark.

# We take the price of Brent crude oil.

# Crude oil is a commodity that is traded outright in dollars per barrel.

# There are multiple crude oil "benchmarks", such as Brent, the highest-grade 
# North Sea crude oil, or West Texas Intermediate (WTI), the standard for US oil.

# Brent is in about two-thirds of all oil contracts, making it the most
# popular marker of oil.

# Dubai crude oil is also in the `commodity_prices` data set.

# The sensitivity of the price of oil to truly anything makes the price of one
# benchmark highly cointegrate with another.

# For example, Robustas and Arabica coffee prices correlate at around 0.79.
cor(coffee_data$coffee_arabica, coffee_data$coffee_robustas)

# Brent and Dubai oil prices correlate at 0.999.

# Plot the nominal Brent crude oil price in USD per barrel
# and spot events such as:
# - the Arab oil embargo following the Yom Kippur War in 1973,
# - the market tightness coinciding with the Iranian revolution in 1979 and 
# - the onset of the Iran-Iraq war in 1980-1988.
# - The "energy crisis" that coincided with the Great Recession Dec 2007 - Jun 2009
# - and more recently, the COVID-19 pandemic.
commodity_prices |> 
  ggplot(mapping = aes(x = date, y = oil_brent)) + 
  geom_line(lwd = 1.2) +
  theme_steve(style = "web") +
  scale_x_date(
    breaks = function(x) { seq.Date(from = as.Date("1960-01-01"), to = as.Date("2025-01-01"), by = "5 years") },
    date_labels = "%Y"
    ) +
  geom_ribbon(mapping = aes(ymin = 0, ymax = oil_brent), alpha = 0.3, fill = "#619cff") +
  labs(
    title = "The (Nominal!) Price of Brent Crude Oil, 1960-2022",
    subtitle = "No one would object to saying the price of oil has greatly increased over time, but this is a bad way of communicating it.",
    x = "", y = "Price (Nominal $/bbl)",
    caption = "?commodity_prices in {stevedata}, by way of World Bank's 'pink sheet', Bloomberg, Energy Intelligence Group (EIG), Organization of Petroleum Exporting Countries (OPEC), and the World Bank."
    )

ggsave("04_brent-nominal.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()

# In Dec 2022, $100/bbl is the kind of benchmark for Americans losing their minds
# about the price of crude.

# The series opens at about $1.63/bbl in Jan 1960, but we should not 
# interpret the nominal series as a 6,000 percent increase from the Jan 1960 price.

# Adjust the Brent crude oil price per barrel for inflation
commodity_prices |> 
  mutate(real_brent = (oil_brent / bench_last) * 100)

# Plot the Brent crude oil price in real USD per barrel
commodity_prices |> 
  mutate(real_brent = (oil_brent / bench_last) * 100) %>%
  ggplot(mapping = aes(x = date, y = real_brent)) +
  geom_line(lwd = 1.2) +
  theme_steve(style = "web") +
  scale_x_date(
    breaks = function(x) { seq.Date(from = as.Date("1960-01-01"), to = as.Date("2025-01-01"), by = "5 years") },
    date_labels = "%Y") +
  geom_ribbon(mapping = aes(ymin = 0, ymax = real_brent), alpha = 0.3, fill = "#619cff") +
  labs(
    x = "", y = "Price of Brent Crude ($/bbl)",
    linetype = "", color="",
    title = "The Price of Brent Crude Oil in Dec. 2022 USD/bbl, 1960-2022",
    subtitle = "Adjusting for inflation not only more honestly communicates price trends, but it better accentuates major events in the time series.",
    caption = "?commodity_prices in {stevedata}, by way of World Bank's 'pink sheet', Bloomberg, Energy Intelligence Group (EIG), Organization of Petroleum Exporting Countries (OPEC), and the World Bank."
    )

ggsave("04_brent-real.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()

# We see that in nominal terms, the price of oil in Dec 2022 is roughly
# twice the price it was in Dec 1970.

# Adjusted for inflation, the price of oil during the 2007/2008 energy crisis
# is about what it was during the worst stretches of the late 1970s.

# The adjusted crude oil price series even lets us identify some events
# that were concealed by the scale of the nominal time series, such as:
# The Iraqi invasion of Kuwait in Aug 1990 that increased the price of oil over 50% from July 1990.
# By October 1990, the price basically doubled from what it was in July.

# This got lost in the scale of the nominal series, given the inflation of the dollar.

# Real USD prices are hard to understand for the lay audience,
# and we may help them by indexing our variables so that some point
# in time is a reference point.

# Make Jan 1960 the reference period.
commodity_prices |> 
  mutate(
    real_brent = (oil_brent / bench_last) * 100,
    index_brent = (real_brent / first(real_brent)) * 100
  )

# Plot the index of real crude oil prices
commodity_prices |> 
  mutate(
    real_brent = (oil_brent / bench_last) * 100,
    index_brent = (real_brent / first(real_brent)) * 100) |> 
  ggplot(mapping = aes(x = date, y = index_brent)) +
  geom_line(size = 1.1) +
  theme_steve(style = "web") +
  scale_x_date(
    breaks = function(x) { seq.Date(from = as.Date("1960-01-01"), to = as.Date("2025-01-01"), by = "5 years") },
    date_labels = "%Y"
    ) +
  geom_ribbon(mapping = aes(ymin = 0, ymax = index_brent), alpha = 0.3, fill = "#619cff") +
  labs(
    x = "", y = "Real Brent Crude Price Index (Jan. 1960 = 100)",
    linetype = "", color="",
    title = "The Price of Brent Crude Oil in Dec. 2022 USD/bbl, 1960-2022, Indexed to Jan. 1960",
    subtitle = "Relative to Jan. 1960, the price of Brent crude in real USD has been high as 9 times more expensive than it was at the start of the series. Now it's about 4 times more expensive.",
    caption = "?commodity_prices in {stevedata}, by way of World Bank's 'pink sheet', Bloomberg, Energy Intelligence Group (EIG), Organization of Petroleum Exporting Countries (OPEC), and the World Bank."
    )

ggsave("05_brent-real-index.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()

# The time series presented here were especially chosen to visualize the importance
# of adjusting nominal prices for the effects of inflation.

# END