# Load tools
library(tidyverse)
library(tidycensus)

# Get real data

pa_income <- get_acs(
  geography = "county",
  variables = "B19013_001",
  state = "PA",
  year = 2023,
  survey = "acs5"
)


pa_income$GEOID
as.numeric("01001") # Leading zero is dropped

# Practicing filter() - picking rows

filter(pa_income, estimate > 60000)

# Predict that there will be fewer than 67 rows - there are 46

# Counties where the margin of error is bigger than 3000
filter(pa_income, moe > 3000)

# Counties where the estimate is under 50000
filter(pa_income, estimate < 50000)

# Practicing select() - picking columns
select(pa_income, NAME, estimate, moe)

# Show only GEOID and estimate
select(pa_income, GEOID, estimate)

# Practing mutate() - makes a new column
mutate(pa_income, moe_pct = moe / estimate * 100)


pa_income<- mutate(pa_income, moe_pct = moe / estimate *100)

# moe_pct tells us the margin of error percentage, telling us how much a poll result may vary from the entire population

# Practicing arrange() - sorts
arrange(pa_income, moe_pct)
arrange(pa_income, desc(moe_pct))

# County with highest moe_pct = Cameron County, PA

step1 <- filter(pa_income, moe_pct > 5)
step2 <- arrange(step1, desc(moe_pct))
step3 <- select(step2, NAME, estimate, moe, moe_pct)
step3

pa_income %>%
  filter(moe_pct > 5) %>%
  arrange(desc(moe_pct)) %>%
  select(NAME, estimate, moe, moe_pct)

# Keep counties with moe_pct over 8, sort by estimate, show NAME and moe_pct

worst <- pa_income %>%
  filter(moe_pct > 8) %>%
  arrange(desc(moe_pct)) %>%
  select(NAME, moe_pct)

# Practice group_by() + summarize () - many rows become few

pa_income <- mutate(pa_income, reliable = moe_pct < 5)

pa_income %>%
  group_by(reliable) %>%
  summarize(n = n(),
            avg_income = mean(estimate))

pa_income <- pa_income %>%
  mutate(reliability = case_when (
          moe_pct < 3 ~ "High confidence",
          moe_pct < 6 ~ "Moderate",
          TRUE        ~ "Low confidence"
  ))

count(pa_income, reliability)

# Count results:
# high confidence = 26
# low confidence = 7
# moderate = 34

#Is the margin of error bigger in small counties?

pa_two <- get_acs(
  geography = "county",
  variables = c("B19013_001", "B01003_001"),
  state = "PA", year = 2023, survey = "acs5"
)

pa_two

pa_wide <- get_acs(
  geography = "county",
  variables = c(income = "B19013_001",
                pop    = "B01003_001"),
                state = "PA", year = 2023, survey = "acs5",
                output = "wide"

)

pa_wide

pa_wide %>%
  mutate(moe_pct = incomeM / incomeE * 100) %>%
  arrange(desc(moe_pct)) %>%
  select(NAME, popE, incomeE, moe_pct) %>%
  head (10)

#end
