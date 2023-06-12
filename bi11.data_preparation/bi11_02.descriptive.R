# Number of transactions

library(dplyr)

num_tr_seoul <- raw %>% 
  group_by(q) %>% 
  summarise(num_tr = n()) %>%
  ungroup()

num_tr_region <- raw %>% 
  group_by(q, region) %>% 
  summarise(num_tr = n()) %>%
  ungroup()

library(ggplot2)

ggplot(num_tr_seoul, aes(x = q, y = num_tr)) +
  geom_col() +
  labs(title = "Number of Transactions in Seoul",
       x = "quarter",
       y = "transactions")

ggplot(num_tr_region, aes(x = q, y = num_tr, col = region)) +
  geom_line(size = 0.7) +
  labs(title = "Number of Transactions by Region",
       x = "quarter",
       y = "transactions")

# Zone and use

raw %>% 
  group_by(no_zone) %>% 
  summarise(zone = zone[1], n = n()) %>% 
  ungroup()

raw %>% 
  group_by(no_use) %>% 
  summarise(use = use[1], n = n()) %>% 
  ungroup()

with(raw, table(zone, use))

ggplot(raw, aes(x = reorder(zone, no_zone))) +
  geom_bar() +
  labs(title = "Number of Transactions by Zone",
       x = "zone",
       y = "transactions")

ggplot(raw, aes(x = reorder(use, no_use))) +
  geom_bar() +
  labs(title = "Number of Transactions by Use",
       x = "Use",
       y = "transactions")

# Road

with(raw, table(region, road))
ggplot(raw, aes(x = reorder(region, no_region), fill = road)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Road Conditions by Region",
       x = "Region",
       y = "proportion")

with(raw, table(zone, road))
ggplot(raw, aes(x = reorder(zone, no_zone), fill = road)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Road Conditions by Zone",
       x = "zone",
       y = "proportion")

with(raw, table(use, road))
ggplot(raw, aes(x = reorder(use, no_use), fill = road)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Road Conditions by Use",
       x = "Use",
       y = "proportion")

# Area and far

summary(raw$area_b)

raw %>% 
  group_by(region) %>% 
  summarise(Min = min(area_b),
            Median = median(area_b),
            Mean = mean(area_b),
            Max = max(area_b)) %>% 
  ungroup()

raw %>% 
  group_by(zone) %>% 
  summarise(Min = min(area_b),
            Median = median(area_b),
            Mean = mean(area_b),
            Max = max(area_b)) %>% 
  ungroup()

raw %>% 
  group_by(use) %>% 
  summarise(Min = min(area_b),
            Median = median(area_b),
            Mean = mean(area_b),
            Max = max(area_b)) %>% 
  ungroup()

ggplot(raw, aes(x = area_b)) +
  geom_histogram(bins=100) +
  labs(title = "Number of Transactions by Area",
       x = "area of buildings",
       y = "transactions")

ggplot(raw, aes(x = reorder(region, no_region))) +
  geom_boxplot(aes(y = area_b)) +
  labs(title = "Area of Buildings by Region",
       x = "Region",
       y = "Area (m2)")

ggplot(raw, aes(x = reorder(zone, no_zone))) +
  geom_boxplot(aes(y = area_b)) +
  labs(title = "Area of Buildings by Zone",
       x = "Zone",
       y = "Area (m2)")

ggplot(raw, aes(x = reorder(use, no_use))) +
  geom_boxplot(aes(y = area_b)) +
  labs(title = "Area of Building by Use",
       x = "Use",
       y = "Area (m2)")


summary(raw$area_l)

raw %>% 
  group_by(region) %>% 
  summarise(Min = min(area_l),
            Median = median(area_l),
            Mean = mean(area_l),
            Max = max(area_l)) %>% 
  ungroup()

raw %>% 
  group_by(zone) %>% 
  summarise(Min = min(area_l),
            Median = median(area_l),
            Mean = mean(area_l),
            Max = max(area_l)) %>% 
  ungroup()

raw %>% 
  group_by(use) %>% 
  summarise(Min = min(area_l),
            Median = median(area_l),
            Mean = mean(area_l),
            Max = max(area_l)) %>% 
  ungroup()

ggplot(raw, aes(x = area_l)) +
  geom_histogram(bins=100) +
  labs(title = "Number of Transactions by Area",
       x = "area of land",
       y = "transactions")

ggplot(raw, aes(x = reorder(region, no_region))) +
  geom_boxplot(aes(y = area_l)) +
  labs(title = "Area of Land by Region",
       x = "Region",
       y = "Area (m2)")

ggplot(raw, aes(x = reorder(zone, no_zone))) +
  geom_boxplot(aes(y = area_l)) +
  labs(title = "Area of Land by Zone",
       x = "Zone",
       y = "Area (m2)")

ggplot(raw, aes(x = reorder(use, no_use))) +
  geom_boxplot(aes(y = area_l)) +
  labs(title = "Area of Land by Use",
       x = "Use",
       y = "Area (m2)")


summary(raw$far)

raw %>% 
  group_by(region) %>% 
  summarise(Min = min(far),
            Median = median(far),
            Mean = mean(far),
            Max = max(far)) %>% 
  ungroup()

raw %>% 
  group_by(zone) %>% 
  summarise(Min = min(far),
            Median = median(far),
            Mean = mean(far),
            Max = max(far)) %>% 
  ungroup()

raw %>% 
  group_by(use) %>% 
  summarise(Min = min(far),
            Median = median(far),
            Mean = mean(far),
            Max = max(far)) %>% 
  ungroup()

ggplot(raw, aes(x = far)) +
  geom_histogram(bins=100) +
  labs(title = "Number of Transactions by FAR",
       x = "far",
       y = "transactions")

ggplot(raw, aes(x = reorder(region, no_region))) +
  geom_boxplot(aes(y = far)) +
  labs(title = "Area of Land by Region",
       x = "Region",
       y = "Area (m2)")

ggplot(raw, aes(x = reorder(zone, no_zone))) +
  geom_boxplot(aes(y = far)) +
  labs(title = "Area of Land by Zone",
       x = "Zone",
       y = "Area (m2)")

ggplot(raw, aes(x = reorder(use, no_use))) +
  geom_boxplot(aes(y = far)) +
  labs(title = "Area of Land by Use",
       x = "Use",
       y = "Area (m2)")

# Price

summary(raw$price_t)
summary(raw$price_b)
summary(raw$price_bl)
summary(raw$price_l)
summary(raw$price_ll)

raw %>% 
  group_by(region) %>% 
  summarise(Min = min(price_b),
            Median = median(price_b),
            Mean = mean(price_b),
            Max = max(price_b)) %>% 
  ungroup()

raw %>% 
  group_by(zone) %>% 
  summarise(Min = min(price_b),
            Median = median(price_b),
            Mean = mean(price_b),
            Max = max(price_b)) %>% 
  ungroup()

raw %>% 
  group_by(use) %>% 
  summarise(Min = min(price_b),
            Median = median(price_b),
            Mean = mean(price_b),
            Max = max(price_b)) %>% 
  ungroup()

ggplot(raw, aes(x = price_b)) +
  geom_histogram(bins=100) +
  labs(title = "Number of Transactions by Price",
       x = "unit price of buildings",
       y = "transactions")

ggplot(raw, aes(x = reorder(region, no_region))) +
  geom_boxplot(aes(y = price_b)) +
  labs(title = "Unit Price of Buildings by Region",
       x = "Region",
       y = "KRW / m2")

ggplot(raw, aes(x = reorder(zone, no_zone))) +
  geom_boxplot(aes(y = price_b)) +
  labs(title = "Unit Price of Buildings by Zone",
       x = "Zone",
       y = "KRW / m2")

ggplot(raw, aes(x = reorder(use, no_use))) +
  geom_boxplot(aes(y = price_b)) +
  labs(title = "Unit Price of Building by Use",
       x = "Use",
       y = "KRW / m2")


summary(raw$price_l)

raw %>% 
  group_by(region) %>% 
  summarise(Min = min(price_l),
            Median = median(price_l),
            Mean = mean(price_l),
            Max = max(price_l)) %>% 
  ungroup()

raw %>% 
  group_by(zone) %>% 
  summarise(Min = min(price_l),
            Median = median(price_l),
            Mean = mean(price_l),
            Max = max(price_l)) %>% 
  ungroup()

raw %>% 
  group_by(use) %>% 
  summarise(Min = min(price_l),
            Median = median(price_l),
            Mean = mean(price_l),
            Max = max(price_l)) %>% 
  ungroup()

ggplot(raw, aes(x = price_l)) +
  geom_histogram(bins=100) +
  labs(title = "Number of Transactions by Price",
       x = "unit price of land",
       y = "transactions")

ggplot(raw, aes(x = reorder(region, no_region))) +
  geom_boxplot(aes(y = price_l)) +
  labs(title = "Unit Price of Land by Region",
       x = "Region",
       y = "KRW / m2")

ggplot(raw, aes(x = reorder(zone, no_zone))) +
  geom_boxplot(aes(y = price_l)) +
  labs(title = "Unit Price of Land by Zone",
       x = "Zone",
       y = "KRW / m2")

ggplot(raw, aes(x = reorder(use, no_use))) +
  geom_boxplot(aes(y = price_l)) +
  labs(title = "Unit Price of Land by Use",
       x = "Use",
       y = "KRW / m2")
