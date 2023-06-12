# Target by the area of buildings

area_b1 <- 100 # 30 py
area_b2 <- 330 # 100 py
area_b3 <- 3300 # 1,000 py
area_b4 <- 9900 # 3,000 py
area_b5 <- 33000 # 10,000 py

raw$size <- cut(raw$area_b,
                breaks = c(0,
                           area_b1,
                           area_b2,
                           area_b3,
                           area_b4,
                           area_b5,
                           Inf),
                right = F,
                include.lowest = T,
                ordered_result = T,
                labels = c("under",
                           "tiny",
                           "little",
                           "small",
                           "medium",
                           "large"))

with(raw, table(size))

library(dplyr)

tar <- raw %>%
  filter(size == "little") # Set the target

summary(tar$area_b)

table(tar$region)
table(tar$zone)
table(tar$use)

library(ggplot2)

ggplot(tar, aes(x = area_b)) +
  geom_histogram(bins=100) +
  labs(title = "Number of Transactions by Area",
       x = "area of buildings",
       y = "transactions")

ggplot(tar, aes(x = reorder(region, no_region))) +
  geom_boxplot(aes(y = area_b)) +
  labs(title = "Area of Buildings by Region",
       x = "region",
       y = "area of buildings")

ggplot(tar, aes(x = reorder(zone, no_zone))) +
  geom_boxplot(aes(y = area_b)) +
  labs(title = "Area of Buildings by Zone",
       x = "zone",
       y = "area of buildings")

ggplot(tar, aes(x = reorder(use, no_use))) +
  geom_boxplot(aes(y = area_b)) +
  labs(title = "Area of Buildings by Use",
       x = "use",
       y = "area of buildings")

# Outlier by the area of land

min_area_l <- 100 # 30 py

tar <- tar %>%
  filter(area_l >= min_area_l)

summary(tar$area_l)

ggplot(tar, aes(x = area_l)) +
  geom_histogram(bins = 100) +
  labs(title = "Number of Transactions by Area",
       x = "area of land",
       y = "transactions")

# Outlier by far

summary(tar$far)

ggplot(tar, aes(x = far)) +
  geom_histogram(bins = 100) +
  labs(title = "Number of Transactions by FAR",
       x = "far",
       y = "transactions")

ggplot(tar, aes(x = reorder(zone, no_zone))) +
  geom_boxplot(aes(y = far)) +
  geom_point(aes(y = far_norm * 0.5), size = 2, col = "yellow") +
  geom_point(aes(y = far_norm * 1.0), size = 2, col = "green") +
  geom_point(aes(y = far_norm * 1.5), size = 2, col = "blue") +
  geom_point(aes(y = far_norm * 2.0), size = 2, col = "red") +
  labs(title = "FAR by Zone",
       x = "zone",
       y = "far")

sum(tar$far < 0.5)
sum(tar$far > 15)

min_far <- 0.5
max_far <- 15

tar <- tar %>%
  filter(far >= min_far & far < max_far)

summary(tar$far)

ggplot(tar, aes(x = far)) +
  geom_histogram(bins = 100) +
  labs(title = "Number of Transactions by FAR",
       x = "far",
       y = "transactions")

ggplot(tar, aes(x = reorder(zone, no_zone))) +
  geom_boxplot(aes(y = far)) +
  geom_point(aes(y = far_norm * 0.5), size = 2, col = "yellow") +
  geom_point(aes(y = far_norm * 1.0), size = 2, col = "green") +
  geom_point(aes(y = far_norm * 1.5), size = 2, col = "blue") +
  geom_point(aes(y = far_norm * 2.0), size = 2, col = "red") +
  labs(title = "FAR by Zone",
       x = "zone",
       y = "far")

# Outlier by price (log of unit price)

range_price <- 3 # X IQR

ggplot(tar, aes(x = price_t)) +
  geom_histogram(bins = 100) +
  labs(title = "Number of Transactions by Total Price",
       x = "total price",
       y = "transactions")

ggplot(tar, aes(x = price_b)) +
  geom_histogram(bins = 100) +
  labs(title = "Number of Transactions by Building Unit Price",
       x = "building unit price",
       y = "transactions")

ggplot(tar, aes(x = price_bl)) +
  geom_histogram(bins = 100) +
  labs(title = "Number of Transactions by the log of BUP",
       x = "log of building unit price",
       y = "transactions")

ggplot(tar, aes(x = price_l)) +
  geom_histogram(bins = 100) +
  labs(title = "Number of Transactions by Land Unit Price",
       x = "land unit price",
       y = "transactions")

ggplot(tar, aes(x = price_ll)) +
  geom_histogram(bins = 100) +
  labs(title = "Number of Transactions by the log of LUP",
       x = "log of land unit price",
       y = "transactions")


ggplot(tar, aes(x = q, y = price_bl)) +
  geom_point(color = "slate gray") +
  geom_smooth(color = "black") +
  labs(title = "Price by Quartar (log BUP)",
       x = "quarter",
       y = "log of building unit price")

ggplot(tar, aes(x = q, y = price_bl)) +
  geom_boxplot(aes(group = q), coef = range_price) +
  labs(title = "Price by Quartar (log BUP)",
       x = "quarter",
       y = "log of building unit price")

ggplot(tar, aes(x = q, y = price_ll)) +
  geom_point(color = "slate gray") +
  geom_smooth(color = "black") +
  labs(title = "Price by Quartar (log LUP)",
       x = "quarter",
       y = "log of land unit price")

ggplot(tar, aes(x = q, y = price_ll)) +
  geom_boxplot(aes(group = q), coef = range_price) +
  labs(title = "Price by Quartar (log LUP)",
       x = "quarter",
       y = "log of land unit price")

tar <- tar %>% 
  group_by(yq, region, use) %>% 
  filter(ifelse(n() >= 3,
                price_bl >= quantile(price_bl, probs = 0.25)
                - IQR(price_bl) * range_price & 
                  price_bl <= quantile(price_bl, probs = 0.75)
                + IQR(price_bl) * range_price,
                !is.na(price_bl))) %>% 
  ungroup()

nrow(tar)

tar <- tar %>% 
  group_by(yq, region, use) %>% 
  filter(ifelse(n() >= 3,
                price_ll >= quantile(price_ll, probs = 0.25)
                - IQR(price_ll) * range_price & 
                  price_ll <= quantile(price_ll, probs = 0.75)
                + IQR(price_ll) * range_price,
                !is.na(price_ll))) %>% 
  ungroup()

nrow(tar)

num_qru <- tar %>% 
  group_by(yq, region, use) %>% 
  summarise(n = n()) %>% 
  ungroup()

summary(num_qru$n)
