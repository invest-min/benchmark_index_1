# Read data and change the names of variables

library(readxl)
raw <- read_excel("data/raw_seoul.xlsx")
names(raw) <- c("dong", "jiphap", "jibun", "doro",
                "zone", "use", "road", "area_b", "area_l",
                "price_t", "floors", "ym", "day",
                "strata", "completion",
                "cancel", "type", "agent")

# Remove useless information

library(dplyr)

raw <- raw %>%
  filter(jiphap == "일반") %>% 
  filter(is.na(strata)) %>%
  select(-c(jiphap, floors, strata, cancel, type, agent))

# Time variables

library(stringr)
raw$year <- str_sub(raw$ym, 1, 4)
raw$month <- str_sub(raw$ym, 5, 6)
raw$t <- str_c(raw$year, raw$month, raw$day, sep="-")
raw$t <- as.Date(raw$t)

library(lubridate)
raw$quarter <- quarter(raw$t)
raw$yq <- str_c(raw$year, raw$quarter, sep="q")
raw$yq <- str_sub(raw$yq, 3, -1)
raw$ym <- str_sub(raw$ym, 3, -1)

yq <- raw$yq
yq <- as.data.frame(yq)
yq <- distinct(yq)
yq$q <- row.names(yq)
raw <- raw %>% 
  left_join(yq, by = "yq")
rm(yq)

raw <- raw %>% 
  relocate(q, yq, ym, t, year, quarter, month, day)

raw$q <- as.numeric(raw$q)
raw$year <- as.numeric(raw$year)
raw$month <- as.numeric(raw$month)
raw$day <- as.numeric(raw$day)

# Location variables

raw$dong <- str_sub(raw$dong, 7)
raw$gu <- str_split(raw$dong, " ", simplify = T)[, 1]

cd_gu <- read_excel("data/cd_law.xlsx", sheet = "gu")
cd_dong <- read_excel("data/cd_law.xlsx", sheet = "dong")

raw <- raw %>% 
  left_join(cd_gu, by = "gu") %>% 
  left_join(cd_dong, by = "dong")

raw <- raw %>% 
  relocate(no_region, region,
           no_gu, cd_gu, gu,
           no_dong, cd_dong, dong,
           jibun, doro,
           .after = day)

# Zone, use and road

raw <- raw %>%
  filter(!is.na(zone))

raw$zone[raw$zone == "제1종전용주거"] <- "전용주거"
raw$zone[raw$zone == "제2종전용주거"] <- "전용주거"
raw$zone[raw$zone == "제1종일반주거"] <- "일반주거1"
raw$zone[raw$zone == "제2종일반주거"] <- "일반주거2"
raw$zone[raw$zone == "제3종일반주거"] <- "일반주거3"
raw$zone[raw$zone == "유통상업"] <- "근린상업"
raw$zone[raw$zone == "보전녹지"] <- "자연녹지"
raw$zone[raw$zone == "생산녹지"] <- "자연녹지"
raw$zone[raw$zone == "개발제한구역"] <- "개발제한"

far_limit <- read_excel("data/zones_far.xlsx", sheet = "limit")
raw <- raw %>% 
  left_join(far_limit, by = "zone") %>% 
  filter(!is.na(far_norm)) %>% 
  filter(!is.na(far_hist))

raw <- raw %>%
  filter(!is.na(use))

raw$use[raw$use == "제1종근린생활"] <- "근생1"
raw$use[raw$use == "제2종근린생활"] <- "근생2"

use <- data.frame(use = names(with(raw, table(use))),
                  no_use = c(5, 1, 2, 7, 6, 4, 3))
use <- use %>% arrange(no_use)

raw <- raw %>% 
  left_join(use, by = "use")

rm(use)

raw <- raw %>%
  filter(!is.na(road))

raw <- raw %>% 
  filter(road != "-")
raw$road <- factor(raw$road,
                   ordered = T,
                   labels = c("8m미만",
                              "12m미만",
                              "25m미만",
                              "25m이상"))

raw <- raw %>% 
  relocate(no_zone, zone, hist, far_norm, far_hist,
           no_use, use, road, .after = doro)

# Area and far

raw$area_b <- as.numeric(raw$area_b)

raw <- raw %>%
  filter(!is.na(area_b))

raw$area_l <- as.numeric(raw$area_l)

raw <- raw %>%
  filter(!is.na(area_l))

raw$far <- raw$area_b / raw$area_l

raw <- raw %>% 
  relocate(area_b, area_l, far, .after = road)

# Completion

raw$completion <- as.numeric(raw$completion)

raw <- raw %>% 
  relocate(completion, .after = far)

# Price

raw$price_t <- gsub(",", "", raw$price_t)
raw$price_t <- as.numeric(raw$price_t)
raw$price_t <- raw$price_t * 10000

raw <- raw %>%
  filter(!is.na(price_t))

raw <- raw %>% 
  mutate(price_b = price_t / area_b,
         price_bl = log(price_b))

raw <- raw %>% 
  mutate(price_l = price_t / area_l,
         price_ll = log(price_l))

# Unique

raw <- unique(raw)

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

tar <- raw %>%
  filter(size == "little") # Set the target

# Outlier by the area of land

min_area_l <- 100 # 30 py

tar <- tar %>%
  filter(area_l >= min_area_l)

# Outlier by far

min_far <- 1/3
max_far <- 15

tar <- tar %>%
  filter(far >= min_far & far < max_far)

# Outlier by price (log of unit price)

range_price <- 3 # X IQR

tar <- tar %>% 
  group_by(yq, region, use) %>% 
  filter(ifelse(n() >= 3,
                price_bl >= quantile(price_bl, probs = 0.25)
                - IQR(price_bl) * range_price 
                & price_bl <= quantile(price_bl, probs = 0.75)
                + IQR(price_bl) * range_price,
                !is.na(price_bl))) %>% 
  ungroup()

tar <- tar %>% 
  group_by(yq, region, use) %>% 
  filter(ifelse(n() >= 3,
                price_ll >= quantile(price_ll, probs = 0.25)
                - IQR(price_ll) * range_price  
                & price_ll <= quantile(price_ll, probs = 0.75)
                + IQR(price_ll) * range_price,
                !is.na(price_ll))) %>% 
  ungroup()

# Convert data into repeat sales form

rp_tar <- tar %>% 
  select(cd_dong, jibun, area_b, area_l,
         q, no_region, region, price_bl, price_ll) %>%
  group_by(cd_dong, jibun, area_b, area_l) %>%
  filter(n() > 1) %>% 
  mutate(cd_rp = cur_group_id()) %>%
  ungroup() %>% 
  select(cd_dong, cd_rp, q, no_region, region,
         price_bl, price_ll) %>% 
  arrange(cd_rp, q)

rp <- rp_tar %>% 
  group_by(cd_rp) %>% 
  mutate(q1 = lag(q), p1 = lag(price_bl)) %>% 
  rename(q2 = q, p2 = price_bl) %>% 
  relocate(q2, p2, .after = p1) %>% 
  filter(!is.na(q1)) %>% 
  filter(q1 != q2) %>%
  select(-price_ll) %>% 
  ungroup()

# Create growth, interval and time dummy variables

rp <- rp %>%
  mutate(growth = p2 - p1) %>% 
  mutate(int = q2 - q1)

for(i in 1:max(tar$q)) {
  rp[[paste0("t", i)]] <- ifelse(rp$q1 == i, -1,
                                 ifelse(rp$q2 == i, 1, 0))
}

# Mean

idxb <- tar %>% 
  group_by(q) %>% 
  summarise(mean = mean(price_b)) %>% 
  ungroup()

idxb <- idxb %>% 
  mutate(mean = mean / mean[1] * 100)

idxbr <- tar %>% 
  group_by(q, region) %>% 
  summarise(mean = mean(price_b)) %>% 
  ungroup()

library(tidyr)

idxbr <- idxbr %>% 
  group_by(region) %>% 
  mutate(mean = mean / mean[1] * 100) %>% 
  ungroup() %>% 
  pivot_longer(mean,
               names_to = "model",
               values_to = "index")

idx_s <- idxb %>% 
  select(q, mean)

idx_r <- idxbr %>% arrange(q, region, model)

rm(idxb, idxbr)

# WRS

data <- rp %>% 
  select(no_region, region, int, growth, starts_with("t")) %>% 
  select(-t1)

fit <- lm(growth ~ . -no_region -region -int -1, data)

fit1 <- lm(abs(resid(fit)) ~ data$int)
wt <- 1 / fitted(fit1)^2
fit2 <- lm(growth ~ . -no_region -region -int -1, data, w = wt)

idx <- data.frame(q = 1:max(tar$q),
                  wrs = c(100, exp(coef(fit2)) * 100))

idx_s <- left_join(idx_s, idx, by = "q")


x <- paste(colnames(data)[-c(1, 2, 3, 4)], collapse = " + ")
x <- paste(x, "-1")
form <- as.formula(paste("growth", x, sep = " ~ "))
d <- data.frame()

for (i in 1:max(tar$no_region)) {
  dt <- data[data$no_region == i, ]
  f <- lm(form, dt)
  f1 <- lm(abs(resid(f)) ~ dt$int) 
  wt <- 1 / fitted(f1)^2
  f2 <- lm(form, dt, w = wt)
  d1 <- data.frame(q = 1:max(tar$q),
                   region = rep(dt$region[1], max(tar$q)),
                   model = rep("wrs", max(tar$q)),
                   index = c(100, exp(coef(f2)) * 100))
  d <- bind_rows(d, d1)
}

idx_r <- bind_rows(idx_r, d)
idx_r <- idx_r %>% 
  arrange(q, region, model)

rm(x, form, i, d, d1, dt, f, f1, f2, fit, fit1, fit2, idx, wt)
