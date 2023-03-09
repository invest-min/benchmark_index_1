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

# Mean and median

idxb <- tar %>% 
  group_by(q) %>% 
  summarise(mean = mean(price_b),
            median = median(price_b)) %>% 
  ungroup()

idxb <- idxb %>% 
  mutate(mean = mean / mean[1] * 100,
         median = median / median[1] * 100)

idxbr <- tar %>% 
  group_by(q, region) %>% 
  summarise(mean = mean(price_b),
            median = median(price_b)) %>% 
  ungroup()

library(tidyr)

idxbr <- idxbr %>% 
  group_by(region) %>% 
  mutate(mean = mean / mean[1] * 100,
         median = median / median[1] * 100) %>% 
  ungroup() %>% 
  pivot_longer(c(mean, median),
               names_to = "model",
               values_to = "index")

# Local regression (LOESS)

fit <- loess(mean ~ q, idxb)
loess <- predict(fit)
loess <- loess / loess[1] * 100
idxb <- data.frame(idxb, loess)

rm(fit, loess)

l <- idxbr %>% 
  filter(model == "mean") %>% 
  group_by(region) %>% 
  do(f = predict(loess(index ~ q, data = .))) %>% 
  ungroup()

l <- unlist(l, recursive = F, use.names = T)
d <- data.frame(q = rep(1:max(idxbr$q),
                        length(unique(idxbr$region))),
                region = c(rep(l[[1]], max(idxbr$q)),
                           rep(l[[2]], max(idxbr$q)),
                           rep(l[[3]], max(idxbr$q)),
                           rep(l[[4]], max(idxbr$q)),
                           rep(l[[5]], max(idxbr$q))),
                model = rep("loess", max(idxbr$q) *
                              length(unique(idxbr$region))),
                index = c(l$f1, l$f2, l$f3, l$f4, l$f5))

d <- d %>% 
  group_by(region) %>% 
  mutate(index = index / index[1] * 100) %>% 
  ungroup()

idxbr <- bind_rows(idxbr, d)

rm(l, d)

idx_s <- idxb %>% 
  select(q, mean, median, loess)

idx_r <- idxbr %>% arrange(q, region, model)

rm(idxb, idxbr)

# ORS

data <- rp %>% 
  select(no_region, region, int, growth, starts_with("t")) %>% 
  select(-t1)

fit <- lm(growth ~ . -no_region -region -int -1, data)
idx <- data.frame(q = 1:max(tar$q),
                  ors = c(100, exp(coef(fit)) * 100))

idx_s <- left_join(idx_s, idx, by = "q")


x <- paste(colnames(data)[-c(1, 2, 3, 4)], collapse = " + ")
x <- paste(x, "-1")
form <- as.formula(paste("growth", x, sep = " ~ "))

l <- data %>% 
  group_by(region) %>% 
  do(f = lm(form, data = .)) %>% 
  ungroup()

l <- unlist(l, recursive = F, use.names = T)
d <- data.frame(q = rep(1:max(tar$q),
                        length(unique(tar$region))),
                region = c(rep(l[[1]], max(tar$q)),
                           rep(l[[2]], max(tar$q)),
                           rep(l[[3]], max(tar$q)),
                           rep(l[[4]], max(tar$q)),
                           rep(l[[5]], max(tar$q))),
                model = rep("ors", max(tar$q) *
                              length(unique(tar$region))),
                index = c(c(100, exp(coef(l$f1)) * 100),
                          c(100, exp(coef(l$f2)) * 100),
                          c(100, exp(coef(l$f3)) * 100),
                          c(100, exp(coef(l$f4)) * 100),
                          c(100, exp(coef(l$f5)) * 100)))

idx_r <- bind_rows(idx_r, d)
idx_r <- idx_r %>% 
  arrange(q, region, model)

rm(idx, x, form, l, d)

# WRS

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

rm(x, form, d, d1, dt, f, f1, f2, fit, fit1, fit2, idx, wt)

# QRS

fit <- lm(growth ~ . -no_region -region -int -1, data)
fit1 <- lm(abs(resid(fit)) ~ data$int)
wt <- 1 / fitted(fit1)^2

library(quantreg)

fit2 <- rq(growth ~ . -no_region -region -int -1, data = data, w = wt)

idx <- data.frame(q = 1:max(tar$q),
                  qrs = c(100, exp(coef(fit2)) * 100))

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
  f2 <- rq(form, data = dt, w = wt)
  d1 <- data.frame(q = 1:max(tar$q),
                   region = rep(dt$region[1], max(tar$q)),
                   model = rep("qrs", max(tar$q)),
                   index = c(100, exp(coef(f2)) * 100))
  d <- bind_rows(d, d1)
}

idx_r <- bind_rows(idx_r, d)
idx_r <- idx_r %>% 
  arrange(q, region, model)

rm(x, form, d, d1, dt, f, f1, f2, fit, fit1, fit2, idx, wt)

# BRS

x <- paste(colnames(data)[-c(1, 2, 3, 4)], collapse = " + ")
x <- paste(x, "-1")
form <- as.formula(paste("growth", x, sep = " ~ "))
dt <- data[-c(1, 2, 3)]

fit <- lm(form, dt)
fit1 <- lm(abs(resid(fit)) ~ data$int)
wt <- 1 / fitted(fit1)^2
fit2 <- lm(form, dt, w = wt)

library(car)

fit3 <- Boot(fit2, R = 500)

idx <- data.frame(q = 1:max(tar$q),
                  brs = c(100, exp(summary(fit3)$bootMed) * 100))

idx_s <- left_join(idx_s, idx, by = "q")


d <- data.frame()

for (i in 1:max(tar$no_region)) {
  dt1 <- data[data$no_region == i, ]
  dt2 <- data[data$no_region == i, -c(1, 2, 3)]
  f <- lm(form, dt2)
  f1 <- lm(abs(resid(f)) ~ dt1$int) 
  wt <- 1 / fitted(f1)^2
  f2 <- lm(form, dt2, w = wt)
  f3 <- Boot(f2, R = 500)
  d1 <- data.frame(q = 1:max(tar$q),
                   region = rep(dt1$region[1], max(tar$q)),
                   model = rep("brs", max(tar$q)),
                   index = c(100, exp(summary(f3)$bootMed) * 100))
  d <- bind_rows(d, d1)
}

idx_r <- bind_rows(idx_r, d)
idx_r <- idx_r %>% 
  arrange(q, region, model)

rm(x, form, d, d1, dt, dt1, dt2, f, f1, f2, f3, fit, fit1, fit2, fit3, idx, wt)

# MRS

dt <- rp %>% 
  filter(rp[[ncol(rp)]] != 1)

dt <- dt %>% 
  select(-ncol(dt)) %>% 
  select(no_region, region, int, growth, starts_with("t"))

names(dt) <- c("no_region", "region", "int", "growth", paste0("t", 2:max(tar$q)))

dt <- bind_rows(data, dt)

fit <- lm(growth ~ . -no_region -region -int -1, dt)
fit1 <- lm(abs(resid(fit)) ~ dt$int)
wt <- 1 / fitted(fit1)^2
fit2 <- lm(growth ~ . -no_region -region -int -1, dt, w = wt)
idx <- data.frame(q = 1:max(tar$q),
                  mrs = c(100, exp(coef(fit2)) * 100))

idx_s <- left_join(idx_s, idx, by = "q")


x <- paste(colnames(data)[-c(1, 2, 3, 4)], collapse = " + ")
x <- paste(x, "-1")
form <- as.formula(paste("growth", x, sep = " ~ "))
d <- data.frame()

for (i in 1:max(tar$no_region)) {
  dtr <- dt[dt$no_region == i, ]
  f <- lm(form, dtr)
  f1 <- lm(abs(resid(f)) ~ dtr$int) 
  wt <- 1 / fitted(f1)^2
  f2 <- lm(form, dtr, w = wt)
  d1 <- data.frame(q = 1:max(tar$q),
                   region = rep(dtr$region[1], max(tar$q)),
                   model = rep("mrs", max(tar$q)),
                   index = c(100, exp(coef(f2)) * 100))
  d <- bind_rows(d, d1)
}

idx_r <- bind_rows(idx_r, d)
idx_r <- idx_r %>% 
  arrange(q, region, model)

rm(x, form, d, d1, dt, dtr, f, f1, f2, fit, fit1, fit2, idx, wt)

# FRS

rp_s <- tar %>% 
  mutate(s1 = (q - 1) %/% 4 + 1) %>%
  mutate(s2 = (q - 2) %/% 4 + 1) %>%
  mutate(s3 = (q - 3) %/% 4 + 1) %>%
  mutate(s4 = (q - 4) %/% 4 + 1) %>%
  select(cd_dong, jibun, area_b, area_l,
         q, s1, s2, s3, s4,
         no_region, region, price_bl) %>%
  group_by(cd_dong, jibun, area_b, area_l) %>%
  filter(n() > 1) %>% 
  mutate(cd_rp = cur_group_id()) %>%
  ungroup() %>% 
  select(s1, s2, s3, s4, cd_rp, q, no_region, region, price_bl) %>%
  arrange(cd_rp, q)

dt <- list()

for (i in 1:4) {
  dt[[i]] <- rp_s %>% 
    filter(rp_s[i] >= 1, rp_s[i] <= (max(tar$q) - (i-1)) %/% 4)
}

for (i in 1:4) {
  s_col <- paste0("s", i)
  dt[[i]] <- dt[[i]] %>% 
    group_by(cd_rp) %>% 
    mutate(t1 = lag(!!sym(s_col)), p1 = lag(price_bl)) %>% 
    rename(t2 = !!sym(s_col), p2 = price_bl) %>% 
    relocate(t2, p2, .after = p1) %>% 
    filter(!is.na(t1)) %>% 
    filter(t1 != t2) %>%
    mutate(growth = p2 - p1) %>% 
    mutate(int = t2 - t1) %>% 
    ungroup()
}

for (i in 1:4) {
  for (j in 1:((max(tar$q) - (i - 1)) %/% 4)) {
    col <- paste0("y", j)
    dt[[i]][[col]] <- ifelse(dt[[i]]$t1 == j, -1,
                             ifelse(dt[[i]]$t2 == j, 1, 0))
  }
}

form <- list()

for (i in 1:4) {
  x <- paste(colnames(dt[[i]])[-c(1:14)], collapse = " + ")
  x <- paste(x, "-1")
  form[[i]] <- as.formula(paste("growth", x, sep = " ~ "))
}

idx <- list()

for (i in 1:4) {
  fit <- lm(form[[i]], dt[[i]])
  fit1 <- lm(abs(resid(fit)) ~ dt[[i]]$int)
  wt <- 1 / fitted(fit1)^2
  fit2 <- lm(form[[i]], dt[[i]], w = wt)
  idx[[i]] <- data.frame(y = 1:((max(tar$q) - (i-1)) %/% 4))
  idx[[i]][paste0("s", i)] <- c(100, exp(coef(fit2)) * 100)
  idx[[i]] <- idx[[i]] %>%
    mutate(q = y * 4 + (i-1)) %>% 
    select(-y)
}

idx_all <- full_join(idx[[1]], idx[[2]], by = "q")
idx_all <- full_join(idx_all, idx[[3]], by = "q")
idx_all <- full_join(idx_all, idx[[4]], by = "q")
idx_all <- idx_all %>%
  relocate(q) %>% 
  arrange(q)

idx_long <- idx_all %>% 
  pivot_longer(-q, names_to = "series", values_to = "idx") %>% 
  na.omit(idx_long)


idx_long <- idx_long %>% 
  group_by(series) %>% 
  mutate(q1 = lag(q), p1 = lag(idx)) %>% 
  rename(q2 = q, p2 = idx) %>% 
  mutate(growth = log(p2) - log(p1)) %>% 
  ungroup() %>% 
  relocate(q2, p2, .after = p1) %>% 
  filter(!is.na(q1))

for(i in 1:(max(tar$q))) {
  idx_long <- idx_long %>%
    mutate(!!paste0("t", i) := ifelse(q1 == i, -1,
                                      ifelse(q2 == i, 1, 0)))
}

growth <- as.matrix(idx_long$growth, ncol = 1)
quarter <- as.matrix(idx_long[, 8:(max(tar$q) + 6)])

library(MASS)

idx_frs <- exp((ginv(quarter) %*% growth)[3:(max(tar$q) - 1), ])
idx_frs <- idx_frs / idx_frs[1] * 100
idx_frs <- data.frame(q = 1:max(tar$q),
                      frs = c(NA, NA, NA, idx_frs))

detach(package:MASS)

idx_s <- left_join(idx_s, idx_frs, by = "q")

rm(rp_s, x, wt, fit, fit1, fit2, growth, quarter,
   idx, idx_long, idx_all, idx_frs, col, i, j, s_col)


d <- data.frame()
idx <- list()
idx_all <- data.frame()

for (i in 1:4) {
  for (j in 1:max(tar$no_region)) {
    d <- dt[[i]][dt[[i]][["no_region"]] == j, ]
    fit <- lm(form[[i]], d)
    fit1 <- lm(abs(resid(fit)) ~ d$int)
    wt <- 1 / fitted(fit1)^2
    fit2 <- lm(form[[i]], d, w = wt)
    idx[[j]] <- data.frame(y = 1:((max(tar$q) - (i-1)) %/% 4),
                           no_region = rep(d$no_region[1], (max(tar$q) - (i-1)) %/% 4),
                           region = rep(d$region[1], (max(tar$q) - (i-1)) %/% 4),
                           series = rep(paste0("s", i), (max(tar$q) - (i-1)) %/% 4),
                           idx = c(100, exp(coef(fit2)) * 100))
    idx[[j]] <- idx[[j]] %>% mutate(q = y * 4 + (i-1))
    idx_all <- bind_rows(idx_all, idx[[j]])
  }
}

idx_all <- idx_all %>% 
  select(c(q, no_region, region, series, idx)) %>% 
  arrange(q, no_region, series)

rownames(idx_all) <- NULL

idx_all <- idx_all %>% 
  group_by(region, series) %>% 
  mutate(q1 = lag(q), p1 = lag(idx)) %>% 
  rename(q2 = q, p2 = idx) %>% 
  mutate(growth = log(p2) - log(p1)) %>% 
  ungroup() %>% 
  relocate(q2, p2, .after = p1) %>% 
  filter(!is.na(q1))

for(i in 1:(max(tar$q))) {
  idx_all <- idx_all %>%
    mutate(!!paste0("t", i) := ifelse(q1 == i, -1,
                                      ifelse(q2 == i, 1, 0)))
}

growth <- list()
quarter <- list()

for (i in 1:max(tar$no_region)) {
  growth[[i]] <- idx_all %>% 
    filter(no_region == i) %>% 
    select(growth)
  growth[[i]] <- as.matrix(growth[[i]], ncol = 1)
  quarter[[i]] <- idx_all %>% 
    filter(no_region == i) %>% 
    select(10:(max(tar$q) + 8))
  quarter[[i]] <- as.matrix(quarter[[i]])
}

library(MASS)

reg <- unique(idx_all[1:2])
idx_reg <- data.frame()

for (i in 1:max(reg$no_region)) {
  idx <- exp((ginv(quarter[[i]]) %*% growth[[i]])[3:(max(tar$q) - 1), ])
  idx <- idx / idx[1] * 100
  idx <- data.frame(q = 1:max(tar$q),
                    region = rep(reg$region[i], max(tar$q)),
                    model = rep("frs", max(tar$q)),
                    index = c(NA, NA, NA, idx))
  idx_reg <- bind_rows(idx_reg, idx)
}

detach(package:MASS)

idx_r <- bind_rows(idx_r, idx_reg)
idx_r <- idx_r %>% 
  arrange(q, region, model)

rm(form, growth, quarter, d, dt, fit, fit1, fit2,
   idx, idx_all, idx_reg, reg, i, j, wt)
