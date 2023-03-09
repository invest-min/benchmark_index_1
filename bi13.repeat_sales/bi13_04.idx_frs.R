# FRS: frequency conversion repeat sales

# Produce annual wrs by each quarter (Seoul)

library(dplyr)

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

library(ggplot2)

colors <- c("s1" = "blue",
            "s2" = "green",
            "s3" = "magenta",
            "s4" = "red")

idx_long %>% 
  ggplot(aes(x = q, y = idx, col = series)) +
  geom_line(linewidth = 0.7) +
  geom_point() +
  labs(title = "Annual WRS by Each Quarter (Seoul)",
       x = "quarter",
       y = "index") +
  scale_color_manual(values = colors) +
  theme(legend.position = c(0.1, 0.65))

# Convert annual indices to a quarterly index (Seoul)

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

ggplot(idx_s, aes(x = q, y = frs)) +
  geom_line(linewidth = 0.7) +
  labs(title = "FRS (Seoul)",
       x = "quarter",
       y = "index")

idx_2 <- data.frame(q = idx_s$q,
                    wrs = idx_s$wrs / idx_s$wrs[4] * 100,
                    frs = idx_s$frs)

colors <- c("wrs" = "blue", "frs" = "red")

ggplot(idx_2, aes(x = q)) +
  geom_line(aes(y = wrs, color = "wrs"), linewidth = 0.7) +
  geom_line(aes(y = frs, color = "frs"), linewidth = 0.7) +
  labs(title = "WRS and FRS (Seoul)",
       x = "quarter",
       y = "index",
       color = "Legend") +
  scale_color_manual(values = colors) +
  theme(legend.position = c(0.1, 0.8))

rm(rp_s, x, wt, fit, fit1, fit2, growth, quarter,
   idx, idx_2, idx_long, idx_all, idx_frs, col, i, j, s_col)

# Produce annual wrs by each quarter (Regions)

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

colors <- c("s1" = "blue",
            "s2" = "green",
            "s3" = "magenta",
            "s4" = "red")

idx_all %>% 
  ggplot(aes(x = q, y = idx, col = series)) +
  geom_line(linewidth = 0.7) +
  geom_point() +
  labs(title = "Annual WRS by Each Quarter (Regions)",
       x = "quarter",
       y = "index") +
  scale_color_manual(values = colors) +
  theme(legend.position = c(0.8, 0.2)) +
  facet_wrap(~region)

# Convert annual indices to a quarterly index (Regions)

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

colors <- c("도심권" = "black",
            "동북권" = "blue",
            "서북권" = "darkgreen",
            "서남권" = "magenta",
            "동남권" = "red")

idx_r %>% 
  filter(model == "frs") %>% 
  ggplot(aes(x = q, y = index, col = region)) +
  geom_line(linewidth = 0.7) +
  labs(title = "FRS (Regions)",
       x = "quarter",
       y = "index") +
  scale_color_manual(values = colors) +
  theme(legend.position = c(0.1, 0.65))

idx_r %>% 
  filter(model == "frs") %>% 
  ggplot(aes(x = q, y = index)) +
  geom_line(size = 0.7) +
  labs(title = "FRS (Regions)",
       x = "quarter",
       y = "index") +
  scale_color_manual(values = colors) +
  facet_wrap(~region)

rm(form, growth, quarter, d, dt, fit, fit1, fit2,
   idx, idx_all, idx_reg, reg, i, j, wt)
