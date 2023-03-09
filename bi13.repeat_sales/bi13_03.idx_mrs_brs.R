# MRS: moving window repeat sales

library(dplyr)

data <- rp %>% 
  select(no_region, region, int, growth, starts_with("t")) %>% 
  select(-t1)

dt <- rp %>% 
  filter(rp[[ncol(rp)]] != 1)

dt <- dt %>% 
  select(-ncol(dt)) %>% 
  select(no_region, region, int, growth, starts_with("t"))

names(dt) <- c("no_region", "region", "int", "growth", paste0("t", 2:max(tar$q)))

dt <- bind_rows(data, dt)

fit <- lm(growth ~ . -no_region -region -int -1, dt)

fit1 <- lm(abs(resid(fit)) ~ dt$int)
summary(fit1)

wt <- 1 / fitted(fit1)^2
fit2 <- lm(growth ~ . -no_region -region -int -1, dt, w = wt)
summary(fit2)

idx <- data.frame(q = 1:max(tar$q),
                  mrs = c(100, exp(coef(fit2)) * 100))

idx_s <- left_join(idx_s, idx, by = "q")

library(ggplot2)

ggplot(idx_s, aes(x = q, y = mrs)) +
  geom_line(linewidth = 0.7) +
  labs(title = "MRS (Seoul)",
       x = "quarter",
       y = "index")

colors <- c("wrs" = "blue", "mrs" = "red")

ggplot(idx_s, aes(x = q)) +
  geom_line(aes(y = wrs, color = "wrs"), linewidth = 0.7) +
  geom_line(aes(y = mrs, color = "mrs"), linewidth = 0.7) +
  labs(title = "WRS and MRS (Seoul)",
       x = "quarter",
       y = "index",
       color = "Legend") +
  scale_color_manual(values = colors) +
  theme(legend.position = c(0.1, 0.8))


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

colors <- c("도심권" = "black",
            "동북권" = "blue",
            "서북권" = "darkgreen",
            "서남권" = "magenta",
            "동남권" = "red")

idx_r %>% 
  filter(model == "mrs") %>% 
  ggplot(aes(x = q, y = index, col = region)) +
  geom_line(linewidth = 0.7) +
  labs(title = "MRS (Regions)",
       x = "quarter",
       y = "index") +
  scale_color_manual(values = colors) +
  theme(legend.position = c(0.1, 0.65))

idx_r %>% 
  filter(model == "mrs") %>% 
  ggplot(aes(x = q, y = index)) +
  geom_line(linewidth = 0.7) +
  labs(title = "MRS (Regions)",
       x = "quarter",
       y = "index") +
  scale_color_manual(values = colors) +
  facet_wrap(~region)

rm(x, form, d, d1, dt, dtr, f, f1, f2, fit, fit1, fit2, idx, wt)

#BRS: bootstrap repeat sales

library(dplyr)

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
summary(fit3)

idx <- data.frame(q = 1:max(tar$q),
                  brs = c(100, exp(summary(fit3)$bootMed) * 100))

idx_s <- left_join(idx_s, idx, by = "q")

library(ggplot2)

ggplot(idx_s, aes(x = q, y = brs)) +
  geom_line(linewidth = 0.7) +
  labs(title = "BRS (Seoul)",
       x = "quarter",
       y = "index")

colors <- c("wrs" = "blue", "brs" = "red")

ggplot(idx_s, aes(x = q)) +
  geom_line(aes(y = wrs, color = "wrs"), linewidth = 0.7) +
  geom_line(aes(y = brs, color = "brs"), linewidth = 0.7) +
  labs(title = "WRS and BRS (Seoul)",
       x = "quarter",
       y = "index",
       color = "Legend") +
  scale_color_manual(values = colors) +
  theme(legend.position = c(0.1, 0.8))


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

colors <- c("도심권" = "black",
            "동북권" = "blue",
            "서북권" = "darkgreen",
            "서남권" = "magenta",
            "동남권" = "red")

idx_r %>% 
  filter(model == "brs") %>% 
  ggplot(aes(x = q, y = index, col = region)) +
  geom_line(size = 0.7) +
  labs(title = "BRS (Regions)",
       x = "quarter",
       y = "index") +
  scale_color_manual(values = colors) +
  theme(legend.position = c(0.1, 0.65))

idx_r %>% 
  filter(model == "brs") %>% 
  ggplot(aes(x = q, y = index)) +
  geom_line(size = 0.7) +
  labs(title = "BRS (Regions)",
       x = "quarter",
       y = "index") +
  scale_color_manual(values = colors) +
  facet_wrap(~region)

rm(x, form, d, d1, dt, dt1, dt2, f, f1, f2, f3, fit, fit1, fit2, fit3, idx, wt)
