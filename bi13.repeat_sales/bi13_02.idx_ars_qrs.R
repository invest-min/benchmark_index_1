# ARS: arithmetic repeat sales

# QRS: quantile repeat sales

library(dplyr)

data <- rp %>% 
  select(no_region, region, int, growth, starts_with("t")) %>% 
  select(-t1)
fit <- lm(growth ~ . -no_region -region -int -1, data)
fit1 <- lm(abs(resid(fit)) ~ data$int)
wt <- 1 / fitted(fit1)^2

library(quantreg)

fit2 <- rq(growth ~ . -no_region -region -int -1, data = data, w = wt)
summary(fit2)

idx <- data.frame(q = 1:max(tar$q),
                  qrs = c(100, exp(coef(fit2)) * 100))

idx_s <- left_join(idx_s, idx, by = "q")

library(ggplot2)

ggplot(idx_s, aes(x = q, y = wrs)) +
  geom_line(linewidth = 0.7) +
  labs(title = "QRS (Seoul)",
       x = "quarter",
       y = "index")

colors <- c("wrs" = "blue", "qrs" = "red")

ggplot(idx_s, aes(x = q)) +
  geom_line(aes(y = wrs, color = "wrs"), linewidth = 0.7) +
  geom_line(aes(y = qrs, color = "qrs"), linewidth = 0.7) +
  labs(title = "WRS and QRS (Seoul)",
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

colors <- c("도심권" = "black",
            "동북권" = "blue",
            "서북권" = "darkgreen",
            "서남권" = "magenta",
            "동남권" = "red")

idx_r %>% 
  filter(model == "qrs") %>% 
  ggplot(aes(x = q, y = index, col = region)) +
  geom_line(size = 0.7) +
  labs(title = "QRS (Regions)",
       x = "quarter",
       y = "index") +
  scale_color_manual(values = colors) +
  theme(legend.position = c(0.1, 0.65))

idx_r %>% 
  filter(model == "qrs") %>% 
  ggplot(aes(x = q, y = index)) +
  geom_line(size = 0.7) +
  labs(title = "QRS (Regions)",
       x = "quarter",
       y = "index") +
  scale_color_manual(values = colors) +
  facet_wrap(~region)

rm(x, form, d, d1, dt, f, f1, f2, fit, fit1, fit2, idx, wt)
