# ORS: ordinary repeat sales

library(dplyr)

data <- rp %>% 
  select(no_region, region, int, growth, starts_with("t")) %>% 
  select(-t1)

fit <- lm(growth ~ . -no_region -region -int -1, data)
idx <- data.frame(q = 1:max(tar$q),
                  ors = c(100, exp(coef(fit)) * 100))

idx_s <- left_join(idx_s, idx, by = "q")

library(ggplot2)

ggplot(idx_s, aes(x = q, y = ors)) +
  geom_line(linewidth = 0.7) +
  labs(title = "ORS (Seoul)",
       x = "quarter",
       y = "index")

colors <- c("mean" = "skyblue", "loess" = "pink", "ors" = "red")

ggplot(idx_s, aes(x = q)) +
  geom_line(aes(y = mean, color = "mean"), linewidth = 0.7) +
  geom_line(aes(y = loess, color = "loess"), linewidth = 0.7) +
  geom_line(aes(y = ors, color = "ors"), linewidth = 0.7) +
  labs(title = "ORS (Seoul)",
       x = "quarter",
       y = "index",
       color = "Legend") +
  scale_color_manual(values = colors) +
  theme(legend.position = c(0.1, 0.8))


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

colors <- c("도심권" = "black",
            "동북권" = "blue",
            "서북권" = "darkgreen",
            "서남권" = "magenta",
            "동남권" = "red")

idx_r %>% 
  filter(model == "ors") %>% 
  ggplot(aes(x = q, y = index, col = region)) +
  geom_line(size = 0.7) +
  labs(title = "ORS (Regions)",
       x = "quarter",
       y = "index") +
  scale_color_manual(values = colors) +
  theme(legend.position = c(0.1, 0.65))

idx_r %>% 
  filter(model == "ors") %>% 
  ggplot(aes(x = q, y = index)) +
  geom_line(size = 0.7) +
  labs(title = "ORS (Regions)",
       x = "quarter",
       y = "index") +
  scale_color_manual(values = colors) +
  facet_wrap(~region)

rm(idx, x, form, l, d)

# WRS: weighted repeat sales

library(lmtest)

bptest(fit)

fit1 <- lm(abs(resid(fit)) ~ data$int)
summary(fit1)

wt <- 1 / fitted(fit1)^2
fit2 <- lm(growth ~ . -no_region -region -int -1, data, w = wt)
summary(fit2)

idx <- data.frame(q = 1:max(tar$q),
                  wrs = c(100, exp(coef(fit2)) * 100))

idx_s <- left_join(idx_s, idx, by = "q")

ggplot(idx_s, aes(x = q, y = wrs)) +
  geom_line(linewidth = 0.7) +
  labs(title = "WRS (Seoul)",
       x = "quarter",
       y = "index")

colors <- c("ors" = "blue", "wrs" = "red")

ggplot(idx_s, aes(x = q)) +
  geom_line(aes(y = ors, color = "ors"), linewidth = 0.7) +
  geom_line(aes(y = wrs, color = "wrs"), linewidth = 0.7) +
  labs(title = "ORS and WRS (Seoul)",
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

colors <- c("도심권" = "black",
            "동북권" = "blue",
            "서북권" = "darkgreen",
            "서남권" = "magenta",
            "동남권" = "red")

idx_r %>% 
  filter(model == "wrs") %>% 
  ggplot(aes(x = q, y = index, col = region)) +
  geom_line(linewidth = 0.7) +
  labs(title = "WRS (Regions)",
       x = "quarter",
       y = "index") +
  scale_color_manual(values = colors) +
  theme(legend.position = c(0.1, 0.65))

idx_r %>% 
  filter(model == "wrs") %>% 
  ggplot(aes(x = q, y = index)) +
  geom_line(size = 0.7) +
  labs(title = "WRS (Regions)",
       x = "quarter",
       y = "index") +
  scale_color_manual(values = colors) +
  facet_wrap(~region)

rm(x, form, d, d1, dt, f, f1, f2, fit, fit1, fit2, idx, wt)
