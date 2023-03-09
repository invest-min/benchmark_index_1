# Mean and median by building area

library(dplyr)

idxb <- tar %>% 
  group_by(q) %>% 
  summarise(mean = mean(price_b),
            median = median(price_b)) %>% 
  ungroup()

idxb <- idxb %>% 
  mutate(mean = mean / mean[1] * 100,
         median = median / median[1] * 100)

library(ggplot2)

colors <- c("mean" = "blue", "median" = "red")

ggplot(idxb, aes(x = q)) +
  geom_line(aes(y = mean, color = "mean"), size = 0.7) +
  geom_line(aes(y = median, color = "median"), size = 0.7) +
  labs(title = "Mean and Median (Seoul, Building)",
       x = "quarter",
       y = "index",
       color = "Legend") +
  scale_color_manual(values = colors) +
  theme(legend.position = c(0.1, 0.8))


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

colors <- c("도심권" = "black",
            "동북권" = "blue",
            "서북권" = "darkgreen",
            "서남권" = "magenta",
            "동남권" = "red")

ggplot(idxbr, aes(x = q, y = index, col = region)) +
  geom_line(size = 0.7) +
  labs(title = "Mean and Median (Regions, Building)",
       x = "quarter",
       y = "index") +
  scale_color_manual(values = colors) +
  theme(legend.position = c(0.1, 0.65)) +
  facet_wrap(~model)

colors <- c("mean" = "blue", "median" = "red")

ggplot(idxbr, aes(x = q, y = index, col = model)) +
  geom_line(size = 0.7) +
  labs(title = "Mean and Median (Regions, Building)",
       x = "quarter",
       y = "index") +
  scale_color_manual(values = colors) +
  theme(legend.position = c(0.8, 0.2)) +
  facet_wrap(~region)

# Mean and median by land area

idxl <- tar %>% 
  group_by(q) %>% 
  summarise(mean = mean(price_l),
            median = median(price_l)) %>% 
  ungroup()

idxl <- idxl %>% 
  mutate(mean = mean / mean[1] * 100,
         median = median / median[1] * 100)

colors <- c("mean" = "blue", "median" = "red")

ggplot(idxl, aes(x = q)) +
  geom_line(aes(y = mean, color = "mean"), size = 0.7) +
  geom_line(aes(y = median, color = "median"), size = 0.7) +
  labs(title = "Mean and Median (Seoul, Land)",
       x = "quarter",
       y = "index",
       color = "Legend") +
  scale_color_manual(values = colors) +
  theme(legend.position = c(0.1, 0.8))


idxlr <- tar %>% 
  group_by(q, region) %>% 
  summarise(mean = mean(price_l),
            median = median(price_l)) %>% 
  ungroup()

idxlr <- idxlr %>% 
  group_by(region) %>% 
  mutate(mean = mean / mean[1] * 100,
         median = median / median[1] * 100) %>% 
  ungroup() %>% 
  pivot_longer(c(mean, median),
               names_to = "model",
               values_to = "index")

colors <- c("도심권" = "black",
            "동북권" = "blue",
            "서북권" = "darkgreen",
            "서남권" = "magenta",
            "동남권" = "red")

ggplot(idxlr, aes(x = q, y = index, col = region)) +
  geom_line(size = 0.7) +
  labs(title = "Mean and Median (Regions, Land)",
       x = "quarter",
       y = "index") +
  scale_color_manual(values = colors) +
  theme(legend.position = c(0.1, 0.65)) +
  facet_wrap(~model)

colors <- c("mean" = "blue", "median" = "red")

ggplot(idxlr, aes(x = q, y = index, col = model)) +
  geom_line(size = 0.7) +
  labs(title = "Mean and Median (Regions, Land)",
       x = "quarter",
       y = "index") +
  scale_color_manual(values = colors) +
  theme(legend.position = c(0.8, 0.2)) +
  facet_wrap(~region)

# Rolling average by building area

library(zoo)

span <- 5
rmean <- c(rep(NA, span - 1), rollmean(idxb$mean, span))
rmedian <- c(rep(NA, span - 1), rollmedian(idxb$median, span))
idxb <- data.frame(idxb, rmean, rmedian)

colors <- c("mean" = "blue", "rmean" = "red")

ggplot(idxb, aes(x = q)) +
  geom_line(aes(y = mean, color = "mean"), size = 0.7) +
  geom_line(aes(y = rmean, color = "rmean"), size = 0.7) +
  labs(title = "Mean and Rolling Mean (Seoul, Building)",
       x = "quarter",
       y = "index",
       color = "Legend") +
  scale_color_manual(values = colors) +
  theme(legend.position = c(0.1, 0.8))

colors <- c("median" = "blue", "rmedian" = "red")

ggplot(idxb, aes(x = q)) +
  geom_line(aes(y = median, color = "median"), size = 0.7) +
  geom_line(aes(y = rmedian, color = "rmedian"), size = 0.7) +
  labs(title = "Median and Rolling Median (Seoul, Building)",
       x = "quarter",
       y = "index",
       color = "Legend") +
  scale_color_manual(values = colors) +
  theme(legend.position = c(0.1, 0.8))

rm(span, rmean, rmedian)

# Rolling average by land area

span <- 5
rmean <- c(rep(NA, span - 1), rollmean(idxl$mean, span))
rmedian <- c(rep(NA, span - 1), rollmedian(idxl$median, span))
idxl <- data.frame(idxl, rmean, rmedian)

colors <- c("mean" = "blue", "rmean" = "red")

ggplot(idxl, aes(x = q)) +
  geom_line(aes(y = mean, color = "mean"), size = 0.7) +
  geom_line(aes(y = rmean, color = "rmean"), size = 0.7) +
  labs(title = "Mean and Rolling Mean (Seoul, Land)",
       x = "quarter",
       y = "index",
       color = "Legend") +
  scale_color_manual(values = colors) +
  theme(legend.position = c(0.1, 0.8))

colors <- c("median" = "blue", "rmedian" = "red")

ggplot(idxl, aes(x = q)) +
  geom_line(aes(y = median, color = "median"), size = 0.7) +
  geom_line(aes(y = rmedian, color = "rmedian"), size = 0.7) +
  labs(title = "Median and Rolling Median (Seoul, Land)",
       x = "quarter",
       y = "index",
       color = "Legend") +
  scale_color_manual(values = colors) +
  theme(legend.position = c(0.1, 0.8))

rm(span, rmean, rmedian)

# Local regression (LOESS) by building area

fit <- loess(mean ~ q, idxb)
loess <- predict(fit)
loess <- loess / loess[1] * 100
idxb <- data.frame(idxb, loess)

colors <- c("rmean" = "blue", "rmedian" = "red", "loess" = "black")

ggplot(idxb, aes(x = q)) +
  geom_line(aes(y = rmean, color = "rmean"), size = 0.7) +
  geom_line(aes(y = rmedian, color = "rmedian"), size = 0.7) +
  geom_line(aes(y = loess, color = "loess"), size = 0.7) +
  labs(title = "LOESS and Rolling Averages (Seoul, Building)",
       x = "quarter",
       y = "index",
       color = "Legend") +
  scale_color_manual(values = colors) +
  theme(legend.position = c(0.1, 0.8))

loess.1 <- predict(loess(mean ~ q, idxb, span = 0.1))
loess.5 <- predict(loess(mean ~ q, idxb, span = 0.5))
loess.9 <- predict(loess(mean ~ q, idxb, span = 0.9))

idxb <- data.frame(idxb, loess.1, loess.5, loess.9)

colors <- c("loess.1" = "blue",
            "loess.5" = "red",
            "loess.9" = "black")

ggplot(idxb, aes(x = q)) +
  geom_line(aes(y = loess.1, color = "loess.1"), size = 0.7) +
  geom_line(aes(y = loess.5, color = "loess.5"), size = 0.7) +
  geom_line(aes(y = loess.9, color = "loess.9"), size = 0.7) +
  labs(title = "LOESS by span (Seoul, Building)",
       x = "quarter",
       y = "index",
       color = "Legend") +
  scale_color_manual(values = colors) +
  theme(legend.position = c(0.1, 0.8))

rm(fit, loess, loess.1, loess.5, loess.9)


l <- idxbr %>% 
  filter(model == "mean") %>% 
  group_by(region) %>% 
  do(f = predict(loess(index ~ q, data = .))) %>% 
  ungroup()

l <- unlist(l, recursive = F, use.names = T)
d <- data.frame(q = rep(1:max(tar$q),
                        length(unique(tar$region))),
                region = c(rep(l[[1]], max(tar$q)),
                           rep(l[[2]], max(tar$q)),
                           rep(l[[3]], max(tar$q)),
                           rep(l[[4]], max(tar$q)),
                           rep(l[[5]], max(tar$q))),
                model = rep("loess", max(tar$q) *
                              length(unique(tar$region))),
                index = c(l$f1, l$f2, l$f3, l$f4, l$f5))

d <- d %>% 
  group_by(region) %>% 
  mutate(index = index / index[1] * 100) %>% 
  ungroup()

idxbr <- bind_rows(idxbr, d)

colors <- c("도심권" = "black",
            "동북권" = "blue",
            "서북권" = "darkgreen",
            "서남권" = "magenta",
            "동남권" = "red")

idxbr %>% 
  filter(model == "loess") %>% 
  ggplot(aes(x = q, y = index, col = region)) +
    geom_line(size = 0.7) +
    labs(title = "LOESS (Regions, Building)",
         x = "quarter",
         y = "index") +
    scale_color_manual(values = colors) +
    theme(legend.position = c(0.1, 0.65))

rm(l, d)

# Local regression (LOESS) by land area

fit <- loess(mean ~ q, idxl)
loess <- predict(fit)
loess <- loess / loess[1] * 100
idxl <- data.frame(idxl, loess)

colors <- c("rmean" = "blue", "rmedian" = "red", "loess" = "black")

ggplot(idxl, aes(x = q)) +
  geom_line(aes(y = rmean, color = "rmean"), size = 0.7) +
  geom_line(aes(y = rmedian, color = "rmedian"), size = 0.7) +
  geom_line(aes(y = loess, color = "loess"), size = 0.7) +
  labs(title = "LOESS and Rolling Averages (Seoul, Land)",
       x = "quarter",
       y = "index",
       color = "Legend") +
  scale_color_manual(values = colors) +
  theme(legend.position = c(0.1, 0.8))

loess.1 <- predict(loess(mean ~ q, idxl, span = 0.1))
loess.5 <- predict(loess(mean ~ q, idxl, span = 0.5))
loess.9 <- predict(loess(mean ~ q, idxl, span = 0.9))

idxl <- data.frame(idxl, loess.1, loess.5, loess.9)

colors <- c("loess.1" = "blue",
            "loess.5" = "red",
            "loess.9" = "black")

ggplot(idxl, aes(x = q)) +
  geom_line(aes(y = loess.1, color = "loess.1"), size = 0.7) +
  geom_line(aes(y = loess.5, color = "loess.5"), size = 0.7) +
  geom_line(aes(y = loess.9, color = "loess.9"), size = 0.7) +
  labs(title = "LOESS by span (Seoul, Land)",
       x = "quarter",
       y = "index",
       color = "Legend") +
  scale_color_manual(values = colors) +
  theme(legend.position = c(0.1, 0.8))

rm(fit, loess, loess.1, loess.5, loess.9)


l <- idxlr %>% 
  filter(model == "mean") %>% 
  group_by(region) %>% 
  do(f = predict(loess(index ~ q, data = .))) %>% 
  ungroup()

l <- unlist(l, recursive = F, use.names = T)
d <- data.frame(q = rep(1:max(tar$q),
                        length(unique(tar$region))),
                region = c(rep(l[[1]], max(tar$q)),
                           rep(l[[2]], max(tar$q)),
                           rep(l[[3]], max(tar$q)),
                           rep(l[[4]], max(tar$q)),
                           rep(l[[5]], max(tar$q))),
                model = rep("loess", max(tar$q) *
                              length(unique(tar$region))),
                index = c(l$f1, l$f2, l$f3, l$f4, l$f5))

d <- d %>% 
  group_by(region) %>% 
  mutate(index = index / index[1] * 100) %>% 
  ungroup()

idxlr <- bind_rows(idxlr, d)

colors <- c("도심권" = "black",
            "동북권" = "blue",
            "서북권" = "darkgreen",
            "서남권" = "magenta",
            "동남권" = "red")

idxlr %>% 
  filter(model == "loess") %>% 
  ggplot(aes(x = q, y = index, col = region)) +
  geom_line(size = 0.7) +
  labs(title = "LOESS (Regions, Land)",
       x = "quarter",
       y = "index") +
  scale_color_manual(values = colors) +
  theme(legend.position = c(0.1, 0.65))

rm(l, d)

# Summarize

idx_s <- idxb %>% 
  select(q, mean, median, loess)

colors <- c("mean" = "blue", "median" = "red", "loess" = "black")

ggplot(idx_s, aes(x = q)) +
  geom_line(aes(y = mean, color = "mean"), size = 0.7) +
  geom_line(aes(y = median, color = "median"), size = 0.7) +
  geom_line(aes(y = loess, color = "loess"), size = 0.7) +
  labs(title = "Mean, Median and LOESS (Seoul)",
       x = "quarter",
       y = "index",
       color = "Legend") +
  scale_color_manual(values = colors) +
  theme(legend.position = c(0.1, 0.8))


idx_r <- idxbr %>% arrange(q, region, model)

colors <- c("mean" = "blue", "median" = "red", "loess" = "black")

ggplot(idx_r, aes(x = q, y = index, col = model)) +
  geom_line(size = 0.7) +
  labs(title = "Mean, Median and LOESS (Region)",
       x = "quarter",
       y = "index") +
  scale_color_manual(values = colors) +
  theme(legend.position = c(0.8, 0.2)) +
  facet_wrap(~region)

rm(idxb, idxbr, idxl, idxlr)
