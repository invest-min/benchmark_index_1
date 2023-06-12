# ols

fit1 <- lm(data = hp,
           price_bl ~ gu + zone + road + area_l +
                      use + age + area_b + far +
                      yq)

summary(fit1)
plot(fit1)

library(car)
vif(fit1)

library(lmtest)
bptest(fit1)

plot(hp$area_l, hp$price_bl)
plot(hp$area_b, hp$price_bl)
plot(hp$far, hp$price_bl)
plot(hp$age, hp$price_bl)

hp$age2 <- hp$age^2

fit2 <- lm(data = hp,
           price_bl ~ gu + zone + road + area_l +
                      use + age + age2 + area_b + far +
                      yq)

summary(fit2)
anova(fit1, fit2)

fit3 <- lm(data = hp,
           price_bl ~ gu + zone + road + 
                      use + age + age2 +
                      yq)

summary(fit3)
anova(fit2, fit3)

fit2["coefficients"]
fit2[["coefficients"]]

coef <- fit2$coefficients

library(stringr)
coef <- coef[str_detect(names(coef), "yq")]

idx <- c(100, exp(coef) * 100)
idx <- data.frame(yq = sort(unique(hp$yq)),
                  model = "ols",
                  idx = idx)
row.names(idx) <- NULL

# wls

fit_wt <- lm(abs(fit2$residuals) ~ fit2$fitted.values)
wt <- 1 / fit_wt$fitted.values^2

fit4 <- lm(data = hp,
           price_bl ~ gu + zone + road + area_l +
                      use + age + age2 + area_b + far +
                      yq,
           w = wt)

bptest(fit4)

library(MASS)

fit5 <- rlm(data = hp,
            price_bl ~ gu + zone + road + area_l +
                       use + age + age2 + area_b + far +
                       yq)

bptest(fit5)

coef <- fit4$coefficients
coef <- coef[str_detect(names(coef), "yq")]

idx_wt <- data.frame(yq = sort(unique(hp$yq)),
                     model = "wls",
                     idx = c(100, exp(coef) * 100))
row.names(idx_wt) <- NULL

idx <- rbind(idx, idx_wt)

# comparison

library(ggplot2)

pt <- 12
xt <- 10
yt <- 10
xl <- 7
yl <- 7

idx %>% 
  ggplot(aes(x = yq,
             y = idx,
             group = model,
             col = model)) +
  geom_line(size = 0.7) +
  labs(title = "OLS and WLS",
       x = "quarter",
       y = "index") +
  scale_x_discrete(labels = function(x)
    ifelse(as.numeric(substring(x, 4)) %% 4 == 1,
           as.character(x), "")) +
  theme(plot.title = element_text(size = pt),
        axis.title.x = element_text(size = xt),
        axis.title.y = element_text(size = yt),
        axis.text.x = element_text(size = xl,
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 0.5),
        axis.text.y = element_text(size = yl))

rm(fit_wt, fit1, fit2, fit3, fit4, fit5, dix_wt,
   area_b1, area_b2, area_b3, area_b4, area_b5, i,
   max_far, min_area_l, min_far, range_price, wt)
