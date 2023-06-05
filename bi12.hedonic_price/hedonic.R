fit <- lm(data = raw,
          price_bl ~ gu + zone + road + area_l +
                     use + age + area_b + far +
                     yq)

summary(fit)
plot(fit)
vif(fit)

library(lmtest)
bptest(fit)
