# Convert data into repeat sales form

str(tar)

library(dplyr)

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

str(rp_tar)

rp <- rp_tar %>% 
  group_by(cd_rp) %>% 
  mutate(q1 = lag(q), p1 = lag(price_bl)) %>% 
  rename(q2 = q, p2 = price_bl) %>% 
  relocate(q2, p2, .after = p1) %>% 
  filter(!is.na(q1)) %>% 
  filter(q1 != q2) %>%
  select(-price_ll) %>% 
  ungroup()

str(rp)

# Number of transactions and repeat sales

num_tar <- tar %>% 
  group_by(q, no_region, region) %>% 
  summarise(num_tar = n()) %>%
  ungroup()

library(ggplot2)

ggplot(num_tar, aes(x = q, y = num_tar, col = region)) +
  geom_line() +
  labs(title = "Number of All Transactions",
       x = "quarter",
       y = "transactions")

num_rp_tar <- rp_tar %>% 
  group_by(q, no_region, region) %>% 
  summarise(num_rp_tar = n()) %>%
  ungroup()

ggplot(num_rp_tar, aes(x = q, y = num_rp_tar, col = region)) +
  geom_line() +
  labs(title = "Number of All Repeat Sales",
       x = "quarter",
       y = "repeat sales")

num_rp <- rp %>% 
  group_by(q2, no_region, region) %>% 
  summarise(num_rp = n()) %>%
  rename(q = q2) %>% 
  ungroup()

ggplot(num_rp, aes(x = q, y = num_rp, col = region)) +
  geom_line() +
  labs(title = "Number of Repeat Sales (Once a Quarter)",
       x = "quarter",
       y = "repeat sales")

str(num_rp)

num_rp %>% 
  ggplot(aes(x = q, y = num_rp)) +
  geom_col()+
  labs(title = "Number of Repeat Sales (Seoul)",
       x = "quarter",
       y = "repeat sales")

num_rp %>% 
  ggplot(aes(x = q, y = num_rp)) +
  geom_col() +
  facet_wrap(~region, ncol = 1) +
  labs(title = "Number of Repeat Sales (Regions)",
       x = "quarter",
       y = "repeat sales")

# Create growth and time dummy variables

rp <- rp %>%
  mutate(growth = p2 - p1) %>% 
  mutate(int = q2 - q1)

for(i in 1:max(tar$q)) {
  rp[[paste0("t", i)]] <- ifelse(rp$q1 == i, -1,
                                 ifelse(rp$q2 == i, 1, 0))
}

str(rp)
