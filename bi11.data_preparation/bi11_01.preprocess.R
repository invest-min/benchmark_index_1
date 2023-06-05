rm(list = ls())

# Create a directory named "data" under the working directory
# Prepare a raw data file under the data directory
# "raw_seoul.xlsx" is the raw data file in this class
# It contains transaction cases of commercial buildings in Seoul

# Read data and change the names of variables

library(readxl)
raw <- read_excel("data/raw_seoul.xlsx")
str(raw)

names(raw) <- c("dong", "jiphap", "jibun", "doro",
                "zone", "use", "road", "area_b", "area_l",
                "price_t", "floors", "ym", "day",
                "strata", "completion",
                "cancel", "type", "agent")
nrow(raw)

# Remove useless information

library(dplyr)

with(raw, table(jiphap, is.na(floors)))
with(raw, table(strata))

raw <- raw %>%
  filter(jiphap == "일반") %>% 
  filter(is.na(strata)) %>%
  select(-c(jiphap, floors, strata, cancel, type, agent))
nrow(raw)

raw <- unique(raw)
nrow(raw)

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

raw$yq <- factor(raw$yq)

str(raw)

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

raw$gu <- factor(raw$gu)
raw$dong <- factor(raw$dong)

str(raw)

# Zone, use and road

with(raw, sum(is.na(zone)))
raw <- raw %>%
  filter(!is.na(zone))

with(raw, table(zone))

raw <- raw %>%
  filter(zone != "개발제한구역" &
         zone != "계획관리" &
         zone != "기타" &
         zone != "용도미지정" &
         zone != "일반공업" &
         zone != "일반주거" &
         zone != "전용공업" &
         zone != "유통상업" )

with(raw, table(zone))

raw$zone[raw$zone == "제1종전용주거"] <- "전용주거"
raw$zone[raw$zone == "제2종전용주거"] <- "전용주거"
raw$zone[raw$zone == "제1종일반주거"] <- "일반주거1"
raw$zone[raw$zone == "제2종일반주거"] <- "일반주거2"
raw$zone[raw$zone == "제3종일반주거"] <- "일반주거3"

raw$zone <- factor(raw$zone,
                   levels = c("전용주거",
                              "일반주거1",
                              "일반주거2",
                              "일반주거3",
                              "준주거",
                              "근린상업",
                              "일반상업",
                              "중심상업",
                              "준공업",
                              "자연녹지"))
summary(raw$zone)

far_limit <- read_excel("data/zones_far.xlsx", sheet = "limit")
raw <- raw %>% 
  left_join(far_limit, by = "zone") %>% 
  filter(!is.na(far_norm)) %>% 
  filter(!is.na(far_hist))


with(raw, sum(is.na(use)))
raw <- raw %>%
  filter(!is.na(use))

with(raw, table(use))
raw$use[raw$use == "제1종근린생활"] <- "근생1"
raw$use[raw$use == "제2종근린생활"] <- "근생2"
with(raw, table(use))

use <- data.frame(use = names(with(raw, table(use))),
                  no_use = c(5, 1, 2, 7, 6, 4, 3))
use <- use %>% arrange(no_use)

raw <- raw %>% 
  left_join(use, by = "use")

raw$use <- factor(raw$use,
                  levels = c("근생1",
                             "근생2",
                             "업무",
                             "판매",
                             "숙박",
                             "교육연구",
                             "기타"))

rm(use)

with(raw, sum(is.na(road)))
raw <- raw %>%
  filter(!is.na(road))

with(raw, table(road))
raw <- raw %>% 
  filter(road != "-")
raw$road <- factor(raw$road,
                   ordered = T,
                   levels = c("8m미만",
                              "12m미만",
                              "25m미만",
                              "25m이상"))
summary(raw$road)

raw <- raw %>% 
  relocate(no_zone, zone, hist, far_norm, far_hist,
           no_use, use, road, .after = doro)

str(raw)

# Area and far

raw$area_b <- as.numeric(raw$area_b)
sum(is.na(raw$area_b))
raw <- raw %>%
  filter(!is.na(area_b))

raw$area_l <- as.numeric(raw$area_l)
sum(is.na(raw$area_l))
raw <- raw %>%
  filter(!is.na(area_l))

raw$far <- raw$area_b / raw$area_l

raw <- raw %>% 
  relocate(area_b, area_l, far, .after = road)

str(raw)

# Completion

raw$completion <- as.numeric(raw$completion)
sum(is.na(raw$completion))
with(raw, table(completion))

raw <- raw %>% 
  filter(!is.na(completion)) %>% 
  filter(completion >= 1954)

sum(is.na(raw$completion))
with(raw, table(completion))

raw <- raw %>% 
  mutate(age = year - completion) %>% 
  relocate(completion, age, .after = far)

summary(raw$age)
sum(raw$age == -1)

raw <- raw %>% 
  filter(age >= 0)

str(raw)

# Price

raw$price_t <- gsub(",", "", raw$price_t)
raw$price_t <- as.numeric(raw$price_t)
raw$price_t <- raw$price_t * 10000

sum(is.na(raw$price_t))
raw <- raw %>%
  filter(!is.na(price_t))

raw <- raw %>% 
  mutate(price_b = price_t / area_b,
         price_bl = log(price_b))

raw <- raw %>% 
  mutate(price_l = price_t / area_l,
         price_ll = log(price_l))

str(raw)
