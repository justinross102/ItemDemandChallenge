library(tidyverse)
library(tidymodels)
library(vroom)
library(patchwork)

# read in data ------------------------------------------------------------
setwd("/Users/justinross/Documents/BYU/stat348/ItemDemandChallenge")
train <- vroom("train.csv")
test <- vroom("test.csv")

# plots -------------------------------------------------------------------

plot1 <- train %>%
  filter(item == 15,
         store == 10) %>% 
  pull(sales) %>% 
  forecast::ggAcf(.) +
  theme(aspect.ratio = 1) +
  labs(title = "Store 10, Item 15")

plot2 <- train %>%
  filter(item == 2,
         store == 2) %>% 
  pull(sales) %>% 
  forecast::ggAcf(.) +
  theme(aspect.ratio = 1) +
  labs(title = "Store 2, Item 2")

plot3 <- train %>%
  filter(item == 32,
         store == 6) %>% 
  pull(sales) %>% 
  forecast::ggAcf(.) +
  theme(aspect.ratio = 1) +
  labs(title = "Store 6, Item 32")

plot4 <- train %>%
  filter(item == 29,
         store == 7) %>% 
  pull(sales) %>% 
  forecast::ggAcf(.) +
  theme(aspect.ratio = 1) +
  labs(title = "Store 7, Item 29")

combined <- (plot1 + plot2) / (plot3 + plot4)
ggsave("ItemStorePlots.png", combined)