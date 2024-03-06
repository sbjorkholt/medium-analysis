
library(tidyverse)

#### COMBINING DATA ####

## Due to a more iterative process of gathering data (adjusting the number of observations), this collapses several rounds of API 
## calls not reflected in 1_fetch_api_data.R

psychology <- readRDS("./data/psychology.rds") %>%
  mutate(download = file.info("./data/psychology.rds")$mtime) # Add timestamp for when dataset was downloaded

psychology2 <- readRDS("./data/psychology2.rds") %>%
  filter(!id %in% psychology$id) %>%
  mutate(download = file.info("./data/psychology2.rds")$mtime)

psychology3 <- readRDS("./data/psychology3.rds") %>%
  filter(!id %in% psychology$id) %>%
  filter(!id %in% psychology2$id) %>%
  mutate(download = file.info("./data/psychology3.rds")$mtime)

data_science <- readRDS("./data/data_science.rds") %>%
  mutate(download = file.info("./data/data_science.rds")$mtime)

creativity <- readRDS("./data/creativity.rds") %>%
  mutate(download = file.info("./data/creatvity.rds")$mtime)

creativity2 <- readRDS("./data/creativity2.rds") %>%
  filter(!id %in% creativity$id) %>%
  mutate(download = file.info("./data/creativity2.rds")$mtime)

creativity3 <- readRDS("./data/creativity3.rds") %>%
  filter(!id %in% creativity$id) %>%
  filter(!id %in% creativity2$id) %>%
  mutate(download = file.info("./data/creativity3.rds")$mtime)

science <- readRDS("./data/science.rds") %>%
  mutate(download = file.info("./data/science.rds")$mtime)

science2 <- readRDS("./data/science2.rds") %>%
  filter(!id %in% science$id) %>%
  mutate(download = file.info("./data/science2.rds")$mtime)

science3 <- readRDS("./data/science3.rds") %>%
  filter(!id %in% science$id) %>%
  filter(!id %in% science2$id) %>%
  mutate(download = file.info("./data/science3.rds")$mtime)

world <- readRDS("./data/world.rds") %>%
  mutate(download = file.info("./data/world.rds")$mtime)

world2 <- readRDS("./data/world2.rds") %>%
  filter(!id %in% world$id) %>%
  mutate(download = file.info("./data/world2.rds")$mtime)

world3 <- readRDS("./data/world3.rds") %>%
  filter(!id %in% world$id) %>%
  filter(!id %in% world2$id) %>%
  mutate(download = file.info("./data/world3.rds")$mtime)

medium <- readRDS("./data/medium.rds") %>%
  mutate(download = file.info("./data/medium.rds")$mtime)

medium2 <- readRDS("./data/medium2.rds") %>%
  filter(!id %in% medium$id) %>%
  mutate(download = file.info("./data/medium2,rds")$mtime)

writing_tips <- readRDS("./data/writing_tips.rds") %>%
  mutate(download = file.info("./data/writing_tips.rds")$mtime)

medium_partner_program <- readRDS("./data/medium_partner_program.rds") %>%
  mutate(download = file.info("./data/medium_partner_program.rds")$mtime)

medium_earnings <- readRDS("./data/medium_earnings.rds") %>%
  mutate(download = file.info("./data/medium_earnings.rds")$mtime)

make_money_online <- readRDS("./data/make_money_online.rds") %>%
  mutate(download = file.info("./data/make_money_online.rds")$mtime)

df_all <- bind_rows(psychology, psychology2, psychology3,
                    creativity, creativity2, creativity3,
                    science, science2, science3, 
                    world, world2, world3,
                    data_science,
                    medium, medium2, writing_tips, medium_partner_program, medium_earnings,
                    make_money_online)

rm(psychology, psychology2, psychology3, creativity, creativity2, creativity3, data_science,
   science, science2, science3, world, world2, world3, medium, medium2, writing_tips,
   medium_earnings, medium_partner_program, make_money_online)

saveRDS(df_all, file = "./data/df_all.rds")
