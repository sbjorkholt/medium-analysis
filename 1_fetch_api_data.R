
library(httr)
library(tidyverse)

### SCIENCE

api_key <- read_lines("./credentials/api_key")

url <- "https://medium2.p.rapidapi.com/recommended_feed/science/"
df_tags <- list()

for (i in 1:50) {
  
  p <- as.character(i)
  
  queryString2 <- list(page = p)
  
  response <- VERB("GET", 
                    url, 
                    query = queryString2, 
                    add_headers('X-RapidAPI-Key' = api_key, 
                                'X-RapidAPI-Host' = 'medium2.p.rapidapi.com'), 
                    content_type("application/octet-stream"))
  
  message(paste0("Remaining: ", headers(response)$`x-ratelimit-all-endpoints-remaining`)) 
  
  text <- content(response, "text")
  df_tags[[i]] <- jsonlite::fromJSON(text, flatten = TRUE)
  
}

articles <- df_tags %>%
  bind_rows() %>%
  distinct(.keep_all = TRUE) %>%
  pull(recommended_feed)

articles <- articles[!articles %in% science2$id]

articles_list <- list()

for(i in 1:length(articles)) {
  
  id <- articles[i]
  
  url <- paste0("https://medium2.p.rapidapi.com/article/", id, "/")
  
  response <- VERB("GET", 
                   url, 
                   add_headers('X-RapidAPI-Key' = api_key, 
                               'X-RapidAPI-Host' = 'medium2.p.rapidapi.com'), 
                   content_type("application/octet-stream"))
  
  text <- content(response, "text")
  df <- jsonlite::fromJSON(text, flatten = TRUE)
  
  articles_list[[i]] <- df
  
  message(paste0("Remaining: ", headers(response)$`x-ratelimit-all-endpoints-remaining`))
  
}

articles2 <- articles_list
topics <- list()

for (i in 1:length(articles2)) {
  
  articles2[[i]][["topics"]] <- NULL
  
}

articles_all <- articles2 %>%
  bind_rows() %>%
  distinct(.keep_all = TRUE) %>%
  group_by(across(c(-tags))) %>%
  nest() %>%
  ungroup()

science <- bind_rows(articles_all)

saveRDS(science, file = "./data/science.rds")


### WORLD

url <- "https://medium2.p.rapidapi.com/recommended_feed/world/"
df_tags <- list()

for (i in 1:50) {
  
  p <- as.character(i)
  
  queryString2 <- list(page = p)
  
  response <- VERB("GET", 
                   url, 
                   query = queryString2, 
                   add_headers('X-RapidAPI-Key' = api_key, 
                               'X-RapidAPI-Host' = 'medium2.p.rapidapi.com'), 
                   content_type("application/octet-stream"))
  
  text <- content(response, "text")
  df_tags[[i]] <- jsonlite::fromJSON(text, flatten = TRUE)
  
  message(paste0("Remaining: ", headers(response)$`x-ratelimit-all-endpoints-remaining`))
  
}

articles <- df_tags %>%
  bind_rows() %>%
  distinct(.keep_all = TRUE) %>%
  pull(recommended_feed)

articles <- articles[!articles %in% world2$id]

articles_list <- list()

for(i in 1:length(articles)) {
  
  id <- articles[i]
  
  url <- paste0("https://medium2.p.rapidapi.com/article/", id, "/")
  
  response <- VERB("GET", 
                   url, 
                   add_headers('X-RapidAPI-Key' = api_key, 
                               'X-RapidAPI-Host' = 'medium2.p.rapidapi.com'), 
                   content_type("application/octet-stream"))
  
  text <- content(response, "text")
  df <- jsonlite::fromJSON(text, flatten = TRUE)
  
  articles_list[[i]] <- df
  
  message(paste0("Remaining: ", headers(response)$`x-ratelimit-all-endpoints-remaining`))
  
}

articles2 <- articles_list

for (i in 1:length(articles2)) {
  
  articles2[[i]][["topics"]] <- NULL
  
}

articles_all <- articles2 %>%
  bind_rows() %>%
  distinct(.keep_all = TRUE) %>%
  group_by(across(c(-tags))) %>%
  nest() %>%
  ungroup()

world <- bind_rows(articles_all)

saveRDS(world, file = "./data/world.rds")

### DATA SCIENCE

url <- "https://medium2.p.rapidapi.com/recommended_feed/data-science/"
df_tags <- list()

for (i in 1:50) {
  
  p <- as.character(i)
  
  queryString2 <- list(page = p)
  
  response <- VERB("GET", 
                   url, 
                   query = queryString2, 
                   add_headers('X-RapidAPI-Key' = api_key, 
                               'X-RapidAPI-Host' = 'medium2.p.rapidapi.com'), 
                   content_type("application/octet-stream"))
  
  message(paste0("Remaining: ", headers(response)$`x-ratelimit-all-endpoints-remaining`)) # x-ratelimit-requests-remaining
  
  text <- content(response, "text")
  df_tags[[i]] <- jsonlite::fromJSON(text, flatten = TRUE)
  
}

articles <- df_tags %>%
  bind_rows() %>%
  distinct(.keep_all = TRUE) %>%
  pull(recommended_feed)

articles_list <- list()

for(i in 1:length(articles)) {
  
  id <- articles[i]
  
  url <- paste0("https://medium2.p.rapidapi.com/article/", id, "/")
  
  response <- VERB("GET", 
                   url, 
                   add_headers('X-RapidAPI-Key' = api_key, 
                               'X-RapidAPI-Host' = 'medium2.p.rapidapi.com'), 
                   content_type("application/octet-stream"))
  
  text <- content(response, "text")
  df <- jsonlite::fromJSON(text, flatten = TRUE)
  
  articles_list[[i]] <- df
  
  message(paste0("Remaining: ", headers(response)$`x-ratelimit-all-endpoints-remaining`))
  
}

articles2 <- articles_list
topics <- list()

for (i in 1:length(articles2)) {
  
  articles2[[i]][["topics"]] <- NULL
  
}

articles_all <- articles2 %>%
  bind_rows() %>%
  distinct(.keep_all = TRUE) %>%
  group_by(across(c(-tags))) %>%
  nest() %>%
  ungroup()

data_science <- bind_rows(articles_all)

saveRDS(data_science, file = "./data/data_science.rds")


### PSYCHOLOGY

url <- "https://medium2.p.rapidapi.com/recommended_feed/psychology/"
df_tags <- list()

for (i in 1:50) {
  
  p <- as.character(i)
  
  queryString2 <- list(page = p)
  
  response <- VERB("GET", 
                   url, 
                   query = queryString2, 
                   add_headers('X-RapidAPI-Key' = api_key, 
                               'X-RapidAPI-Host' = 'medium2.p.rapidapi.com'), 
                   content_type("application/octet-stream"))
  
  text <- content(response, "text")
  df_tags[[i]] <- jsonlite::fromJSON(text, flatten = TRUE)
  
  message(paste0("Remaining: ", headers(response)$`x-ratelimit-all-endpoints-remaining`))
  
}

articles <- df_tags %>%
  bind_rows() %>%
  distinct(.keep_all = TRUE) %>%
  pull(recommended_feed)

articles_list <- list()

for(i in 1:length(articles)) {
  
  id <- articles[i]
  
  url <- paste0("https://medium2.p.rapidapi.com/article/", id, "/")
  
  response <- VERB("GET", 
                   url, 
                   add_headers('X-RapidAPI-Key' = api_key, 
                               'X-RapidAPI-Host' = 'medium2.p.rapidapi.com'), 
                   content_type("application/octet-stream"))
  
  text <- content(response, "text")
  df <- jsonlite::fromJSON(text, flatten = TRUE)
  
  articles_list[[i]] <- df
  
  message(paste0("Remaining: ", headers(response)$`x-ratelimit-all-endpoints-remaining`))
  
}

articles2 <- articles_list

for (i in 1:length(articles2)) {
  
  articles2[[i]][["topics"]] <- NULL
  
}

articles_all <- articles2 %>%
  bind_rows() %>%
  distinct(.keep_all = TRUE) %>%
  group_by(across(c(-tags))) %>%
  nest() %>%
  ungroup()

psychology <- bind_rows(articles_all)

saveRDS(psychology, file = "./data/psychology.rds")


### CREATIVITY

url <- "https://medium2.p.rapidapi.com/recommended_feed/creativity/"
df_tags <- list()

for (i in 1:50) {
  
  p <- as.character(i)
  
  queryString2 <- list(page = p)
  
  response <- VERB("GET", 
                   url, 
                   query = queryString2, 
                   add_headers('X-RapidAPI-Key' = api_key, 
                               'X-RapidAPI-Host' = 'medium2.p.rapidapi.com'), 
                   content_type("application/octet-stream"))
  
  text <- content(response, "text")
  df_tags[[i]] <- jsonlite::fromJSON(text, flatten = TRUE)
  
  message(paste0("Remaining: ", headers(response)$`x-ratelimit-all-endpoints-remaining`))
  
}

articles <- df_tags %>%
  bind_rows() %>%
  distinct(.keep_all = TRUE) %>%
  pull(recommended_feed)

articles_list <- list()

for(i in 1:length(articles)) {
  
  id <- articles[i]
  
  url <- paste0("https://medium2.p.rapidapi.com/article/", id, "/")
  
  response <- VERB("GET", 
                   url, 
                   add_headers('X-RapidAPI-Key' = api_key, 
                               'X-RapidAPI-Host' = 'medium2.p.rapidapi.com'), 
                   content_type("application/octet-stream"))
  
  text <- content(response, "text")
  df <- jsonlite::fromJSON(text, flatten = TRUE)
  
  articles_list[[i]] <- df
  
  message(paste0("Remaining: ", headers(response)$`x-ratelimit-all-endpoints-remaining`, "\\n",
                 "Finished ", i, " of ", length(articles)))
  
}

articles2 <- articles_list

for (i in 1:length(articles2)) {
  
  articles2[[i]][["topics"]] <- NULL
  
}

articles_all <- articles2 %>%
  bind_rows()  %>%
  distinct(.keep_all = TRUE) %>%
  group_by(across(c(-tags))) %>%
  nest() %>%
  ungroup()

creativity <- bind_rows(articles_all)

saveRDS(creativity, file = "./data/creativity.rds")


### MEDIUM
# medium
# writing-tips
# medium-partner-program
# medium-earnings
# make-money-online

url <- "https://medium2.p.rapidapi.com/recommended_feed/medium/" # Shift out tag here

df_tags <- list()

for (i in 1:50) {
  
  p <- as.character(i)
  
  queryString2 <- list(page = p)
  
  response <- VERB("GET", 
                   url, 
                   query = queryString2, 
                   add_headers('X-RapidAPI-Key' = api_key, 
                               'X-RapidAPI-Host' = 'medium2.p.rapidapi.com'), 
                   content_type("application/octet-stream"))
  
  text <- content(response, "text")
  df_tags[[i]] <- jsonlite::fromJSON(text, flatten = TRUE)
  
  message(paste0("Remaining: ", headers(response)$`x-ratelimit-all-endpoints-remaining`))
  
}

articles <- df_tags %>%
  bind_rows() %>%
  distinct(.keep_all = TRUE) %>%
  pull(recommended_feed)

articles <- articles[!articles %in% medium2$id]

articles_list <- list()

for(i in 1:length(articles)) {
  
  id <- articles[i]
  
  url <- paste0("https://medium2.p.rapidapi.com/article/", id, "/")
  
  response <- VERB("GET", 
                   url, 
                   add_headers('X-RapidAPI-Key' = api_key, 
                               'X-RapidAPI-Host' = 'medium2.p.rapidapi.com'), 
                   content_type("application/octet-stream"))
  
  text <- content(response, "text")
  df <- jsonlite::fromJSON(text, flatten = TRUE)
  
  articles_list[[i]] <- df
  
  message(paste0("Remaining: ", headers(response)$`x-ratelimit-all-endpoints-remaining`, " \n ",
                 "Finished ", i, " of ", length(articles)))
  
}

articles2 <- articles_list

for (i in 1:length(articles2)) {
  
  articles2[[i]][["topics"]] <- NULL
  
}

articles_all <- articles2 %>%
  bind_rows() %>%
  distinct(.keep_all = TRUE) %>%
  group_by(across(c(-tags))) %>%
  nest() %>%
  ungroup()

## Save depending on tag

medium <- bind_rows(medium)
saveRDS(medium, file = "./data/medium.rds")

writing_tips <- bind_rows(writing_tips)
saveRDS(writing_tips, file = "./data/writing_tips.rds")

medium_partner_program <- bind_rows(articles_all)
saveRDS(medium_partner_program, file = "./data/medium_partner_program.rds")

medium_earnings <- bind_rows(articles_all)
saveRDS(medium_earnings, file = "./data/medium_earnings.rds")

make_money_online <- bind_rows(articles_all)
saveRDS(make_money_online, file = "./data/make_money_online.rds")

