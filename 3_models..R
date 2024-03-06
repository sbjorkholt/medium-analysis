
library(tidyverse)
library(tidymodels)
library(fixest)
library(ggfixest)
library(vip)

df_all <- readRDS("./data/df_all.rds")

### Readying the data

df_model <- df_all %>%
  unnest() %>%
  filter(tags %in% c("science", "medium", "psychology", "world", "creativity", "writing-tips", 
                     "medium-partner-program", "medium-earnings",
                     "make-money-online", "data-science")) %>%
  mutate(tags = str_replace_all(tags, "-", "_")) %>%
  mutate(year = substr(published_at, 1, 4)) %>%
  mutate(month = substr(published_at, 6, 7)) %>%
  mutate(published_at = as.POSIXlt(published_at)) %>%
  mutate(time_up = download-published_at) %>%
  mutate(time_up = log(as.numeric(time_up))) %>% 
  mutate(word_count = log(word_count)) %>%
  mutate(publish = ifelse(publication_id == "*Self-Published*", "self", "publication")) %>%
  mutate(publish = factor(publish, levels = c("self", "publication"))) %>%
  mutate(is_shortform = factor(is_shortform, levels = c("FALSE", "TRUE"))) %>%
  mutate(is_locked = factor(is_locked, levels = c("FALSE", "TRUE"))) %>%
  mutate(is_series = factor(is_series, levels = c("FALSE", "TRUE"))) 


#### DESCRIPTIVES ####

options(scipen = 999)

plot_desc <- df_model %>%
  group_by(tags) %>%
  add_count() %>%
  reframe(claps = sum(claps)/n,
            reading_time = sum(reading_time)/n,
            responses_count = sum(responses_count)/n) %>%
  unique()

plot_desc_claps <- plot_desc %>%
  ggplot(aes(fct_reorder(tags, claps), claps, fill = claps)) +
  geom_bar(stat = "identity", fill = "#3DB7E4") + 
  coord_flip() +
  theme_classic() + 
  labs(x = "", y = "") +
  ggtitle("Claps") + 
  theme(legend.position = "none",
        text = element_text(size = 17))

plot_desc_reading_time <- plot_desc %>%
  ggplot(aes(fct_reorder(tags, reading_time), reading_time, fill = reading_time)) +
  geom_bar(stat = "identity", fill = "#FF8849") + 
  coord_flip() +
  theme_classic() + 
  labs(x = "", y = "") +
  ggtitle("Reading Time") + 
  theme(legend.position = "none",
        text = element_text(size = 17))

plot_desc_responses_count <- plot_desc %>%
  ggplot(aes(fct_reorder(tags, responses_count), responses_count, fill = responses_count)) +
  geom_bar(stat = "identity", fill = "#69BE28") + 
  coord_flip() +
  theme_classic() + 
  labs(x = "", y = "") +
  ggtitle("Responses Count") + 
  theme(legend.position = "none",
        text = element_text(size = 17))

cowplot::plot_grid(plot_desc_claps, plot_desc_reading_time, plot_desc_responses_count, ncol = 3)

ggsave("./figures/descriptive.png", height = 9, width = 15)


#### CAUSAL MODELS ####

df_model <- df_model %>%
  select(id, claps, reading_time, word_count, responses_count, year, month, tags, publish, time_up, is_shortform, is_locked, is_series) %>%
  distinct(id, .keep_all = TRUE) %>%
  mutate(tag = 1) %>%
  pivot_wider(names_from = "tags", values_from = tag) %>%
  unnest(cols = c(science, psychology, world, creativity, medium, writing_tips, medium_partner_program, medium_earnings)) %>%
  mutate(science = ifelse(is.na(science), 0, science),
        psychology = ifelse(is.na(psychology), 0, psychology),
        world = ifelse(is.na(world), 0, world),
        creativity = ifelse(is.na(creativity), 0, creativity),
        data_science = ifelse(is.na(data_science), 0, data_science),
        medium = ifelse(is.na(medium), 0, medium),
        writing_tips = ifelse(is.na(writing_tips), 0, writing_tips),
        medium_partner_program = ifelse(is.na(medium_partner_program), 0, medium_partner_program),
        medium_earnings = ifelse(is.na(medium_earnings), 0, medium_earnings),
        make_money_online = ifelse(is.na(make_money_online), 0, make_money_online)) %>%
  na.omit()

run_models <- function(tag){
  
  form_claps <- as.formula(paste("claps ~ ", tag, " + publish + word_count + time_up + is_shortform + is_locked | year + month"))
  form_response <- as.formula(paste("responses_count ~ ", tag, " + publish + word_count + time_up + is_shortform + is_locked | year + month"))
  form_reading <- as.formula(paste("reading_time ~ ", tag, " + publish + word_count + time_up + is_shortform + is_locked | year + month"))
  
  model_claps <- feols(form_claps, 
                       cluster = "year",
                       data = df_model) 
  
  model_claps <- bind_cols(model_claps %>% tidy() %>%
                             filter(term == tag) %>%
                             select(term, estimate),
                           confint(model_claps) %>% 
                             rownames_to_column() %>%
                             filter(rowname == tag)) %>%
    mutate(depvar = "claps")
  
  model_responses <- feols(form_response, 
                           cluster = "year",
                           data = df_model)
  
  model_responses <- bind_cols(model_responses %>% tidy() %>%
                             filter(term == tag) %>%
                             select(term, estimate),
                           confint(model_responses) %>% 
                             rownames_to_column() %>%
                             filter(rowname == tag)) %>%
    mutate(depvar = "responses_count")
  
  
  model_reading <- feols(form_reading, 
                         cluster = "year",
                         data = df_model)
  
  model_reading <- bind_cols(model_reading %>% tidy() %>%
                                 filter(term == tag) %>%
                                 select(term, estimate),
                               confint(model_reading) %>% 
                                 rownames_to_column() %>%
                                 filter(rowname == tag)) %>%
    mutate(depvar = "reading_time")
  
  return(list(model_claps, model_reading, model_responses))
  
}


coefdata <- bind_rows(run_models("medium"),
                      run_models("medium_partner_program"),
                      run_models("medium_earnings"),
                      run_models("writing_tips"),
                      run_models("make_money_online"),
                      run_models("science"),
                      run_models("world"),
                      run_models("creativity"),
                      run_models("psychology"),
                      run_models("data_science")) %>%
  mutate(term = str_to_sentence(term),
         depvar = str_to_sentence(depvar),
         depvar = str_replace_all(depvar, "_", " "))

coef_plot <- coefdata %>%
  mutate(term = factor(term, levels = c("Science", "World", "Psychology", "Creativity", "Data_science",
                                        "Medium", "Medium_partner_program", "Medium_earnings",
                                        "Writing_tips", "Make_money_online"))) %>%
  ggplot(aes(x = estimate, y = term, color = term)) +
  geom_point(position = position_dodge(width=.75), size = 2) +
  geom_errorbarh(aes(xmin=`2.5 %`, xmax=`97.5 %`), position=position_dodge(width=.75), height=0, size = 1) +
  labs(x="Effect of having tag", y="", colour="Coef") + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~ depvar, scales = "free") +
  scale_color_brewer(palette = "Paired") +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size = 17))

ggsave("./figures/coefplot.png", height = 9, width = 15)


## Illustration

get_pred <- function(tag){
  
  form_claps <- as.formula(paste("claps ~ ", tag, " + publish + word_count + time_up + is_shortform + is_locked | year + month"))
  form_reading <- as.formula(paste("reading_time ~ ", tag, " + publish + word_count + time_up + is_shortform + is_locked | year + month"))
  form_responses <- as.formula(paste("responses_count ~ ", tag, " + publish + word_count + time_up + is_shortform + is_locked | year + month"))
  
  model_claps <- feols(form_claps, 
                       cluster = "year",
                       data = df_model) 
  
  model_reading <- feols(form_reading, 
                         cluster = "year",
                         data = df_model)

  model_responses <- feols(form_responses, 
                           cluster = "year",
                           data = df_model)

  newdata <- tibble(publish = "publication",
                    word_count = log(1000),
                    is_shortform = as.factor(FALSE),
                    is_locked = as.factor(TRUE),
                    is_series = as.factor(FALSE),
                    year = 2024,
                    month = "03",
                    tag = 1,
                    time_up = 3.178054) %>%
    mutate(is_shortform = factor(is_shortform, levels = c("FALSE", "TRUE"))) %>%
    mutate(is_locked = factor(is_locked, levels = c("FALSE", "TRUE"))) %>%
    mutate(is_series = factor(is_series, levels = c("FALSE", "TRUE"))) %>%
    rename(!!tag := tag)
  
  return(list(model_claps, model_reading, model_responses, newdata))
  
}


tibble(tag = c("medium_partner_program", "writing_tips", "medium_earnings", "data_science", "science", "psychology"),
       claps = c(predict(get_pred("medium_partner_program")[[1]], newdata = get_pred("medium_partner_program")[[4]]),
                 predict(get_pred("writing_tips")[[1]], newdata = get_pred("writing_tips")[[4]]),
                 predict(get_pred("medium_earnings")[[1]], newdata = get_pred("medium_earnings")[[4]]),
                 predict(get_pred("data_science")[[1]], newdata = get_pred("data_science")[[4]]),
                 predict(get_pred("science")[[1]], newdata = get_pred("science")[[4]]),
                 predict(get_pred("psychology")[[1]], newdata = get_pred("psychology")[[4]])),
       reading = c(predict(get_pred("medium_partner_program")[[2]], newdata = get_pred("medium_partner_program")[[4]]),
                   predict(get_pred("writing_tips")[[2]], newdata = get_pred("writing_tips")[[4]]),
                   predict(get_pred("medium_earnings")[[2]], newdata = get_pred("medium_earnings")[[4]]),
                   predict(get_pred("data_science")[[2]], newdata = get_pred("data_science")[[4]]),
                   predict(get_pred("science")[[2]], newdata = get_pred("science")[[4]]),
                   predict(get_pred("psychology")[[2]], newdata = get_pred("psychology")[[4]])),
       responses = c(predict(get_pred("medium_partner_program")[[3]], newdata = get_pred("medium_partner_program")[[4]]),
                     predict(get_pred("writing_tips")[[3]], newdata = get_pred("writing_tips")[[4]]),
                     predict(get_pred("medium_earnings")[[3]], newdata = get_pred("medium_earnings")[[4]]),
                     predict(get_pred("data_science")[[3]], newdata = get_pred("data_science")[[4]]),
                     predict(get_pred("science")[[3]], newdata = get_pred("science")[[4]]),
                     predict(get_pred("psychology")[[3]], newdata = get_pred("psychology")[[4]]))) %>%
  mutate(claps = round(claps, 0),
         claps = ifelse(claps <= 0, 0, claps),
         responses = round(responses, 0),
         responses = ifelse(responses <= 0, 0, responses),
         reading = round(reading, 2),
         reading = ifelse(reading <= 0, 0, reading)) %>%
  rename("Claps" = "claps",
         "Reading time" = "reading",
         "Responses count" = "responses",
         "Tag" = "tag") %>%
  kableExtra::kable() %>%
  kableExtra::kable_styling(font_size = 15) %>%
  kableExtra::kable_minimal() %>%
  kableExtra::save_kable("./figures/table.png")



#### PREDICTION MODELS ####

df_model_pred1 <- df_all %>%
  unnest() %>%
  mutate(tags = str_replace_all(tags, "-", "_")) %>%
  mutate(year = substr(published_at, 1, 4)) %>%
  mutate(month = substr(published_at, 6, 7)) %>%
  mutate(published_at = as.POSIXlt(published_at)) %>%
  mutate(time_up = download-published_at) %>%
  mutate(time_up = log(as.numeric(time_up))) %>% 
  mutate(word_count = log(word_count)) %>% 
  mutate(publish = ifelse(publication_id == "*Self-Published*", "self", "publication")) %>%
  mutate(publish = factor(publish, levels = c("self", "publication"))) %>%
  mutate(is_shortform = factor(is_shortform, levels = c("FALSE", "TRUE"))) %>%
  mutate(is_locked = factor(is_locked, levels = c("FALSE", "TRUE"))) %>%
  mutate(is_series = factor(is_series, levels = c("FALSE", "TRUE"))) %>%
  select(id, claps, reading_time, responses_count, word_count, year, month, tags, publish, time_up, is_shortform, is_locked, is_series) %>%
  group_by(tags) %>%
  add_count() 

ggplot(df_model_pred1,
       aes(n)) + 
  geom_histogram(bins = 50)

summary(df_model_pred1$n)

df_model_pred1 <- df_model_pred1 %>%
  filter(n >= 61) %>% # Filtering out tags that occur less than the median of tag occurrence
  select(-n) %>%
  ungroup() %>%
  drop_na()

set.seed(281)

df_model_pred <- df_model_pred1 %>%
  mutate(tag_value = 1) %>%
  pivot_wider(names_from = "tags", values_from = "tag_value") %>%
  unnest() %>%
  mutate(across(13:length(.), ~replace_na(.x, 0)))

split <- initial_split(df_model_pred, 4/5)
train <- training(split)
test <- testing(split)

vars <- paste(colnames(df_model_pred)[13:length(df_model_pred)], collapse = " + ")

pred_model_run <- function(x){
  
  form <- as.formula(paste(x, " ~ publish + word_count + time_up + is_shortform + is_locked + is_series + year + month + id + ", vars))
  
  rec <- recipe(form, data = df_model_pred) %>%
    update_role(id, new_role = "id") %>%
    step_dummy(year, one_hot = TRUE) %>%
    step_dummy(month, one_hot = TRUE) %>%
    step_dummy(publish, one_hot = TRUE) %>%
    #step_dummy(tags, one_hot = TRUE) %>%
    step_dummy(is_shortform, one_hot = TRUE) %>%
    step_dummy(is_locked, one_hot = TRUE) %>%
    step_dummy(is_series, one_hot = TRUE) %>%
    step_zv(all_numeric(), -all_outcomes()) %>%
    step_normalize(all_numeric(), -all_outcomes())
  
  prep <- rec %>%
    prep(strings_as_factors = TRUE)
  
  lasso_spec <- linear_reg(penalty = 0.1, mixture = 1) %>%
    set_engine("glmnet")
  
  wf <- workflow() %>%
    add_recipe(rec)
  
  lasso_fit <- wf %>%
    add_model(lasso_spec) %>%
    fit(data = train)
  
  lasso_fit %>%
    pull_workflow_fit() %>%
    tidy()
  
  boot <- bootstraps(train)
  
  tune_spec <- linear_reg(penalty = tune(), mixture = 1) %>%
    set_engine("glmnet")
  
  lambda_grid <- grid_regular(penalty(), levels = 50)
  
  doParallel::registerDoParallel()
  
  lasso_grid <- tune_grid(
    wf %>% add_model(tune_spec),
    resamples = boot,
    grid = lambda_grid
  )
  
  lowest_rmse <- lasso_grid %>%
    select_best("rmse")
  
  final_lasso <- finalize_workflow(
    wf %>% add_model(tune_spec),
    lowest_rmse
  )
  
  return(list(final_lasso, lowest_rmse))
  
}

### Claps

clap_model <- pred_model_run("claps")

claps_plot_tags <- clap_model[[1]] %>%
  fit(train) %>%
  pull_workflow_fit() %>%
  vi(lambda = clap_model[[2]]$penalty) %>%
  mutate(
    Importance = abs(Importance),
    Variable = forcats::fct_reorder(Variable, Importance)
  ) %>%
  filter(Variable %in% c(colnames(df_model_pred)[13:length(df_model_pred)])) %>%
  arrange(desc(Importance)) %>%
  ggplot(aes(x = Importance, y = fct_reorder(Variable, Importance), fill = Sign)) +
  geom_col() +
  scale_fill_manual(
    values = c("POS" = "#154840",
               "NEG"    = "#ce8386")) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = NULL, x = "Importance of tag") +
  ggtitle("Claps") +
  theme_classic() + 
  theme(legend.position = "none",
        text = element_text(size = 15))

claps_plot <- clap_model[[1]] %>%
  fit(train) %>%
  pull_workflow_fit() %>%
  vi(lambda = clap_model[[2]]$penalty) %>%
  mutate(
    Importance = abs(Importance),
    Variable = forcats::fct_reorder(Variable, Importance)
  ) %>%
  mutate(Variable = as.character(Variable)) %>%
  filter(!Variable %in% c(colnames(df_model_pred)[13:length(df_model_pred)])) %>%
  mutate(Variable = ifelse(Variable == "month_X01", "January",
                           ifelse(Variable == "month_X02", "February",
                                  ifelse(Variable == "month_X03", "March",
                                         ifelse(Variable == "month_X04", "April",
                                                ifelse(Variable == "month_X05", "May",
                                                       ifelse(Variable == "month_X06", "June",
                                                              ifelse(Variable == "month_X07", "July",
                                                                     ifelse(Variable == "month_X08", "August",
                                                                            ifelse(Variable == "month_X09", "September",
                                                                                   ifelse(Variable == "month_X10", "October",
                                                                                          ifelse(Variable == "month_X11", "November",
                                                                                                 ifelse(Variable == "month_X12", "December",
                                                                                                        Variable))))))))))))) %>%
  mutate(Variable = str_replace_all(Variable, "year_X", "Year_")) %>%
  mutate(Variable = str_to_sentence(Variable)) %>%
  arrange(desc(Importance)) %>%
  ggplot(aes(x = Importance, y = fct_reorder(Variable, Importance), fill = Sign)) +
  geom_col() +
  scale_fill_manual(
    values = c("POS" = "#154840",
               "NEG"    = "#ce8386")) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = NULL, x = NULL) +
  ggtitle("Claps") +
  theme_classic() + 
  theme(legend.position = "none",
        text = element_text(size = 15))

### Reading time

reading_time_model <- pred_model_run("reading_time")

reading_time_plot_tags <- reading_time_model[[1]] %>%
  fit(train) %>%
  pull_workflow_fit() %>%
  vi(lambda = reading_time_model[[2]]$penalty) %>%
  mutate(
    Importance = abs(Importance),
    Variable = forcats::fct_reorder(Variable, Importance)
  ) %>%
  filter(Variable %in% c(colnames(df_model_pred)[13:length(df_model_pred)])) %>%
  arrange(desc(Importance)) %>%
  ggplot(aes(x = Importance, y = fct_reorder(Variable, Importance), fill = Sign)) +
  geom_col() +
  scale_fill_manual(
    values = c("POS" = "#154840",
               "NEG"    = "#ce8386")) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = NULL, x = "Importance of tag") +
  ggtitle("Reading time") +
  theme_classic() + 
  theme(legend.position = "bottom",
        text = element_text(size = 15))

reading_time_plot <- reading_time_model[[1]] %>%
  fit(train) %>%
  pull_workflow_fit() %>%
  vi(lambda = reading_time_model[[2]]$penalty) %>%
  mutate(
    Importance = abs(Importance),
    Variable = forcats::fct_reorder(Variable, Importance)
  ) %>%
  mutate(Variable = as.character(Variable)) %>%
  filter(!Variable %in% c(colnames(df_model_pred)[13:length(df_model_pred)])) %>%
  mutate(Variable = ifelse(Variable == "month_X01", "January",
                           ifelse(Variable == "month_X02", "February",
                                  ifelse(Variable == "month_X03", "March",
                                         ifelse(Variable == "month_X04", "April",
                                                ifelse(Variable == "month_X05", "May",
                                                       ifelse(Variable == "month_X06", "June",
                                                              ifelse(Variable == "month_X07", "July",
                                                                     ifelse(Variable == "month_X08", "August",
                                                                            ifelse(Variable == "month_X09", "September",
                                                                                   ifelse(Variable == "month_X10", "October",
                                                                                          ifelse(Variable == "month_X11", "November",
                                                                                                 ifelse(Variable == "month_X12", "December",
                                                                                                        Variable))))))))))))) %>%
  mutate(Variable = str_replace_all(Variable, "year_X", "Year_")) %>%
  mutate(Variable = str_to_sentence(Variable)) %>%
  arrange(desc(Importance)) %>%
  ggplot(aes(x = Importance, y = fct_reorder(Variable, Importance), fill = Sign)) +
  geom_col() +
  scale_fill_manual(
    values = c("POS" = "#154840",
               "NEG"    = "#ce8386")) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = NULL, x = NULL) +
  ggtitle("Reading time") +
  theme_classic() + 
  theme(legend.position = "bottom",
        text = element_text(size = 15))


### Responses

responses_count_model <- pred_model_run("responses_count")

responses_count_plot_tags <- responses_count_model[[1]] %>%
  fit(train) %>%
  pull_workflow_fit() %>%
  vi(lambda = responses_count_model[[2]]$penalty) %>%
  mutate(
    Importance = abs(Importance),
    Variable = forcats::fct_reorder(Variable, Importance)
  ) %>%
  filter(Variable %in% c(colnames(df_model_pred)[13:length(df_model_pred)])) %>%
  arrange(desc(Importance)) %>%
  ggplot(aes(x = Importance, y = fct_reorder(Variable, Importance), fill = Sign)) +
  geom_col() +
  scale_fill_manual(
    values = c("POS" = "#154840",
               "NEG"    = "#ce8386")) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = NULL, x = "Importance of tag") +
  ggtitle("Responses count") +
  theme_classic() + 
  theme(legend.position = "none",
        text = element_text(size = 15))

responses_count_plot <- responses_count_model[[1]] %>%
  fit(train) %>%
  pull_workflow_fit() %>%
  vi(lambda = responses_count_model[[2]]$penalty) %>%
  mutate(
    Importance = abs(Importance),
    Variable = forcats::fct_reorder(Variable, Importance)
  ) %>%
  mutate(Variable = as.character(Variable)) %>%
  filter(!Variable %in% c(colnames(df_model_pred)[13:length(df_model_pred)])) %>%
  mutate(Variable = ifelse(Variable == "month_X01", "January",
                           ifelse(Variable == "month_X02", "February",
                                  ifelse(Variable == "month_X03", "March",
                                         ifelse(Variable == "month_X04", "April",
                                                ifelse(Variable == "month_X05", "May",
                                                       ifelse(Variable == "month_X06", "June",
                                                              ifelse(Variable == "month_X07", "July",
                                                                     ifelse(Variable == "month_X08", "August",
                                                                            ifelse(Variable == "month_X09", "September",
                                                                                   ifelse(Variable == "month_X10", "October",
                                                                                          ifelse(Variable == "month_X11", "November",
                                                                                                 ifelse(Variable == "month_X12", "December",
                                                                                                        Variable))))))))))))) %>%
  mutate(Variable = str_replace_all(Variable, "year_X", "Year_")) %>%
  mutate(Variable = str_to_sentence(Variable)) %>%
  arrange(desc(Importance)) %>%
  ggplot(aes(x = Importance, y = fct_reorder(Variable, Importance), fill = Sign)) +
  geom_col() +
  scale_fill_manual(
    values = c("POS" = "#154840",
               "NEG"    = "#ce8386")) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = NULL, x = NULL) +
  ggtitle("Responses count") +
  theme_classic() + 
  theme(legend.position = "none",
        text = element_text(size = 15))

cowplot::plot_grid(claps_plot_tags,
                   reading_time_plot_tags,
                   responses_count_plot_tags,
                   ncol = 3)

ggsave("./figures/pred_plot_tags.png", height = 9, width = 15)


cowplot::plot_grid(claps_plot,
                   reading_time_plot,
                   responses_count_plot,
                   ncol = 3)

ggsave("./figures/pred_plot_all.png", height = 9, width = 15)
