if (require("tidyverse")) {
  print("Loaded package tidyverse.")
} else {
  print("Failed to load package tidyverse.")  
}

if (require("sentimentr")) {
  print("Loaded package sentimentr.")
} else {
  print("Failed to load package tidyverse.")  
}

if (require("tidytext")) {
  print("Loaded package tidytext.")
} else {
  print("Failed to load package tidyverse.")  
}

rm(list=ls())

args = (commandArgs(trailingOnly=TRUE))

if (length(args) < 1) {
  stop("usage: Rscript read_data.R <data file>")
}

col_types <- cols(
  marketplace = col_character(),
  customer_id = col_character(),
  review_id = col_character(),
  product_id = col_character(),
  product_parent = col_integer(),
  product_title = col_character(),
  product_category = col_character(),
  star_rating = col_integer(),
  helpful_votes = col_integer(),
  total_votes = col_integer(),
  vine = col_character(),
  verified_purchase = col_character(),
  review_headline = col_character(),
  review_body = col_character(),
  review_date = col_date(format = "%Y-%m-%d")
)

# Load the AFINN lexicon
afinn = read_delim("AFINN-111.txt", delim = "\t", col_names = c("word", "value"), col_types = cols(word = col_character(), value = col_integer()), skip = 0)


# Load the dataset
df <- read_tsv(args[1], col_types = col_types)

df <- df %>% select(review_id, review_body, star_rating, review_date, product_id, product_category) %>%
  mutate(season = case_when(
    as.numeric(format(review_date, "%m")) %in% 3:5 ~ "Spring",
    as.numeric(format(review_date, "%m")) %in% 6:8 ~ "Summer",
    as.numeric(format(review_date, "%m")) %in% 9:11 ~ "Fall",
    TRUE ~ "Winter"))

# Preprocess the data
df_clean <- df %>%
  mutate(review_body = str_to_lower(review_body)) %>% 
  unnest_tokens(word, review_body) %>%
  anti_join(get_stopwords(language = "en"), by = "word") %>%
  mutate(word = str_remove_all(word, "[^[:alnum:]]"),
         word_len = str_length(word)) %>%
  filter(word_len > 2) 

# Calculate the sentiment score for the whole review
df_sentiment <- df_clean %>%
  group_by(review_id, product_category) %>%
  summarize(sentiment = sum(afinn$value[match(word, afinn$word)], na.rm = TRUE)) %>%
  ungroup() %>% 
  rename(product_category_s = product_category)

# Join the sentiment score to the original dataset
df_result <- df %>%
  select(-review_body) %>%
  inner_join(df_sentiment, by = "review_id")


# Categorize sentiment as positive, negative, or neutral
df_result <- df_result %>%
  mutate(sentiment_category = case_when(
    sentiment > 0 ~ "positive",
    sentiment < 0 ~ "negative",
    TRUE ~ "neutral"
  ))

out_file = paste(sep="", "df_result_", args[1], ".csv")
write.csv(df_result, file = out_file, row.names = FALSE)
