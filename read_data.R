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

# Load the dataset
df <- read_tsv(args[1], col_types = col_types)

# df <- read_tsv("./Desktop/Rohit/Stat405/Group4_STAT405/data/amazon_reviews_us_Mobile_Electronics_v1_00.tsv", col_types = col_types)

# Preprocess the data
df_clean <- df %>%
  select(review_id, review_body, star_rating, review_date, product_id, marketplace, product_category) %>%
  mutate(review_body = str_to_lower(review_body)) %>%
  unnest_tokens(word, review_body) %>%
  anti_join(get_stopwords(language = "en"), by = "word") %>%
  mutate(word = str_remove_all(word, "[^[:alnum:]]"),
         word_len = str_length(word)) %>%
  filter(word_len > 2) %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(season = case_when(
    as.numeric(format(review_date, "%m")) %in% 3:5 ~ "Spring",
    as.numeric(format(review_date, "%m")) %in% 6:8 ~ "Summer",
    as.numeric(format(review_date, "%m")) %in% 9:11 ~ "Fall",
    TRUE ~ "Winter"
  ), product_category) %>%
  summarise(avg_sentiment = sum(value), .groups = "drop_last")

out_file = paste(sep="", "df_clean", args[1], ".csv")
write.csv(df_clean, file = out_file, row.names = FALSE)