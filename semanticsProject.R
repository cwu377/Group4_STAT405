library(tidyverse)
library(sentimentr)
library(tidytext)

rm(list=ls())

args = (commandArgs(trailingOnly=TRUE))

if (length(args) < 1) {
  stop("usage: Rscript hw4.R <data file>")
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
  group_by(region = case_when(
    marketplace %in% c("US", "CA", "MX") ~ "North America",
    marketplace %in% c("UK", "DE", "FR", "IT", "ES") ~ "Europe",
    marketplace %in% c("JP") ~ "Asia",
    TRUE ~ "Other"
  ), product_category) %>%
  summarise(avg_sentiment = sum(value), .groups = "drop_last")

out_file = paste(sep="", "df_clean", args[1], ".csv")
write.csv(df_clean, file = out_file, row.names = FALSE)

# Visualize the results
ggplot(data = df_clean, aes(x = region, y = avg_sentiment, fill = product_category)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  ggtitle("Sentiment Analysis by Region and Category") + 
  xlab("Region") + 
  ylab("Average Sentiment")

out_file2 = paste(sep="", args[1], "sentiment_plot.pdf")
ggsave(out_file2, plot = last_plot())

# Hypothesis Testing
# Comparing the mean sentiment scores for different categories and regions

# Perform t-test for mean sentiment scores
ttest_na_eu <- t.test(df_clean$avg_sentiment[df_clean$region == "North America" & df_clean$product_category == "Personal Care Appliances"],
                      df_clean$avg_sentiment[df_clean$region == "Europe" & df_clean$product_category == "Personal Care Appliances"])
ttest_na_as <- t.test(df_clean$avg_sentiment[df_clean$region == "North America" & df_clean$product_category == "Personal Care Appliances"],
                      df_clean$avg_sentiment[df_clean$region == "Asia" & df_clean$product_category == "Personal Care Appliances"])
ttest_na_ot <- t.test(df_clean$avg_sentiment[df_clean$region == "North America" & df_clean$product_category == "Personal Care Appliances"],
                      df_clean$avg_sentiment[df_clean$region == "Other" & df_clean$product_category == "Personal Care Appliances"])

# Print the t-test results
print(ttest_na_eu)
print(ttest_na_as)
print(ttest_na_ot)

out_file3 = paste(sep="", "ttest_results", args[1], ".csv")

# Write the t-test results to a CSV file
write.csv(list("NA vs. EU" = ttest_na_eu, "NA vs. Asia" = ttest_na_as, "NA vs. Other" = ttest_na_ot),
          file = out_file3, row.names = FALSE)

# Regression Analysis
# Determining the relationship between sentiment of reviews and product category, brand, and region

# Subset the data for relevant variables
df_relevant <- df_clean %>%
  select(product_category, region, value)

# Create dummy variables for categorical variables
df_dummy <- df_relevant %>%
  mutate(product_category = as.factor(product_category),
         region = as.factor(region)) %>%
  pivot_wider(names_from = product_category, values_from = value) %>%
  pivot_wider(names_from = region, values_from = value)

# Create the linear regression model
model <- lm(. ~ ., data = df_dummy)

# Perform ANOVA for model comparison
anova(model)

out_file4 = paste(sep="", "anova_results", args[1], ".csv")

# Write the ANOVA results to a CSV file
write.csv(summary(anova(model)), file = out_file4)
         
