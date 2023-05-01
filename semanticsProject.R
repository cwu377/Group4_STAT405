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
  stop("usage: Rscript semanticsProject.R <data file>")
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
df_result <- read_csv(args[1], col_types = col_types)

# Group the results by season and sentiment category
df_season_sentiment <- df_result %>%
  group_by(season, product_category, sentiment_category) %>%
  summarize(num_reviews = n()) %>%
  ungroup()

# Create a grouped bar chart of sentiment by season and category
ggplot(df_season_sentiment, aes(x = season, y = num_reviews, fill = sentiment_category)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ product_category, ncol = 2) +
  labs(title = "Sentiment by Season and Category",
       x = "Season",
       y = "Number of Reviews") +
  scale_fill_manual(values = c("#FC4E07", "#FEB019", "#00AFBB"), labels = c("Negative", "Neutral", "Positive"))

out_file1 = paste(sep="", args[1], "sentiment_plot.pdf")
ggsave(out_file1, plot = last_plot())

# Hypothesis Testing
# Comparing the mean sentiment scores for different categories and seasons

# Identify the top 5 categories by count of reviews
top_categories <- names(sort(table(df_clean$product_category), decreasing = TRUE)[1:5])

# Loop through the top categories and perform t-test for mean sentiment scores
ttest_results <- list()
for (category in top_categories) {
  for (season in unique(df_clean$season)) {
    ttest_result <- t.test(df_clean$avg_sentiment[df_clean$season == season & df_clean$product_category == category])
    ttest_results[[paste(category, season)]] <- ttest_result
  }
}

# View the t-test results
cat(ttest_results)
out_file2 = paste(sep="", "ttest_", args[1], ".csv")
write.csv(ttest_results, file = out_file2, row.names = FALSE)


# Perform ANOVA for mean sentiment scores
anova_result <- anova(aov(avg_sentiment ~ season * product_category, data = df_clean))

# View the ANOVA results
cat(anova_result)
out_file3 = paste(sep="", "anova_", args[1], ".csv")
write.csv(anova_result, file = out_file3, row.names = FALSE)


                                                     
