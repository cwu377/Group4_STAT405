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

# Load the dataset
df_result <- read_csv(args[1])

# Group the results by season and sentiment category
df_season_sentiment <- df_result %>%
  group_by(season, sentiment_category) %>%
  summarize(num_reviews = n()) %>%
  ungroup()

pdf(NULL)

category_name <- sub("^.*_us_(\\w+)_v1.*$", "\\1", args[1])

# Create a grouped bar chart of sentiment by season and category
ggplot(df_season_sentiment, aes(x = season, y = num_reviews, fill = sentiment_category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = paste("Sentiment by Season and Category", category_name),
       x = "Season",
       y = "Number of Reviews") +
  scale_fill_manual(values = c("#FC4E07", "#FEB019", "#00AFBB"), labels = c("Negative", "Neutral", "Positive"))

out_file1 = paste(sep="", args[1], "sentiment_plot.pdf")
ggsave(out_file1, plot = last_plot())

# Hypothesis Testing
# Comparing the mean sentiment scores for different sentiment categories and seasons

# Loop through the seasons and perform t-test for mean sentiment scores
seasons <- unique(df_result$season)

ttest_results <- data.frame(season = character(), p_value = numeric(), stringsAsFactors = FALSE)

for (season in seasons) {
  ttest_result <- t.test(df_result$sentiment[df_result$season == season], mu = mean(df_result$sentiment))
  ttest_results <- rbind(ttest_results, data.frame(season = season, p_value = ttest_result$p.value, stringsAsFactors = FALSE))
}

# View the t-test results
print(ttest_results)
out_file2 <- paste0("ttest_", args[1], ".csv")
write.csv(ttest_results, file = out_file2, row.names = FALSE)

# Perform ANOVA for mean sentiment scores
anova_result <- anova(aov(sentiment ~ season, data = df_result))

# View the ANOVA results
print(anova_result)
out_file3 = paste(sep="", "anova_", args[1], ".csv")
write.csv(anova_result, file = out_file3, row.names = FALSE)


                                                     
