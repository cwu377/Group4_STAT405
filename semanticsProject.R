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
df_clean <- read_csv(args[1], col_types = col_types)

# Visualize the results
ggplot(data = df_clean, aes(x = season, y = avg_sentiment, fill = product_category)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  ggtitle("Sentiment Analysis by Season and Category") + 
  xlab("Season") + 
  ylab("Average Sentiment")

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


                                                     