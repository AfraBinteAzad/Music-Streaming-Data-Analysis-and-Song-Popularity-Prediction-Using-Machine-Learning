install.packages('tidyverse')
install.packages('Amelia')

library(tidyverse)
library(Amelia)


df <- read.csv("C:/Users/User/Desktop/Data Analysis/Spotify and Youtube data analysis/Dataset.csv")
colnames(df)
df <- df[,-1]
colnames(df)
glimpse(df)
summary(df)
df[df == ""] <- NA
colSums(is.na(df))
round(colMeans(is.na(df))*100,2)

total_missingness <- sum(is.na(df))/(nrow(df)*ncol(df))*100
total_missingness

missing_df <- data.frame(
  Variable = names(df),
  MissingPercent = round(colMeans(is.na(df)) * 100, 2)
)

missing_df <- data.frame(
  Variable = names(df),
  MissingPercent = round(colMeans(is.na(df)) * 100, 2)
)

ggplot(missing_df, aes(x = reorder(Variable, -MissingPercent), 
                       y = MissingPercent)) +
  geom_col(fill = "steelblue") +
  labs(title = "Percentage of Missing Values per Variable",
       x = "Variable",
       y = "Missing (%)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))



missmap(df, main = "Missing Map", col = c('yellow','black'), Legend = TRUE)

str(df)

audio_cols <- c("Danceability","Energy","Key","Loudness","Speechiness",
                "Acousticness","Instrumentalness","Liveness","Valence",
                "Tempo","Duration_ms")

for(col in audio_cols){
  df[[col]][is.na(df[[col]])] <- median(df[[col]],na.rm=TRUE)
}

colSums(is.na(df))
df$missing_youtube <- ifelse(is.na(df$Url_youtube), 1, 0)
df$missing_stream   <- ifelse(is.na(df$Stream), 1, 0)
df$missing_engagement <- ifelse(is.na(df$Views) | is.na(df$Likes) | is.na(df$Comments), 1, 0)

colnames(df)

