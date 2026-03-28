# title: "text_analysis"

# Libraries:
library(tidyverse)
library(tidytext)
library(wordcloud)
library(scales)

afinn_lexicon <- get_sentiments("afinn")
nrc_lexicon <- get_sentiments("nrc")

load("data/cnbc_immigration_2016.rda")
load("data/cnbc_immigration_2024.rda")
load("data/vox_immigration_2016.rda")
load("data/vox_immigration_2024.rda")
load("data/dw_immigration_2016.rda")
load("data/dw_immigration_2024.rda")

# CNBC Sentiment Data
cnbc_nrc_articles <-
  cnbc_immigration_2016 |>
  inner_join(nrc_lexicon, by = "word") |>
  filter(sentiment %in% c("anger", 
                          "anticipation", 
                          "disgust",
                          "fear", 
                          "joy", 
                          "sadness",
                          "suprise", 
                          "trust")) |>
  group_by(sentiment) |>
  arrange(sentiment, desc(n))

cnbc_afinn_2016 <-
  cnbc_immigration_2016|>
  inner_join(afinn_lexicon, by = "word") |>
  group_by(value) |>
  arrange(value, desc(n))

cnbc_afinn_2024 <-
  cnbc_immigration_2024|>
  inner_join(afinn_lexicon, by = "word") |>
  group_by(value) |>
  arrange(value, desc(n))

# VOX Sentiment Data 
vox_nrc_articles <-
  vox_immigration_2016 |>
  inner_join(nrc_lexicon, by = "word") |>
  filter(sentiment %in% c("anger", 
                          "anticipation", 
                          "disgust",
                          "fear", 
                          "joy", 
                          "sadness",
                          "suprise", 
                          "trust")) |>
  group_by(sentiment) |>
  arrange(sentiment, desc(n))

vox_afinn_2016 <-
  vox_immigration_2016|>
  inner_join(afinn_lexicon, by = "word") |>
  group_by(value) |>
  arrange(value, desc(n))

vox_afinn_2024 <-
  vox_immigration_2024|>
  inner_join(afinn_lexicon, by = "word") |>
  group_by(value) |>
  arrange(value, desc(n))


# Dailywire Sentiment Data 
dw_nrc_articles <-
  dw_immigration_2016 |>
  inner_join(nrc_lexicon, by = "word") |>
  filter(sentiment %in% c("anger", 
                          "anticipation", 
                          "disgust",
                          "fear", 
                          "joy", 
                          "sadness",
                          "suprise", 
                          "trust")) |>
  group_by(sentiment) |>
  arrange(sentiment, desc(n))

dw_afinn_2016 <-
  dw_immigration_2016|>
  inner_join(afinn_lexicon, by = "word") |>
  group_by(value) |>
  arrange(value, desc(n))

dw_afinn_2024 <-
  dw_immigration_2024|>
  inner_join(afinn_lexicon, by = "word") |>
  group_by(value) |>
  arrange(value, desc(n))


# CNBC WordClouds
set.seed(66)
png("images/cnbc_wc2016.png", width = 800, height = 800) 
cnbc_immigration_2016 |>
  with(wordcloud(words = word, 
                 freq = n,
                 max.words = 50,
                 random.order = TRUE,
                 scale = c(7.5, 1),
                 rot.per = 0.15,
                 colors = brewer.pal(8, "Dark2"),
                 family = "sans"))
dev.off()

set.seed(66)
png("images/cnbc_wc2024.png", width = 800, height = 800) 
cnbc_immigration_2024 |>
  with(wordcloud(words = word, 
                 freq = n,
                 max.words = 50,
                 random.order = TRUE,
                 scale = c(7.5, 1), 
                 rot.per = 0.15,
                 colors = brewer.pal(8, "Dark2"),
                 family = "sans"))
dev.off()


# CNBC Sentiment Analysis
cnbc_sentiment_2016 <- 
  cnbc_afinn_2016 |> 
  summarise(
    count = sum(n),
    temp = sum(n*value),
  ) |>
  summarise(
    avg_sentiment = sum(temp)/sum(count),
    year = 2016
  )

cnbc_sentiment_2024 <- 
  cnbc_afinn_2024 |> 
  summarise(
    count = sum(n),
    temp = sum(n*value)
  ) |>
  summarise(
    avg_sentiment = sum(temp)/sum(count),
    year = 2024
  )

cnbc_sentiment <- bind_rows(cnbc_sentiment_2016, cnbc_sentiment_2024)


ggplot(cnbc_sentiment, aes(x = factor(year), y = avg_sentiment, fill = year)) +
  geom_col(show.legend = FALSE, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "CNBC Immigration Sentiment Trends",
    x = "Year",
    y = "Sentiment Score"
  ) +
  ylim(-0.08, 0.08) +
  theme(axis.text.x = element_text(size = 14, face = "bold"),
        axis.title.x = element_text(face = "bold", size = 18),
        axis.title.y = element_text(face = "bold", size = 18, margin = margin(r = 12)),
        axis.text.y = element_text(size = 12)
  )

ggsave("images/cnbc_sentiment_trends.jpg", 
       width = 10, 
       height = 6, 
       units = "in", 
       dpi = 300)


# Vox WordClouds
set.seed(66)
png("images/vox_wc2024.png", width = 800, height = 800) 
vox_immigration_2024 |>
  with(wordcloud(words = word, 
                 freq = n,
                 max.words = 50,
                 random.order = TRUE,
                 scale = c(7.5, 1), 
                 rot.per = 0.15,
                 colors = brewer.pal(8, "Dark2"),
                 family = "sans"))
dev.off()

set.seed(66)
png("images/vox_wc2016.png", width = 800, height = 800) 
vox_immigration_2016 |>
  with(wordcloud(words = word, 
                 freq = n,
                 max.words = 50,
                 random.order = TRUE,
                 scale = c(7.5, 1), 
                 rot.per = 0.15,
                 colors = brewer.pal(8, "Dark2"),
                 family = "sans"))
dev.off()


# Vox Sentiment Analysis
vox_sentiment_2016 <- 
  vox_afinn_2016 |> 
  summarise(
    count = sum(n),
    temp = sum(n*value)
  ) |>
  summarise(
    avg_sentiment = sum(temp)/sum(count),
    year = 2016
  )

vox_sentiment_2024 <- 
  vox_afinn_2024 |> 
  summarise(
    count = sum(n),
    temp = sum(n*value)
  ) |>
  summarise(
    avg_sentiment = sum(temp)/sum(count),
    year = 2024
  )

vox_sentiment <- bind_rows(vox_sentiment_2016, vox_sentiment_2024)


ggplot(vox_sentiment, aes(x = factor(year), y = avg_sentiment, fill = year)) +
  geom_col(show.legend = FALSE, alpha = 0.7) +
  labs(
    title = "Immigration Sentiment Trends - Election Years (2016 & 2024)", 
    x = "Year",  
    y = "Sentiment Score" 
  ) +
  ylim(-0.3, 0.1) +
  theme(
    axis.text.x = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 18, margin = margin(r = 12)),
    axis.text.y = element_text(size = 12)
  )

ggsave("images/vox_sentiment_trends.jpg", 
       width = 10, 
       height = 6, 
       units = "in", 
       dpi = 300)


# Dailywire WordClouds
set.seed(66)
png("images/dw_wc2016.png", width = 800, height = 800) 
dw_immigration_2016 |>
  with(wordcloud(words = word, 
                 freq = n,
                 max.words = 50,
                 random.order = TRUE,
                 scale = c(7.5, 1), 
                 rot.per = 0.15,
                 colors = brewer.pal(8, "Dark2"),
                 family = "sans"))
dev.off()

set.seed(66)
png("images/dw_wc2024.png", width = 800, height = 800) 
dw_immigration_2024 |>
  with(wordcloud(words = word, 
                 freq = n,
                 max.words = 50,
                 random.order = TRUE,
                 scale = c(7.5, 1), 
                 rot.per = 0.15,
                 colors = brewer.pal(8, "Dark2"),
                 family = "sans"))
dev.off()


# Dailywire Sentiment Analysis
dw_sentiment_2016 <- 
  dw_afinn_2016 |> 
  summarise(
    count = sum(n),
    temp = sum(n*value),
  ) |>
  summarise(
    avg_sentiment = sum(temp)/sum(count),
    year = 2016
  )

dw_sentiment_2024 <- 
  dw_afinn_2024 |> 
  summarise(
    count = sum(n),
    temp = sum(n*value)
  ) |>
  summarise(
    avg_sentiment = sum(temp)/sum(count),
    year = 2024
  )

dw_sentiment <- bind_rows(dw_sentiment_2016, dw_sentiment_2024)


ggplot(dw_sentiment, aes(x = factor(year), y = avg_sentiment, fill = year)) +
  geom_col(show.legend = FALSE, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Dailywire Immigration Sentiment Trends",
    x = "Year",
    y = "Sentiment Score"
  ) +
  ylim(-1.4, 0.1) +
  theme(
    axis.text.x = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 18, margin = margin(r = 12)),
    axis.text.y = element_text(size = 12)
  )

ggsave("images/dw_sentiment_trends.jpg", 
       width = 10, 
       height = 6, 
       units = "in", 
       dpi = 300)


# CNBC Bar Plot of Emotions
cnbc_emotion_bars_2016 <- cnbc_immigration_2016 |>
  inner_join(nrc_lexicon, by = "word") |>
  filter(sentiment %in% c("anger", 
                          "anticipation", 
                          "disgust",
                          "fear", 
                          "joy", 
                          "sadness",
                          "surprise", 
                          "trust")) |>
  group_by(sentiment) |>
  summarise(count = sum(n)) |>
  mutate(proportion = (count / sum(count)),
         year = "2016") |> 
  mutate(house = "CNBC")

cnbc_emotion_bars_2024 <- cnbc_immigration_2024 |>
  inner_join(nrc_lexicon, by = "word") |>
  filter(sentiment %in% c("anger", 
                          "anticipation", 
                          "disgust",
                          "fear", 
                          "joy", 
                          "sadness",
                          "surprise", 
                          "trust")) |>
  group_by(sentiment) |>
  summarise(count = sum(n)) |>
  mutate(proportion = (count / sum(count)),
         year = "2024") |> 
  mutate(house = "CNBC")

cnbc_emotion_bars <- bind_rows(cnbc_emotion_bars_2016, cnbc_emotion_bars_2024)


ggplot(cnbc_emotion_bars, aes(x = sentiment, y = proportion, fill = year)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_y_continuous(labels = percent, breaks = seq(0, 1, 0.05) ) +
  scale_fill_manual(values = c("2016" = "#0087D2", "2024" = "#F37023")) +
  scale_x_discrete(
    labels = c(
      "anger" = "Anger",
      "anticipation" = "Anticipation",
      "disgust" = "Disgust",
      "fear" = "Fear",
      "joy" = "Joy",
      "sadness" = "Sadness",
      "surprise" = "Surprise",
      "trust" = "Trust")) +
  labs(title = paste0("Proportion of Sentiments in ",
                      "CNBC Immigration Coverage (2016 vs. 2024)"),
       x = "Emotion",
       y = "Proportion of Total Words",
       fill = "Year") +
  theme_gray() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.title.x = element_text(face = "bold", size = 18),
        axis.title.y = element_text(face = "bold", size = 18, margin = margin(r = 12)),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),        
        legend.title = element_text(size = 14, face = "bold"))

ggsave("images/cnbc_emotions_barplot.png", 
       width = 10, 
       height = 6, 
       units = "in", 
       dpi = 300)


# Vox Bar Plot of Emotions
vox_emotion_bars_2016 <- vox_immigration_2016 |>
  inner_join(nrc_lexicon, by = "word") |>
  filter(sentiment %in% c("anger", 
                          "anticipation", 
                          "disgust",
                          "fear", 
                          "joy", 
                          "sadness",
                          "surprise", 
                          "trust")) |>
  group_by(sentiment) |>
  summarise(count = sum(n)) |>
  mutate(proportion = (count / sum(count)),
         year = "2016") |> 
  mutate(house = "Vox")

vox_emotion_bars_2024 <- vox_immigration_2024 |>
  inner_join(nrc_lexicon, by = "word") |>
  filter(sentiment %in% c("anger", 
                          "anticipation", 
                          "disgust",
                          "fear", 
                          "joy", 
                          "sadness",
                          "surprise", 
                          "trust")) |>
  group_by(sentiment) |>
  summarise(count = sum(n)) |>
  mutate(proportion = (count / sum(count)),
         year = "2024") |> 
  mutate(house = "Vox")

vox_emotion_bars <- bind_rows(vox_emotion_bars_2016, vox_emotion_bars_2024)


ggplot(vox_emotion_bars, aes(x = sentiment, y = proportion, fill = year)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_y_continuous(labels = percent, breaks = seq(0, 1, 0.05) ) +
  scale_fill_manual(values = c("2016" = "#FFD700", "2024" = "#696969")) +
  scale_x_discrete(
    labels = c(
      "anger" = "Anger",
      "anticipation" = "Anticipation",
      "disgust" = "Disgust",
      "fear" = "Fear",
      "joy" = "Joy",
      "sadness" = "Sadness",
      "surprise" = "Surprise",
      "trust" = "Trust")) +
  labs(title = paste0("Proportion of Sentiments in ",
                      "Vox Immigration Coverage (2016 vs. 2024)"),
       x = "Emotion",
       y = "Proportion of Total Words",
       fill = "Year") +
  theme_gray() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.title.x = element_text(face = "bold", size = 18),
        axis.title.y = element_text(face = "bold", size = 18, margin = margin(r = 12)),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),        
        legend.title = element_text(size = 14, face = "bold"))

ggsave("images/vox_emotions_barplot.png", 
       width = 10, 
       height = 6, 
       units = "in", 
       dpi = 300)


# Dailywire Bar Plot of Emotions
dw_emotion_bars_2016 <- dw_immigration_2016 |>
  inner_join(nrc_lexicon, by = "word") |>
  filter(sentiment %in% c("anger", 
                          "anticipation", 
                          "disgust",
                          "fear", 
                          "joy", 
                          "sadness",
                          "surprise", 
                          "trust")) |>
  group_by(sentiment) |>
  summarise(count = sum(n)) |>
  mutate(proportion = (count / sum(count)),
         year = "2016") |> 
  mutate(house = "The Daily Wire")

dw_emotion_bars_2024 <- dw_immigration_2024 |>
  inner_join(nrc_lexicon, by = "word") |>
  filter(sentiment %in% c("anger", 
                          "anticipation", 
                          "disgust",
                          "fear", 
                          "joy", 
                          "sadness",
                          "surprise", 
                          "trust")) |>
  group_by(sentiment) |>
  summarise(count = sum(n)) |>
  mutate(proportion = (count / sum(count)),
         year = "2024") |> 
  mutate(house = "The Daily Wire")

dw_emotion_bars <- bind_rows(dw_emotion_bars_2016, dw_emotion_bars_2024)


ggplot(dw_emotion_bars, aes(x = sentiment, y = proportion, fill = year)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_y_continuous(labels = percent, breaks = seq(0, 1, 0.05) ) +
  scale_fill_manual(values = c("2016" = "#DC143C", "2024" = "#36454F")) +
  scale_x_discrete(
    labels = c(
      "anger" = "Anger",
      "anticipation" = "Anticipation",
      "disgust" = "Disgust",
      "fear" = "Fear",
      "joy" = "Joy",
      "sadness" = "Sadness",
      "surprise" = "Surprise",
      "trust" = "Trust")) +
  labs(title = paste0("Proportion of Sentiments in ",
                      "The Daily Wire Immigration Coverage (2016 vs. 2024)"),
       x = "Emotion",
       y = "Proportion of Total Words",
       fill = "Year") +
  theme_gray() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.title.x = element_text(face = "bold", size = 18),
        axis.title.y = element_text(face = "bold", size = 18, margin = margin(r = 12)),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),        
        legend.title = element_text(size = 14, face = "bold"))

ggsave("images/dw_emotions_barplot.png", 
       width = 10, 
       height = 6, 
       units = "in", 
       dpi = 300)


# Comparative Analysis
emotion_bars <- bind_rows(
  cnbc_emotion_bars_2016,
  cnbc_emotion_bars_2024,
  vox_emotion_bars_2016,
  vox_emotion_bars_2024,
  dw_emotion_bars_2016,
  dw_emotion_bars_2024) |>
  mutate(house = factor(house, levels = c("Vox", "CNBC", "The Daily Wire")))


ggplot(emotion_bars |> filter(year == "2016"), 
       aes(x = sentiment, y = proportion, fill = house)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_y_continuous(labels = percent, breaks = seq(0, 1, 0.05)) +
  scale_fill_manual(values = c("CNBC" = "#F37023", 
                               "Vox" = "#FFD700", 
                               "The Daily Wire" = "#DC143C")) +
  scale_x_discrete(
    labels = c(
      "anger" = "Anger",
      "anticipation" = "Anticipation",
      "disgust" = "Disgust",
      "fear" = "Fear",
      "joy" = "Joy",
      "sadness" = "Sadness",
      "surprise" = "Surprise",
      "trust" = "Trust")) +
  labs(
    title = "Emotion Coverage in 2016",
    x = "Emotion",
    y = "Proportion of Total Words",
    fill = "News House"
  ) +
  theme_gray() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.title.x = element_text(face = "bold", size = 18),
        axis.title.y = element_text(face = "bold", size = 18, margin = margin(r = 12)),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),        
        legend.title = element_text(size = 14, face = "bold"))

ggsave("images/comparison_barplot_2016.png", 
       width = 10, 
       height = 6, 
       units = "in", 
       dpi = 300)


ggplot(emotion_bars |> filter(year == "2024"),
       aes(x = sentiment, y = proportion, fill = house)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_y_continuous(labels = percent, breaks = seq(0, 1, 0.05)) +
  scale_fill_manual(values = c("CNBC" = "#F37023", 
                               "Vox" = "#FFD700", 
                               "The Daily Wire" = "#DC143C")) +
  scale_x_discrete(
    labels = c(
      "anger" = "Anger",
      "anticipation" = "Anticipation",
      "disgust" = "Disgust",
      "fear" = "Fear",
      "joy" = "Joy",
      "sadness" = "Sadness",
      "surprise" = "Surprise",
      "trust" = "Trust")) +
  labs(
    title = "Emotion Coverage in 2024",
    x = "Emotion",
    y = "Proportion of Total Words",
    fill = "News House"
  ) +
  theme_gray() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.title.x = element_text(face = "bold", size = 18),
        axis.title.y = element_text(face = "bold", size = 18, margin = margin(r = 12)),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),        
        legend.title = element_text(size = 14, face = "bold"))

ggsave("images/comparison_barplot_2024.png", 
       width = 10, 
       height = 6, 
       units = "in", 
       dpi = 300)



