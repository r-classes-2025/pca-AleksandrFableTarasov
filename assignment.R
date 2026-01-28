# установите и загрузите пакеты
library(friends)
library(tidyverse)
library(tidytext)
library(factoextra) 

# 1. отберите 6 главных персонажей (по количеству реплик)
top_speakers <- friends |> 
  count(speaker, sort = TRUE) |> 
  slice_head(n = 6) |> 
  pull(speaker) |> 
  as.character()

# 2. отфильтруйте топ-спикеров, токенизируйте реплики, удалите цифры
# ВАЖНО: НЕ удаляйте однобуквенные слова (только пустые строки после удаления цифр)
friends_tokens <- friends |> 
  filter(speaker %in% top_speakers) |> 
  unnest_tokens(word, text) |> 
  mutate(word = str_remove_all(word, "\\d+")) |> 
  filter(word != "") |>  # удаляем ТОЛЬКО пустые строки
  select(speaker, word)

# 3. отберите по 500 самых частотных слов для каждого персонажа
# относительные частоты считаем относительно ВСЕХ слов спикера
speaker_totals <- friends_tokens |>
  count(speaker, name = "total_words")

friends_tf <- friends_tokens |>
  count(speaker, word, name = "n_words") |> 
  left_join(speaker_totals, by = "speaker") |> 
  group_by(speaker) |> 
  arrange(desc(n_words), word, .by_group = TRUE) |> 
  slice_head(n = 500) |> 
  mutate(tf = n_words / total_words) |> 
  ungroup() |> 
  arrange(factor(speaker, levels = top_speakers)) |>  # фиксируем порядок спикеров
  select(speaker, word, tf)

# 4. преобразуйте в широкий формат
friends_tf_wide <- friends_tf |> 
  pivot_wider(
    names_from = word, 
    values_from = tf, 
    values_fill = 0
  ) |> 
  column_to_rownames("speaker") |> 
  as.data.frame()

# СТРОГО алфавитный порядок столбцов (критично для воспроизводимости)
friends_tf_wide <- friends_tf_wide[, order(colnames(friends_tf_wide))]

# 5. кластеризация k-means
set.seed(123)
scaled_data <- scale(friends_tf_wide)
km.out <- kmeans(scaled_data, centers = 3, nstart = 20)

# 6. PCA
pca_fit <- prcomp(friends_tf_wide, center = TRUE, scale. = TRUE)

# 7. биплот
q <- fviz_pca_biplot(
  pca_fit,
  label = "var",
  select.var = list(cos2 = 20),
  geom.var = "text",
  geom.ind = "text",
  col.ind = as.factor(km.out$cluster),
  col.var = "gray50",
  repel = TRUE,
  labelsize = 3,
  pointsize = 0,
  addlabels = FALSE
) +
  geom_text(
    aes(label = rownames(friends_tf_wide)),
    size = 5,
    fontface = "bold"
  ) +
  labs(
    title = "PCA Biplot: Лексические особенности персонажей Friends",
    subtitle = "Цвета показывают кластеры по k-means (k=3)"
  ) +
  theme_minimal()

print(q)
