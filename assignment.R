# установите и загрузите пакеты
library(friends)
library(tidyverse)
library(tidytext)
library(factoextra) 

# 1. отберите 6 главных персонажей (по количеству реплик)
# сохраните как символьный вектор, упорядоченный по убыванию
top_speakers <- friends |> 
  count(speaker, sort = TRUE) |> 
  slice_max(n, n = 6) |>  # современная замена устаревшему top_n()
  pull(speaker) |> 
  as.character()

# 2. отфильтруйте топ-спикеров, 
# токенизируйте их реплики, удалите из них цифры
# столбец с токенами должен называться word
# оставьте только столбцы speaker, word
friends_tokens <- friends |> 
  filter(speaker %in% top_speakers) |> 
  unnest_tokens(word, text) |> 
  mutate(word = str_remove_all(word, "\\d+")) |> 
  filter(word != "", str_length(word) > 1) |>  # убираем однобуквенные слова
  select(speaker, word)

# 3. КРИТИЧЕСКОЕ ИСПРАВЛЕНИЕ: 
# Относительные частоты должны считаться относительно ВСЕХ слов спикера,
# а не только топ-500. Сначала считаем общее количество слов на спикера.
speaker_totals <- friends_tokens |>
  count(speaker, name = "total_words")

friends_tf <- friends_tokens |>
  count(speaker, word, name = "n_words") |> 
  left_join(speaker_totals, by = "speaker") |> 
  group_by(speaker) |> 
  arrange(desc(n_words), word, .by_group = TRUE) |> 
  slice_head(n = 500) |> 
  mutate(tf = n_words / total_words) |>  # ПРАВИЛЬНЫЙ расчёт: относительно всех слов спикера
  ungroup() |> 
  # Гарантируем порядок строк как в top_speakers (по убыванию количества реплик)
  arrange(factor(speaker, levels = top_speakers)) |> 
  select(speaker, word, tf)

# 4. преобразуйте в широкий формат; 
# столбец c именем спикера превратите в имя ряда
friends_tf_wide <- friends_tf |> 
  pivot_wider(
    names_from = word, 
    values_from = tf, 
    values_fill = 0
  ) |> 
  column_to_rownames("speaker") |> 
  as.data.frame()

# Упорядочиваем столбцы по алфавиту для воспроизводимости
friends_tf_wide <- friends_tf_wide[, order(colnames(friends_tf_wide))]

# 5. установите зерно 123
# проведите кластеризацию k-means (k = 3) на относительных значениях частотности (nstart = 20)
# используйте scale()
set.seed(123)
scaled_data <- scale(friends_tf_wide)
km.out <- kmeans(scaled_data, centers = 3, nstart = 20)

# 6. примените к матрице метод главных компонент (prcomp)
# центрируйте и стандартизируйте, использовав аргументы функции
pca_fit <- prcomp(friends_tf_wide, center = TRUE, scale. = TRUE)

# 7. Покажите наблюдения и переменные вместе (биплот)
# в качестве геома используйте текст (=имя персонажа)
# цветом закодируйте кластер, выделенный при помощи k-means
# отберите 20 наиболее значимых переменных (по косинусу)
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
  addlabels = FALSE  # убираем автоматические метки, чтобы не дублировать
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
