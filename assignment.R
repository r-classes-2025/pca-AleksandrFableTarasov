# установите и загрузите пакеты
library(friends)
library(tidyverse) 
library(tidytext)
library(factoextra)

# 1. отберите 6 главных персонажей (по количеству реплик)
# сохраните как символьный вектор
top_speakers <- count(friends, speaker, sort = TRUE) |> 
  slice_head(n = 6) |> 
  pull(speaker)

# 2. отфильтруйте топ-спикеров,
# токенизируйте их реплики, удалите из них цифры
# столбец с токенами должен называться word
# оставьте только столбцы speaker, word
friends_tokens <- friends |>
  filter(speaker %in% top_speakers) |>
  unnest_tokens(word, text) |>
  mutate(word = str_remove_all(word, "\\d+")) |>
  filter(word != "") |>
  select(speaker, word)

# 3. отберите по 500 самых частотных слов для каждого персонажа
# посчитайте относительные частотности для слов
friends_tf <- friends_tokens |>
  count(speaker, word) |>
  group_by(speaker) |>
  mutate(tf = n / sum(n)) |>
  arrange(desc(n), word) |>  # сортируем по частоте и алфавиту для стабильности
  slice_head(n = 500) |>
  ungroup() |>
  select(speaker, word, tf)

# 4. преобразуйте в широкий формат;
# столбец c именем спикера превратите в имя ряда, используя подходящую функцию
friends_tf_wide <- friends_tf |>
  pivot_wider(names_from = word, values_from = tf, values_fill = 0) |>
  as.data.frame() |>  # убедимся, что это data.frame
  column_to_rownames("speaker")

# 5. установите зерно 123
# проведите кластеризацию k-means (k = 3) на относительных значениях частотности (nstart = 20)
# используйте scale()
set.seed(123)
km.out <- kmeans(scale(friends_tf_wide), centers = 3, nstart = 20)

# 6. примените к матрице метод главных компонент (prcomp)
# центрируйте и стандартизируйте, использовав аргументы функции
pca_fit <- prcomp(friends_tf_wide, center = TRUE, scale. = TRUE)

# 7. Покажите наблюдения и переменные вместе (биплот)
# в качестве геома используйте текст (=имя персонажа)
# цветом закодируйте кластер, выделенный при помощи k-means
# отберите 20 наиболее значимых переменных (по косинусу, см. документацию к функции)
# сохраните график как переменную q
q <- fviz_pca_biplot(
  pca_fit,
  label = "var",  # показываем метки переменных
  select.var = list(cos2 = 20),  # 20 наиболее значимых переменных
  geom.var = "text",  # переменные как текст
  geom.ind = "text",  # наблюдения как текст
  repel = TRUE,  # предотвращаем наложение текста
  col.ind = as.factor(km.out$cluster),  # цвет по кластерам
  palette = "Set1",  # цветовая палитра Set1 (яркие различимые цвета)
  addEllipses = FALSE,  # убираем эллипсы для чистоты графика
  title = "PCA Biplot: Лексические особенности персонажей Friends",
  subtitle = "Цвета соответствуют кластерам k-means (k=3)",
  ggtheme = theme_minimal()
) +
  # Добавляем имена персонажей более заметно
  geom_text(
    aes(label = rownames(friends_tf_wide)), 
    size = 5, 
    fontface = "bold",
    check_overlap = FALSE
  ) +
  # Улучшаем легенду
  labs(color = "Кластер") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right"
  )

# Выводим график
print(q)
