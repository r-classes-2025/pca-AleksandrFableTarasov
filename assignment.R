# установите и загрузите пакеты
library(friends)
library(tidyverse)
library(tidytext)
library(factoextra) 

# 1. отберите 6 главных персонажей (по количеству реплик)
# сохраните как символьный вектор
top_speakers <- friends |> 
  count(speaker, sort = TRUE) |> 
  slice_max(n, n = 6) |>  # используем slice_max вместо top_n
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
  # ВАЖНО: удаляем пустые токены после удаления цифр
  filter(word != "") |> 
  select(speaker, word)

# 3. отберите по 500 самых частотных слов для каждого персонажа
# посчитайте относительные частотности для слов
friends_tf <- friends_tokens |>
  count(speaker, word, name = "n_words") |> 
  group_by(speaker) |> 
  # ВАЖНО: сначала сортируем по убыванию частотности
  arrange(desc(n_words), .by_group = TRUE) |> 
  # ВАЖНО: используем slice_head вместо slice_max
  slice_head(n = 500) |> 
  # ВАЖНО: считаем tf ПОСЛЕ отбора 500 слов
  mutate(tf = n_words / sum(n_words)) |> 
  ungroup() |> 
  select(speaker, word, tf)

# 4. преобразуйте в широкий формат; 
# столбец c именем спикера превратите в имя ряда, используя подходящую функцию 
friends_tf_wide <- friends_tf |> 
  pivot_wider(
    names_from = word, 
    values_from = tf, 
    values_fill = 0
  ) |> 
  as.data.frame() |> 
  column_to_rownames("speaker")

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
# отберите 20 наиболее значимых переменных (по косинусу, см. документацию к функции)
# сохраните график как переменную q
q <- fviz_pca_biplot(
  pca_fit,
  label = "var",           # показываем переменные
  select.var = list(cos2 = 20),  # 20 наиболее значимых переменных по косинусу
  geom.var = "text",       # переменные как текст
  geom.ind = "text",       # наблюдения как текст (имена персонажей)
  repel = TRUE,           # предотвращаем наложение текста
  col.ind = as.factor(km.out$cluster),  # цвет по кластерам из k-means
  col.var = "gray50",     # цвет для переменных
  labelsize = 3,          # размер шрифта для переменных
  pointsize = 0,          # убираем точки для наблюдений
  addlabels = TRUE        # добавляем метки к наблюдениям
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

# Выведите график
print(q)
