library(zlib)

# Укажите путь к вашему файлу .vec.gz
file_path <- "models/ruscorpora_upos_skipgram_300_5_2018.vec.gz"

# Открываем сжатый файл для чтения
con <- gzfile(file_path, "rb")

# Считываем размерность данных из первой строки
first_line <- scan(con, what = character(), nlines = 1)

# Извлекаем размерность
dimensions <- as.numeric(strsplit(first_line, " "))

# Создаем пустую матрицу для эмбеддингов
embedding_matrix <- matrix(numeric(), nrow = dimensions[1], ncol = dimensions[2])
words = c()

# Заполняем матрицу эмбеддингов
for (i in 1:dimensions[1]) {
  line <- scan(con, what = character(), nlines = 1, sep = " ", quiet = TRUE)
  words = c(words, line[1])
  embedding_matrix[i, ] <- as.numeric(line[-1])
}


embedding_matrix <- embedding_matrix[1:98911, ]
rownames(embedding_matrix) = words[1:98911]

save(embedding_matrix, file = "models/rusEmb.RData")
# Закрываем соединение с файлом
close(con)

# Проверяем размерность матрицы
dim(embedding_matrix)