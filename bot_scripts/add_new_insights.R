library(httr)
library(jsonlite)
library(text2vec)
library(stringr)
library(tidytext)
library(tidyverse)

load("models/rusEmb.RData")

# функция получения текста из видео по API
transcribe <- function(video_id) {
  url <- "https://youtube-transcriptor.p.rapidapi.com/transcript"
  
  queryString <- list(video_id = video_id)
  
  response <- VERB(
    "GET",
    url,
    query = queryString,
    add_headers('X-RapidAPI-Key' = 'dc70f7734dmshc3a393a1c4e0b9fp157638jsn01eee408974d',
                'X-RapidAPI-Host' = 'youtube-transcriptor.p.rapidapi.com'),
    content_type("application/octet-stream")
  )
  
  # Преобразование JSON-строки в объект R
  json_data <- fromJSON(content(response, "text"))
  
  # Извлечение только текста из массива transcription
  transcription <- json_data$transcription
  text <- sapply(transcription, function(x) x$subtitle)
  
  text <- paste(text, collapse = " ")
  message_text <- gsub("\\n", " ", text)
  message_text <- gsub("&#39;", " ", message_text)
  
  return(message_text)
}

# функция получения инсайтов из текста по API
get_insights <- function(text) {
  #IAM_TOKEN <- "t1.9euelZrGnozIyo7Pl5LOyZnOzZzPi-3rnpWajJvPiZOSlJDOz8iRio-ekJ3l8_cfXhhO-e8mVx8u_d3z918MFk757yZXHy79zef1656VmsnNyIySypOdz5GOi8_IipuX7_zF656VmsnNyIySypOdz5GOi8_IipuX.CbxSd6EWk2FqXf-5JzHg-Aj84D5JItxz7d_ouV-gjGulsg5tqbBDb4lIGwRJVlKZ_IMcEN3P7GuNJc-jk6gKDA"
  source("bot_scripts/get_IAM_token.R")
  IAM_TOKEN <- get_token()
  FOLDER_ID <- "b1gml016n03hjg1jadt1"
  
  # Создание JSON-объекта
  json_data <- list(
    "modelUri" = "gpt://b1gml016n03hjg1jadt1/yandexgpt-lite",
    "completionOptions" = list(
      "stream" = FALSE,
      "temperature" = 0.4,
      "maxTokens" = "8000"
    ),
    "messages" = list(
      list(
        "role" = "system",
        "text" = "Найди в тексте инсайты в сфере рекрутмента. Под инсайтами я подразумеваю высказывания в формате Кто-то ушёл откуда-то, кто-то перешёл в другую команду. Выведи только найденные инсайты в виде списка (1., 2., 3. ...) без вводных конструкций, простым текстом не используя спецсимволы."
      ),
      list(
        "role" = "user",
        "text" = text
      )
    )
  )
  
  # URL и заголовки
  url <- "https://llm.api.cloud.yandex.net/foundationModels/v1/completion"
  headers <- c(
    "Content-Type" = "application/json",
    "Authorization" = paste("Bearer", IAM_TOKEN),
    "x-folder-id" = FOLDER_ID
  )
  
  # Отправка POST-запроса
  response <- httr::POST(
    url,
    body = json_data,
    encode = "json",
    httr::add_headers(.headers=headers)
  )
  
  # Извлечение текста из ответа
  result <- httr::content(response, "parsed")
  alternative_text <- result$result$alternatives[[1]]$message$text
  
  return(alternative_text)
}

# функция получения эмбеддтнга инсайта с помощью doc2vec алгоритма
doc2vec <- function(insights) {
  source("models/emb.R")
  
  univ_tokens = insights %>% 
    unnest_tokens(output = words, input = text)
  
  dict = row.names(embedding_matrix)
  
  univ_tokens = univ_tokens %>% 
    filter(words %in% dict) %>% 
    unique()
  
  univ_embeddings = univ_tokens %>% 
    rowwise() %>%
    mutate(emb = str_c(get_embeddings(words), collapse = " ") ) 
  
  univ_embeddings = univ_embeddings %>% 
    separate(emb, into = as.character(c(1:300)), sep = " ")
  
  doc_emb = univ_embeddings %>%
    select(-words) %>% 
    group_by(id) %>% 
    mutate_at(vars(-group_cols()), as.numeric) %>% 
    summarise_all(mean) %>%
    left_join(insights %>% select(id, text), by = 'id')
  
  doc_emb_tmp = doc_emb %>% select(-id, -text)
  colnames(doc_emb_tmp) <- paste0("X", seq_len(ncol(doc_emb_tmp)))
  
  doc_emb = doc_emb %>% select(id, text) %>% cbind(doc_emb_tmp)
  
  return(doc_emb)
}

# НАЧАЛО СКРИПТА ---------------------------------------------------------------

main <- function(url) {
  if (grepl("v=", url)) {
    video_id <- str_extract(url, "(?<=v=)[^&]+")
  } else {
    video_id <- url
  }
  
  # Получаем текст из видео
  text = transcribe(video_id)
  
  # Получаем инсайты
  recieved_isights = get_insights(substr(text, start = 1, stop = 8000))
  # Обрабатываем полученные инсайты формируя список'
  print(recieved_isights)
  recieved_isights <- Filter(function(x) nchar(x) > 0, strsplit(recieved_isights, "\n")[[1]])
  recieved_isights <- gsub("^\\d+\\.\\s+|\\t", "", recieved_isights)
  
  # Даём каждому инсайту его уникальный ID в формате {video_id}_{insight_number}
  source <- paste(video_id, seq_along(recieved_isights), sep = "_")
  
  # Получение doc2vec формата для инсайтов
  new_insights = data.frame(text = recieved_isights, id = source)
  new_insights2vec = doc2vec(new_insights)
  
  # Добавление новых инсайтов в датасет со всеми инсайтами
  load("data/insights.RData")
  
  # Перебираем строки таблицы new_insights2vec
  for (i in 1:nrow(new_insights2vec)) {
    new_id <- new_insights2vec$id[i]
    
    # Проверяем, есть ли такой id в основной таблице
    if (!(new_id %in% insights_table$id)) {
      # Если нет, добавляем строку из new_insights2vec в основную таблицу
      insights_table <- rbind(insights_table, new_insights2vec[i, ])
    }
  }
  
  sim_matrix_data = insights_table %>% select(-text)
  rownames(sim_matrix_data) = NULL
  sim_matrix_data = sim_matrix_data %>% column_to_rownames("id")
  sim_matrix = lsa::cosine(t(as.matrix(sim_matrix_data)))
  diag(sim_matrix) = 0
  
  save(sim_matrix, insights_table, file = "data/insights.RData")
}
  
