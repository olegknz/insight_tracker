library(telegram.bot)
library(httr2)
library(jsonlite)
library(stringr)
library(httr)
library(RSQLite)
library(dplyr)

con <- dbConnect(SQLite(), "users.db")

updater <- Updater('6996387328:AAHS4H8zjdTJGroQt2QhBxGnjq2JLHUJYcI')

# Рекомендация инсайта новому пользователю
recommend_new_user = function() {
  load("insights.RData")
  
  # получаем случайную строку из датасета с инсайтами
  random_insight <- select(insights_table[sample(nrow(insights_table), 1), ], id, text)
  
  return(random_insight)
}

# Рекомендация инсайта старому пользователю
recommend_insight = function(id) {
  load("insights.RData")
  
  query <- paste0("SELECT * FROM users WHERE user_id = '", id, "'")
  user <- dbGetQuery(con, query)
  
  # если у пользователя есть оценки >= 4 -> рекомендуем похожие инсайты
  if (nrow(user %>% filter(rating >= 4)) > 0) {
    user = user %>% filter(rating >= 4)
    simCut = as.matrix(sim_matrix[,user$insight_id])
    mostSimilar = head(sort(simCut, decreasing = T), n = 5)
  # если у пользователя нет хороших оценок -> рекомендуем не похожие на оценненые инсайты
  } else {
    user = user %>% filter(rating < 4)
    simCut = as.matrix(sim_matrix[,user$insight_id])
    mostSimilar = sort(simCut, decreasing = F)
    mostSimilar = mostSimilar[mostSimilar > 0] %>% head(5)
  }
  
  # начинаем рекомендацию
  a = which(simCut %in% mostSimilar, arr.ind = TRUE, useNames = T)
  result = rownames(a)
  index = arrayInd(a, .dim = dim(simCut))
  
  result = rownames(sim_matrix)[index[,1]]
  mostSimilar = data.frame(id = result, similar = simCut[index])
  
  # выбор только одного инсайта из рекомедации
  recommendation = mostSimilar %>% 
    left_join(insights_table) %>% 
    select(id, text, similar) %>% 
    arrange(-similar) %>% 
    head(1) %>%
    select(id, text)
  
  return(recommendation)
}

# Функция для проверки, существует ли пользователь в базе данных
check_user <- function(user_id) {
  query <- paste0("SELECT COUNT(*) FROM users WHERE user_id = '", user_id, "'")
  result <- dbGetQuery(con, query)
  # Если есть записи в таблице юзеров с данным id
  if (result[[1]] > 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Обработчик команды /start (начало использования бота)
start <- function(bot, update) {
  user_id <- update$message$from$id
  if (check_user(user_id)) {
    bot$sendMessage(update$message$chat_id,
                    text = "Привет! Похоже вы уже использовали нашего бота.\n\nПродолжим?",
                    parse_mode = NULL)
  } else {
    # клавиатура для получения первого инсайта
    RKM <- ReplyKeyboardMarkup(
      keyboard = list(
        list(KeyboardButton("Получить инсайт"))
      ),
      resize_keyboard = TRUE,
      one_time_keyboard = FALSE
    )
    bot$sendMessage(update$message$chat_id,
                    text = "Добро пожаловать! Этот бот позволит вам отслеживать свежие инсайты в сфере рекрутмента\n\nНачнём?",
                    parse_mode = NULL,
                    reply_markup = RKM)
  }
}

# Функция формирование сообщения с рекомндованным инсайтом
send_insight <- function(bot, update) {
  user_id <- update$message$from$username
  
  # получаем рекомендованный инсайт
  if (check_user(user_id)) {
    recomendation = recommend_insight(user_id)
  } else {
    recomendation = recommend_new_user()
  }
  
  # Формируем ссылку на YouTube видео (источник инсайта)
  parts <- strsplit(recomendation$id, "_")[[1]]
  video_id <- parts[1]
  youtube_link <- paste("https://www.youtube.com/watch?v=", video_id, sep = "")
  
  # создаём клавиатуру для получения оценки
  IKM <- InlineKeyboardMarkup(
    inline_keyboard = list(
      list(
        InlineKeyboardButton("1", callback_data = paste(recomendation$id, '1')),
        InlineKeyboardButton("2", callback_data = paste(recomendation$id, '2')),
        InlineKeyboardButton("3", callback_data = paste(recomendation$id, '3')),
        InlineKeyboardButton("4", callback_data = paste(recomendation$id, '4')),
        InlineKeyboardButton("5", callback_data = paste(recomendation$id, '5'))
      )
    )
  )
  
  # Отправка сообщения
  bot$sendMessage(update$message$chat_id, 
                  text = paste(recomendation$text, youtube_link, sep = "\n\n"),
                  parse_mode = NULL,
                  reply_markup = IKM)
}

# Обработчик оценки инсайта
add_insight_rating <- function(bot, update) {

  # полученные данные с кнопки
  data <- update$callback_query$data
  
  splited_data = strsplit(data, " ")[[1]]
  insight_id = splited_data[1]
  rating = splited_data[2]
  
  # получаем имя пользователя, нажавшего кнопку
  uname <- update$effective_user()$username
  
  # Проверяем, существует ли уже запись с данным user_id и insight_id
  query_check <- paste0("SELECT COUNT(*) FROM users WHERE user_id = '", uname, "' AND insight_id = '", insight_id, "'")
  result <- dbGetQuery(con, query_check)
  
  if (result[[1]] == 0) {
    # Если запись не существует, добавляем новую запись
    query_insert <- paste0("INSERT INTO users (user_id, insight_id, rating) VALUES ('", uname, "', '", insight_id, "', ", rating, ")")
    dbSendQuery(con, query_insert)
    bot$sendMessage(chat_id = update$from_chat_id(),
                    text = "Ваша оценка записана")
  } else {
    # Если запись уже существует, выводим сообщение об этом
    bot$sendMessage(chat_id = update$from_chat_id(),
                    text = "Вы уже оценивали этот инсайт")
  }
  
  # сообщаем боту, что запрос с кнопки принят
  bot$answerCallbackQuery(callback_query_id = update$callback_query$id) 
}

MessageFilters$get_insight <- BaseFilter(function(message) {
    # проверяем текст сообщения
    message$text == "Получить инсайт"
  }
)

# hi_hendler <- CommandHandler('hi', say_hello)
# summary_hendler <- CommandHandler('summary', send_summary, pass_args = TRUE)
# parse_hendler <- CommandHandler('parse', parse, pass_args = TRUE)
# test_hendler <- CommandHandler('test', test)

start_handler <- CommandHandler('start', start)
firstInsight_handler <- MessageHandler(send_insight, filters = MessageFilters$get_insight)
insight_rating_handler <- CallbackQueryHandler(add_insight_rating)

updater <- updater + 
            start_handler +
            firstInsight_handler +
            insight_rating_handler

updater$start_polling()

