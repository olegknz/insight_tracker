say_hello <- function(bot, update) {
  user_name <- update$message$from$first_name
  
  bot$sendMessage(update$message$chat_id,
                  text = paste0("Hello, ", user_name, "!"),
                  parse_mode = "Markdown",
                  reply_to_message_id = update$message$message_id)
}

send_summary <- function(bot, update, args) {
  if (length(args) != 1) {
    bot$sendMessage(update$message$chat_id,
                    text = "No url to summarize(",
                    parse_mode = "Markdown")
  } else {
    summary <- get_summary(args)
    
    bot$sendMessage(update$message$chat_id,
                    text = summary,
                    parse_mode = "Markdown")
  }
}

get_summary <- function(source) {
  endpoint <- 'https://300.ya.ru/api/sharing-url'
  
  req <- request(endpoint)
  req <- req %>% req_headers(Authorization = 'OAuth y0_AgAAAABoUJB_AAoX4wAAAAD8vkxtAAD-Kl26KapKCYXm3-DcnXI-_JY5Ww')
  req <- req %>% req_body_json(list(article_url = source))
  req <- req %>% req_retry(max_tries = 5)
  resp <- req_perform(req) %>% resp_body_json()
  
  return(resp$sharing_url)
}

upload_posts <- function(num_posts, channel) {
  command <- paste0("snscrape --max-result ", num_posts, " --jsonl telegram-channel ", channel)
  output <- system(command, inter = TRUE)
  writeLines(output, paste0(channel, ".txt"))
}

collect_posts <- function(channel) {
  file_content <- readLines(paste0(channel, ".txt"), encoding = "UTF-8")
  
  posts <- list()
  for (line in file_content) {
    post <- jsonlite::fromJSON(line)
    content <- post$content
    outlinks <- post$outlinks
    
    links <- grep(channel, outlinks, value = TRUE, invert = TRUE, fixed = TRUE)
    p <- paste(content, "\n\n", paste(links, collapse = "\n"), collapse = "")
    
    posts[[length(posts) + 1]] <- p
  }
  return(posts)
}

parse <- function(bot, update, args) {
  tryCatch({
    source <- args[1]
    count <- as.integer(args[2])  # Преобразуем количество в целое число
    
    upload_posts(count, source)
    posts <- collect_posts(source)
    
    for (post in posts) {
      bot$sendMessage(update$message$chat_id,
                      text = post,
                      parse_mode = NULL)
    }
  }, error = function(e) {
    bot$sendMessage(update$message$chat_id,
                    text = e$message,
                    parse_mode = NULL,
                    reply_to_message_id = update$message$message_id)
  })
}