get_token <- function() {
  oauth_token <- "y0_AgAAAABoUJB_AATuwQAAAAEBEZwkAAC74cONeK9G4a_4Uj_wV7gRMFkL0w"
  
  # Формируем команду curl
  curl_command <- paste0("curl -X POST -d '{\"yandexPassportOauthToken\":\"", oauth_token, "\"}' https://iam.api.cloud.yandex.net/iam/v1/tokens")
  
  # Выполнение команды и сохранение вывода
  output <- system(curl_command, intern = TRUE)
  
  # Вывод результата
  return(output)
}