# folder id  - b1gml016n03hjg1jadt1
# OAuth - y0_AgAAAABoUJB_AATuwQAAAAEBEZwkAAC74cONeK9G4a_4Uj_wV7gRMFkL0w
# $yandexPassportOauthToken = "y0_AgAAAABoUJB_AATuwQAAAAEBEZwkAAC74cONeK9G4a_4Uj_wV7gRMFkL0w"
# $Body = @{ yandexPassportOauthToken = "$yandexPassportOauthToken" } | ConvertTo-Json -Compress
# Invoke-RestMethod -Method 'POST' -Uri 'https://iam.api.cloud.yandex.net/iam/v1/tokens' -Body $Body -ContentType 'Application/json' | Select-Object -ExpandProperty iamToken
IAM_TOKEN <- "t1.9euelZqalY2Pk5rHxsaSm8aWksaVmO3rnpWajJvPiZOSlJDOz8iRio-ekJ3l8_csUmtO-e9CcXtk_t3z92wAaU7570Jxe2T-zef1656VmpzMk5GQzZbPlY2TmZfMiZHK7_zF656VmpzMk5GQzZbPlY2TmZfMiZHK.ui792kk2LQf2FM2il5LjOsB_SsSwCPuxc743EibX5mq6C-PKZ99iZYPrh49bern6IG9ZSx-pIg_y86EvZ-BAAA"
FOLDER_ID <- "b1gml016n03hjg1jadt1"

library(jsonlite)
library(httr)

json_data <- jsonlite::read_json("body.json")
url <- "https://llm.api.cloud.yandex.net/foundationModels/v1/completion"

headers <- c(
  "Content-Type" = "application/json",
  "Authorization" = paste("Bearer", IAM_TOKEN),
  "x-folder-id" = FOLDER_ID
)

response <- httr::POST(
  url,
  body = json_data,
  encode = "json",
  httr::add_headers(.headers=headers)
)

print(httr::content(response))
