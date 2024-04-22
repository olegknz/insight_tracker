library(text2vec)
find_close_words <- function(input_word = "paris", n = 5) {
  # matrix input
  if (is.matrix(input_word)) {
    sim_word_m = sim2(
      x = word_vectors,
      y = input_word,
      method = "cosine",
      norm = "l2"
    )
    sim_word = head(sort(sim_word_m[, 1], decreasing = TRUE), n)
    return(sim_word %>% tibble::enframe())
  }
  else {
    # text input
    word_to_find = word_vectors[input_word, , drop = FALSE]
    sim_word_m = sim2(
      x = word_vectors,
      y = word_to_find,
      method = "cosine",
      norm = "l2"
    )
    sim_word = head(sort(sim_word_m[, 1], decreasing = TRUE), n)
    return(sim_word %>% tibble::enframe())
  }
}

get_embeddings <- function(input_word = "paris"){
  embeddings = word_vectors[input_word, , drop = FALSE]
  return(embeddings)
}