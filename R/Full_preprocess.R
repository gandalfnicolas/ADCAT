#' @title Full preprocess
#'
#' @description This function applies all response preprocessing functions included in the package. (includes lowering, trimming whitespace, eliminating spaces and symbols, lemmatizing, and spellcheking; some of these only work on single words)
#' @param words text to preprocess
#' @param parallelize use parallel processors?
#' @return fully preprocessed words
#' @export Full_preprocess


Full_preprocess = function(words, parallelize = T, print =T, debug=F){
  res = sapply(words,clean_naresponses)
  res = sapply(res,trimws) #removes whitespace
  res = sapply(res,tolower)
  res = sapply(res,clean_symbols)
  res = sapply(res, Lemmatize, debug = debug)
  res = sapply(res,delete_ending_Ss)
  res = mapply(Spellcheck,raw = words, cleaned = res, MoreArgs = list(rawlist = words, dict_cleaned= Dictionaries$word))
  res = as.character(res)
  res = sapply(res,tolower)
  return(res)
}
