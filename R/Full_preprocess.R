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
  Lemmatizex = function(word, print = print, debug=debug){
    if (print == T){
      print(word)
    }
    lemmax = koRpus::treetag(as.character(word), treetagger="manual", format="obj", TT.tknz=T, debug=debug, lang="en", TT.options=list(path="C:\\treetagger", preset="en"))
    if(lemmax@TT.res[["lemma"]] == "<unknown>"){
      if (print == T){
        print(lemmax@TT.res[["token"]])}
      return (lemmax@TT.res[["token"]])}
    else{
      if (print == T){
        print(lemmax@TT.res[["lemma"]])}
      return(lemmax@TT.res[["lemma"]])
    }}
  if(parallelize == T){
    cl <- parallel::makeCluster(parallel::detectCores() - 1)
    parallel::clusterEvalQ(cl, {
      library(dplyr)
      library(koRpus.lang.en)})
    parallel::clusterExport(cl=cl, varlist=c("Lemmatize", "res"), envir = environment())
    res = parallel::parSapply(cl,res, Lemmatizex)
    parallel::stopCluster(cl)}
  else{
    res = sapply(res,Lemmatizex)
  }
  res = sapply(res,delete_ending_Ss)
  res = mapply(Spellcheck,raw = words, cleaned = res, MoreArgs = list(rawlist = words, dict_cleaned= Dictionaries$word))
  res = as.character(res)
  res = sapply(res,tolower)
  return(res)
}
