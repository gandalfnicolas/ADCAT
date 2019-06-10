#' @title Lemmatizer with debug
#'
#' @description This function allows you lemmatize words using the treetag lemmatizer
#' @param word word to lemmatize, if multiple use loop/apply/dplyr
#' @param print Whether to print word being lemmatized. Useful for long lists of words. Defaults to TRUE
#' @return lemmatized words
#' @export Lemmatize


Lemmatize_debug = function(word, print =T){
  if (print == T){
    print(word)
  }
  lemmax = koRpus::treetag(as.character(word), treetagger="manual", format="obj", TT.tknz=T, debug=T, lang="en", TT.options=list(path="C:\\treetagger", preset="en"))
  if(lemmax@TT.res[["lemma"]] == "<unknown>"){
    if (print == T){
      print(lemmax@TT.res[["token"]])}
    return (lemmax@TT.res[["token"]])}
  else{
    if (print == T){
      print(lemmax@TT.res[["lemma"]])}
    return(lemmax@TT.res[["lemma"]])
  }}
