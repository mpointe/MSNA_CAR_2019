
#' set string to lower case and replace everything that is not alphanumeric or underscore by a dot "."
#' @param x string or vector of strings
#' @return vector of strings with characters replaced
to_alphanumeric_lowercase <- function (x)
{
  tolower(gsub("[^a-zA-Z0-9_]", "\\.", x))
}


#' set string to lower case and replace everything that is not alphanumeric or underscore by a dot "."
#' @param x a dataframe containing strings or vectors to be converted
#' @param convert.colnames T/F on whether column names should be converted to lowercase. Defaults to True
#' @return vector of strings with characters replaced
df_to_alphanumeric_lowercase<-function(x,convert_content = T, convert.colnames = T){
  if(!is.data.frame(x)){stop("x must be a data frame")}

  if(convert.colnames){
    colnames(x)<-to_alphanumeric_lowercase(colnames(x))

  }

  if(convert.content){
    lapply(x,to_alphanumeric_lowercase) %>% as.data.frame(stringsAsFactors = F)
  }
}
