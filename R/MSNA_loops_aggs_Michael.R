#' Make proportions
#'
#'Calculate proportions with or without "dont know"
#'@param data dataframe
#'@param agg_var Name of the geographic aggregation unit(e.g., lga)
#'@param indicator_index Column INDEX of the first column to be aggregated
#'@param dontknow_action a string, either "exclude" if "dont know" should be excluded OR "included" if "dont know" should included
#'@param dontknow_format IN QUOTATIONS: How "dont know" is spelled in the data
#'@return a dataframe in wide format with the aggregated categories in the dataframe for each aggregation unit 
#'@example
#'@export
#'
make_proportions <- function(data,agg_var,indicator_index, dontknow_action = "exlude", dontknow_format){
  var_name <- colnames(data)[indicator_index]
  locationz <- as.vector(unique(data[grep(paste0("^",agg_var,"$"),colnames(data))]))
  #locationz <- sort(locationz, decreasing=TRUE)
  data<- add_column(data, onesz = 1)
  dontknow_format <- paste0("'",dontknow_format,"'")
  if(dontknow_action == "exclude"){
    dontknow_action <- FALSE
  } else if (dontknow_action == "include") {
    dontknow_action <- TRUE
  }
  if(dontknow_action == FALSE){
    data  <- data %>%
      dplyr ::filter(!is.na(data[grep(paste0("^",var_name,"$"), colnames(data))])) 
    data  <- dplyr:: filter_(data, paste(var_name,"!=", dontknow_format,sep=" "))
  } else {
    data  <- data %>%
      dplyr ::  filter(!is.na(data[grep(paste0("^",var_name,"$"), colnames(data))]))
  }
  agg_var_indicator <- as.formula(paste0(agg_var,"~",var_name))  
  result <- data %>% dcast(agg_var_indicator, fun.aggregate = sum,value.var="onesz", na.rm=TRUE)
  namez <- paste0(names(result)[2:ncol(result)],"_",var_name)
  names(result)[2:ncol(result)] <- namez
  result<- add_column(result, total_respondents = rowSums(result[2:ncol(result)]))
  denom_column <- ncol(result)
  props <- list()
  for(i in 2:(ncol(result)-1)){
    props[[i]]<-  result[i]/result[denom_column]
  }
  props[sapply(props, is.null)] <- NULL
  props <- as.data.frame(props)
  names(props) <- paste0("pr_",names(props))
  result <- data.frame(result, props )
  result<-merge(locationz, result, by=agg_var , all.x=TRUE)
  return(result)
}