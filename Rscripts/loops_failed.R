#  not working:
#' loop / apply function to all subsets - not working!!
# datasetsCats <- lapply(c("I","II", "III","IV","V","VI"), function(x){paste0("dataCats", x)}) #x

# datasetsTime <- lapply(c("BBNJWG","PrepCom", "IGC"), function(x){paste0("data", x)}) #x
# result_udpipeTime <- lapply(datasetsTime, udpipe_for_df(datasetsTime,UDP_file))

# res_Cats <- lapply(datasetsCats, udpipe_for_df)
# res_Catsb <- lapply(datasetsCats, udpipe_for_dfb)
# 
# my_list <- list()               # Create empty list
# my_list
# for (i in seq_along(datasetsCats)) {
#   output <- udpipe_for_df(datasetsCats[[i]])
#   my_list[[i]] <- output  
# }
#  

#not working - returns only 1 obs!!
# datasetsCats <- c("dataCatsI","dataCatsII","dataCatsIII","dataCatsIV","dataCatsV","dataCatsVI")
# res <- sapply(datasetsCats,udpipe_for_df)
#res2 <- udpipe_for_df(datasetsCats[[2]],UDP_file)
#e.g. dat$Rating[dat$Season == seasons[i]]

#' with lists
# udpipe_for_lists <- function(x, file) {
#   model <- udpipe_load_model(file)
#   x <- udpipe_annotate(model, x = as.character(x))
#   as.data.frame(x, detailed = TRUE)
# }
# 
# 
# Cats.split <- split(data$Message, data$Category)
# head(Cats.split)
# result_udpCats <- sapply(Cats.split, udpipe_for_lists(Cats.split, UDP_file))



#' ### remove stop words (NOUN): agreement and BBNJ
#' #add code lines here
#' 
