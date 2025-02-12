#' ## 1. Linguistic analysis -  part A
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
x <- udpipe_annotate(ud_model, x = data$Message) #' Select field for analysis
x <- as.data.frame(x)

#' alternative code lines for looping with lapply
ud_model <- udpipe_download_model(language = "english") #udpipe EN version file download
#' 1st define new udpipe function
udpipe_dat <- function(x, file) {
  model <- udpipe_load_model(file)
  x <- udpipe_annotate(model, x = x$Message)
  as.data.frame(x, detailed = TRUE)
}

#' 2nd apply function to all subsets
datasetsCats <- lapply(c("I","II", "III","IV","V","VI"), function(x){paste0("dataCats", x)}) #x
result_udpipeCats <- lapply(datasetsCats, udpipe_dat(datasetsCats,ud_model))

datasetsTime <- lapply(c("BBNJWG","PrepCom", "IGC"), function(x){paste0("data", x)}) #x
result_udpipeTime <- lapply(datasetsTime, udpipe_dat(datasetsTime,ud_model))

result <- lapply(files, test, H0=1)

#' with lists
udpipe_for_lists <- function(x, file) {
  model <- udpipe_load_model(file)
  x <- udpipe_annotate(model, x = x)
  as.data.frame(x, detailed = TRUE)
}


Cats.split <- split(data$Message, data$Category)
head(Cats.split)
result_udpCats <- sapply(Cats.split, udpipe_dat (Cats.split, UDP_file))
