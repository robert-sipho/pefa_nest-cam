library(tidyverse)


# Read/convert yolo results -----------------------------------------------
convert_yolo_pefa <- function(source, weights){
  
  # Read in results
  results <- readtext::readtext(paste0(source))$text # read in text file
  
  
  # create line breaks at the word "enter", since this is where the data for each image begins
  t <- stringr::str_split(toString(results), "Enter ")
  t <- t[[1]][2:length(t[[1]])]
  t <- tibble::tibble(col = unlist(t))
  x <- data.frame(do.call('rbind', strsplit(t$col,'s.',fixed=TRUE))) 
  x$X2 <- stringr::str_replace_all(as.character(x$X2), "\n", "")
  
  # maximum number of objects detected
  #max(sapply(strsplit(as.character(x$X2),'%'), length))
  
  objects <- as.data.frame(stringr::str_split_fixed(x$X2, "%", 8))
  
  results <- as.data.frame(cbind(x$X1, objects))
  
  names(results)[1] <- "path"
  results <- results[1:nrow(results)-1,]
  
  # image name
  start <- stringr::str_locate(results$path, "/2016")
  end <- stringr::str_locate(results$path, ".JPG")
  images <- stringr::str_sub(results$path, start[,1]+1, end[,2])
  
  
  # date/other image attributes
  path <- stringr::str_replace(results$path, "Image Path: ", "")
  path <- stringr::str_replace(path, "/media/erik/", "/Volumes/")
  path_end <- stringr::str_locate(path, ": Predicted")
  path <- stringr::str_sub(path, 0, path_end[,1]-1)
  
  ad <- data.frame(
    path = path,
    image = images,
    obj1 = as.character(results$V1),
    obj2 = as.character(results$V2),
    obj3 = as.character(results$V3),
    obj4 = as.character(results$V4),
    obj5 = as.character(results$V5),
    obj6 = as.character(results$V6),
    obj7 = as.character(results$V7),
    obj8 = as.character(results$V8)
  )
  

  # sum the occurences of object detections
  ad$adult <- Reduce(`+`, lapply(ad[-1], 
                                 function(x) lengths(stringr::str_extract_all(x, "adult"))))
  ad$nestling <- Reduce(`+`, lapply(ad[-1], 
                                    function(x) lengths(stringr::str_extract_all(x, "nestling"))))
  ad$eggs <- Reduce(`+`, lapply(ad[-1], 
                                function(x) lengths(stringr::str_extract_all(x, "eggs"))))
  ad$bband <- Reduce(`+`, lapply(ad[-1], 
                                 function(x) lengths(stringr::str_extract_all(x, "bband"))))
  ad$sband <- Reduce(`+`, lapply(ad[-1], 
                                 function(x) lengths(stringr::str_extract_all(x, "sband"))))
  
  dat <- ad %>% 
    dplyr::rename_all(paste0,weights) %>% 
    dplyr::select(starts_with(c("image","adult","nestling","eggs","bband","sband"))) %>%
    dplyr::rename("image" = starts_with("image")) %>%
    dplyr::filter(image != ".DS_Store") %>%
    dplyr::mutate(image = str_replace(image, "2016/", "")) 
  
  return(dat)}



# Convert verification data -----------------------------------------------
file <- results[1]

read_verif <- function(file){
  x <- read.delim(paste0(file), sep = ",", 
                  col.names = c("image", "adult", "nestling", "eggs", "bband", "sband"),
                  colClasses = rep("character", 6))
 # x$path <- stringr::str_sub(file, 0 , stringr::str_locate(file, "output")[,1]-1)  
  names(x)
  x <- x %>%
    filter(adult != "unk") %>%
    dplyr::mutate_at(vars("adult", "nestling", "eggs", "bband", "sband"), funs(as.numeric)) %>%
    dplyr::rename_all(paste0,"_human") %>% 
    select(starts_with(c("image","adult","nestling","eggs","bband","sband"))) %>%
    rename("image" = starts_with("image"))
  return(x)
}

