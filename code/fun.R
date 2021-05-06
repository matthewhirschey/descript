library(tidyverse)
library(glue)
library(httr)
library(jsonlite)
library(janitor)
library(tuneR)
library(av)

### MAKE SURE TO SOURCE token.R IN ORDER TO SET YOUR_PERSONAL_TOKEN 

get_voice_id <- function(voice = "Matthew") {
  #find the id of the Voice with which to generate
  #choices include: c("Matthew", "Don", "Emily", "Carla", "Ruth", "Malcom", "Ethan", "Henry", "Nancy")  
  
  voice_id_json <- httr::GET(url = "https://descriptapi.com/v1/overdub/voices", 
                             add_headers(
                               authorization = glue::glue('Bearer {YOUR_PERSONAL_TOKEN}')
                             ))
  voice_id_df <- fromJSON(rawToChar(voice_id_json$content)) 
  
  voice_id <-
    voice_id_df %>% 
    filter(name == voice) %>% 
    pull(id)
  
  return(voice_id)
}
#get_voice_id()

get_speech <- function(voice = "Matthew", 
                       text,
                       save = FALSE) {
  #get voice id
  id <- get_voice_id(voice)
  
  #first build the POST body
  json_body <- jsonlite::toJSON(
    list(text = text, 
         voice_id = id), 
    auto_unbox = TRUE
  )
  
  #generate an async task via post
  queue <- httr::POST(
    url = "https://descriptapi.com/v1/overdub/generate_async", 
    add_headers(
      authorization = glue::glue('Bearer {YOUR_PERSONAL_TOKEN}')), 
    content_type_json(),
    body = json_body, 
    encode = "raw"
  )
  queue_list <- fromJSON(rawToChar(queue$content))
  queue_id <- queue_list[["id"]]
  
  #second fetch the result of the task (should be available after a few seconds):
  audio_file_url <- NULL
  base_url <- "https://descriptapi.com/v1/overdub/generate_async/"
  GET_url <- glue::glue('{base_url}{queue_id}')
  
  while (is.null(audio_file_url)) { #sleep 1sec, then test
    Sys.sleep(1)
    request <- httr::GET(url = GET_url, 
                         add_headers(
                           authorization = glue::glue('Bearer {YOUR_PERSONAL_TOKEN}')
                         ))
    request_list <- fromJSON(rawToChar(request$content))
    audio_file_url <- request_list[["url"]]
  }

  if (save == FALSE) {
    tmp <- tempfile(fileext = ".wav")
    download.file(url = audio_file_url, 
                  destfile = tmp)
    return(tmp)
    
  } else {
    clean_time <- janitor::make_clean_names(Sys.time())
    file_name <- glue::glue('{voice}_{stringr::str_remove(clean_time, "x")}.wav') #.mp3
    download.file(url = audio_file_url, 
                  destfile = file_name)
    return()
  }
}

#FAILED CODE -----
# av::av_audio_convert(tmp, 
#                      output = glue::glue('{file_name}'))
# on.exit(unlink(tmp))
# tuneR::readMP3(glue::glue('{file_name}'))
# writeBin(out, con = tmp)
# 
# message(glue::glue('Audio file {file_name} downloaded'))

# out <- get_speech(text="Did this actually work?")
# tmp <- tempfile()
# writeBin(out, con = tmp)

#TEST CODE-----
#get_speech(text = 'This is what Matthew sounds like. The Center for Computational Thinking at Duke University teaches students to apply computational thinking to ideas, challenges, and opportunities, while simultaneously considering the ethical, legal, and social impacts of technology on humans, for the betterment of society.')
#get_speech(text = "Holy Smokes! I think this actually worked")
           
#listen to the different options
# voices <- c("Matthew", "Don", "Emily", "Carla", "Ruth", "Malcom", "Ethan", "Henry", "Nancy") 
# for (i in voices) {
# get_speech(voice = i,
#            text = glue::glue('This is what {i} sounds like. The Center for Computational Thinking at Duke University teaches students to apply computational thinking to ideas, challenges, and opportunities while simultaneously considering the ethical, legal, and social impacts of technology on humans for the betterment of society.'))
# }
