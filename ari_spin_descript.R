ari_spin_descript <- function(
  images, paragraphs,
  output = tempfile(fileext = ".mp4"),
  voice = "Matthew",
  service = "descript",
  subtitles = FALSE,
  duration = NULL,
  tts_args = NULL,
  key_or_json_file = NULL,
  ...){
  # check for ffmpeg before any synthesizing
  ffmpeg_exec()
  
  stopifnot(length(images) > 0)
  images <- normalizePath(images)
  output_dir <- normalizePath(dirname(output))
  
  if (length(paragraphs) == 1) {
    if (file.exists(paragraphs)) {
      paragraphs = readLines(paragraphs, warn = FALSE)
      paragraphs = paragraphs[ !paragraphs %in% "" ]
    }
  }
  
  semi_colon = trimws(paragraphs) == ";"
  if (any(semi_colon)) {
    warning(paste0("Some paragraphs are simply a semicolon - ",
                   "likely needs to be replaced or slide removed!"))
  }
  
  stopifnot(
    length(paragraphs) > 0,
    identical(length(images), length(paragraphs)),
    all(file.exists(images)),
    dir.exists(output_dir)
  )
  
  wavs <- vector(mode = "list", length = length(paragraphs))
  par_along <- seq_along(paragraphs)
  ideal_duration <- rep(NA, length(paragraphs))
  
  pb <- progress::progress_bar$new(
    format = "Fetching Narration [:bar] :percent",
    total = length(par_along))
  
  for (i in par_along) {
    args = tts_args
    args$text = paragraphs[i]
    args$voice = voice
    wav <- do.call(tts_descript, args = args)
    #wav <- do.call(text2speech::tts, args = args)
    wav = reduce(wav$wav, bind)
    wav = pad_wav(wav, duration = duration[i])
    ideal_duration[i] =  length(wav@left) / wav@samp.rate
    wavs[[i]] <- wav
    pb$tick()
  }
  
  if (subtitles) {
    sub_file = paste0(file_path_sans_ext(output), ".srt")
    ari_subtitles(paragraphs, wavs, sub_file)
  }
  
  
  res = ari_stitch(images, wavs, output, ...)
  args = list(...)
  cleanup = args$cleanup
  if (is.null(cleanup)) {
    cleanup = TRUE
  }
  if (!cleanup) {
    attr(res, "wavs") = wavs
  }
  attr(res, "voice") = voice
  if (subtitles) {
    attr(res, "subtitles") = sub_file
  }
  attr(res, "service") = service
  return(res)
}
