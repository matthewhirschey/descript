tts_descript = function(
  text,
  output_format = c("wav"),
  voice = "Matthew",
  ...) {
  
  limit = 1000
  output_format = match.arg(output_format)
  audio_type = output_format
  
  res = lapply(text, function(string) {
    strings = tts_split_text(string, limit = limit)
    
    res = vapply(strings, function(tt) {
      output = tts_temp_audio(audio_type)
      out = get_speech(
        text = tt,
        voice = voice)
    }, FUN.VALUE = character(1L))

    out = lapply(res, tuneR::readWave)
    
    df = dplyr::tibble(original_text = string,
                       text = strings,
                       wav = out, file = res)
  })
  names(res) = length(text)
  res = dplyr::bind_rows(res, .id = "index")
  res$index = as.numeric(res$index)
  res$audio_type = audio_type
  
  if ("wav" %in% colnames(res)) {
    res$duration = vapply(res$wav, wav_duration, FUN.VALUE = numeric(1))
  }
  
  return(res)
}
#' 
#' #' @export
#' #' @rdname tts
#' #' @examples \dontrun{
#' #' text='<speak>
#' #'   He was caught up in the game.<break time="1s"/> In the middle of the
#' #'   10/3/2014 <sub alias="World Wide Web Consortium">W3C</sub> meeting,
#' #'   he shouted, "Nice job!" quite loudly. When his boss stared at him, he repeated
#' #'   <amazon:effect name="whispered">"Nice job,"</amazon:effect> in a
#' #'   whisper.
#' #' </speak>'
#' #' }
#' tts_amazon = function(
#'   text,
#'   output_format = c("mp3", "wav"),
#'   voice = "Joanna",
#'   bind_audio = TRUE,
#'   ...) {
#'   if (!requireNamespace("aws.polly", quietly = TRUE)) {
#'     stop(paste0(
#'       "This function requires aws.polly to operate",
#'       " please use\n",
#'       "install.packages('aws.polly')\n",
#'       "to use these functions"))
#'   }
#'   limit = 1500
#'   output_format = match.arg(output_format)
#'   audio_type = output_format
#'   
#'   sample_rate = switch(
#'     output_format,
#'     "mp3" = 24000,
#'     "wav" = 16000)
#'   output_format = switch(
#'     output_format,
#'     "mp3" = "mp3",
#'     "wav" = "pcm")
#'   
#'   args = list(...)
#'   if (is.null(args$rate)) {
#'     args$rate = sample_rate
#'   }
#'   if (!is.null(args$format)) {
#'     warning(
#'       paste0(
#'         "format was specified in ... for tts_amazon",
#'         ", this should be specified in output_format argument, format",
#'         " is overridden")
#'     )
#'   }
#'   res = lapply(text, function(string) {
#'     strings = tts_split_text(string,
#'                              limit = limit)
#'     
#'     res = vapply(strings, function(tt) {
#'       output = tts_temp_audio(audio_type)
#'       
#'       args$text = tt
#'       args$voice = voice
#'       args$format = output_format
#'       
#'       out = do.call(
#'         aws.polly::get_synthesis,
#'         args = args)
#'       
#'       writeBin(out, con = output)
#'       if (audio_type == "wav") {
#'         output = pcm_to_wav(input = output, sample_rate = args$rate)
#'       }
#'       output
#'     }, FUN.VALUE = character(1L))
#'     out = lapply(res, tts_audio_read,
#'                  output_format = audio_type)
#'     df = dplyr::tibble(original_text = string,
#'                        text = strings,
#'                        wav = out, file = res)
#'     df
#'   })
#'   names(res) = length(text)
#'   res = dplyr::bind_rows(res, .id = "index")
#'   res$index = as.numeric(res$index)
#'   res$audio_type = audio_type
#'   if (bind_audio) {
#'     res = tts_bind_wav(res)
#'   }
#'   if ("wav" %in% colnames(res)) {
#'     res$duration = vapply(res$wav, wav_duration, FUN.VALUE = numeric(1))
#'   }
#'   
#'   return(res)
#'   
#' }