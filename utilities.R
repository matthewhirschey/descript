make_same_sample_rate = function(audio, verbose = TRUE) {
  if (inherits(audio, "Wave")) return(audio)
  
  sample_rate = sapply(audio, function(r) r@samp.rate)
  if (!all(sample_rate == sample_rate[[1]]) && verbose) {
    message("enforcing same sample rate, using minimum")
  }
  sample_rate = min(sample_rate, na.rm = TRUE)
  if (verbose) {
    message(paste0("Sample rate downsampled to ", sample_rate))
  }
  audio = lapply(audio, function(x) {
    if (x@samp.rate == sample_rate) return(x)
    tuneR::downsample(x, samp.rate = sample_rate)
  })
  sample_rate = sapply(audio, function(r) r@samp.rate)
  stopifnot(all(sample_rate == sample_rate[[1]]))
  return(audio)
}

is_Wave <- function(x){
  identical(suppressWarnings(as.character(class(x))), "Wave")
}

# get random string
grs <- function(){
  paste(sample(c(seq(10), letters, LETTERS),
               size = 12, replace = TRUE), collapse = "")
}

# # how long is a wav?
# duration <- function(wav){
#   stopifnot(is_Wave(wav))
#   length(wav@left) / wav@samp.rate
# }

# get from list
# list, name of element, default
gfl <- function(l, n, d){
  if(is.null(l[[n]])){
    d
  } else {
    l[[n]]
  }
}

#' @importFrom purrr map_chr compose
string_tirm <- function(s){
  str_rev <- function(t){
    paste(rev(strsplit(t, NULL)[[1]]), collapse = "")
  }
  
  str_trim_right <- function(x){
    sub("\\s+$", "", x)
  }
  
  str_trim_left <- function(x){
    x <- str_rev(x)
    x <- str_trim_right(x)
    str_rev(x)
  }
  
  lr <- compose(str_trim_left, str_trim_right)
  map_chr(s, lr)
}

# get text from html comments in an Rmd
parse_html_comments <- function(path){
  lines_ <- readLines(path, warn = FALSE)
  starts <- grep("<!--", lines_)
  ends <- grep("-->", lines_)
  
  if(length(starts) != length(ends)){
    stop("There's a comment open/close mismatch.")
  }
  
  result <- rep(NA, length(starts))
  
  for(i in seq_along(starts)){
    if(starts[i] == ends[i]){ # Single line
      result[i] <- lines_[starts[i]]
    } else {
      # Multiple lines
      result[i] <- paste(string_tirm(lines_[starts[i]:ends[i]]),
                         collapse = " ")
    }
    result[i] <- sub("<!--", "", result[i])
    result[i] <- sub("-->", "", result[i])
  }
  
  string_tirm(result)
}

# split a big string into equal-ish sized pieces
#' @importFrom purrr map
split_up_text <- function(text){
  pieces <- ceiling(nchar(text)/1500)
  words <- strsplit(text, " ")[[1]]
  chunks <- split(words, ceiling(seq_along(words)/(length(words)/pieces)))
  map(chunks, paste, collapse = " ")
}

tts_audio_read = function(
  file,
  output_format = c("mp3", "wav") ) {
  output_format = match.arg(output_format)
  out = switch(
    output_format,
    wav = tuneR::readWave(file),
    mp3 = tuneR::readMP3(file),
  )
  return(out)
}
tts_temp_audio = function(output_format = c("mp3", "wav") ) {
  output_format = match.arg(output_format)
  ext = paste0(".", output_format)
  tempfile(fileext = ext)
}
# text = stri_rand_lipsum(5)
# text = paste(text[1:5], collapse = " ")

tts_split_text = function(text, limit = 5000) {
  stopifnot(is.character(text) & length(text) == 1)
  nc = nchar(text)
  if (any(nc > limit)) {
    pieces <- ceiling(nchar(text)/limit)
    words <- strsplit(text, " ")[[1]]
    indices = ceiling(seq_along(words)/(length(words)/pieces))
    chunks <- split(words, indices)
    text = vapply(chunks, paste, collapse = " ",
                  FUN.VALUE = character(1))
  }
  return(text)
}

#' Convert PCM to Wav
#'
#' @param input output from `get_synthesis`` from \code{aws.polly} or
#' PCM filename
#' @param output output file for Wav file
#' @param extensible passed to [tuneR::writeWave]
#' @param sample_rate Sampling rate for [tuneR::Wave]
#' @return A filename of the output
#' @export
#' @examples
#' fname = system.file("extdata", "pcm_file.wav", package = "text2speech")
#' res = pcm_to_wav(fname)
#' testthat::expect_error(tuneR::readWave(fname))
#' testthat::expect_is(tuneR::readWave(res), "Wave")
#'
#' \dontrun{
#' if (requireNamespace("aws.polly", quietly = TRUE)) {
#' text = "hey, ho, let's go!"
#' if (tts_amazon_auth()) {
#'    res = tts_amazon(text, output_format = "wav")
#' }
#' }
#' }
pcm_to_wav = function(
  input,
  output = tempfile(fileext = ".wav"),
  sample_rate = 16000,
  extensible = FALSE) {
  
  
  tfile = tempfile(fileext = ".pcm")
  if (is.raw(input)) {
    writeBin(input, con = tfile)
    input = tfile
  }
  stopifnot(file.exists(input))
  sz = file.size(input)
  buffer = 1000
  pcm <- readBin(input, what = integer(),
                 n = 2 * sz + buffer,
                 size = 2, endian = "little", signed = TRUE)
  wav = tuneR::Wave(pcm, samp.rate = sample_rate, bit = 16, pcm = TRUE)
  tuneR::writeWave(object = wav, filename = output, extensible = extensible)
  return(output)
}

wav_duration = function(object) {
  if (inherits(object, "Wave")) {
    l <- length(object@left)
    return(round(l / object@samp.rate, 2))
  } else {
    return(NA_real_)
  }
}


#' Pad Wave Objects
#'
#' @param wav list of Wave objects
#' @param duration If \code{NULL}, the duration will simply round
#' the Wave up to the next whole integer.  If not, these are the 
#' duration to pad the Wave *to*. For example 12 means the output 
#' Wave will have a length of 12 seconds.  Pass \code{NA} to those
#' Waves that you want simple rounding.
#'
#' @return A list of Wave objects, same length as input \code{wav}
#' @export
#'
#' @examples
#' wavs = list(
#' tuneR::noise(duration = 1.85*44100), 
#' tuneR::noise())
#' out = pad_wav(wavs)
#' dur = sapply(out, function(x)length(x@left)/ x@samp.rate)
#' duration = c(2, 2)
#' out = pad_wav(wavs, duration = duration)
#' dur = sapply(out, function(x)length(x@left)/ x@samp.rate)
#' stopifnot(all(dur == duration))
#' duration = c(2, 2.5)
#' out = pad_wav(wavs, duration = duration)
#' dur = sapply(out, function(x)length(x@left)/ x@samp.rate)
#' stopifnot(isTRUE(all.equal(dur, duration)))
pad_wav = function(wav, duration = NULL) {
  is_Wave = inherits(wav, "Wave")
  if (is_Wave) {
    wav = list(wav) 
  }
  if (is.null(duration)) {
    duration = rep(NA, length(wav))
  }
  stopifnot(length(duration) == length(wav))
  duration = mapply(function(wav, dur) {
    ideal_duration = ceiling(length(wav@left)/wav@samp.rate)
    if (!is.na(dur)) {
      ideal_duration = max(ideal_duration, dur)
    }
    ideal_duration
  }, wav, duration)
  
  out_wav = mapply(function(wav, ideal_duration) {
    left = rep(0, wav@samp.rate * ideal_duration - length(wav@left))
    right = numeric(0)
    if (wav@stereo) {
      right = left
    }
    end_wav = tuneR::Wave(
      left = left,
      right = right,
      bit = wav@bit, 
      samp.rate = wav@samp.rate,
      pcm = wav@pcm)
    wav <-  tuneR::bind(wav, end_wav)
    wav
  }, wav, duration, SIMPLIFY = FALSE)
  if (is_Wave) {
    out_wav = out_wav[[1]]
  }
  return(out_wav)
}

#' Create a video from images and audio
#'
#' Given a vector of paths to images (preferably \code{.jpg}s
#' or \code{.png}s) and a flat list of \code{\link[tuneR]{Wave}}s of equal
#' length this function will create an \code{.mp4} video file where each image
#' is shown with its corresponding audio. Take a look at the
#' \code{\link[tuneR]{readWave}} function if you want to import your audio
#' files into R. Please be sure that all images have the same dimensions.
#'
#' This function uses \href{https://ffmpeg.org/}{FFmpeg}
#' which you should be sure is installed before using this function. If running
#' \code{Sys.which("ffmpeg")} in your R console returns an empty string after
#' installing FFmpeg then you should set the path to FFmpeg on you computer to
#' an environmental variable using \code{Sys.setenv(ffmpeg = "path/to/ffmpeg")}.
#' The environmental variable will always override the result of
#' \code{Sys.which("ffmpeg")}.
#'
#' @param images A vector of paths to images.
#' @param audio A list of \code{Wave}s from tuneR.
#' @param duration a vector of numeric durations for each audio
#' track.  See \code{\link{pad_wav}}
#' @param output A path to the video file which will be created.
#' @param verbose print diagnostic messages.  If > 1, then more are printed
#' @param cleanup If \code{TRUE}, interim files are deleted
#' @param ffmpeg_opts additional options to send to \code{ffmpeg}.
#' This is an advanced option, use at your own risk
#' @param divisible_height Make height divisible by 2, which may
#' be required if getting "height not divisible by 2" error.
#' @param audio_codec The audio encoder for the splicing.  If this
#' fails, try \code{copy}.
#' @param video_codec The video encoder for the splicing.  If this
#' fails, see \code{ffmpeg -codecs}
#' @param audio_bitrate Bit rate for audio. Passed to \code{-b:a}.
#' @param video_bitrate Bit rate for video. Passed to \code{-b:v}.
#' @param video_sync_method Video sync method.  Should be
#' "auto" or `"vfr"` or a numeric.  See \url{https://ffmpeg.org/ffmpeg.html}.
#' @param pixel_format pixel format to encode for `ffmpeg`.
#' @param fast_start Adding `faststart` flags for YouTube and other sites,
#' see \url{https://trac.ffmpeg.org/wiki/Encode/YouTube}
#' @param deinterlace should the video be de-interlaced,
#' see \url{https://ffmpeg.org/ffmpeg-filters.html}, generally for
#' YouTube
#' @param frames_per_second frames per second of the video, should
#' be an integer
#' @param stereo_audio should the audio be forced to stereo,
#' corresponds to `-ac 2`
#' @param video_filters any options that are passed to \code{-vf} arguments
#' for \code{ffmpeg}
#' @param check_inputs Should the inputs be checked?  Almost always should
#' be \code{TRUE}, but may be useful if trying to do customized stuff.
#' @return A logical value, with the attribute \code{outfile} for the
#' output file.

#' @importFrom purrr reduce discard
#' @importFrom tuneR bind writeWave
#' @export
#' @examples
#' \dontrun{
#' if (ffmpeg_version_sufficient()) {
#' result = ari_stitch(
#' ari_example(c("mab1.png", "mab2.png")),
#' list(tuneR::noise(), tuneR::noise()))
#' result = ari_stitch(
#' ari_example(c("mab1.png", "mab2.png")),
#' list(tuneR::noise(), tuneR::noise()), ffmpeg_opts = "-qscale 0",
#' verbose = 2)
#' # system2("open", attributes(result)$outfile)
#' }
#' }
ari_stitch <- function(
  images, audio,
  output = tempfile(fileext = ".mp4"),
  verbose = FALSE,
  cleanup = TRUE,
  ffmpeg_opts = "",
  divisible_height = TRUE,
  audio_codec = get_audio_codec(),
  video_codec = get_video_codec(),
  video_sync_method = "2",
  audio_bitrate = NULL,
  video_bitrate = NULL,
  pixel_format = "yuv420p",
  fast_start = FALSE,
  deinterlace = FALSE,
  stereo_audio = TRUE,
  duration = NULL,
  video_filters = NULL,
  frames_per_second = NULL,
  check_inputs = TRUE
){
  stopifnot(length(images) > 0)
  images <- normalizePath(images)
  output_dir <- normalizePath(dirname(output))
  stopifnot(
    length(audio) > 0,
    dir.exists(output_dir)
  )
  if (check_inputs) {
    stopifnot(
      identical(length(images), length(audio)),
      all(file.exists(images))
    )
  }
  if (is.character(audio)) {
    
    audio = lapply(audio, function(x) {
      ext = tolower(tools::file_ext(x))
      func = switch(ext,
                    wav = tuneR::readWave,
                    mp3 = tuneR::readMP3,
                    tuneR::readMP3)
      func(x)
    })
    audio = pad_wav(audio, duration = duration)
    #
    # audio = lapply(audio, function(wav) {
    #   ideal_duration <- ceiling(length(wav@left) / wav@samp.rate)
    #   left = rep(0,
    #              wav@samp.rate * ideal_duration - length(wav@left))
    #   right = numeric(0)
    #   if (wav@stereo) {
    #     right = left
    #   }
    #   end_wav = tuneR::Wave(
    #     left = left,
    #     right = right,
    #     bit = wav@bit, samp.rate = wav@samp.rate)
    #   wav <- bind(wav, end_wav)
    #   wav
    # })
  }
  # Make a hard path
  output = file.path(output_dir, basename(output))
  
  if (verbose > 0) {
    message("Writing out Wav for audio")
  }
  wav <- purrr::reduce(audio, bind)
  wav_path <- file.path(output_dir, paste0("ari_audio_", grs(), ".wav"))
  writeWave(wav, filename = wav_path)
  if (cleanup) {
    on.exit(unlink(wav_path, force = TRUE), add = TRUE)
  }
  
  
  # converting all to gif
  img_ext = tolower(tools::file_ext(images))
  any_gif = any(img_ext %in% "gif")
  if (any_gif & !all(img_ext %in% "gif")) {
    if (verbose > 0) {
      message("Converting All files to gif!")
    }
    for (i in seq_along(images)) {
      iext = img_ext[i]
      if (iext != "gif") {
        tfile = tempfile(fileext = ".gif")
        ffmpeg_convert(images[i], outfile = tfile)
        images[i] = tfile
      }
    }
  }
  
  input_txt_path <- file.path(output_dir,
                              paste0("ari_input_",
                                     grs(),
                                     ".txt"))
  ## on windows ffmpeg cancats names adding the working directory, so if
  ## complete url is provided it adds it twice.
  if (.Platform$OS.type == "windows") {
    new_image_names = file.path(output_dir, basename(images))
    if (!any(file.exists(new_image_names))) {
      file.copy(images, to = new_image_names)
    } else {
      warning("On windows must make basename(images) for ffmpeg to work")
    }
    images <- basename(images)
  }
  for (i in seq_along(images)) {
    cat(paste0("file ", "'", images[i], "'", "\n"),
        file = input_txt_path, append = TRUE)
    cat(paste0("duration ", wav_duration(audio[[i]]), "\n"),
        file = input_txt_path, append = TRUE)
  }
  cat(paste0("file ", "'", images[i], "'", "\n"),
      file = input_txt_path, append = TRUE)
  input_txt_path = normalizePath(input_txt_path, winslash = "/")
  
  # needed for users as per
  # https://superuser.com/questions/718027/
  # ffmpeg-concat-doesnt-work-with-absolute-path
  # input_txt_path = normalizePath(input_txt_path, winslash = "\\")
  
  ffmpeg = ffmpeg_exec(quote = TRUE)
  
  if (!is.null(frames_per_second)) {
    video_filters = c(video_filters, paste0("fps=", frames_per_second))
  } else {
    video_filters = c(video_filters, "fps=5")
  }
  if (divisible_height) {
    video_filters = c(video_filters, '"scale=trunc(iw/2)*2:trunc(ih/2)*2"')
  }
  
  
  # workaround for older ffmpeg
  # https://stackoverflow.com/questions/32931685/
  # the-encoder-aac-is-experimental-but-experimental-codecs-are-not-enabled
  experimental = FALSE
  if (!is.null(audio_codec)) {
    if (audio_codec == "aac") {
      experimental = TRUE
    }
  }
  if (deinterlace) {
    video_filters = c(video_filters, "yadif")
  }
  video_filters = paste(video_filters, collapse = ",")
  video_filters = paste0("-vf ", video_filters)
  
  if (any(grepl("-vf", ffmpeg_opts))) {
    warning("Found video filters in ffmpeg_opts, may not be used correctly!")
  }
  ffmpeg_opts = c(video_filters, ffmpeg_opts)
  ffmpeg_opts = paste(ffmpeg_opts, collapse = " ")
  
  
  # shQuote should seankross/ari#5
  command <- paste(
    ffmpeg, "-y",
    "-f concat -safe 0 -i", shQuote(input_txt_path),
    "-i", shQuote(wav_path),
    ifelse(!is.null(video_codec), paste("-c:v", video_codec),
           ""),
    ifelse(!is.null(audio_codec), paste("-c:a", audio_codec),
           ""),
    ifelse(stereo_audio, "-ac 2", ""),
    ifelse(!is.null(audio_bitrate), paste("-b:a", audio_bitrate),
           ""),
    ifelse(!is.null(video_bitrate), paste("-b:v", video_bitrate),
           ""),
    " -shortest",
    # ifelse(deinterlace, "-vf yadif", ""),
    ifelse(!is.null(video_sync_method), paste("-vsync", video_sync_method),
           ""),
    ifelse(!is.null(pixel_format), paste("-pix_fmt", pixel_format),
           ""),
    ifelse(fast_start, "-movflags +faststart", ""),
    ffmpeg_opts,
    ifelse(!is.null(frames_per_second), paste0("-r ", frames_per_second), ""),
    ifelse(experimental, "-strict experimental", ""),
    "-max_muxing_queue_size 9999",
    "-threads 2",
    shQuote(output))
  if (verbose > 0) {
    message(command)
  }
  if (verbose > 1) {
    message("Input text path is:")
    cat(readLines(input_txt_path), sep = "\n")
  }
  res = system(command)
  if (res != 0) {
    warning("Result was non-zero for ffmpeg")
  }
  
  if (cleanup) {
    on.exit(unlink(input_txt_path, force = TRUE), add = TRUE)
  }
  res = file.exists(output) && file.size(output) > 0
  if (!cleanup) {
    attr(res, "txt_path") = input_txt_path
    attr(res, "wav_path") = wav_path
    attr(res, "cmd") = command
  }
  attr(res, "outfile") = output
  attr(res, "images") = images
  invisible(res)
}

get_os = function() {
  sys_info = Sys.info()
  os = tolower(sys_info[["sysname"]])
  return(os)
}

#' Set Default Audio and Video Codecs
#'
#' @param codec The codec to use or get for audio/video.  Uses the
#' `ffmpeg_audio_codec` and `ffmpeg_video_codec` options
#' to store this information.
#' @seealso [ffmpeg_codecs()] for options
#' @return A `NULL` output
#'
#' 
#' @rdname codecs
#' @export
#' 
#' @examples
#' \dontrun{
#' if (have_ffmpeg_exec()) {
#' print(ffmpeg_version())
#' get_audio_codec()
#' set_audio_codec(codec = "libfdk_aac")
#' get_audio_codec()
#' set_audio_codec(codec = "aac")
#' get_audio_codec()
#' }
#' if (have_ffmpeg_exec()) {
#' get_video_codec()
#' set_video_codec(codec = "libx265") 
#' get_video_codec()
#' set_video_codec(codec = "libx264")
#' get_video_codec()
#' }
#' ## empty thing
#' if (have_ffmpeg_exec()) {
#' video_codec_encode("libx264")
#' 
#' audio_codec_encode("aac")
#' }
#' }
set_audio_codec = function(codec) {
  if (missing(codec)) {
    os = get_os()
    codec = switch(os,
                   darwin = "libfdk_aac",
                   windows = "ac3",
                   linux = "aac"
    )
  }
  options(ffmpeg_audio_codec = codec)
}

#' @export
#' @rdname codecs
set_video_codec = function(codec = "libx264") {
  options(ffmpeg_video_codec = codec)
}

#' @export
#' @rdname codecs
get_audio_codec = function() {
  codec = getOption("ffmpeg_audio_codec")
  if (is.null(codec)) {
    os = get_os()
    res = ffmpeg_audio_codecs()
    if (is.null(res)) {
      fdk_enabled = FALSE
    } else {
      fdk_enabled = grepl("fdk", res[ res$codec == "aac", "codec_name"])
    }
    if (fdk_enabled) {
      os_audio_codec = "libfdk_aac"
    } else {
      os_audio_codec = "aac"
    }
    codec = switch(os,
                   darwin = os_audio_codec,
                   windows = "ac3",
                   linux = "aac"
    )
    set_audio_codec(codec = codec)
  }    
  return(codec)
}

#' @export
#' @rdname codecs
get_video_codec = function() {
  codec = getOption("ffmpeg_video_codec")
  if (is.null(codec)) {
    codec = "libx264"
    set_video_codec(codec = codec)
  }
  return(codec)
}


#' @rdname codecs
#' @export
audio_codec_encode = function(codec) {
  res = ffmpeg_audio_codecs()
  if (is.null(res)) {
    warning("Codec could not be checked")
    return(NA)
  }  
  stopifnot(length(codec) == 1)
  res = res[ res$codec %in% codec | 
               grepl(codec, res$codec_name), ]
  res$encoding_supported
}

#' @rdname codecs
#' @export
video_codec_encode = function(codec) {
  res = ffmpeg_video_codecs()
  if (is.null(res)) {
    warning("Codec could not be checked")
    return(NA)
  }  
  stopifnot(length(codec) == 1)
  res = res[ res$codec %in% codec | 
               grepl(codec, res$codec_name), ]
  res$encoding_supported
}

#' Get Codecs for ffmpeg
#'
#' @return A `data.frame` of codec names and capabilities
#' @export
#'
#' @examples
#' \dontrun{
#' if (ffmpeg_version_sufficient()) {
#' ffmpeg_codecs()
#' ffmpeg_video_codecs()
#' ffmpeg_audio_codecs()
#' }
#' }
ffmpeg_codecs = function() {
  ffmpeg = ffmpeg_exec(quote = TRUE)
  cmd = paste(ffmpeg, "-codecs")
  result = system(cmd,  ignore.stderr = TRUE, ignore.stdout = TRUE)
  res = system(cmd, intern = TRUE, ignore.stderr = TRUE)
  res = trimws(res)
  if (length(res) == 0) {
    res = ""
  }
  if (result != 0 & all(res %in% "")) {
    warning("No codecs output from ffmpeg for codecs")
    return(NULL)
  }
  res = res[grepl("^([.]|D)", res)]
  res = strsplit(res, " ")
  res = t(vapply(res, function(x) {
    x = trimws(x)
    x = x[ x != ""]
    if (length(x) >= 3) {
      x[3:length(x)] = paste(x[3:length(x)], collapse = " ")
    }
    return(x[seq(3)])
  }, FUN.VALUE = character(3)))
  colnames(res) = c("capabilities", "codec", "codec_name")
  res = as.data.frame(res, stringsAsFactors = FALSE)
  
  if (nrow(res) == 0) {
    warning("No codecs output from ffmpeg for codecs")
    return(NULL)
  }   
  res$capabilities = trimws(res$capabilities)
  
  cap_defns = res[ res$codec == "=", ]
  res = res[ res$codec != "=", ]
  
  cap = do.call("rbind", strsplit(res$capabilities, split = ""))
  
  cap_defns$codec_name = tolower(cap_defns$codec_name)
  cap_defns$codec_name = gsub(" ", "_", cap_defns$codec_name)
  cap_defns$codec_name = gsub("-", "_", cap_defns$codec_name)
  cap_def = do.call("rbind", strsplit(cap_defns$capabilities, split = ""))
  
  mat = matrix(NA, ncol = nrow(cap_defns), nrow = nrow(cap))
  colnames(mat) = cap_defns$codec_name
  
  icol = 4
  indices = apply(cap_def, 1, function(x) which(x != "."))
  for (icol in seq(nrow(cap_def))) {
    x = cap[, indices[icol]]
    mat[, icol] = x %in% cap_def[icol, indices[icol]]
  }
  mat = as.data.frame(mat, stringsAsFactors = FALSE)
  
  res = cbind(res, mat)
  if (any(rowSums(
    res[, c("video_codec", "audio_codec", "subtitle_codec")])
    > 1)) {
    warning("Format may have changed, please post this issue")
  }
  
  # L = list(capabilities = cap_defns,
  #          codecs = res)
  # return(L)
  return(res)
}

#' @rdname ffmpeg_codecs
#' @export
ffmpeg_video_codecs = function() {
  res = ffmpeg_codecs()
  if (is.null(res)) {
    return(NULL)
  }
  res = res[ res$video_codec, ]
  res$video_codec = res$audio_codec = res$subtitle_codec = NULL
  res
}

#' @rdname ffmpeg_codecs
#' @export
ffmpeg_audio_codecs = function() {
  res = ffmpeg_codecs()
  if (is.null(res)) {
    return(NULL)
  }  
  res = res[ res$audio_codec, ]
  res$video_codec = res$audio_codec = res$subtitle_codec = NULL
  res
}



#' @rdname ffmpeg_codecs
#' @export
ffmpeg_muxers = function() {
  ffmpeg = ffmpeg_exec(quote = TRUE)
  cmd = paste(ffmpeg, "-muxers")
  result = system(cmd, ignore.stderr = TRUE, ignore.stdout = TRUE)
  res = system(cmd, intern = TRUE, ignore.stderr = TRUE)
  res = trimws(res)
  if (length(res) == 0) {
    res = ""
  }  
  if (result != 0 & all(res %in% "")) {
    warning("No codecs output from ffmpeg for muxers")
    return(NULL)
  }   
  res = res[grepl("^E", res)]
  res = strsplit(res, " ")
  res = t(vapply(res, function(x) {
    x = trimws(x)
    x = x[ x != ""]
    if (length(x) >= 3) {
      x[3:length(x)] = paste(x[3:length(x)], collapse = " ")
    }
    return(x[seq(3)])
  }, FUN.VALUE = character(3)))
  colnames(res) = c("capabilities", "muxer", "muxer_name")
  res = as.data.frame(res, stringsAsFactors = FALSE)
  if (nrow(res) == 0) {
    warning("No codecs output from ffmpeg for muxers")
    return(NULL)
  }   
  res$capabilities = trimws(res$capabilities)
  
  return(res)
}

#' @rdname ffmpeg_codecs
#' @export
ffmpeg_version = function() {
  ffmpeg = ffmpeg_exec(quote = TRUE)
  cmd = paste(ffmpeg, "-version")
  result = system(cmd, ignore.stderr = TRUE, ignore.stdout = TRUE)
  res = system(cmd, intern = TRUE, ignore.stderr = TRUE)
  res = trimws(res)
  if (length(res) == 0) {
    res = ""
  }  
  if (result != 0 & all(res %in% "")) {
    warning("No codecs output from ffmpeg for version")
    return(NULL)
  }  
  res = res[grepl("^ffmpeg version", res)]
  res = sub("ffmpeg version (.*) Copyright .*", "\\1", res)
  res = sub("(ubuntu|debian).*", "", res)
  res = sub("-.*", "", res)
  res = sub("[+].*", "", res)
  res = trimws(res)
  return(res)
}

#' @rdname ffmpeg_codecs
#' @export
ffmpeg_version_sufficient = function() {
  if (have_ffmpeg_exec()) {
    ver = package_version("3.2.4")
    ff_ver = ffmpeg_version()
    if (is.null(ff_ver)) {
      warning(paste0("Cannot get ffmpeg version from ", 
                     "ffmpeg_version, returning FALSE"))
      return(FALSE)
    }
    ff_ver_char = ff_ver
    ff_ver = package_version(ff_ver, strict = FALSE)
    if (is.na(ff_ver)) {
      warning(
        paste0(
          "ffmpeg version is not parsed, probably a development version,",
          "version was ", ff_ver_char, ", make sure you have >= ", 
          as.character(ver)
        )
      )
      return(TRUE)
    }
    res = ff_ver >= ver
  } else {
    res = FALSE
  }
  res
}

#' @rdname ffmpeg_codecs
#' @export
check_ffmpeg_version = function() {
  if (!ffmpeg_version_sufficient()) {
    ff = ffmpeg_version()
    stop(paste0(
      "ffmpeg version is not high enough,", 
      " ffmpeg version is: ", ff))
  }
  return(invisible(NULL))
}

#FFMPEG
#' Get Path to ffmpeg Executable
#'
#' @return The path to the \code{ffmpeg} executable, or an error.
#' @note This looks using `Sys.getenv("ffmpeg")` and `Sys.which("ffmpeg")`
#' to find `ffmpeg`.  If `ffmpeg` is not in your PATH, then please set the
#' path to `ffmpeg` using `Sys.setenv(ffmpeg = "/path/to/ffmpeg")`
#' @param quote should \code{\link{shQuote}} be run before returning?
#' @export
#'
#' @examples
#' \dontrun{
#' if (have_ffmpeg_exec()) {
#' ffmpeg_exec()
#' }
#' }
ffmpeg_exec = function(quote = FALSE) {
  ffmpeg <- discard(c(Sys.getenv("ffmpeg"),
                      Sys.which("ffmpeg")), ~ nchar(.x) == 0)[1]
  
  if (is.na(ffmpeg)) {
    stop(paste("Could not find ffmpeg. See the documentation ",
               "for ari_stitch() ",
               "for more details."))
  }
  if (!ffmpeg %in% c("ffmpeg", "ffmpeg.exe")) {
    ffmpeg = normalizePath(ffmpeg, winslash = "/")
  }
  if (quote) {
    ffmpeg = shQuote(ffmpeg)
  }
  return(ffmpeg)
}

#' @export
#' @rdname ffmpeg_exec
have_ffmpeg_exec = function() {
  exec = try({
    ari::ffmpeg_exec()
  }, silent = TRUE)
  !inherits(exec, "try-error")
}

#' Check error output from individual video
#'
#' @return The output of the error log
#' @param file path to video
#' @param verbose print diagnostic messages
#' @export
#'
ffmpeg_error_log = function(file, verbose = TRUE) {
  ffmpeg = ffmpeg_exec(quote = TRUE)
  
  file = normalizePath(file, winslash = "/", mustWork = TRUE)
  error_file = tempfile(fileext = ".txt")
  command <- paste(
    ffmpeg, "-v error",
    "-i", shQuote(file),
    paste0("-f null - 2>", error_file)
  )
  
  if (verbose > 0) {
    message(command)
  }
  res = system(command)
  if (!file.exists(error_file)) {
    stop("Error file not generated")
  }
  return(readLines(error_file))
}

