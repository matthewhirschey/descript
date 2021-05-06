#ari
library(tidyverse)
#library(ari)
library(here)

source(here::here("utilities.R"))
source(here::here("code", "fun.R"))
source(here::here("code", "token.R"))
source(here::here("ari_spin_descript.R"))
source(here::here("ari_narrate.R"))
source(here::here("tts_descript.R"))

# ari_narrate(
#   ari_example("ari_comments.Rmd"),
#   ari_example("ari_intro.html"),
#   voice = "Kendra")

# ari::ari_narrate(
#   script = "ari_test.Rmd",
#   slides = "ari_test.html",
#   service = "amazon",
#   output = glue::glue('{here::here()}/descript.mp4'),
#   voice = "Kendra")

ari_narrate(
  script = "ari_test.Rmd",
  slides = "ari_test.html",
  service = "descript",
  output = glue::glue('{here::here()}/descript.mp4'),
  voice = "Matthew", 
  delay = 0.5,
  capture_method = "iterative")
