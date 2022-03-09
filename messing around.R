library(dplyr)
library(spotifyr)
library(plotly)
library(ggplot2)

pop <- get_playlist_audio_features("", "37i9dQZF1DWSoyxGghlqv5")
classic <- get_playlist_audio_features("", "37i9dQZF1EIgLoMVUd9oTU")
electronic <- get_playlist_audio_features("", "37i9dQZF1DX0wMD4IoQ5aJ")
studying <- get_playlist_audio_features("", "7ex5hFjCiAdaXy6FFoe2Vi")

study <-
  pop %>%
  mutate(country = "Pop study") %>%
  bind_rows(classic %>% mutate(country = "Classical study")) %>%
  bind_rows(electronic %>% mutate(country = "Electronic study")) %>%
  bind_rows(studying %>% mutate(country = "Stu(dying)"))%>%
  mutate(
    country = fct_relevel(country, "Pop study, Classical study, Electronical study", "Stu(dying)")
  )
green <- "#1ed760"
yellow <- "#e7e247"
pink <- "#ff6f59"
blue <- "#17bebb"


Vieuw(study)
features <- get_track_audio_features(pop)
