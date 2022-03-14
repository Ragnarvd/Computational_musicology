bebop <-
  get_playlist_audio_features(
    "thesoundsofspotify",
    "7ex5hFjCiAdaXy6FFoe2Vi"
  ) %>%
  slice(1:30) %>%
  add_audio_analysis()
bigband <-
  get_playlist_audio_features(
    "thesoundsofspotify",
    "2tyb47DidJEgrVQ6VwhO6M"
  ) %>%
  slice(1:30) %>%
  add_audio_analysis()
jazz <-
  bebop %>%
  mutate(genre = "My study playlist") %>%
  bind_rows(bigband %>% mutate(genre = "Spotify study playlist"))

jazz %>%
  mutate(
    timbre =
      map(
        segments,
        compmus_summarise,
        timbre,
        method = "mean"
      )
  ) %>%
  select(genre, timbre) %>%
  compmus_gather_timbre() %>%
  ggplot(aes(x = basis, y = value, fill = genre)) +
  geom_violin() +
  scale_fill_viridis_d() +
  labs(x = "Spotify Timbre Coefficients", y = "", fill = "Genre")