lavinia <-
  get_tidy_audio_analysis("06MWWxWRuBtYxcJP71tN0q") %>%
  select(segments) %>%
  unnest(segments) %>%
  select(start, duration, pitches)
tierssen <-
  get_tidy_audio_analysis("14rZjW3RioG7WesZhYESso") %>%
  select(segments) %>%
  unnest(segments) %>%
  select(start, duration, pitches)
maria_dist <-
  compmus_long_distance(
    lavinia %>% mutate(pitches = map(pitches, compmus_normalise, "euclidean")),
    tierssen %>% mutate(pitches = map(pitches, compmus_normalise, "euclidean")),
    feature = pitches,
    method = "cosine"
  )
```

```{r tallis-plot}
maria <-
  maria_dist %>%
  mutate(
    lavinia = xstart + xduration / 2,
    tierssen = ystart + yduration / 2
  ) %>%
  ggplot(
    aes(
      x = lavinia,
      y = tierssen,
      fill = d
    )
  ) +
  geom_tile(aes(width = xduration, height = yduration)) +
  coord_fixed() +
  scale_x_continuous(
    breaks = c(0, 11, 29, 45, 65, 82, 100, 119, 126),
    labels =
      c(
        "start",
        "part 1join of the second hand",
        "part 2, higher pitches",
        "part 3 flurry of repeated triplets",
        "return to part 1",
        "return to part 2",
        "return to part 3",
        "fades out",
        ""
      ),
  ) +
  scale_y_continuous(
    breaks = c(0, 11, 31, 49, 69, 90, 110, 131, 140),
    labels =
      c(
        "start",
        "part 1join of the second hand",
        "part 2, higher pitches",
        "part 3 flurry of repeated triplets",
        "return to part 1",
        "return to part 2",
        "return to part 3",
        "fades out",
        ""
      ),
  ) +
  scale_fill_viridis_c(option = "E", guide = "none") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(x = "Lavinia meijers", y = "Yann Tierssen")
maria