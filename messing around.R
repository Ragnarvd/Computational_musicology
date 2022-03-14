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


viz4 <- ggplot(study, aes(x=valence, fill=playlist_name,
                            text = paste(playlist_name)))+
  geom_density(alpha=0.7, color=NA)+
  scale_fill_manual(values=c(green, yellow, pink, blue))+
  labs(x="Valence", y="Density") +
  guides(fill=guide_legend(title="country"))+
  theme_minimal()+
  ggtitle("Distribution of Valence Data")

ggplotly(viz4, tooltip=c("text"))

key_country <- study%>%
  select(country, key)%>%
  group_by(country, key)%>%
  mutate(n=n())%>%
  unique()%>%
  group_by(key)%>%
  mutate(total=sum(n))%>%
  mutate(percent=round((n/total)*100))


viz3 <- ggplot(key_country, aes(x=key, fill=country, y = n, 
                                text = paste("Number of Songs: ", n, "<br>",
                                             "Percent Songs in Key: ", percent, "%")))+
  geom_bar(width=0.5, stat = "identity")+
  scale_fill_manual(values=c(green, yellow, pink, blue))+
  labs(x="Key", y="Number of Songs") +
  guides(fill=guide_legend(title="country"))+
  theme_minimal()+
  ggtitle("Musical Key Makeup by Playlist")

ggplotly(viz3, tooltip=c("text"))
