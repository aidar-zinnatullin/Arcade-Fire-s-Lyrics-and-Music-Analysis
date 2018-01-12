### Arcade Fire

# Firstly, Let's download all necessary packages.
library(tidyverse)
library(httr)
library(stringr)
library(curl)
library(devtools)
library(rvest)
library(tidytext)
library(lubridate)
library(spotifyr)
library(scales)
library(RColorBrewer)
library(highcharter)


# Then you need to create client_id and client_secret from https://developer.spotify.com/web-api/. 
client_id <- 'XXXXXXXXXXX'
client_secret <- 'XXXXXXXXXXXXXXX'

Sys.setenv(SPOTIFY_CLIENT_ID = 'XXXXXXXXXXXXXXXXXX')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'XXXXXXXXXXXXXXXXXXX')

spotify_df <- get_artist_audio_features('Arcade Fire')

str(spotify_df)
# Filter out remixes and EPs
non_studio_albums <- c('Ghost Stories Live 2014', 'A Head Full Of Dreams Tour Edition', 'Viva La Vida - Prospekts March Edition', 'A Head Full Of Dreams Tour Edition')
spotify_df <- filter(spotify_df, !album_name %in% non_studio_albums)

token <- 'XXXXXXXXXXXXXXXXXXXXXXX'
genius_get_artists <- function(artist_name, n_results = 10) {
  baseURL <- 'https://api.genius.com/search?q=' 
  requestURL <- paste0(baseURL, gsub(' ', '%20', artist_name),
                       '&per_page=', n_results,
                       '&access_token=', token)
  
  res <- GET(requestURL) %>% content %>% .$response %>% .$hits
  
  map_df(1:length(res), function(x) {
    tmp <- res[[x]]$result$primary_artist
    list(
      artist_id = tmp$id,
      artist_name = tmp$name
    )
  }) %>% unique
}

genius_artists <- genius_get_artists('Arcade Fire')
genius_artists
genius_artists <- genius_artists %>% 
  filter(artist_name == 'Arcade Fire')




#Next, I looped through the contents of the songs endpoint 
#(the limit is 50 per page), pulling down each result 
#(a list containing the url of the tracks’ lyrics) 
#until the next_page parameter was null.

baseURL <- 'https://api.genius.com/artists/' 
requestURL <- paste0(baseURL, genius_artists$artist_id[1], '/songs')

track_lyric_urls <- list()
i <- 1
while (i > 0) {
  tmp <- GET(requestURL, query = list(access_token = token, per_page = 50, page = i)) %>% content %>% .$response
  track_lyric_urls <- c(track_lyric_urls, tmp$songs)
  if (!is.null(tmp$next_page)) {
    i <- tmp$next_page
  } else {
    break
  }
}

length(track_lyric_urls)


summary(track_lyric_urls[[1]])



#From here, I used rvest to scrape the “lyrics” elements from the urls provided above.

lyric_scraper <- function(url) {
  read_html(url) %>% 
    html_node('div.lyrics') %>% 
    html_text
}


genius_df <- map_df(1:length(track_lyric_urls), function(x) {
  # add in error handling
  lyrics <- try(lyric_scraper(track_lyric_urls[[x]]$url))
  if (class(lyrics) != 'try-error') {
    # strip out non-lyric text and extra spaces
    lyrics <- str_replace_all(lyrics, '\\[(Verse [[:digit:]]|Pre-Chorus [[:digit:]]|Hook [[:digit:]]|Chorus|Outro|Verse|Refrain|Hook|Bridge|Intro|Instrumental)\\]|[[:digit:]]|[\\.!?\\(\\)\\[\\],]', '')
    lyrics <- str_replace_all(lyrics, '\\n', ' ')
    lyrics <- str_replace_all(lyrics, '([A-Z])', ' \\1')
    lyrics <- str_replace_all(lyrics, ' {2,}', ' ')
    lyrics <- tolower(str_trim(lyrics))
  } else {
    lyrics <- NA
  }
  
  tots <- list(
    track_name = track_lyric_urls[[x]]$title,
    lyrics = lyrics
  )
  
  return(tots)
})


str(genius_df)


genius_df$track_name[genius_df$track_name == "Neighborhood #2 (Laïka)"] <- "Neighborhood #2 (Laika)"
genius_df$track_name[genius_df$track_name == "Une année sans lumière"] <- "Une Annee Sans Lumiere"
genius_df$track_name[genius_df$track_name == "Vampire / Forest Fire"] <- "Vampires / Forest Fire"
genius_df$track_name[genius_df$track_name == "Black Wave / Bad Vibrations"] <- "Black Wave/Bad Vibrations"
genius_df$track_name[genius_df$track_name== "Infinite_Content (2)"] <- "Infinite_Content"
genius_df$track_name[genius_df$track_name == "Everything Now (continued) (2)"] <- "Everything Now (continued)"



genius_df <- genius_df %>% 
  mutate(track_name_join = tolower(str_replace(track_name, '[[:punct:]]', ''))) %>% 
  filter(!duplicated(track_name_join)) %>% 
  select(-track_name)

track_df <- spotify_df %>%
  mutate(track_name_join = tolower(str_replace(track_name, '[[:punct:]]', ''))) %>%
  left_join(genius_df, by = 'track_name_join') %>%
  select(track_name, valence, duration_ms, lyrics, album_name, album_release_year, album_img)

str(track_df)

track_df %>% 
  select(valence, track_name) %>%
  arrange(valence) %>% 
  slice(1:10)


sad_words <- sentiments %>% 
  filter(lexicon == 'nrc', sentiment == 'sadness') %>% 
  select(word) %>% 
  mutate(sad = T)

sent_df <- track_df %>% 
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words, by = 'word') %>%
  left_join(sad_words, by = 'word') %>%
  group_by(track_name) %>% 
  summarise(pct_sad = round(sum(sad, na.rm = T) / n(), 4),
            word_count = n()) %>% 
  ungroup

sent_df %>% 
  select(pct_sad, track_name) %>%
  arrange(-pct_sad) %>% 
  head(10)






track_df <- track_df %>% 
  left_join(sent_df, by = 'track_name') %>% 
  mutate_at(c('pct_sad', 'word_count'), funs(ifelse(is.na(.), 0, .))) %>% 
  mutate(lyrical_density = word_count / duration_ms * 1000,
         gloom_index = round(rescale(1 - ((1 - valence) + (pct_sad * (1 + lyrical_density))) / 2, to = c(1, 100)), 2)) 

track_df %>%
  select(gloom_index, track_name) %>%
  arrange(gloom_index) %>%
  head(10)




plot_df <- track_df %>% 
  rowwise %>% 
  mutate(tooltip = paste0('<a style = "margin-right:', max(max(nchar(track_name), nchar(album_name)) * 7, 55), 'px">', # dynamic sizing
                          '<img src=', album_img, ' height="50" style="float:left;margin-right:5px">',
                          '<b>Album:</b> ', album_name,
                          '<br><b>Track:</b> ', track_name)) %>% 
  ungroup

avg_line <- plot_df %>% 
  group_by(album_release_year, album_name, album_img) %>% 
  summarise(avg = mean(gloom_index)) %>% 
  ungroup %>% 
  transmute(x = as.numeric(as.factor(album_release_year)), 
            y = avg,
            tooltip = paste0('<a style = "margin-right:55px">',
                             '<img src=', album_img, ' height="50" style="float:left;margin-right:5px">',
                             '<b>Album:</b> ', album_name,
                             '<br><b>Average Gloom Index:</b> ', round(avg, 2),
                             '</a>'))
plot_track_df <- plot_df %>% 
  mutate(tooltip = paste0(tooltip, '<br><b>Gloom Index:</b> ', gloom_index, '</a>'),
         album_number = as.numeric(as.factor(album_release_year))) %>% 
  ungroup

album_chart <- hchart(plot_track_df, 'scatter', hcaes(x = as.numeric(as.factor(album_release_year)), y = gloom_index, group = album_name)) %>% 
  hc_add_series(data = avg_line, type = 'line') %>%
  hc_tooltip(formatter = JS(paste0("function() {return this.point.tooltip;}")), useHTML = T) %>% 
  hc_colors(c(sample(brewer.pal(n_distinct(track_df$album_name), 'Paired')), 'black')) %>% 
  hc_xAxis(title = list(text = 'Album'), labels = list(enabled = F)) %>% 
  hc_yAxis(max = 100, title = list(text = 'Индекс мрака')) %>% 
  hc_title(text = 'Анализ текстов и музыки Arcade Fire по Spotify и Genius') %>% 
  hc_subtitle(text = 'Грусть в песнях Arcade Fire') %>% 
  hc_add_theme(hc_theme_smpl())
album_chart$x$hc_opts$series[[7]]$name <- 'Средний показатель по альбомам'
album_chart
