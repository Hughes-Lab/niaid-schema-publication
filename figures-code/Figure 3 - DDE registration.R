library(tidyverse)
library(httr)
library(jsonlite)
library(extrafont)
extrafont::loadfonts()


url = "https://api.data.niaid.nih.gov/v1/query?q=includedInDataCatalog.name:%22Data%20Discovery%20Engine%22&size=1000"

resp = httr::GET(url)

if(resp$status_code == 200) {
  results = jsonlite::fromJSON(content(resp, as="text"))
  df = results$hits
}



# Donut chart -------------------------------------------------------------
donutColors = c("#e05e8f", "#32142d")

df %>% 
  mutate(category = str_replace_all(`@type`, "niaid:", ""),
         category = str_replace_all(category, "Niaid", "")) %>% 
  count(category) %>% 
  mutate(ymax = sum(n), 
         ymin = n  ) %>% 
  ggplot(aes(x = 2.7, y = n, fill = category)) +
  geom_col() +
  annotate(geom = "text", x = 1, y = 0, label = str_c(df %>% count(), " total"), family = "Lato", size = 10) +
  annotate(geom = "text", x = 3.5, y = 0, label = str_c(df %>% filter(`@type` == "ComputationalTool") %>% count(), " computational tools"), family = "Lato", size = 6, colour = donutColors[1], hjust = 0) +
  annotate(geom = "text", x = 3.25, y = 100, label = str_c(df %>% filter(`@type` == "Dataset") %>% count(), " datasets"), family = "Lato", size = 6, colour = donutColors[2], hjust = 1) +
  coord_polar(theta="y", direction = -1) +
  xlim(c(1, 4)) +
  scale_fill_manual(values = donutColors) +
  theme_void() +
  theme(legend.position = "none")


# sdPublisher -------------------------------------------------------------
source = df %>% mutate(source = sapply(sdPublisher, function(x) x$name)) %>% 
  unnest(cols = source)


# pathogens ---------------------------------------------------------
pathogens = df %>% mutate(pathogens = sapply(infectiousAgent, function(x) x$name)) %>% 
  unnest(cols = pathogens)

pathogens %>% count(pathogens) %>% 
  arrange(desc(n))
