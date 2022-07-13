library(tidyverse)
library(httr)
library(jsonlite)
library(extrafont)
extrafont::loadfonts()


# url = "https://api.data.niaid.nih.gov/v1/query?q=includedInDataCatalog.name:%22Data%20Discovery%20Engine%22&size=1000"
url = "https://discovery.biothings.io/api/dataset/query?q=_meta.guide:(%22/guide/niaid%22%20OR%20%22/guide/niaid/ComputationalTool%22)&size=1000"

resp = httr::GET(url)

if(resp$status_code == 200) {
  results = jsonlite::fromJSON(content(resp, as="text"))
  df = results$hits
}

# Clean up outdated type info
df = df %>% mutate(category = str_replace_all(`@type`, "niaid:", ""),
       category = str_replace_all(category, "Niaid", "")) 

# Donut chart -------------------------------------------------------------
donutColors = c("#e05e8f", "#32142d")

df %>% 
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
getName = function(x) {
  if(length(x) > 0 && x != "NULL") {
    if("name" %in% names(x)) {
      source_names = x$name  
    } else {
      source_names = x
    }
    
    if(length(source_names) > 1) {
      return(paste(source_names, collapse = "/"))
    } else {
      return(source_names)
    }
  } else {
    return("not specified")
  }
}

sources = df %>% 
  mutate(source_str = sapply(sdPublisher, function(x) getName(x)),
         source_str = str_replace_all(source_str, " and ", "/"),
         source = str_split(source_str, "/"))


sources2 = sources %>%   
  unnest(cols = source) %>% 
  mutate(source = ifelse(str_trim(source) == "GEO (pprivate)", "GEO", source)) %>% 
  count(category, source) %>%
  arrange(desc(n))

sources2$source = factor(sources2$source, sources2 %>% pull(source) %>% unique() %>% rev())

ggplot(sources2, aes(x = source, y = n)) + 
  geom_col() +
  coord_flip() +
  facet_wrap(~category, ncol=1) +
  ggtitle("Original metadata repository") +
  theme_minimal() +
  theme(text = element_text(family = "Lato", size = 18),
        axis.title = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "none")
  

# pathogens ---------------------------------------------------------
pathogens = df %>% mutate(pathogens = sapply(infectiousAgent, function(x) x$name)) %>% 
  unnest(cols = pathogens)

pathogens %>% count(pathogens) %>% 
  arrange(desc(n))

# measurement techniques ---------------------------------------------------------
pathogens = df %>% mutate(pathogens = sapply(measurementTechnique, function(x) getName(x))) %>% 
  unnest(cols = pathogens)

pathogens %>% count(pathogens) %>% 
  arrange(desc(n))
