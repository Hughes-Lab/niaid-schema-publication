library(googlesheets4)
library(tidyverse)
library(extrafont)


extrafont::loadfonts()

colour = "#123f55"

url = "https://docs.google.com/spreadsheets/d/1K1Jqy3CeCtRYtgC3RiC1FICqQFA8sCy2h5UafiAFk-U/edit#gid=1332971556"

df = googlesheets4::read_sheet(url) 

df = df %>% 
  mutate(generalLabel = ifelse(generalPurpose == TRUE, "generalist", "specialist"))

totals = df %>% 
  group_by(generalLabel) %>% 
  count(schemaorgCompliant) %>% 
  mutate(total = sum(n),
         pct = scales::percent(n/total),
         label = str_c("(", n, " / ", total, ")")) %>% 
  filter(schemaorgCompliant == TRUE)


ggplot(df, aes(x = generalLabel, fill = schemaorgCompliant)) + 
  geom_bar() + 
  geom_text(aes(label = pct, y = total + 3), colour = colour, data = totals, family = "Lato", nudge_x = 0.15, hjust = 0, size = 5) +
  geom_text(aes(label = label, y = total + 3), colour = "#555555", data = totals, family = "Lato", nudge_x = -0.15, hjust = 0) +
  coord_flip() +
  ylim(c(0, 230)) + 
  ggtitle("Specialist repositories have lagged in adopting schema.org standards compared to generalist sites",
  # ggtitle("Generalist repositories have adopted schema.org standards more than specialist ones",
          subtitle = "Number of repositories with schema.org-compliant datasets") +
  scale_fill_manual(values = c("#babab0", colour)) +
  theme_minimal() + 
  theme(text = element_text(family = "Lato", size = 24), 
        title = element_text(size = 16),
        panel.grid.major.y = element_blank(),
        axis.title = element_blank())
