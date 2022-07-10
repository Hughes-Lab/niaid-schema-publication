library(googlesheets4)
library(tidyverse)
url = "https://docs.google.com/spreadsheets/d/1B9ecSU4nv_6S6gbovXrYDxwVvk5JL3hlJthDmydUsiY/edit"

df = googlesheets4::read_sheet(url)

df_wide = df %>% select(namespace, property, required) %>% 
  spread(namespace, required)

df_wide %>% 
  select(property, `schema.org`, Google, NIAID) %>% 
  write_csv("~/Desktop/test.csv")

df %>% select(namespace, property, type) %>% 
  spread(namespace, type) %>% 
  select(property, `schema.org`, Google, NIAID) %>% 
  write_csv("~/Desktop/test2.csv")
