library(tidyverse)
library(plotly)
library(showtext)
font_add("Gill Sans Light", "images/Gill Sans Light Regular.otf")
showtext_auto()


data <- read_csv('person_obtainer/labels.csv') |> 
  filter(confidence > 0.25)

data_cleaned <- data |> 
  mutate(book_id = as.integer(book_id)) |> 
  mutate(page_id = as.integer(page_id))

people <- data_cleaned |> 
  group_by(book_id, page_id, person) |> 
  tally()

ggplot(people)+
  geom_tile(aes(x=book_id, y=page_id, fill=n))+
  # scale_fill_gradient(high = "#361a7a",low = "#f5f5f5")+
  # scale_fill_gradient(high = "#aa347e",low = "#f5f5f5")+
  # scale_fill_gradient(high = "#e1c07c",low = "#f5f5f5")+
  # scale_fill_gradient(high = "#6f208a",low = "#f5f5f5")+
  scale_fill_gradient(high = "#da9a6f",low = "#f5f5f5")+
  scale_color_identity()+
  facet_wrap(vars(person), nrow=1, ncol=5)+
  scale_y_reverse()+
  ylab('')+
  xlab('')+
  theme(text=element_text(family="Gill Sans Light", size = 25))+
  theme(panel.background = element_rect(fill = '#f0f0f0', color='black', linewidth = 0.2),
        plot.background = element_rect(fill = '#f0f0f0'),
        panel.grid = element_line(color = '#f0f0f0'),
        axis.ticks.length  = unit(0.05, "cm"),
        axis.ticks  = element_line(color='black', linewidth=.2),
        legend.background = element_rect(fill='#f0f0f0'))+
  guides(fill = guide_colorbar(ticks.colour = NA, title=''))

panel_counts <- read_csv('person_obtainer/panel_counts.csv')

panel_counts <- panel_counts |> 
  group_by(book_id) |> 
  mutate(panel_number = row_number())

panels_per_book <- panel_counts |> 
  group_by(book_id) |> 
  summarize(
    n_pages = n_distinct(page_id),
    n_panels = n()
  ) |> 
  mutate(book_id = as.integer(book_id))

# person_appearances_panel <- data_cleaned |> 
#   distinct(book_id, page_id, panel_id, person, .keep_all = T) |> 
#   group_by(book_id, person) |> 
#   summarize(
#     panel_apperances = n()
#   ) |> 
#   left_join(panels_per_book) |> 
#   mutate(pct_panels = panel_apperances/n_panels)
# 
# person_appearances_panel |> ggplot(aes(x=book_id, y=pct_panels, fill=person), color='black')+
#   geom_bar(stat="identity")+
#   scale_colour_manual(values = c('tintin' = '#361a7a','snowy' = '#aa347e',
#                                  'thompson' = '#e1c07c','haddock' = '#6f208a', 'calculus' = '#da9a6f'),
#                       aesthetics = c('fill'))+
#   facet_wrap(vars(person), nrow=5, ncol=1)+
#   scale_y_continuous(labels = scales::percent)+
#   ylab('')+
#   xlab('')+
#   theme(text=element_text(family="Gill Sans Light", size=25))+
#   theme(panel.background = element_rect(fill = '#f0f0f0'),
#         plot.background = element_rect(fill = '#f8f8f9'))

####################
# person_appearances_page <- data_cleaned |> 
#   distinct(book_id, page_id, person, .keep_all = T) |> 
#   group_by(book_id, person) |> 
#   summarize(
#     page_apperances = n()
#   ) |> 
#   left_join(panels_per_book) |> 
#   mutate(pct_pages = page_apperances/n_pages)
# 
# person_appearances_page |> ggplot(aes(x=book_id, y=pct_pages, fill=person), color='black')+
#   geom_bar(stat="identity")+
#   scale_colour_manual(values = c('tintin' = '#361a7a','snowy' = '#aa347e',
#                                  'thompson' = '#e1c07c','haddock' = '#6f208a', 'calculus' = '#da9a6f'),
#                       aesthetics = c('fill'))+
#   facet_wrap(vars(person), nrow=5, ncol=1)+
#   scale_y_continuous(labels = scales::percent)+
#   ylab('')+
#   xlab('')+
#   theme(text=element_text(family="Gill Sans Light"))+
#   theme(panel.background = element_rect(fill = '#f0f0f0'),
#         plot.background = element_rect(fill = '#f8f8f9'))


characters_per_page <- data |>   
  distinct(book_id, page_id, person, .keep_all = T)|> 
  group_by(book_id, page_id) |> 
  tally()

ggplot(characters_per_page)+
  geom_tile(aes(y=as.integer(book_id), x=as.integer(page_id), fill=n))+
  #theme_void()+
  scale_fill_gradient(high = "#361a7a",low = "#f0f0f0")+
  scale_color_identity()+
  scale_y_reverse()+
  ylab('')+
  xlab('')+
  theme(text=element_text(family="Gill Sans Light", size=25))+
  theme(panel.background = element_rect(fill = '#f0f0f0'),
        plot.background = element_rect(fill = '#f0f0f0'),
        panel.grid = element_line(color = '#f0f0f0'),
        axis.ticks.length  = unit(0.05, "cm"),
        axis.ticks  = element_line(color='black', linewidth=.2),
        legend.background = element_rect(fill='#f0f0f0'))+
  guides(fill = guide_colorbar(ticks.colour = NA, title=''))


