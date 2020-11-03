#midsom_R ikea furniture data

library(tidyverse)

ikea <- read_csv('./data/ikea.csv')

ikea_clean <- ikea %>% na.omit()

#getting additional information and boolean operator information
spicy_dog <- function(ikea_clean){
  funkea <- funkea %>% rowwise %>% mutate(other_colors_bool = if(other_colors == 'No') FALSE else TRUE,
                                          old_price_bool = if(old_price == 'No old price') FALSE else TRUE,
                                          size = depth * height * width,
                                          name_length = nchar(name),
                                          designer_name_length = nchar(designer))
  return(funkea)
}

funkea <- spicy_dog(ikea_clean)

ikea_deepmind <- function(funkea){
  punkea <- funkea %>% rowwise %>% mutate(is_above_90 = str_detect(as.character(price), ".*9[:digit:]$"))
  return(punkea)
}

punkea <- ikea_deepmind(funkea)

length(punkea$is_above_90[punkea$is_above_90 == TRUE])

punkea %>% ggplot(mapping = aes(x = is_above_90, y = price)) +
  geom_boxplot()

punkea %>% filter(designer_name_length < 30) %>% ggplot(mapping = aes(x = size, y = name_length, color = price)) +
  scale_color_viridis_c() +
  geom_point() +
  facet_wrap(~cut_interval(designer_name_length, 8))

