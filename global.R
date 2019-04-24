library(tidyverse)
library(protoclust)
library(viridis)

## read in data
myURL <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-09-04/fastfood_calories.csv"

food <- read_csv(file = myURL)

## clean up data

## make item all lowercase for filtering later
food_use <- food %>% mutate(item = str_to_lower(item),
                            item = str_trim(item))

## remove salad column which has only one value = 'Other'
food_use <- food_use %>% select(-salad)

## modify calories and total_fat columns:
food_use <- food_use %>% mutate(cal_nonfat = calories - cal_fat,
                                other_fat = total_fat - sat_fat - trans_fat) %>% 
  select(-calories, -total_fat)

## filter out items such that represents ~1 psuedo order (subjective - here designated 6 piece chicken nuggets as an order; and 6" over footlong; also no kid-size subs)
food_use <- food_use %>% filter(!str_detect(item, "kid|footlong|piece")) %>% 
  bind_rows(food_use %>% filter(str_detect(item, "6 piece")))

## remove vit_a, vit_c, calcium bc many missing obs
food_use <- food_use %>% select(-vit_a, -vit_c, -calcium) %>% na.omit()

## remove what appear to be entry errors:
food_use <- food_use %>% filter(cal_nonfat > 0) ## negative value
food_use %>% filter(cholesterol > 600) ## extreme outlier
food_use <- food_use %>% filter(!X1 %in% 193) ## extreme outlier

## make new grouping variables (including vegetarian -- manually identified previously)
veg_x1 <- c(49, 55, 104:106, 192, 188, 185, 233, 234, 238, 303, 365, 367, 393, 397, 404, 407, 413, 414, 454, 462, 488, 489, 490)
food_use <- food_use %>% mutate(veg = ifelse(X1 %in% veg_x1, "vegetarian", "non-vegetarian"))
food_use <- food_use %>% mutate(salad = ifelse(str_detect(item, "salad"), "salad", "non-salad"))
food_use <- food_use %>% mutate(bacon = ifelse(str_detect(item, "bacon"), "bacon", "no bacon"))
food_use <- food_use %>% mutate(crispy = ifelse(str_detect(item, "crispy"), "crispy", "non-crispy"))
food_use <- food_use %>% mutate(bbq = ifelse(str_detect(item, "bbq"), "bbq", "non-bbq"))

## center/scale data before calculating distance 
prepro1 <- caret::preProcess(food_use %>% select(-X1, -restaurant, -item, -bbq, -bacon, -salad, -crispy, -veg), method = c("center", "scale"))
food_use_clust <- predict(prepro1, food_use)

## ID variable as categorical or continuous
cat_x <- c("cluster_method_1", "cluster_method_2", "restaurant", "veg", "salad", "bacon", "crispy", "bbq")
cont_x <- c("cal_fat", "cal_nonfat", "sat_fat", "trans_fat", "other_fat", "cholesterol", "sodium", "total_carb", "fiber", "sugar", "protein")
