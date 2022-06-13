##################
# Test relationships with streetview image data
#########################

library(tidyverse)
library(here)

image_data <- here("google street view images",
                   "all_image_features.csv") %>%
  read_csv()

### Come up with a few regression models that predict
 # a census variable using only image variables

pred_income_model <- lm(median_income_E ~
                          road_score +
                          sidewalk_score +
                          building_score +
                          pole_score +
                          terrain_score +
                          sky_score.1,
                        data = image_data)

summary(pred_income_model)

AIC(pred_income_model)

pred_gini_model <- lm(gini_index_E ~
                          road_score +
                          sidewalk_score +
                          building_score +
                          pole_score +
                          terrain_score +
                          sky_score.1,
                        data = image_data)

summary(pred_gini_model)

AIC(pred_gini_model)

### Come up with a few regression models that predict an 
## image variable using only census variables

pred_veg_model <- lm(vegetation_score ~
                        income_ratio_sf +
                        mean_rate +
                        pct_rental_E +
                        pct_sf_purchase +
                        annual_loans_per_sf_home +
                        pct_sf_homes_E,
                      data = image_data)

summary(pred_veg_model)

AIC(pred_veg_model)


ggplot(image_data, 
       aes(x = building_score,
           y = median_income_E)) +
  geom_point() +
  stat_smooth() +
  theme_minimal()

## For more on visualizing regression results:

#### https://jtools.jacob-long.com/articles/summ.html
#### https://jtools.jacob-long.com/articles/effect_plot.html
#### https://interactions.jacob-long.com/

  
