##################
# Test relationships with street view image data
#########################

library(tidyverse)
library(here)

# here()
image_data <- here("google street view images",
                   "all_image_features.csv") %>%
  read_csv(show_col_types = FALSE) #%>%
  # filter(!is.na(median_income_E))

### Come up with a few regression models that predict
 # a census variable using only image variables

pred_income_model <- lm(median_income_E ~
                          paved_score +
                          built_score +
                          pole_score +
                          terrain_score +
                          sky_score,
                        data = image_data)

summary(pred_income_model)

AIC(pred_income_model)

pred_gini_model <- lm(gini_index_E ~
                          paved_score +
                          built_score +
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


                      data = image_data)

summary(pred_veg_model)

AIC(pred_veg_model)

ggplot(image_data,
       aes(x=building_score,
           y=median_income_E))+
  geom_point()+
  stat_smooth(method = "lm")+
  theme_minimal()

ggplot(image_data, 
       aes(x = building_score,
           y = median_income_E)) +
  geom_hex() +
  stat_smooth(formula = y ~ x,method = "lm") +
  theme_minimal()


ggplot(image_data, 
       aes(x = pole_score,
           y = median_income_E)) +
  geom_hex(bins = 60) +
  scale_fill_viridis_c() +
  stat_smooth(method = "lm",
              color = "orange") +
  theme_minimal()


cor.test(image_data$gini_index_E, image_data$pole_score)

## For more on visualizing regression results:

#### https://jtools.jacob-long.com/articles/summ.html
#### https://jtools.jacob-long.com/articles/effect_plot.html
#### https://interactions.jacob-long.com/

imgdata <- as.data.frame(image_data)
img_pre <- c('built_score','paved_score','sky_score','nature_score','terrain_score','vegetation_score','pole_score')
cen_pre <- c('median_income_E','gini_index_E','pct_rental_E','pct_sf_homes_E')
for ( i in img_pre) {
  for (j in cen_pre){
    print(c(i,",",j))
    x <- imgdata[,j]
    y <- imgdata[,i]
    print(cor.test(x, y))
  }
}  

for ( i in cen_pre) {
  for (j in img_pre){
    print(c(i,",",j))
    x <- imgdata[,j]
    y <- imgdata[,i]
    print(cor.test(x, y))
  }
}

# for ( i in img_pre) {
#   for (j in cen_pre){
#     print(c(i,",",j))
#     x <- imgdata[,j]
#     y <- imgdata[,i]
#     ggplot(image_data, 
#            aes(x = j,
#                y = i)) +
#       geom_hex(bins = 60) +
#       scale_fill_viridis_c() +
#       stat_smooth(method = "lm",
#                   color = "white") +
#       theme_minimal()
#     ggsave(here("regression",
#                 "cor_img",
#                 c(i,j,".png")))
#   }
# }
