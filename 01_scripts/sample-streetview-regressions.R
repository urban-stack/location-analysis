##################
# Test relationships with street view image data
#########################

library(tidyverse)
library(here)
library(cowplot)

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

pred_pct_rental_E_model <- lm(pct_rental_E ~
                        paved_score +
                        built_score +
                        terrain_score +
                        sky_score +
                        vegetation_score+
                        pole_score,
                      data = image_data)

summary(pred_pct_rental_E_model)

AIC(pred_pct_rental_E_model)

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

imgdata <- as.data.frame(image_data)
x1 <- imgdata[,'nature_score']
y1 <- imgdata[,'pct_sf_purchase']

imgplot <- ggplot(imgdata, 
       aes(x = x1,
           y = y1)) +
  scale_fill_viridis_c(
    # limit=c(0,150)
  ) +
  scale_y_continuous(name = "",
                      sec.axis = dup_axis(name = "pct_sf_purchase")) +
  scale_x_continuous(name = "",
                     #sec.axis = dup_axis(name = "built_score")
                     ) +
  geom_hex(bins = 30) +
  xlim(0, 100)+
  # ylim(0, 1.1)+
  stat_smooth(method = "lm",
              color = "orange") +
  theme_void()+
  theme(panel.grid = element_line(color = 'grey93'),
        #legend.position = "none",
        #axis.text = element_blank(),
        #axis.text.y = element_blank(),
        #axis.text.x = element_blank(),#element_text(),
        axis.text.x.bottom = element_text(size = 4),
        axis.text.y.left = element_text(size = 4),
        #axis.title.y.left = element_text(angle = 90),
        plot.margin = margin(2, 2, 2, 2, unit = "pt"),
        legend.key.size = unit(0.15,'cm'),
        legend.title = element_text(size = 5),
        legend.text = element_text(size = 4)
        )

diverse_drive <- ggplot(sites, 
                        aes(x = f_diverse, y = f_drivable)) +
  geom_hex(bins = 40) +
  scale_fill_viridis_c(trans = "log",
                       limits = c(1, 15000),
                       breaks = 10^seq(-1, 4, by=1)) +
  scale_y_continuous(name = "") +
  scale_x_continuous(name = "",
                     sec.axis = dup_axis(name = "Diverse")) +
  theme_void()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title.x.top = element_text(),
        plot.margin = margin(2, 2, 2, 2, unit = "pt"))

diverse_drive

cor_test_result <- cor.test(x1,y1)
annotation=paste0("cor=",signif(cor_test_result$estimate,digits=2),",   p-value=",signif(cor_test_result$p.value,digits=6))
annotation

cor_w_anno <- ggdraw(add_sub(imgplot,annotation,size = 6.5)) 
  
cor_w_anno


ggplot(imgdata, 
       aes(x = x1,
           y = y1)) +
  scale_x_continuous(name = "  ") +
  scale_y_continuous(name = "  ",
                     #sec.axis = dup_axis(name = "Amenities")
  ) +
  geom_hex(bins = 30) +
  xlim(0, 100)+
  # ylim(0, 1.1)+
  scale_fill_viridis_c(
    # limit=c(0,150)
  ) +
  stat_smooth(method = "lm",
              color = "orange") +
  theme_minimal()+
  theme(#panel.grid = element_line(color = 'grey90'),
        #legend.position = "none",
        axis.text = element_blank(),
        #axis.text.y = element_blank(),
        #axis.text.x = element_blank(),#element_text(),
        #axis.title.y.right = element_text(angle = 270),
        plot.margin = margin(2, 2, 2, 2, unit = "pt")
  )


cor.test(image_data$gini_index_E, image_data$pole_score)

cor_w_legend <- ggdraw() +
  draw_plot(cor_mat, x = 0, y = 0, width = 0.8, height = 1) +
  draw_plot(legend, x = 0.8, y= 0, width = 0.2, height = 1)

## For more on visualizing regression results:

#### https://jtools.jacob-long.com/articles/summ.html
#### https://jtools.jacob-long.com/articles/effect_plot.html
#### https://interactions.jacob-long.com/

imgdata <- as.data.frame(image_data)
img_pre <- c('built_score','paved_score','sky_score','nature_score','terrain_score','vegetation_score','pole_score')
cen_pre <- c('median_income_E','gini_index_E','pct_rental_E','pct_sf_homes_E')
for ( i in cen_pre) {
  for (j in img_pre){
    print(c(i,",",j))
    x <- imgdata[,j]
    y <- imgdata[,i]
    print(cor.test(x, y))
  }
}



