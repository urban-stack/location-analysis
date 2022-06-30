##################
# Plotting relationships between street view image data and census data
##################
library(tidyverse)
library(here)
library(cowplot)

image_data <- here("google street view images",
                   "all_image_features.csv") %>%
  read_csv(show_col_types = FALSE)
imgdata <- as.data.frame(image_data)

central_plot <- function(x0,y0){
  x1 <- imgdata[,x0]
  y1 <- imgdata[,y0]
  
  cor_test_result <- cor.test(x1,y1)
  annotation <- paste0("cor = ",signif(cor_test_result$estimate,digits=2),
                       ",   p-value = ",signif(cor_test_result$p.value,digits=6))
  
  if(cor_test_result$p.value < 0.01){
  imgplot <- ggplot(imgdata, 
                    aes(x = x1,
                        y = y1)) +
    scale_x_continuous(name = "  ",limits = c(0,100)) +
    scale_y_continuous(name = "  " ) +
    geom_hex(bins = 30) +
    xlim(0, 100)+
    scale_fill_viridis_c() +
    stat_smooth(method = "lm",
                color = "orange") +
    theme_void()+
    theme(panel.grid = element_line(color = 'grey93'),
          axis.text = element_blank(),
          plot.margin = margin(2, 2, 2, 2, unit = "pt"),
          legend.key.size = unit(0.15,'cm'),
          legend.title = element_text(size = 5),
          legend.text = element_text(size = 4))
  } else {
  imgplot <- ggplot(imgdata, 
                    aes(x = x1,
                        y = y1)) +
    scale_x_continuous(name = "  ",limits = c(0,100)) +
    scale_y_continuous(name = "  " ) +
    theme_void()+
    theme(panel.grid = element_line(color = 'white'),
          axis.text = element_blank(),
          plot.margin = margin(2, 2, 2, 2, unit = "pt"),
          legend.key.size = unit(0.15,'cm'),
          legend.title = element_text(size = 5),
          legend.text = element_text(size = 4))  
  }
  
  cor_anno <- ggdraw(add_sub(imgplot,annotation,size = 6.5)) 
  
  return(cor_anno)
}


left_plot <- function(x0,y0){
  x1 <- imgdata[,x0]
  y1 <- imgdata[,y0]
  
  cor_test_result <- cor.test(x1,y1)
  annotation <- paste0("cor = ",signif(cor_test_result$estimate,digits=2),
                       ",   p-value = ",signif(cor_test_result$p.value,digits=6))
  
  if(cor_test_result$p.value < 0.01){
    imgplot <- ggplot(imgdata, 
                      aes(x = x1,
                          y = y1)) +
      scale_x_continuous(name = "  ",limits = c(0,100)) +
      scale_y_continuous(name = "  " ) +
      geom_hex(bins = 30) +
      xlim(0, 100)+
      scale_fill_viridis_c() +
      stat_smooth(method = "lm",
                  color = "orange") +
      theme_void()+
      theme(panel.grid = element_line(color = 'grey93'),
            axis.text.y.left = element_text(size = 4),
            plot.margin = margin(2, 2, 2, 2, unit = "pt"),
            legend.key.size = unit(0.15,'cm'),
            legend.title = element_text(size = 5),
            legend.text = element_text(size = 4))
  } else {
    imgplot <- ggplot(imgdata, 
                      aes(x = x1,
                          y = y1)) +
      scale_x_continuous(name = "  ",limits = c(0,100)) +
      scale_y_continuous(name = "  " ) +
      theme_void()+
      theme(panel.grid = element_line(color = 'white'),
            axis.text.y.left = element_text(size = 4),
            plot.margin = margin(2, 2, 2, 2, unit = "pt"),
            legend.key.size = unit(0.15,'cm'),
            legend.title = element_text(size = 5),
            legend.text = element_text(size = 4))  
  }
  
  cor_anno <- ggdraw(add_sub(imgplot,annotation,size = 6.5)) 
  
  return(cor_anno)
}

bottom_plot <- function(x0,y0){
  x1 <- imgdata[,x0]
  y1 <- imgdata[,y0]
  
  cor_test_result <- cor.test(x1,y1)
  annotation <- paste0("cor = ",signif(cor_test_result$estimate,digits=2),
                       ",   p-value = ",signif(cor_test_result$p.value,digits=6))
  
  if(cor_test_result$p.value < 0.01){
    imgplot <- ggplot(imgdata, 
                      aes(x = x1,
                          y = y1)) +
      scale_x_continuous(name = "  ",limits = c(0,100)) +
      scale_y_continuous(name = "  " ) +
      geom_hex(bins = 30) +
      xlim(0, 100)+
      scale_fill_viridis_c() +
      stat_smooth(method = "lm",
                  color = "orange") +
      theme_void()+
      theme(panel.grid = element_line(color = 'grey93'),
            axis.text.x.bottom = element_text(size = 4),
            plot.margin = margin(2, 2, 2, 2, unit = "pt"),
            legend.key.size = unit(0.15,'cm'),
            legend.title = element_text(size = 5),
            legend.text = element_text(size = 4))
  } else {
    imgplot <- ggplot(imgdata, 
                      aes(x = x1,
                          y = y1)) +
      scale_x_continuous(name = "  ",limits = c(0,100)) +
      scale_y_continuous(name = "  " ) +
      theme_void()+
      theme(panel.grid = element_line(color = 'white'),
            axis.text.x.bottom = element_text(size = 4),
            plot.margin = margin(2, 2, 2, 2, unit = "pt"),
            legend.key.size = unit(0.15,'cm'),
            legend.title = element_text(size = 5),
            legend.text = element_text(size = 4))  
  }
  
  cor_anno <- ggdraw(add_sub(imgplot,annotation,size = 6.5)) 
  
  return(cor_anno)
}

bottomleft_plot <- function(x0,y0){
  x1 <- imgdata[,x0]
  y1 <- imgdata[,y0]
  
  cor_test_result <- cor.test(x1,y1)
  annotation <- paste0("cor = ",signif(cor_test_result$estimate,digits=2),
                       ",   p-value = ",signif(cor_test_result$p.value,digits=6))
  
  if(cor_test_result$p.value < 0.01){
    imgplot <- ggplot(imgdata, 
                      aes(x = x1,
                          y = y1)) +
      scale_x_continuous(name = "",limits = c(0,100)) +
      scale_y_continuous(name = "  " ) +
      geom_hex(bins = 30) +
      xlim(0, 100)+
      scale_fill_viridis_c() +
      stat_smooth(method = "lm",
                  color = "orange") +
      theme_void()+
      theme(panel.grid = element_line(color = 'grey93'),
            axis.text.x.bottom = element_text(size = 4),
            axis.text.y.left = element_text(size = 4),
            plot.margin = margin(2, 2, 2, 2, unit = "pt"),
            legend.key.size = unit(0.15,'cm'),
            legend.title = element_text(size = 5),
            legend.text = element_text(size = 4))
  } else {
    imgplot <- ggplot(imgdata, 
                      aes(x = x1,
                          y = y1)) +
      scale_x_continuous(name = "",limits = c(0,100)) +
      scale_y_continuous(name = "  " ) +
      theme_void()+
      theme(panel.grid = element_line(color = 'white'),
            axis.text.x.bottom = element_text(size = 4),
            axis.text.y.left = element_text(size = 4),
            plot.margin = margin(2, 2, 2, 2, unit = "pt"),
            legend.key.size = unit(0.15,'cm'),
            legend.title = element_text(size = 5),
            legend.text = element_text(size = 4))  
  }
  
  cor_anno <- ggdraw(add_sub(imgplot,annotation,size = 6.5)) 
  
  return(cor_anno)
}


plot_1_2 <- central_plot('paved_score','median_income_E')
plot_1_3 <- central_plot('sky_score','median_income_E')
plot_1_4 <- central_plot('nature_score','median_income_E')
plot_1_5 <- central_plot('terrain_score','median_income_E')
plot_1_6 <- central_plot('vegetation_score','median_income_E')
plot_2_2 <- central_plot('paved_score','gini_index_E')
plot_2_3 <- central_plot('sky_score','gini_index_E')
plot_2_4 <- central_plot('nature_score','gini_index_E')
plot_2_5 <- central_plot('terrain_score','gini_index_E')
plot_2_6 <- central_plot('vegetation_score','gini_index_E')
plot_3_2 <- central_plot('paved_score','pct_rental_E')
plot_3_3 <- central_plot('sky_score','pct_rental_E')
plot_3_4 <- central_plot('nature_score','pct_rental_E')
plot_3_5 <- central_plot('terrain_score','pct_rental_E')
plot_3_6 <- central_plot('vegetation_score','pct_rental_E')
plot_4_2 <- central_plot('paved_score','pct_sf_homes_E')
plot_4_3 <- central_plot('sky_score','pct_sf_homes_E')
plot_4_4 <- central_plot('nature_score','pct_sf_homes_E')
plot_4_5 <- central_plot('terrain_score','pct_sf_homes_E')
plot_4_6 <- central_plot('vegetation_score','pct_sf_homes_E')
plot_5_2 <- central_plot('paved_score','annual_loans_per_sf_home')
plot_5_3 <- central_plot('sky_score','annual_loans_per_sf_home')
plot_5_4 <- central_plot('nature_score','annual_loans_per_sf_home')
plot_5_5 <- central_plot('terrain_score','annual_loans_per_sf_home')
plot_5_6 <- central_plot('vegetation_score','annual_loans_per_sf_home')

plot_7_2 <- central_plot('paved_score','income_ratio_sf')
plot_7_3 <- central_plot('sky_score','income_ratio_sf')
plot_7_4 <- central_plot('nature_score','income_ratio_sf')
plot_7_5 <- central_plot('terrain_score','income_ratio_sf')
plot_7_6 <- central_plot('vegetation_score','income_ratio_sf')
plot_8_2 <- central_plot('paved_score','pct_sf_purchase')
plot_8_3 <- central_plot('sky_score','pct_sf_purchase')
plot_8_4 <- central_plot('nature_score','pct_sf_purchase')
plot_8_5 <- central_plot('terrain_score','pct_sf_purchase')
plot_8_6 <- central_plot('vegetation_score','pct_sf_purchase')
plot_9_2 <- central_plot('paved_score','pct_sf_refi')
plot_9_3 <- central_plot('sky_score','pct_sf_refi')
plot_9_4 <- central_plot('nature_score','pct_sf_refi')
plot_9_5 <- central_plot('terrain_score','pct_sf_refi')
plot_9_6 <- central_plot('vegetation_score','pct_sf_refi')
plot_10_2 <- central_plot('paved_score','pct_sf_rehab')
plot_10_3 <- central_plot('sky_score','pct_sf_rehab')
plot_10_4 <- central_plot('nature_score','pct_sf_rehab')
plot_10_5 <- central_plot('terrain_score','pct_sf_rehab')
plot_10_6 <- central_plot('vegetation_score','pct_sf_rehab')
plot_11_2 <- central_plot('paved_score','pct_sf_cashout')
plot_11_3 <- central_plot('sky_score','pct_sf_cashout')
plot_11_4 <- central_plot('nature_score','pct_sf_cashout')
plot_11_5 <- central_plot('terrain_score','pct_sf_cashout')
plot_11_6 <- central_plot('vegetation_score','pct_sf_cashout')

plot_1_1 <- left_plot('built_score','median_income_E')
plot_2_1 <- left_plot('built_score','gini_index_E')
plot_3_1 <- left_plot('built_score','pct_rental_E')
plot_4_1 <- left_plot('built_score','pct_sf_homes_E')
plot_5_1 <- left_plot('built_score','annual_loans_per_sf_home')

plot_7_1 <- left_plot('built_score','income_ratio_sf')
plot_8_1 <- left_plot('built_score','pct_sf_purchase')
plot_9_1 <- left_plot('built_score','pct_sf_refi')
plot_10_1 <- left_plot('built_score','pct_sf_rehab')
plot_11_1 <- left_plot('built_score','pct_sf_cashout')

plot_6_2 <- bottom_plot('paved_score','annual_loans_per_mf_home')
plot_6_3 <- bottom_plot('sky_score','annual_loans_per_mf_home')
plot_6_4 <- bottom_plot('nature_score','annual_loans_per_mf_home')
plot_6_5 <- bottom_plot('terrain_score','annual_loans_per_mf_home')
plot_6_6 <- bottom_plot('vegetation_score','annual_loans_per_mf_home')

plot_12_2 <- bottom_plot('paved_score','mean_rate')
plot_12_3 <- bottom_plot('sky_score','mean_rate')
plot_12_4 <- bottom_plot('nature_score','mean_rate')
plot_12_5 <- bottom_plot('terrain_score','mean_rate')
plot_12_6 <- bottom_plot('vegetation_score','mean_rate')

plot_6_1 <- bottomleft_plot('built_score','annual_loans_per_mf_home')
plot_12_1 <- bottomleft_plot('built_score','mean_rate')

cor_mat1 <- ggdraw() +
  draw_plot(plot_1_1,
            x = 0, y = 1-0.166*1, height = 0.166, width = 0.166) +
  draw_plot(plot_1_2,
            x = 0.166*1, y = 1-0.166*1, height = 0.166, width = 0.166) +
  draw_plot(plot_1_3,
            x = 0.166*2, y = 1-0.166*1, height = 0.166, width = 0.166) +
  draw_plot(plot_1_4,
            x = 0.166*3, y = 1-0.166*1, height = 0.166, width = 0.166) +
  draw_plot(plot_1_5,
            x = 0.166*4, y = 1-0.166*1, height = 0.166, width = 0.166) +
  draw_plot(plot_1_6,
            x = 0.166*5, y = 1-0.166*1, height = 0.166, width = 0.166) +
  draw_plot(plot_2_1,
            x = 0, y = 1-0.166*2, height = 0.166, width = 0.166) +
  draw_plot(plot_2_2,
            x = 0.166*1, y = 1-0.166*2, height = 0.166, width = 0.166) +
  draw_plot(plot_2_3,
            x = 0.166*2, y = 1-0.166*2, height = 0.166, width = 0.166) +
  draw_plot(plot_2_4,
            x = 0.166*3, y = 1-0.166*2, height = 0.166, width = 0.166) +
  draw_plot(plot_2_5,
            x = 0.166*4, y = 1-0.166*2, height = 0.166, width = 0.166) +
  draw_plot(plot_2_6,
            x = 0.166*5, y = 1-0.166*2, height = 0.166, width = 0.166) +
  draw_plot(plot_3_1,
            x = 0, y = 1-0.166*3, height = 0.166, width = 0.166) +
  draw_plot(plot_3_2,
            x = 0.166*1, y = 1-0.166*3, height = 0.166, width = 0.166) +
  draw_plot(plot_3_3,
            x = 0.166*2, y = 1-0.166*3, height = 0.166, width = 0.166) +
  draw_plot(plot_3_4,
            x = 0.166*3, y = 1-0.166*3, height = 0.166, width = 0.166) +
  draw_plot(plot_3_5,
            x = 0.166*4, y = 1-0.166*3, height = 0.166, width = 0.166)+
  draw_plot(plot_3_6,
            x = 0.166*5, y = 1-0.166*3, height = 0.166, width = 0.166) +
  draw_plot(plot_4_1,
            x = 0, y = 1-0.166*4, height = 0.166, width = 0.166) +
  draw_plot(plot_4_2,
            x = 0.166*1, y = 1-0.166*4, height = 0.166, width = 0.166) +
  draw_plot(plot_4_3,
            x = 0.166*2, y = 1-0.166*4, height = 0.166, width = 0.166) +
  draw_plot(plot_4_4,
            x = 0.166*3, y = 1-0.166*4, height = 0.166, width = 0.166) +
  draw_plot(plot_4_5,
            x = 0.166*4, y = 1-0.166*4, height = 0.166, width = 0.166)+
  draw_plot(plot_4_6,
            x = 0.166*5, y = 1-0.166*4, height = 0.166, width = 0.166) +
  draw_plot(plot_5_1,
            x = 0, y = 1-0.166*5, height = 0.166, width = 0.166) +
  draw_plot(plot_5_2,
            x = 0.166*1, y = 1-0.166*5, height = 0.166, width = 0.166) +
  draw_plot(plot_5_3,
            x = 0.166*2, y = 1-0.166*5, height = 0.166, width = 0.166) +
  draw_plot(plot_5_4,
            x = 0.166*3, y = 1-0.166*5, height = 0.166, width = 0.166) +
  draw_plot(plot_5_5,
            x = 0.166*4, y = 1-0.166*5, height = 0.166, width = 0.166)+
  draw_plot(plot_5_6,
            x = 0.166*5, y = 1-0.166*5, height = 0.166, width = 0.166) +
  draw_plot(plot_6_1,
            x = 0, y = 1-0.166*6, height = 0.166, width = 0.166) +
  draw_plot(plot_6_2,
            x = 0.166*1, y = 1-0.166*6, height = 0.166, width = 0.166) +
  draw_plot(plot_6_3,
            x = 0.166*2, y = 1-0.166*6, height = 0.166, width = 0.166) +
  draw_plot(plot_6_4,
            x = 0.166*3, y = 1-0.166*6, height = 0.166, width = 0.166) +
  draw_plot(plot_6_5,
            x = 0.166*4, y = 1-0.166*6, height = 0.166, width = 0.166)+
  draw_plot(plot_6_6,
            x = 0.166*5, y = 1-0.166*6, height = 0.166, width = 0.166)

cor_w_legend1 <- ggdraw() +
  draw_plot(cor_mat1, x = 0.05, y = 0, width = 0.95, height = 0.95) +
  draw_label("built_score",x=0.05+0.95/12*1,y=0.96,size = 9.5)+
  draw_label("paved_score",x=0.05+0.95/12*3,y=0.96,size = 9.5)+
  draw_label("sky_score",x=0.05+0.95/12*5,y=0.96,size = 9.5)+
  draw_label("nature_score",x=0.05+0.95/12*7,y=0.96,size = 9.5)+
  draw_label("terrain_score",x=0.05+0.95/12*9,y=0.96,size = 9.5)+
  draw_label("vegetation_score",x=0.05+0.95/12*11,y=0.96,size = 9.5)+
  draw_label("median_  \nincome_E",x=0.025,y=0.95-0.95/12*1,size = 9.5)+
  draw_label("gini_     \nindex_E",x=0.025,y=0.95-0.95/12*3,size = 9.5)+
  draw_label("pct_       \nrental_E",x=0.025,y=0.95-0.95/12*5,size = 9.5)+
  draw_label("pct_sf_     \nhomeds_E",x=0.025,y=0.95-0.95/12*7,size = 9.5)+
  draw_label("annual_   \nloans_per\nsf_home ",x=0.025,y=0.95-0.95/12*9,size = 9.5)+
  draw_label("annual_   \nloans_per\nmf_home ",x=0.025,y=0.95-0.95/12*11,size = 9.5)

cor_w_legend1


cor_mat2 <- ggdraw() +
  draw_plot(plot_7_1,
            x = 0, y = 1-0.166*1, height = 0.166, width = 0.166) +
  draw_plot(plot_7_2,
            x = 0.166*1, y = 1-0.166*1, height = 0.166, width = 0.166) +
  draw_plot(plot_7_3,
            x = 0.166*2, y = 1-0.166*1, height = 0.166, width = 0.166) +
  draw_plot(plot_7_4,
            x = 0.166*3, y = 1-0.166*1, height = 0.166, width = 0.166) +
  draw_plot(plot_7_5,
            x = 0.166*4, y = 1-0.166*1, height = 0.166, width = 0.166) +
  draw_plot(plot_7_6,
            x = 0.166*5, y = 1-0.166*1, height = 0.166, width = 0.166) +
  draw_plot(plot_8_1,
            x = 0, y = 1-0.166*2, height = 0.166, width = 0.166) +
  draw_plot(plot_8_2,
            x = 0.166*1, y = 1-0.166*2, height = 0.166, width = 0.166) +
  draw_plot(plot_8_3,
            x = 0.166*2, y = 1-0.166*2, height = 0.166, width = 0.166) +
  draw_plot(plot_8_4,
            x = 0.166*3, y = 1-0.166*2, height = 0.166, width = 0.166) +
  draw_plot(plot_8_5,
            x = 0.166*4, y = 1-0.166*2, height = 0.166, width = 0.166) +
  draw_plot(plot_8_6,
            x = 0.166*5, y = 1-0.166*2, height = 0.166, width = 0.166) +
  draw_plot(plot_9_1,
            x = 0, y = 1-0.166*3, height = 0.166, width = 0.166) +
  draw_plot(plot_9_2,
            x = 0.166*1, y = 1-0.166*3, height = 0.166, width = 0.166) +
  draw_plot(plot_9_3,
            x = 0.166*2, y = 1-0.166*3, height = 0.166, width = 0.166) +
  draw_plot(plot_9_4,
            x = 0.166*3, y = 1-0.166*3, height = 0.166, width = 0.166) +
  draw_plot(plot_9_5,
            x = 0.166*4, y = 1-0.166*3, height = 0.166, width = 0.166)+
  draw_plot(plot_9_6,
            x = 0.166*5, y = 1-0.166*3, height = 0.166, width = 0.166) +
  draw_plot(plot_10_1,
            x = 0, y = 1-0.166*4, height = 0.166, width = 0.166) +
  draw_plot(plot_10_2,
            x = 0.166*1, y = 1-0.166*4, height = 0.166, width = 0.166) +
  draw_plot(plot_10_3,
            x = 0.166*2, y = 1-0.166*4, height = 0.166, width = 0.166) +
  draw_plot(plot_10_4,
            x = 0.166*3, y = 1-0.166*4, height = 0.166, width = 0.166) +
  draw_plot(plot_10_5,
            x = 0.166*4, y = 1-0.166*4, height = 0.166, width = 0.166)+
  draw_plot(plot_10_6,
            x = 0.166*5, y = 1-0.166*4, height = 0.166, width = 0.166) +
  draw_plot(plot_11_1,
            x = 0, y = 1-0.166*5, height = 0.166, width = 0.166) +
  draw_plot(plot_11_2,
            x = 0.166*1, y = 1-0.166*5, height = 0.166, width = 0.166) +
  draw_plot(plot_11_3,
            x = 0.166*2, y = 1-0.166*5, height = 0.166, width = 0.166) +
  draw_plot(plot_11_4,
            x = 0.166*3, y = 1-0.166*5, height = 0.166, width = 0.166) +
  draw_plot(plot_11_5,
            x = 0.166*4, y = 1-0.166*5, height = 0.166, width = 0.166) +
  draw_plot(plot_11_6,
            x = 0.166*5, y = 1-0.166*5, height = 0.166, width = 0.166) +
  draw_plot(plot_12_1,
            x = 0, y = 1-0.166*6, height = 0.166, width = 0.166) +
  draw_plot(plot_12_2,
            x = 0.166*1, y = 1-0.166*6, height = 0.166, width = 0.166) +
  draw_plot(plot_12_3,
            x = 0.166*2, y = 1-0.166*6, height = 0.166, width = 0.166) +
  draw_plot(plot_12_4,
            x = 0.166*3, y = 1-0.166*6, height = 0.166, width = 0.166) +
  draw_plot(plot_12_5,
            x = 0.166*4, y = 1-0.166*6, height = 0.166, width = 0.166) +
  draw_plot(plot_12_6,
            x = 0.166*5, y = 1-0.166*6, height = 0.166, width = 0.166) 

cor_w_legend2 <- ggdraw() +
  draw_plot(cor_mat2, x = 0.05, y = 0, width = 0.95, height = 0.95) +
  draw_label("built_score",x=0.05+0.95/12*1,y=0.96,size = 9.5)+
  draw_label("paved_score",x=0.05+0.95/12*3,y=0.96,size = 9.5)+
  draw_label("sky_score",x=0.05+0.95/12*5,y=0.96,size = 9.5)+
  draw_label("nature_score",x=0.05+0.95/12*7,y=0.96,size = 9.5)+
  draw_label("terrain_score",x=0.05+0.95/12*9,y=0.96,size = 9.5)+
  draw_label("vegetation_score",x=0.05+0.95/12*11,y=0.96,size = 9.5)+
  draw_label("income_\nratio_sf  ",x=0.025,y=0.95-0.95/12*1,size = 9.5)+
  draw_label("pct_sf_   \npurchase",x=0.025,y=0.95-0.95/12*3,size = 9.5)+
  draw_label("pct_sf_\nrefi       ",x=0.025,y=0.95-0.95/12*5,size = 9.5)+
  draw_label("pct_sf_ \nrehab    ",x=0.025,y=0.95-0.95/12*7,size = 9.5)+
  draw_label("pct_sf_ \ncashout",x=0.025,y=0.95-0.95/12*9,size = 9.5)+
  draw_label("mean_\nrate     ",x=0.025,y=0.95-0.95/12*11,size = 9.5)


cor_w_legend2
