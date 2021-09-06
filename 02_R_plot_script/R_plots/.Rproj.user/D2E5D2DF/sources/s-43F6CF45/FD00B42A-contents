library(ggplot2)
library(GGally)
library(tidyr)
library(dplyr)
library(cowplot)
library(stringr)
library(ggpmisc)
library(ggThemeAssist)


#________________________Read CSV_____________________________________
Train_loss = read.csv('../../Data/Training_loss/Train_loss_1993_1995_and_2005_2007_prj_2017_2019.csv') 

ACC_metrics = read.csv('../../01_Jupyter_scripts/Variables/Acc_df_2005_2007_2017_2019.csv') 

landscape_metrics = read.csv('../../Data/Result_csv/lanscape_metrics_2005_2007_2017_2019.csv')

Historical_area = read.csv('../../Data/Result_csv/pix_count_df.csv')

Project_area = read.csv('../../Data/Result_csv/fit_ext_tf.csv')


#___________________Making Plots of area km2 trajectory ___________________
Area_trajectory = 
  ggplot() +
  geom_point(data = Historical_area,aes(x=year,y=area_km2,color='Hisorical'),size=2) +
  geom_line(data = Project_area,aes(x=year,y=area_km2,color = "Exponetial Fit")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line.x.bottom = element_line(),
        axis.line.y.left = element_line(),
        legend.position = c(0.2,0.8),
        legend.key = element_blank()) +
  scale_y_continuous(breaks = seq(0,1000000,50000),labels = seq(0,100,5)) +
  labs(y = bquote('Area ('*10^5 ~km^2*')'),
       x = 'Year')


#___________________Making Plots of model training ___________________
Train_fig = Train_loss %>% 
  filter(val_loss<0.028) %>% 
  ggplot() +
  geom_point(aes(x=epoch,y=val_loss),color='grey50',alpha=0.3) +
  geom_smooth(aes(x=epoch,y=val_loss),color='grey30',fill='grey65') +
  scale_x_continuous(breaks = seq(10,201,20)) +
  scale_y_continuous(breaks = seq(0,10,0.001)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line.x.bottom = element_line(),
        axis.line.y.left = element_line()) +
  xlab('Epoch')+
  ylab('Mean Squared Error') 




#___________________Making Plots of map-overlapping ACC ___________________
AUC_fig = ACC_metrics %>% 
  ggplot() +
  geom_histogram(aes(x = AUC),binwidth = 0.01,fill='grey70') +
  geom_density(aes(x=AUC)) + 
  scale_x_continuous(breaks = seq(0,1,0.05)) +
  scale_y_continuous(breaks = seq(0,10,2)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line.x.bottom = element_line(),
        axis.line.y.left = element_line()) +
  xlab('AUC')+
  ylab('Count') 


#__________

Hits_fig = ACC_metrics %>% 
  ggplot() +
  geom_histogram(aes(x = Hits),binwidth = 0.01,fill='grey70') +
  geom_density(aes(x=Hits))+ 
  scale_x_continuous(breaks = seq(0,1,0.05))+
  scale_y_continuous(breaks = seq(0,20,2)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line.x.bottom = element_line(),
        axis.line.y.left = element_line()) +
  xlab('Hits')+
  ylab('Count') 

FoM_fig = ACC_metrics %>% 
  ggplot() +
  geom_histogram(aes(x = FoM),binwidth = 0.01,fill='grey70') +
  geom_density(aes(x=FoM))+ 
  scale_x_continuous(breaks = seq(0,1,0.05))+
  scale_y_continuous(breaks = seq(0,20,2)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line.x.bottom = element_line(),
        axis.line.y.left = element_line()) +
  xlab('FoM')+
  ylab('') 

Overal_Acc_fig = ACC_metrics %>% 
  ggplot() +
  geom_histogram(aes(x = Overal_Acc),binwidth = 0.01,fill='grey70') +
  geom_density(aes(x=Overal_Acc))+ 
  scale_x_continuous(breaks = seq(0,1,0.05))+
  scale_y_continuous(breaks = seq(0,20,2)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line.x.bottom = element_line(),
        axis.line.y.left = element_line()) +
  xlab('Overal Accuracy')+
  ylab('') 

#_____
Overlay_acc_fig =  plot_grid(Hits_fig,
                              FoM_fig,
                              Overal_Acc_fig,
                              align = 'h',
                              labels = c('a)','b)','c)'),
                              label_x = 0.25,
                              label_y = 0.9,
                              vjust = 1.8,
                              rel_widths = c(3.6,4,4),
                              nrow = 1,
                              label_size = 12,
                              label_fontface = 'plain')
Overlay_acc_fig

#___
ACC_metrics_long = ACC_metrics %>% 
  pivot_longer(cols =  colnames(ACC_metrics)[3:length(ACC_metrics)],names_to = 'type') %>% 
  filter(type!='Change_ratio') %>% 
  mutate(type=case_when(type == 'Overal_Acc' ~ 'Overal Accuracy',
                        type == 'Hits' ~ 'Hit rate',
                        TRUE ~ as.character(type)))

ACC_metrics_long$type = factor(ACC_metrics_long$type,
                               levels = c("AUC","Overal Accuracy","Hit rate","FoM"))

ACC_metrics_long_fig = ACC_metrics_long %>% 
  ggplot() +
  geom_boxplot(aes(x=type,y=value)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line.x.bottom = element_line(),
        axis.line.y.left = element_line()) +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  xlab('Metric type')+
  ylab('Value') 

#___________________Making Plots of Landscape metrics ___________________
lm_eqn <- function(df,formula ){
  m <- lm(formula, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}


formula = Proj_number_of_patches ~ True_number_of_patches
Patch_number = landscape_metrics %>% 
  ggplot() +
  geom_point(aes(x=True_number_of_patches,y=Proj_number_of_patches)) +
  geom_abline(slope = 1,intercept = 0,linetype="dashed") +
  geom_smooth(aes(x=True_number_of_patches,y=Proj_number_of_patches),
              method = 'lm',color='grey30',fill='grey50') +
  geom_text(x = 153000, y = 57000,size = 2.5, 
            label = lm_eqn(landscape_metrics,formula), 
            parse = TRUE) +
  scale_y_continuous(breaks = seq(0,1000000,50000),labels = seq(0,100,5)) +
  scale_x_continuous(breaks = seq(0,1000000,50000),labels = seq(0,100,5)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line.x.bottom = element_line(),
        axis.line.y.left = element_line()) +
  labs(y = bquote('Patch Number prediction ('*10^5*') '),
       x = bquote('Patch Number real ('*10^5*') '))

  
formula = Proj_area_mn ~ True_area_mn
Mean_patch_area = landscape_metrics %>% 
  ggplot() +
  geom_point(aes(x=True_area_mn,y=Proj_area_mn)) +
  geom_smooth(aes(x=True_area_mn,y=Proj_area_mn),
              method = 'lm',color='grey30',fill='grey50') +
  geom_text(x = 0.7, y = 3, 
            label = lm_eqn(landscape_metrics,formula), 
            parse = TRUE) +
  geom_abline(slope = 1,intercept = 0,linetype="dashed") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line.x.bottom = element_line(),
        axis.line.y.left = element_line()) +
  xlab('Mean Patch Area (real)')+
  ylab('Mean Patch Area (prediction)')

formula = Proj_patch_density ~ True_patch_density
Patch_density_fig = landscape_metrics %>% 
  ggplot() +
  geom_point(aes(x=True_patch_density,y=Proj_patch_density)) +
  geom_smooth(aes(x=True_patch_density,y=Proj_patch_density),
              method = 'lm',color='grey30',fill='grey50') +
  geom_abline(slope = 1,intercept = 0,linetype="dashed") +
  geom_text(x = 2, y = 4, size = 2.5,
            label = lm_eqn(landscape_metrics,formula), 
            parse = TRUE) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line.x.bottom = element_line(),
        axis.line.y.left = element_line()) +
  xlab('Patch Denstity (real)')+
  ylab('Patch Denstity (prediction)') 

formula = Proj_landscape_shape_index ~ True_landscape_shape_index
Shape_index_fig = landscape_metrics %>% 
  ggplot() +
  geom_point(aes(x=True_landscape_shape_index,y=Proj_landscape_shape_index)) +
  geom_smooth(aes(x=True_landscape_shape_index,y=Proj_landscape_shape_index),
              method = 'lm',color='grey30',fill='grey50') +
  geom_abline(slope = 1,intercept = 0,linetype="dashed") +
  geom_text(x = 200, y = 400, size = 2.5, 
            label = lm_eqn(landscape_metrics,formula), 
            parse = TRUE) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line.x.bottom = element_line(),
        axis.line.y.left = element_line()) +
  xlab('Shape Index (real)')+
  ylab('Shape Index (prediction)') 

formula = Proj_largest_patch_index ~ True_largest_patch_index
Largest_patch = landscape_metrics %>% 
  filter(True_largest_patch_index<0.5) %>% 
  filter(Proj_largest_patch_index<0.5) %>% 
  ggplot() +
  geom_point(aes(x=True_largest_patch_index,y=Proj_largest_patch_index)) +
  geom_smooth(aes(x=True_largest_patch_index,y=Proj_largest_patch_index),
              method = 'lm',color='grey30',fill='grey50') +
  geom_abline(slope = 1,intercept = 0,linetype="dashed") +
  geom_text(x = 0.05, y = 0.4, size = 2.5, 
            label = lm_eqn(landscape_metrics,formula), 
            parse = TRUE) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line.x.bottom = element_line(),
        axis.line.y.left = element_line()) +
  xlab('Largest Patch Index (real)')+
  ylab('Largest Patch (prediction)') 


Landscape_fig =  plot_grid(Patch_number,
                           Shape_index_fig,
                           Largest_patch,
                           align = 'h',
                           labels = c('a)','b)','c)'),
                           label_x = 0.25,
                           label_y = 0.9,
                           vjust = 1.8,
                           rel_widths = c(1,1,1),
                           nrow = 1,
                           label_size = 12,
                           label_fontface = 'plain')
Landscape_fig

#_________________

#__Training
ggsave(plot = Train_fig,
       "../Figures/R_Fig_1_Train_fig.png", 
       width = 10, 
       height = 6, 
       units = "cm",
       dpi=500)

ggsave(plot = Train_fig,
       "../Figures/R_Fig_1_Train_fig.svg", 
       width = 10, 
       height = 6, 
       units = "cm",
       dpi=500)

#__AUC
ggsave(plot = AUC_fig,
       "../Figures/R_Fig_2_AUC_fig.png", 
       width = 10, 
       height = 6, 
       units = "cm",
       dpi=500)

ggsave(plot = AUC_fig,
       "../Figures/R_Fig_2_AUC_fig.pdf", 
       width = 10, 
       height = 6, 
       units = "cm",
       dpi=500)

#__Overlay metrics
ggsave(plot = Overlay_acc_fig,
       "../Figures/R_Fig_3_Overlay_acc.png", 
       width = 25, 
       height = 8, 
       units = "cm",
       dpi=500)


ggsave(plot = ACC_metrics_long_fig,
       "../Figures/R_Fig_3_2_ACC_metrics_long_fig.svg", 
       width = 10, 
       height = 8, 
       units = "cm",
       dpi=500)

ggsave(plot = ACC_metrics_long_fig,
       "../Figures/R_Fig_3_2_ACC_metrics_long_fig.png", 
       width = 10, 
       height = 8, 
       units = "cm",
       dpi=500)

ggsave(plot = Overlay_acc_fig,
       "../Figures/R_Fig_3_Overlay_acc.svg", 
       width = 25, 
       height = 8, 
       units = "cm",
       dpi=500)



#__Area trajectory
ggsave(plot = Area_trajectory,
       "../Figures/R_Fig_4_Area_trajectory.png", 
       width = 10, 
       height = 6, 
       units = "cm",
       dpi=500)

ggsave(plot = Area_trajectory,
       "../Figures/R_Fig_4_Area_trajectory.svg", 
       width = 10, 
       height = 6, 
       units = "cm",
       dpi=500)

#__Landscape 
ggsave(plot = Landscape_fig,
       "../Figures/R_Fig_5_Landscape_fig.pdf", 
       width = 29, 
       height = 9, 
       units = "cm",
       dpi=500)










