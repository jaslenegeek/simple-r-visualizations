install.packages('readxl')
install.packages('tidyverse')
install.packages('plyr')
install.packages('dplyr')
install.packages('stringr')
install.packages('taRifx')
install.packages('ggpubr')
install.packages('ggplot2')
install.packages('gridExtra')
install.packages('grid')
install.packages('lattice')
install.packages('reshape2')
install.packages('xlsx')
install.packages('cowplot')
install.packages('data.table')
install.packages('formattable')
install.packages('tidyr')
install.packages('htmltools')
install.packages('webshot') 
webshot::install_phantomjs()

# Load libraries
library(readxl)
library(tidyverse)
library(plyr)
library(dplyr)
library(stringr)
library(taRifx)
library(ggpubr)
library(ggplot2)
library(gridExtra)
library(grid)
library(lattice)
library(reshape2)
library(gplots)
library(xlsx)
library(cowplot)
library(data.table)
library(formattable)
library(tidyr)
library(htmltools)
library(webshot) 
library(htmlwidgets)

# Set working directory
setwd('directory/folder') # change path

data<-read_excel('***path for data here***', sheet='Sheet1')

# Copy data for contingency tables
data2<-data

# Replace NA characters
data$T2_radius = str_replace(data$T2_radius,'NA','0')
data$T2_radius<-sapply(data$T2_radius, as.numeric)

# Replace values for T2_touching and CETO_touching
data$T2_touching[data$T2_touching == '1'] <- 'touching'
data$T2_touching[data$T2_touching == '0'] <- 'not touching'
data$CETO_touching[data$CETO_touching == '1'] <- 'touching'
data$CETO_touching[data$CETO_touching == '0'] <- 'not touching'

### FUNCTIONS

# Function for renaming t-test data
rename_ttest <- function(data) {
  names(data)[1] <- 'variable'
  names(data)[2] <- 't-value'
  names(data)[3] <- 'p-value'
  data
}

# Table visualization function    
table_viz <- function(data) {
  formattable(data, align = c("l", "r", "r"), # l is left align, r is right align
              list('colname' = formatter("span",
                                         style = ~ style(color = "grey",
                                                         font.weight = "bold"))))
}

# Function to save formattable as image
export_formattable <- function(f, file, width = "40%", height = NULL, 
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}

##### T-TESTS FOR MALE AND FEMALE GROUPS #####

table(data$sex) 

# Male and female subjects have different numbers so we will be using
# independent t-test with unequal variance

# Define dependent variables for looping
variables1<-c('T1Gd_radius',	'T0_radius', 'T2_radius',	'FLAIR_radius',
              'log10(D/rho)',	'D',	'rho',	'OverallSurvival',	'Pre_T2_distance',
              'Pre_CET0_distance',	'T0GD_overlap',	'CE_overlap',	'T2_overlap',
              'Age')

data_var<-data[variables1]
data_var<-as.data.frame(sapply(data_var, as.numeric))

# T-test dataframe
results1 <- ldply(
  variables1,
  function(colname) {
    test = t.test(data_var[[colname]] ~ data$sex)
    t_val = sprintf('%.5f', test$statistic)
    p_value = sprintf('%.5f', test$p.value)
    return(data.frame(colname=colname, t_value=t_val, p_value=p_value))
  })

results1 <- rename_ttest(results1)

table1 <- table_viz(results1)

export_formattable(table1, 'table_t-test_sex.png')

# Function for violin plot
violin_plot<-function(x,y) {
  ggplot(data, aes(x=data[[x]], y=data[[y]], fill=sex)) +
    geom_violin(trim=FALSE) +
    scale_fill_brewer(palette='Dark2') + 
    stat_summary(fun.y=mean, geom='point', size=2) + 
    theme_classic() +
    theme(legend.position='none',
          axis.title.x=element_blank()) +
    labs(y=y)
}

# Plotting multiple violin plots into one
variables1<-set_names(variables1)
sex_plots<-map(variables1, ~violin_plot('sex',.x))
cowplot1<-cowplot::plot_grid(plotlist = sex_plots)
save_plot('t-test_sex.png', cowplot1, base_height = 8, base_aspect_ratio = 1.4)

