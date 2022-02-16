install.packages('readxl')
install.packages('plyr')
install.packages('dplyr)')
install.packages('tidyverse')
install.packages('ggplot2')
install.packages('tidytext')
install.packages('bbplot')
install.packages('data.table')
install.packages('readr')
install.packages('lubridate')
install.packages('openxlsx')
library(readxl)
library(plyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(tidytext)
library(bbplot)
library(data.table)
library(readr)
library(lubridate)
library(openxlsx)

# Import data 
my_data<- read_excel("***Path to oncology data***", sheet = "main (trial)")


###################################  DISEASE_TT  ########################################

# Manipulate data and store in dt
dt <- my_data %>% 
  unnest_tokens(type, Disease_tt, token = "lines") %>% #take each line from Disease_tt and store in column called type
  dplyr::count(type, type, sort = TRUE) %>% #count column type and sort.
  mutate(percent = n/sum(n)*100) %>% #calucate percentage and stored in a new column call percent.
  drop_na()

# Set decimal places to 2 
dt$percent <- round(dt$percent ,digit=2)

ggplot(data = dt, aes(x = reorder(type, -percent), percent)) +
  geom_bar(stat = "identity",
           fill = "#1380A1") +
  theme_classic() +
  theme(text = element_text(size=10),
        plot.title = element_text(size=18,
                                  face="bold",
                                  color="#222222"),
        # plot.subtitle = element_text(size=12,
        #                              margin=ggplot2::margin(t=0,r=0,b=9,l=0)),
        panel.grid.major.x = element_line(color="#CBCBCB", size=2),
        panel.border = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_line(color = 'black', size = 1),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color='#CBCBCB'),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.margin = margin(t=5, r=5, b=5, l=5, "pt")) +
  coord_flip() +
  geom_text(aes(label=percent), vjust=0.3, hjust=-0.1, size=2.5) +
  expand_limits(x = 0, y = c(0, 12)) +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_discrete(expand = c(0, 0)) +
  labs(title='Percentage per Disease') +
  expand_limits(x = 0, y = c(0, 8))


###################################  SPONSOR_TT  ########################################

dt2 <- my_data %>% 
       unnest_tokens(group, Sponsor_tt, token = "lines") # same as above, separate data by lines, stored in group

tidy_dat<- dt2 %>%  select(group) #select only group

#filer each group according to words and stored in University, NIH_NIC and  Industry
University <- tidy_dat %>% group_by(group) %>% 
         filter(grepl('university|academic|cancer center', group)) #filter

NIH_NIC  <- tidy_dat %>% group_by(group) %>% 
  filter(grepl('nih|national institutes of health|nic|national cancer institute', group))

Industry <- tidy_dat %>% group_by(group) %>% 
  filter(!grepl('nih|national institutes of health|nic|national cancer institute|university|academic|cancer center', group))

#define 3 groups data to plot which is the length of each group
y <- c(length(University$group), length(NIH_NIC$group), length(Industry$group))
x <- c("University", "NIH/NIC", "Industry")

#to plot, it needs to be data frame
dataframe <- data.frame(x,y)

#plot
ggplot(dataframe, aes(x = reorder(x, y), y)) +
  geom_bar(stat="identity", 
           position="identity", 
           fill="#1380A1") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  bbc_style() +
  labs(title = "Sponsors")
  #      subtitle = "<Insert subtitle if any>")


#####################################  PHASE_TT  ########################################

dat3<- my_data

dat3$Phase_tt <- mapvalues(dat3$Phase_tt, "1/2", "2") #redefine group 1/2 ->2
dat3$Phase_tt <- mapvalues(dat3$Phase_tt, "2/3", "3")
dat3$Phase_tt <- mapvalues(dat3$Phase_tt, "3/4", "4")

#data manipulation same as above
dt3 <- dat3 %>% 
  unnest_tokens(type, Phase_tt, token = "lines") %>% 
  dplyr::count(type, type, sort = TRUE) %>%
  mutate(percent = n/sum(n)*100) %>%
  drop_na()

barchart_h(dt3, percent, type) +
  labs(title='Number of Investments - Originator') +
  expand_limits(x = 0, y = c(0, 55))

# Set decimal places to 2 
dt3$percent <- round(dt3$percent ,digit=2)

# Reorder Phases
dt3$type <- factor(dt3$type,levels = c("other", "4", "3", "2", "1"))

ggplot(data = dt3, aes(x = type, y = percent)) +
  geom_bar(stat = "identity",
           fill = "#1380A1") +
  theme_classic() +
  theme(text = element_text(size=10),
        plot.title = element_text(size=18,
                                  face="bold",
                                  color="#222222"),
        # plot.subtitle = element_text(size=12,
        #                              margin=ggplot2::margin(t=0,r=0,b=9,l=0)),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_line(color = 'black', size = 1),
        axis.ticks.y = element_line(color='#CBCBCB'),
        axis.ticks.x = element_line(color='#CBCBCB'),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(t=5, r=5, b=5, l=5, "pt")) +
  coord_flip() +
  geom_text(aes(label=percent), vjust=0.3, hjust=-0.1, size=2.5) +
  expand_limits(x = 0, y = c(0, 12)) +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_discrete(expand = c(0, 0)) +
  labs(title='Percentage per Phase') +
  expand_limits(x = 0, y = c(0, 40))

###################################  SPONSOR_CT  ########################################

dt5 <- my_data %>% 
  unnest_tokens(group, Sponsor_ct, token = "lines") 

tidy_dat5 <- dt5 %>%  select(group)

University <- tidy_dat5 %>% group_by(group) %>% 
  filter(grepl('university|academic|cancer center', group))

NIH_NIC  <- tidy_dat5 %>% group_by(group) %>% 
  filter(grepl('nih|national institutes of health|nic|national cancer institute', group))

Industry <- tidy_dat5 %>% group_by(group) %>% 
  filter(!grepl('nih|national institutes of health|nic|national cancer institute|university|academic|cancer center', group))

#define 3 groups data to plot
y <- c(length(University$group), length(NIH_NIC$group), length(Industry$group))
x <- c("University", "NIH/NIC", "Industry")

dataframe <- data.frame(x,y)

ggplot(dataframe, aes(x = reorder(x, y), y)) +
  geom_bar(stat="identity", 
           position="identity", 
           fill="#1380A1") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  bbc_style() +
  labs(title = "Sponsors_CT")

### FUNCTION

## Plot horizontal bar charts 

barchart_h <- function(dat, x, y){
  
  x <- enquo(x)
  y <- enquo(y)
  # y.var <- rlang::sym(y.var)
  
  # For ascending order: should be reorder (!!y, !!-x)
  # For descending, just remove the negative sign  
  ggp <- ggplot(data = dat, aes(x = reorder(!!y, -!!x), !!x)) +
    geom_bar(stat = "identity",
             fill = "#1380A1") +
    theme_classic() +
    theme(text = element_text(size=10),
          plot.title = element_text(size=18,
                                    face="bold",
                                    color="#222222"),
          # plot.subtitle = element_text(size=12,
          #                              margin=ggplot2::margin(t=0,r=0,b=9,l=0)),
          panel.grid.major.x = element_line(color="#CBCBCB", size=2),
          panel.border = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_line(color = 'black', size = 1),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_line(color='#CBCBCB'),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          plot.margin = margin(t=5, r=5, b=5, l=5, "pt")) +
    coord_flip() +
    geom_text(aes(label=n), vjust=0.3, hjust=-0.4, size=2.5) +
    expand_limits(x = 0, y = c(0, 12)) +
    scale_x_discrete(expand = c(0, 0)) + 
    scale_y_discrete(expand = c(0, 0))
  
  print(ggp)
}
