library(plyr)
library(dplyr)
library(ggplot2)

## Read Data
full.data <- read.csv("full_dataset_statchecked.csv")
# Removing rows with NA for error 
full.data <- full.data[complete.cases(full.data[,21]),]

#Group Error by Article
full.data %>% 
  dplyr::group_by(PMCID, error) %>% #grouping article by PMCID
  dplyr::summarise(total_errors = sum(error)) -> full_errors_by_article #create a frequency table with number of errors per article

#Optional Themes
install.packages("ggsci")
library("ggsci")

# Plotting 
plot1 <- ggplot(full_errors_by_article, aes(x = PMCID, y = total_errors)) +
  geom_col(fill = "chocolate4")+ 
  xlab("Article") + 
  ylab("Number of Statistical Reporting Errors") + 
  theme_classic()+ 
  theme(axis.text.x=element_blank()) +
  ylim(0,80) 

plot1

# Reorder the plot
plot2 <- ggplot(error_count_by_article, aes(x = reorder(PMCID, Count), y = Count)) +
  geom_col(position = "dodge")+ 
  xlab("Article") + 
  ylab("Number of Statistical Reporting Errors") + 
  theme_classic()+ 
  theme(axis.text.x=element_blank()) +
  ylim(0,80)
  
plot2

# Geoff's suggestion for code edits
foo <- hist(full_errors_by_article$total_errors[!full_errors_by_article$total_errors==0], 
            breaks=1:max(full_errors_by_article$total_errors),
            xlim = c(1, 20),
            xaxt='n',
            xlab = "Article with x Number of Errors",
            ylab = "Frequency of Errors",
            main = element_blank(),
            col = "#374E55FF")
axis(side=1,at=foo$mids,labels=seq(1,78))

# Try to make it pretty?
plot3 <- qplot(full_errors_by_article$total_errors[!full_errors_by_article$total_errors==0],
               geom = "histogram",
               binwidth = 1,
               breaks=1:max(full_errors_by_article$total_errors),
               xlim = c(1, 20),
               xaxt='n',
               xlab = "Article with x Number of Errors",
               ylab = "Frequency of Errors",
               main = element_blank(),
               fill = I("#374E55FF"),
               col = I("black"),
               grid())  
plot3 <- plot3 + theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
plot3

# Geoff's code
full.data <- read.csv("full_dataset_statchecked.csv")
full.data <- full.data[complete.cases(full.data[,21]),]
#Group Error by Article
full.data %>%
  dplyr::group_by(PMCID, error) %>% #grouping article by PMCID
  dplyr::summarise(total_errors = sum(error)) -> errors_by_article #create a frequency table with number of errors per article


# Plotting
plot3 <- qplot(errors_by_article$total_errors[!errors_by_article$total_errors==0],
               geom = "histogram",
               binwidth = 1,
               xlim = c(1.5, 20),
               xlab = "Article with x Number of Errors",
               ylab = "Frequency of Errors",
               main = element_blank(),
               fill = I("#374E55FF"),
               col = I("black"))

plot3 <- plot3 + theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
plot3

## Geoff's code
# Read Data
pre.data <- read.csv("categorised_pre_data.csv")

# Removing rows with NA for error
pre.data <- pre.data[complete.cases(pre.data[,23]),]

#Group Error by Article
pre.data %>%
  dplyr::group_by(PMCID, error) %>% #grouping article by PMCID
  dplyr::summarise(total_errors = sum(error)) -> errors_by_article #create a frequency table with number of errors per article
errors_by_article <- pre.data %>% group_by(PMCID,error) %>% summarise(total_errors = sum(error))