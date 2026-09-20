#business case
#by Rosa Vazquez
#install packages
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud2")

#load packages
library(RColorBrewer)
library(RCurl)
library(tm)
library(SnowballC)
library(wordcloud2)

#frequency table of words
freqTable=read.table("clipboard",header=TRUE)

#> head(freqTable)
#word freq
#1    food  312
#2    good  276
#3    taco  271
#4   place  267
#5   great  230
#6 service  192

# bar plot of the frequency
barplot(freqTable$freq, las = 2, 
        names.arg = freqTable$word,
        col ="#00b3ca", main ="Most frequent words",
        ylab = "Word frequencies")

# Plot the word cloud
wordcloud2(freqTable, size=1.6, color='random-dark')

#more settings for wordcloud #https://r-graph-gallery.com/196-the-wordcloud2-library.html
#http://www.sthda.com/english/wiki/word-cloud-generator-in-r-one-killer-function-to-do-everything-you-need


#load packages
library(ggplot2)

# create a dataset
data=read.table("clipboard",header=TRUE, sep='\t')

#> head(data)
#Restaurant    Word   Percent label
#1 Arriba Arriba Sunnyside    food 0.7034884    70
#2 Arriba Arriba Sunnyside    good 0.4883721    49
#3 Arriba Arriba Sunnyside   place 0.4651163    47
#4 Arriba Arriba Sunnyside    taco 0.1511628    15
#5 Arriba Arriba Sunnyside   great 0.3023256    30
#6 Arriba Arriba Sunnyside service 0.3430233    34


#plot1
ggplot(data, aes(fill=Restaurant, y=label, x=Word)) + 
  geom_bar(position="stack", stat="identity")+
  theme_light()+
  ylab("% of reviews") + 
  xlab("Word") +
  theme(axis.text.x = element_text(angle = 90, hjust=1))


#plot2
library(dplyr)

ce <- data %>%
  arrange(Word, rev(Restaurant))

ce <- ce %>%
  group_by(Word) %>%
  mutate(label_y = cumsum(label) - 0.5 * label)

ggplot(ce, aes(fill=Restaurant, y=label, x=Word)) + 
  geom_col()+
  geom_text(data=subset(ce, label != 0), aes(label = label, y=label_y), size=3, color = "white")+
  theme_minimal()+
  ylab("% of reviews") + 
  xlab("Word") +
  theme(axis.text.x = element_text(angle = 90, hjust=1))

#negative review with the word taco
ggplot(ce, aes(fill=Restaurant, y=label, x=Word)) + 
  geom_col()+
  geom_text(data=subset(ce, label != 0), aes(label = label, y=label_y), size=3, color = "white")+
  theme_minimal()+
  ylab("% of negative reviews with the word 'taco'") + 
  xlab("Word") +
  theme(axis.text.x = element_text(angle = 90, hjust=1))

#positive review with the word taco
ggplot(ce, aes(fill=Restaurant, y=label, x=Word)) + 
  geom_col()+
  geom_text(data=subset(ce, label != 0), aes(label = label, y=label_y), size=3, color = "white")+
  theme_minimal()+
  ylab("% of positive reviews with the word 'taco'") + 
  xlab("Word") +
  theme(axis.text.x = element_text(angle = 90, hjust=1))  

##pie chart
# Load ggplot2
library(ggplot2)
library(dplyr)


# Create Data
data=read.table("clipboard",header=TRUE, sep='\t')

# Compute the position of labels
data <- data %>% 
  arrange(desc(Restaurant)) %>%
  mutate(prop = Reviews / sum(data$Reviews) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

# Basic piechart
ggplot(data, aes(x="", y=prop, fill=Restaurant)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")


geom_text(aes(y = ypos, label = prop), color = "white", size=3) +
  scale_fill_brewer(palette="Set1")


# Create Data-rating
data=read.table("clipboard",header=TRUE, sep='\t')

# Compute the position of labels
data <- data %>% 
  arrange(desc(Rating)) %>%
  mutate(prop = Reviews / sum(data$Reviews) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

# Basic piechart
ggplot(data, aes(x="", y=prop, fill=Rating)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")

  geom_text(aes(y = prop, label = prop), color = "white", size=3) +
  scale_fill_brewer(palette="Set1")
