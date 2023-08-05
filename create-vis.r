library(plyr) #pull in library
library(ggplot2)

quotes <- read.csv(file="quotation_info.csv") #pull in CSV
q1<-data.frame(quotes) #create frame
Unique_q1 <-unique(c(as.character(q1$quoteID)))
quoteID=as.numeric(factor(q1$quoteID,levels=Unique_q1)) #turn the QID into a factor

q1$speaker <- revalue(q1$speaker,
                      c("Emma"="0", "Mr. Frank Churchill"="1", "Harriet Smith"="2", "Isabella"="3" , "Jane Fairfax"="4" , "Mr. John Knightley"="5" , "Mr. Knightley"="6" , "Miss Bates"="7" , "Mr. Elton"="8" , "Mr. Weston"="9" , "Mr. Woodhouse"="10" , "Mrs. Bates"="11" , "Mrs. Cole"="12", "Mrs. Elton"="13", "Mrs. Ford"="14", "Mrs. Weston"="15", "_group"="16" , "_unknowable"="17")) #assign numerals to speakers

speaker <- as.numeric(q1$speaker)   #change speaker to numeric


q1$quoteType <- revalue(q1$quoteType,
          c("Anaphoric"="0", "Explicit"="1", "Implicit"="2")) #quotetype assigned numeral

quoteType <- as.numeric(q1$quoteType) #quotetype to numeral

quotecluster <- data.frame(quoteID,speaker, quoteType)
ladyquote <- filter(quotecluster, speaker == 0)
ladyquote <- head(ladyquote,40)
manquote <- filter(quotecluster, speaker == 6)  
manquote<- head(manquote,40)
bothquotes <- rbind(ladyquote, manquote) #bind them back together
bothquotes$speaker <- as.character(bothquotes$speaker)
lessquote <- revalue(bothquotes$speaker, c("6" = "1"))
lessquote <- as.numeric(lessquote)
q2<-data.frame(bothquotes$quoteID,lessquote,bothquotes$quoteType)
head(q2) #create new frame for less space quotes
#do counts 
data_mod <- aggregate(bothquotes.quoteID ~ lessquote + bothquotes.quoteType,
                    data = q2,
                     FUN = length)
print (data_mod)
#do vis
p1 <- ggplot(data = q2) + aes(y = lessquote, x = bothquotes.quoteType) + geom_jitter(width = 0.3, height=0.05, size = 7, shape = "♥", alpha = 0.5, aes(color=factor(lessquote)), show.legend = FALSE) + scale_x_continuous( breaks = c(0, 1, 2), label = c("Anaphoric", "Explicit", "Implicit")) + scale_y_continuous( breaks = c(0, 1), label = c("Anne", "Cpt. Wentworth")) + scale_shape_manual(values=c(0, 1)) + scale_color_manual(values=c('#40AEEB','#3347FF')) 
p1 + theme(plot.title = element_blank(),
           axis.title.x = element_blank(),
           axis.title.y = element_blank(),
           panel.background = element_rect(fill = "white", colour = NA),
           panel.border = element_blank(), 
           panel.grid.major = element_line(colour = "grey92"), 
           panel.grid.minor = element_line(colour = "grey92", size = 0.25))
