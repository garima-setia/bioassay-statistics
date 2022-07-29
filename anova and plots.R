install.packages("datasets")
install.packages("ggplot2")
install.packages("multcompView")
install.packages("dplyr")

library("datasets")
library("ggplot2")
library("multcompView")
library("dplyr")

setwd("/Users/gsetia1/Desktop/CpBioassay")
test = read.table("cpfourwood.txt")


str(test)
colnames(test) = c("food","consumption")

anova = aov(consumption~food, data = test)
summary(anova)
tukey = TukeyHSD(anova)
print(tukey)
cld = multcompLetters4(anova, tukey)
print(cld)
tk = group_by(test, food) %>% summarise(mean=mean(consumption), quant = quantile(consumption, probs = 0.75)) %>% arrange(desc(mean))
cld = as.data.frame.list(cld$food)
tk$cld = cld$Letters
print(tk)

ggplot(test,aes(food,consumption)) + geom_boxplot(fill = "lightblue", color = "darkblue") + labs(x = "Food Source", y = "Net Consumption (in g)") + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_text(face="bold", size=15), axis.text.x = element_text(size=12, face="bold", colour = "black"), axis.text.y = element_text(size=12, face="bold", colour = "black")) + geom_text(data = tk, aes(label = cld, x = food, y = quant, fontface='bold'), vjust = -1, hjust = -1, size = 5, color = "darkblue")
ggsave("boxplotcpfour1.jpg", width = 10, height = 7, dpi = 2000)
ggsave("boxplotcpfour2.jpg", width = 10, height = 7, dpi = 1000)
ggsave("boxplotcpfour3.png", width = 10, height = 7, dpi = 1000)
ggsave("boxplotcpfour1.png", width = 10, height = 7, dpi = 2000)

#### Ns two low#####
testlow = read.table("cptwolow.txt")

str(testlow)
colnames(testlow) = c("foodlow","consumptionlow")

anovalow = aov(consumptionlow~foodlow, data = testlow)
summary(anovalow)
tukeylow = TukeyHSD(anovalow)
print(tukeylow)
cldlow = multcompLetters4(anovalow, tukeylow)
print(cldlow)
tklow = group_by(testlow, foodlow) %>% summarise(mean=mean(consumptionlow), quant = quantile(consumptionlow, probs = 0.75)) %>% arrange(desc(mean))
cldlow = as.data.frame.list(cldlow$foodlow)
tklow$cldlow = cldlow$Letters
print(tklow)

ggplot(testlow,aes(foodlow,consumptionlow)) + geom_boxplot(fill = "lightblue", color = "darkblue") + labs(x = "Food Source", y = "Net Consumption (in g)") + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_text(face="bold", size=15), axis.text.x = element_text(size=12, face="bold", colour = "black"), axis.text.y = element_text(size=12, face="bold", colour = "black")) + geom_text(data = tklow, aes(label = cldlow, x = foodlow, y = quant, fontface='bold'), vjust = -1, hjust = -1, size = 5, color = "darkblue")
ggsave("cptwolow.jpg", width = 10, height = 4, dpi = 2000)


#### Ns two med#####
testmed = read.table("cptwomed.txt")

str(testmed)
colnames(testmed) = c("foodmed","consumptionmed")

anovamed = aov(consumptionmed~foodmed, data = testmed)
summary(anovamed)
tukeymed = TukeyHSD(anovamed)
print(tukeymed)
cldmed = multcompLetters4(anovamed, tukeymed)
print(cldmed)
tkmed = group_by(testmed, foodmed) %>% summarise(mean=mean(consumptionmed), quant = quantile(consumptionmed, probs = 0.75)) %>% arrange(desc(mean))
cldmed = as.data.frame.list(cldmed$foodmed)
tkmed$cldmed = cldmed$Letters
print(tkmed)

ggplot(testmed,aes(foodmed,consumptionmed)) + geom_boxplot(fill = "lightblue", color = "darkblue") + labs(x = "Food Source", y = "Net Consumption (in g)") + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_text(face="bold", size=15), axis.text.x = element_text(size=12, face="bold", colour = "black"), axis.text.y = element_text(size=12, face="bold", colour = "black")) + geom_text(data = tkmed, aes(label = cldmed, x = foodmed, y = quant, fontface='bold'), vjust = -1, hjust = -1, size = 5, color = "darkblue")
ggsave("nstwomed.jpg", width = 10, height = 4, dpi = 2000)

#### Ns two high#####
testhigh = read.table("cptwohigh.txt")


str(testhigh)
colnames(testhigh) = c("foodhigh","consumptionhigh")

anovahigh = aov(consumptionhigh~foodhigh, data = testhigh)
summary(anovahigh)
tukeyhigh = TukeyHSD(anovahigh)
print(tukeyhigh)
cldhigh = multcompLetters4(anovahigh, tukeyhigh)
print(cldhigh)
tkhigh = group_by(testhigh, foodhigh) %>% summarise(mean=mean(consumptionhigh), quant = quantile(consumptionhigh, probs = 0.75)) %>% arrange(desc(mean))
cldhigh = as.data.frame.list(cldhigh$foodhigh)
tkhigh$cldhigh = cldhigh$Letters
print(tkhigh)

ggplot(testhigh,aes(foodhigh,consumptionhigh)) + geom_boxplot(fill = "lightblue", color = "darkblue") + labs(x = "Food Source", y = "Net Consumption (in g)") + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_text(face="bold", size=15), axis.text.x = element_text(size=12, face="bold", colour = "black"), axis.text.y = element_text(size=12, face="bold", colour = "black")) + geom_text(data = tkhigh, aes(label = cldhigh, x = foodhigh, y = quant, fontface='bold'), vjust = -1, hjust = -1, size = 5, color = "darkblue")
ggsave("cptwohigh.jpg", width = 10, height = 4, dpi = 2000)


install.packages("ggpubr")

library(ggpubr)

ggarrange(ggplot(testlow,aes(foodlow,consumptionlow)) + geom_boxplot(fill = "lightblue", color = "darkblue") + labs(x = "", y = "") + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_text(face="bold", size=15), axis.text.x = element_text(size=12, face="bold", colour = "black"), axis.text.y = element_text(size=12, face="bold", colour = "black")) + geom_text(data = tklow, aes(label = cldlow, x = foodlow, y = quant, fontface='bold'), vjust = -1, hjust = -1, size = 5, color = "darkblue"),                                                 # First row with scatter plot
          ggplot(testmed,aes(foodmed,consumptionmed)) + geom_boxplot(fill = "lightblue", color = "darkblue") + labs(x = "", y = "Net Consumption (in g)") + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_text(face="bold", size=15), axis.text.x = element_text(size=12, face="bold", colour = "black"), axis.text.y = element_text(size=12, face="bold", colour = "black")) + geom_text(data = tkmed, aes(label = cldmed, x = foodmed, y = quant, fontface='bold'), vjust = -1, hjust = -1, size = 5, color = "darkblue"), # Second row with box and dot plots
          ggplot(testhigh,aes(foodhigh,consumptionhigh)) + geom_boxplot(fill = "lightblue", color = "darkblue") + labs(x = "Food Source", y = "") + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_text(face="bold", size=15), axis.text.x = element_text(size=12, face="bold", colour = "black"), axis.text.y = element_text(size=12, face="bold", colour = "black")) + geom_text(data = tkhigh, aes(label = cldhigh, x = foodhigh, y = quant, fontface='bold'), vjust = -1, hjust = -1, size = 5, color = "darkblue"),
          nrow = 3, labels = "" 
          # Labels of the scatter plot
) 
ggsave("cptwoARRANGED.png", width = 10, height = 15, dpi = 2000)


#####no-choice test#####

testno = read.table("cpnochoice.txt")

str(testno)
colnames(testno) = c("foodno","consumptionno")

anovano = aov(consumptionno~foodno, data = testno)
summary(anovano)
tukeyno = TukeyHSD(anovano)
print(tukeyno)
cldno = multcompLetters4(anovano, tukeyno)
print(cldno)
tkno = group_by(testno, foodno) %>% summarise(mean=mean(consumptionno), quant = quantile(consumptionno, probs = 0.75)) %>% arrange(desc(mean))
cldno = as.data.frame.list(cldno$foodno)
tkno$cldno = cldno$Letters
print(tkno)

ggplot(testno,aes(foodno,consumptionno)) + geom_boxplot(fill = "lightblue", color = "darkblue") + labs(x = "Food Source", y = "Net Consumption (in g)") + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_text(face="bold", size=15), axis.text.x = element_text(size=12, face="bold", colour = "black"), axis.text.y = element_text(size=12, face="bold", colour = "black")) + geom_text(data = tkno, aes(label = cldno, x = foodno, y = quant, fontface='bold'), vjust = -1, hjust = -1, size = 5, color = "darkblue")
ggsave("cpnochoice.png", width = 10, height = 4, dpi = 1000)