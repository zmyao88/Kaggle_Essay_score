install.packages("openNLP")
install.packages("openNLPmodels.en")
require(gdata)
require(randomForest)
require(adabag)
require(openNLP)
require(tm)
require(Snowball)
Train_dat <- read.delim("D:/Rlab/W4242/Hw4/Kaggle/train.tsv", header=T, sep="\t")
head(Train_dat)
Train_dat[,3] <- as.character(Train_dat[,3])


##### plain txt for col 7
Train_dat[,7] <- apply(Train_dat, 1, function(x) gsub("([[:punct:]]+)","",x[3]))
names(Train_dat)[7] <- "Essay_Plain_Text"


##### Calculate Number of word in essay
Train_dat[,8] <- apply(Train_dat, 1, function(x){
    mm <- x[3]
    if (regexpr("[[:punct:]]+", mm) == 1)
    {
        mm <- sub("([[:punct:]]+)","",mm)
    }
    if (regexpr("[[:space:]]+", mm) == 1)
    {
        mm <- sub("([[:space:]]+)","",mm)    
    }
    nn <- length(gregexpr("\\W+", mm)[[1]])
    return (nn)   
    #return(mm)
})
names(Train_dat)[8] <- c("word_count")
class(Train_dat[,2])
Train_dat[,2] <- factor(Train_dat[,2])
names(Train_dat)

##### Calculate Number of sentences in essay
sentence_count <- apply(Train_dat, 1, function(x) length(sentDetect(x[3], language = "en")))
Train_dat <- cbind(Train_dat,sentence_count)

##### Calculate avg sentence length
Train_dat$avg_stnce_lgth <- Train_dat[,8]/Train_dat[,9]

##### Calculate Word Count based on plain txt 
Train_dat$word_count2 <- apply(Train_dat, 1, function(x) length(tokenize(x[7], language = "en")))

##### Calculate Total chars in essay
Train_dat$total_char <- apply(Train_dat, 1, function(x) nchar(gsub("([[:space:]]+)","",x[7]), type="chars", allowNA=F))

##### avg word length 
Train_dat$avg_word_length <- Train_dat$total_char/Train_dat$word_count2
Train_dat$diff <- Train_dat$word_count-Train_dat$word_count2





sentence <- "This is a short sentence consisting of some nouns, verbs, and adjectives."
a1 <- tagPOS(sentence, language = "en")

s <- "This is a sentence sentences. ? ! ! "

ss <- tokenize(Train_dat[1,10], language = "en")

length(ss)
nchar(ss, type="chars", allowNA=F)
a3 <-tagPOS(Train_dat[1,3], language="en")
View(a2)

?regexpr

Train_dat[1,10]
















# Check the distribution of word.count in different respectively
for (i in 1:5)
{
    print(summary(subset(Train_dat,set==i)$word_count))
}

# Handeling Missing value 
Train_dat[which(!complete.cases(Train_dat)),]
Train_dat2 <- na.omit(Train_dat)
train <- Train_dat2[,c(2,6:7)]


# using AdaBoosting 
mod1 <- boosting.cv(grade~., data=train,
                    v=5,boos=T,coeflearn="Breiman")
# using RandomForrest
mod2 <-randomForest(factor(grade)~set+word_count, data=Train_dat2,
                    importance=TRUE,proximity=TRUE)

mod2
plot(mod2)


# Load in Test set
Test_dat <- read.delim("D:/Rlab/W4242/Hw4/Kaggle/test.tsv", header=T, sep="\t")

Test_dat[,4] <- apply(Test_dat, 1, function(x){
    mm <- x[3]
    if (regexpr("[[:punct:]]+", mm) == 1)
    {
        mm <- sub("([[:punct:]]+)","",mm)
    }
    if (regexpr("[[:space:]]+", mm) == 1)
    {
        mm <- sub("([[:space:]]+)","",mm)    
    }
    nn <- length(gregexpr("\\W+", mm)[[1]])
    return (nn)   
    #return(mm)
})

#names(Test_dat)[2] <- "set_no"
names(Test_dat)[4] <- "word_count"
class(Test_dat[,2])
Test_dat[,2] <- factor(Test_dat[,2])

Test_pred<-as.data.frame(predict(mod2,Test_dat))
summary(Test_pred)

#Viz
require(ggplot2)
ggplot(Train_dat,aes(x=set, y=word_count))+
    geom_point(aes(size=grade),alpha=1/30)

# output

wt <- rep(1,dim(Test_dat)[1])
Test_output <- cbind(Test_dat[,1:2],wt,Test_pred)
names(Test_output)[3:4] <- c("weight", "grade")
write.csv(Test_output,"D:/Rlab/W4242/Hw4/output.csv")

