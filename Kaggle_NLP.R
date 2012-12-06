install.packages("openNLP")
install.packages("openNLPmodels.en")
require(gdata)
require(randomForest)
require(adabag)
require(rpart)
require(openNLP)
require(tm)
require(Snowball)
require(stringr)


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
Train_dat$avg_word_length <- Train_dat$total_char/Train_dat$word_count
Train_dat$diff <- Train_dat$word_count-Train_dat$word_count2

##### adj adv dt to in count
essay_tagged <- as.data.frame(tagPOS(Train_dat[,3], language="en"))
Train_dat$adj_count <- apply(essay_tagged, 1, function(x) sum(str_count(x, c("/JJ", "/JJR", "/JJS"))))
Train_dat$adv_count <- apply(essay_tagged, 1, function(x) sum(str_count(x, c("/RB", "/RBR", "/RBS"))))
Train_dat$to_count <- apply(essay_tagged, 1, function(x) str_count(x, "/TO"))
Train_dat$dt_count <- apply(essay_tagged, 1, function(x) str_count(x, "/DT"))
Train_dat$in_count <- apply(essay_tagged, 1, function(x) str_count(x, "/IN"))




# Check the distribution of word.count in different respectively
for (i in 1:5)
{
    print(summary(subset(Train_dat,set==i)$word_count))
}

# Handeling Missing value 
Train_dat[which(!complete.cases(Train_dat)),]
Train_dat2 <- na.omit(Train_dat)
train <- Train_dat2[,c(2,6:7)]


dat1 <- Train_dat2[Train_dat2[,2]==1,]
dat2 <- Train_dat2[Train_dat2[,2]==2,]
dat3 <- Train_dat2[Train_dat2[,2]==3,]
dat4 <- Train_dat2[Train_dat2[,2]==4,]
dat5 <- Train_dat2[Train_dat2[,2]==5,]

# using AdaBoosting 
?boosting.cv
names(Train_dat3)
Train_dat3 <- Train_dat2[,-c(1,3,4,5,12,14)]
Train_dat3$grade <- factor(Train_dat3$grade)

mod11 <- boosting.cv(grade ~ ., data=Train_dat3, v=10,boos=T,coeflearn="Breiman")
# using RandomForrest
mod1 <-randomForest(factor(grade)~word_count2+avg_stnce_lgth+avg_word_length+sentence_count+adj_count+adv_count+to_count+dt_count+in_count, data=dat1,
                    importance=TRUE,proximity=TRUE)
mod2 <-randomForest(factor(grade)~word_count2+avg_stnce_lgth+avg_word_length+sentence_count+adj_count+adv_count+to_count+dt_count+in_count, data=dat2,
                    importance=TRUE,proximity=TRUE)
mod3 <-randomForest(factor(grade)~word_count2+avg_stnce_lgth+avg_word_length+sentence_count+adj_count+adv_count+to_count+dt_count+in_count, data=dat3,
                    importance=TRUE,proximity=TRUE)
mod4 <-randomForest(factor(grade)~word_count2+avg_stnce_lgth+avg_word_length+sentence_count+adj_count+adv_count+to_count+dt_count+in_count, data=dat4,
                    importance=TRUE,proximity=TRUE)
mod5 <-randomForest(factor(grade)~word_count2+avg_stnce_lgth+avg_word_length+sentence_count+adj_count+adv_count+to_count+dt_count+in_count, data=dat5,
                    importance=TRUE,proximity=TRUE)
mod6 <-randomForest(factor(grade)~set + word_count2+avg_stnce_lgth+avg_word_length+sentence_count+adj_count+adv_count+to_count+dt_count+in_count, data=Train_dat2,
                    importance=TRUE,proximity=TRUE)


mod1
mod2
mod3
mod4
mod5
mod6
plot(mod2)




##############################
###### Test Set ##############
##############################
# Load in Test set
Test_dat <- read.delim("D:/Rlab/W4242/Hw4/Kaggle/test.tsv", header=T, sep="\t")



Test_dat[,4] <- apply(Test_dat, 1, function(x) gsub("([[:punct:]]+)","",x[3]))
names(Test_dat)[4] <- "Essay_Plain_Text"

Test_dat[,5] <- apply(Test_dat, 1, function(x){
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
names(Test_dat)[5] <- "word_count"
class(Test_dat[,2])
Test_dat[,2] <- factor(Test_dat[,2])


##### Calculate Number of sentences in essay
Test_dat$sentence_count <- apply(Test_dat, 1, function(x) length(sentDetect(x[3], language = "en")))


##### Calculate avg sentence length
Test_dat$avg_stnce_lgth <- Test_dat[,5]/Test_dat[,6]

##### Calculate Word Count based on plain txt 
Test_dat$word_count2 <- apply(Test_dat, 1, function(x) length(tokenize(x[4], language = "en")))

##### Calculate Total chars in essay
Test_dat$total_char <- apply(Test_dat, 1, function(x) nchar(gsub("([[:space:]]+)","",x[4]), type="chars", allowNA=F))

##### avg word length 
Test_dat$avg_word_length <- Test_dat$total_char/Test_dat$word_count
Test_dat$diff <- Test_dat$word_count-Test_dat$word_count2

##### adj adv dt to in count
essay_tagged2 <- as.data.frame(tagPOS(Test_dat[,4], language="en"))
Test_dat$adj_count <- apply(essay_tagged2, 1, function(x) sum(str_count(x, c("/JJ", "/JJR", "/JJS"))))
Test_dat$adv_count <- apply(essay_tagged2, 1, function(x) sum(str_count(x, c("/RB", "/RBR", "/RBS"))))
Test_dat$to_count <- apply(essay_tagged2, 1, function(x) str_count(x, "/TO"))
Test_dat$dt_count <- apply(essay_tagged2, 1, function(x) str_count(x, "/DT"))
Test_dat$in_count <- apply(essay_tagged2, 1, function(x) str_count(x, "/IN"))



T_dat1 <- Test_dat[Test_dat[,2]==1,]
T_dat2 <- Test_dat[Test_dat[,2]==2,]
T_dat3 <- Test_dat[Test_dat[,2]==3,]
T_dat4 <- Test_dat[Test_dat[,2]==4,]
T_dat5 <- Test_dat[Test_dat[,2]==5,]

Test_pred1<-as.data.frame(predict(mod1,T_dat1))
Test_pred2<-as.data.frame(predict(mod2,T_dat2))
Test_pred3<-as.data.frame(predict(mod3,T_dat3))
Test_pred4<-as.data.frame(predict(mod4,T_dat4))
Test_pred5<-as.data.frame(predict(mod5,T_dat5))

Test_pred6<-as.data.frame(predict(mod6,Test_dat))


summary(Test_pred3)

#Viz
require(ggplot2)
ggplot(Train_dat,aes(x=set, y=word_count))+
    geom_point(aes(size=grade),alpha=1/30)

# output

wt <- rep(1,dim(Test_dat)[1])
names(Test_pred1) <- "pred"
names(Test_pred2) <- "pred"
names(Test_pred3) <- "pred"
names(Test_pred4) <- "pred"
names(Test_pred5) <- "pred"
Test_pred_all <- rbind(Test_pred1, Test_pred2, Test_pred3, Test_pred4, Test_pred5)

Test_output <- cbind(Test_dat[,1:2],wt,Test_pred6)
names(Test_output)[3:4] <- c("weight", "grade")
write.csv(Test_output,"D:/Rlab/W4242/Hw4/output.csv")


a1 <- Test_pred_all[,1] == Test_pred6[,1]
a2 <- as.numeric(a1)
sum(a2)
