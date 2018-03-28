#Set the workspace where the raw data file has been downloaded
setwd("/Users/gbavota/Desktop/replication-perf-bugs/")

#RQ2: Survivability
data<-read.table("raw-data-rq2.csv",sep=",",header=TRUE)
android<-data[which(data["os"] == 'android'),]
ios<-data[which(data["os"] == 'ios'),]


#data
a <- mean(data$days_needed_to_remove_min)
s <- sd(data$days_needed_to_remove_min)
n <- nrow(data)
error <- qnorm(0.975)*s/sqrt(n)
a-error
a+error

a <- mean(data$days_needed_to_remove_max)
s <- sd(data$days_needed_to_remove_max)
n <- nrow(data)
error <- qnorm(0.975)*s/sqrt(n)
a-error
a+error

#android
a <- mean(android$days_needed_to_remove_min)
s <- sd(android$days_needed_to_remove_min)
n <- nrow(android)
error <- qnorm(0.975)*s/sqrt(n)
a-error
a+error

a <- mean(android$days_needed_to_remove_max)
s <- sd(android$days_needed_to_remove_max)
n <- nrow(android)
error <- qnorm(0.975)*s/sqrt(n)
a-error
a+error

#ios
a <- mean(ios$days_needed_to_remove_min)
s <- sd(ios$days_needed_to_remove_min)
n <- nrow(ios)
error <- qnorm(0.975)*s/sqrt(n)
a-error
a+error

a <- mean(ios$days_needed_to_remove_max)
s <- sd(ios$days_needed_to_remove_max)
n <- nrow(ios)
error <- qnorm(0.975)*s/sqrt(n)
a-error
a+error

#####END RQ2#####

#RQ3: ratings#
library(orddom) #Needed to compute the cliffdelta

################Functions to print on a file################
printOnFile <- function(testNames, adjPvalues, effectSizes, fileName, lowestBetter){
formattedPvalues<-c()
for(i in 1:(length(adjPvalues))){
	if(adjPvalues[i] < 0.01){
		formattedPvalues[i]<-"$<$0.01"
	} else {
		formattedPvalues[i]<-format(as.numeric(adjPvalues[i]), digits=2, nsmall=2)
	} 
}


formattedEffectSizes<-c()
for(i in 1:(length(effectSizes))){
	absEffectSize<-abs(as.numeric(effectSizes[i]))
	if(absEffectSize < 0.148){
		formattedEffectSizes[i]<-paste(format(as.numeric(effectSizes[i]), digits=2, nsmall=2)," (Negligible)\\\\", sep="")
	} else if(absEffectSize >= 0.148 && absEffectSize < 0.33){
		formattedEffectSizes[i]<-paste(format(as.numeric(effectSizes[i]), digits=2, nsmall=2)," (Small)\\\\", sep="")
	} else if(absEffectSize >= 0.33 && absEffectSize < 0.474){
		formattedEffectSizes[i]<-paste(format(as.numeric(effectSizes[i]), digits=2, nsmall=2)," (Medium)\\\\", sep="")
	} else if(absEffectSize > 0.474){
		formattedEffectSizes[i]<-paste(format(as.numeric(effectSizes[i]), digits=2, nsmall=2)," (Large)\\\\", sep="")
	}
}


formattedNames<-c()
for(i in 1:(length(testNames))){
	splitname<-strsplit(as.character(testNames[i]), " ")[[1]]
	firstDistName<-splitname[1]
	secondDistName <-splitname[3]
	if(adjPvalues[i] < 0.05){
		if(as.numeric(effectSizes[i]) > 0){
			if(lowestBetter){
				formattedNames[i]<-paste("\\textbf{", firstDistName, "} $vs$ ", secondDistName, sep="")
			} else {
				formattedNames[i]<-paste(firstDistName, " $vs$ \\textbf{", secondDistName, "}", sep="")
			}
		} else {
			if(lowestBetter){
				formattedNames[i]<-paste(firstDistName, " $vs$ \\textbf{", secondDistName, "}", sep="")
			} else {
				formattedNames[i]<-paste("\\textbf{", firstDistName, "} $vs$ ", secondDistName, sep="")
			}

		}
		
	} else {
		formattedNames[i]<-testNames[i]
	}
}

mat <- matrix(c(formattedNames, formattedPvalues, formattedEffectSizes),nrow=length(formattedNames))
write.table(mat, file=fileName, row.names=FALSE, col.names=FALSE, sep=" & ", quote=FALSE)
}
################END Functions to print on a file################

################Functions to run statistical analysis################
runStatAnalysis <- function(dist1, dist2){
result<-c()
result[1]<-wilcox.test(dist1,dist2,alternative="less",paired=TRUE)$p.value

t1=as.matrix(dist1)
colnames(t1)<-c("t1")
t2=as.matrix(dist2)
colnames(t2)<-c("t2")
o<-orddom(t1,t2)
result[2]<-o[13,1]
return(result)
}
################END Functions to run statistical analysis################

data<-read.table("rq3-raw-data-android.csv",sep=",",header=TRUE)

pdf("rq3.pdf", height=4, width=4)
boxplot(data$avg_rating, data$rating_without_perf, data$avg_rating_perf_reviews,
col="gray", boxwex = 0.3, at = 1:3 - 0.0, names = c("Avg. Rating","Avg. Rating\nWithout Perf","Avg. Rating\nPerf"), cex.axis=0.5, ylab="Average Rating")
points(1,mean(data$avg_rating), col="red", pch=16)
points(2,mean(data$rating_without_perf), col="red", pch=16)
points(3,mean(data$avg_rating_perf_reviews), col="red", pch=16)
dev.off()

#min
names<-c()
pval<-c()
esize<-c()

names<-c(names,"avg-rating $vs$ avg-rating-no-perf")
result<-runStatAnalysis(data$avg_rating,data$rating_without_perf)
pval<-c(pval,result[1])
esize<-c(esize,result[2])

printOnFile(names,pval,esize,"rq3-tests.txt",FALSE)

