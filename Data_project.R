
#onlinedata=read.csv("C:/Users/Muhannad/Downloads/Online Retail.csv",header=T)


onlinedata=na.omit(onlinedata)
ourdata=onlinedata[,-3]

ourdata=ourdata[ourdata$Quantity>0,] # to remove the quantities # that have negative values 
ourdata=ourdata[ourdata$UnitPrice>0.001,] # to remove inconsistent values # that are not clear or have nagative values
head(ourdata)
ourdata$Amount=ourdata$Quantity*ourdata$UnitPrice
d=ourdata$InvoiceDate
dd=data.frame(do.call(rbind, strsplit(as.vector(d), split = " ")))
names(dd)=c("Date","Time")
ourdata=ourdata[,-4] #we eliminate the old date column after processing because we do not need it
ourdata=cbind(ourdata,dd)
ourdata=ourdata[order(ourdata$CustomerID),] # just to see the number of transactions for each customer
Monetary=aggregate(ourdata$Amount,by = list(ourdata$CustomerID), FUN=sum)
head(Monetary)
names(Monetary)=c("Cutomer ID","Monetary")
Prepare=aggregate(ourdata$Amount,by = list(ourdata$CustomerID,ourdata$InvoiceNo), FUN=sum)
Maximum=aggregate(Prepare$x,by = list(Prepare$Group.1), FUN=max)
head(Maximum)
names(Maximum)=c("CutomerID","Maximum")
Minimum=aggregate(Prepare$x,by = list(Prepare$Group.1), FUN=min)
names(Minimum)=c("CutomerID","Minimum")
##### need for decisions
##########d=ourdata[order(ourdata$Date,ourdata$Time),]
library(plyr) 
transactions=count(ourdata, c("ourdata$CustomerID","ourdata$Date","ourdata$Time")) ## to calculate transactions 
#for each customer in different times
transactions[c(1:60),]
Freque2=count(ourdata, "transactions$ourdata.CustomerID") ##to calculate times of 
#purchase for each cutomer
head(Freque2)
transactions$newdate=strptime(as.character(transactions$ourdata.Date),"%d/%m/%Y")# add new column for R date format
elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}
Maxdate = aggregate(transactions$newdate,by=list(transactions$ourdata.CustomerID),max)# to calculate the latest 
#date of purchase for each cutomer
head(Maxdate)
Recency=elapsed_months("2011-12-30",Maxdate$x)# to calculate the recent purchase depending end of 2011 as 
#the receniest time its variable of one row so we will change it to column
Recency=data.frame(Recency)
datafinal=data.frame(Freque2$transactions.ourdata.CustomerID,Recency,Freque2$freq,Monetary$Monetary,Maximum$Maximum,Minimum$Minimum) # we gathered all the variables in one dataset
names(datafinal)=c("CustomerID","Recency","Frequency","Monetary","Maximum","Minimum")
head(datafinal)
# in case we took the mild thrushold 1.5*IQ+3QL
#routliers=datafinal[datafinal$Recency<12,]
#foutliers=routliers[routliers$Frequency<12,]
#moutliers=foutliers[foutliers$Monetary<3693,]
# in case we took the extreme thrushold 3*IQ+3QL
foutliers=datafinal[datafinal$Frequency<17,]
moutliers=foutliers[foutliers$Monetary<5724.7,]
ret=moutliers[,c(-1,-5,-6,-7)] #read the prepared data
km=kmeans(ret,4)#Kmeans with 4 clusters
scatterplot3d(ret[1:3],color=as.numeric(km$cluster),pch=20,angle=55) #visualize the clusters
#combine the clusters numbers with that data
w=km$cluster
x=cbind(ret,w)
#with(x, plot3d(x,type="s",col=as.numeric(km$cluster),angle=90)) #draw the clustering with moving
#validation the data (L curve)
km1=NULL
for (i in 1:10) km1[i]=kmeans(ret,i)$tot.withinss
plot(km1,type='o')
scatterplot3d(x[x$w==3,],pch=("*"),angle=55)   	#draw each cluster
#with(x, plot3d(x[x$w==1,],type="s",col=10))	#draw each cluster with moving
#####em algorithm########
emdata=x[,-4]# to keep k-means cluster column
head(emdata)
emdata=as.matrix(emdata)# to convert the x data frame to matrix because the em tool works with matrix only
require(mixtools)
require(scatterplot3d)
em=mvnormalmixEM(emdata,k=4,arbvar = FALSE,epsilon = 0.03)
pred <- apply(em$posterior, 1, function(row) which.max(row))#to predict clusters
emdata=cbind(emdata,pred)
emdata=data.frame(emdata)
scatterplot3d(emdata[1:3],color=as.numeric(emdata$pred),pch=20,angle=55) #visualize

###

