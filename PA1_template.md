
## my first Knitr work



```r
library(ggplot2)
activity<-read.csv(file="C:/Users/Yao/Documents/activity.csv")
```

## What is mean total number of steps taken per day?


```r
daytotalstep<-tapply(activity$steps,activity$date,sum)#�������������
daytotalstep<-data.frame(names(daytotalstep),daytotalstep)#����ת�������ݿ�
names(daytotalstep)<-c("name","total")#���ݿ��������
qplot(total,data=daytotalstep,geom="histogram",xlab="Total Step per Day",bins=20)#��ֱ��ͼ
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![plot of chunk daytotalstep](figure/daytotalstep-1.png)

```r
print("Mean")
```

```
## [1] "Mean"
```

```r
mean(daytotalstep$total,na.rm=TRUE)#��ȡ��ֵ
```

```
## [1] 10766.19
```

```r
print("Median")
```

```
## [1] "Median"
```

```r
median(daytotalstep$total,na.rm=TRUE)#��ȡ��λ��
```

```
## [1] 10765
```

## What is the average daily activity pattern?


```r
meanstep<-tapply(activity$steps,activity$interval,mean,na.rm=TRUE)#��ʱ������
meanstep<-data.frame(names(meanstep),meanstep)#����ת�������ݿ�
names(meanstep)<-c("interval","mean")#���ݿ��������
qplot(interval,mean,data=meanstep,geom="line",group=1)#������ͼ
```

![plot of chunk meanstep](figure/meanstep-1.png)

```r
zuida<-max(meanstep$mean)#�ҳ�ÿ������˶�����ʱ���
print("the Max Step of Interval")
```

```
## [1] "the Max Step of Interval"
```

```r
subset(meanstep,mean==zuida)#��ȡʱ���
```

```
##     interval     mean
## 835      835 206.1698
```

## Imputing missing values


```r
nomissing<-activity
interval<-meanstep$interval
sum(is.na(activity))#��ȡȱʧֵ����
```

```
## [1] 2304
```

```r
for(i in 1:length(nomissing[,1]))#ȱʧֵ�岹����ֵ����interval
        {if(is.na(nomissing[i,1])){
                nomissing[i,1]<-meanstep[which(interval==nomissing[i,3]),2]
        }
        else if (is.na(nomissing[i,1])==FALSE)
      {nomissing[i,1]<-nomissing[i,1]}
        }
daytotalstep1<-tapply(nomissing$steps,nomissing$date,sum)#�岹������ݣ���ÿ���˶�������ֱ��ͼ
daytotalstep1<-data.frame(names(daytotalstep1),daytotalstep1)
names(daytotalstep1)<-c("name","total")
qplot(total,data=daytotalstep1,geom="histogram",xlab="Total Step Per Day Using Imputed Values",bins=20)
```

![plot of chunk nomissing](figure/nomissing-1.png)

```r
print("Old Mean")
```

```
## [1] "Old Mean"
```

```r
mean(daytotalstep$total,na.rm=TRUE)#��ȡ��ֵ
```

```
## [1] 10766.19
```

```r
print("Old Median")
```

```
## [1] "Old Median"
```

```r
median(daytotalstep$total,na.rm=TRUE)#��ȡ��λ��
```

```
## [1] 10765
```

```r
print("New Mean")
```

```
## [1] "New Mean"
```

```r
mean(daytotalstep1$total,na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
Print("New Median")
```

```
## Error in eval(expr, envir, enclos): û��"Print"�������
```

```r
median(daytotalstep1$total,na.rm=TRUE)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?


```r
nomissing$date<-as.Date(nomissing$date)#ת�����ڸ�ʽ�����ܱ�weekdays()������ȡ
for (i in 1:length(nomissing[, 2])){ #�����µı������ֹ����պ���ĩ
    if (weekdays(nomissing[i, 2])=="������")
        {nomissing$weekend[i]<-'weekend'}
    else if ((weekdays(nomissing[i, 2])=="������"))
        {nomissing$weekend[i]<-'weekend'}
    else {nomissing$weekend[i]<-'weekday'}
}
weekday<-aggregate(nomissing$steps, nomissing[,c("interval","weekend")],mean)#��ʱ�����Ƿ���ĩ�����ֵ
names(weekday)[3]<-"mean"
ggplot(data=weekday, aes(x=interval, y=mean, color=weekend))+geom_line() + facet_grid(weekend~.)#��ͼ
```

![plot of chunk weekday](figure/weekday-1.png)
