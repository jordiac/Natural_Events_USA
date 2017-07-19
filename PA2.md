---
title: "Analysis of the Impact Of Natural Events On The Economy And The Public Health Of The United States"
output: html_document
---

By jordiac, November 2016

## Reproducible Research: Peer Assessment 2

Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.

This project involves exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage.


## Synopsis

This report consists on analyzing the NOAA storm database containing data on extreme climate events. This data was collected during the period from 1950 through 2011. The purpose of this analysis is to determine which types of events are most harmful with respect to population health and which types of events have the greatest economic consequences. 

In order to answer both questions, the input data has been regrouped in 6 major groups of types of events : "Extreme_Wind" , "High_temp", "Earth_events", "Low_temp", "Storms", "Water_events". After analysing the results, we can conclude that :

1-The events that are most harmful with respect to population health are the Extreme Wind events (mainly the Tornado events).

2-The events that have the greatest economic consequences are the Water events (mainly the Flood events).


## Data Processing

### Loading libraries


```r
## *****  Loading libraries ********
library(ggplot2)       #for plotting
library(R.utils)       #for bzip2
library(gridExtra)     #multiplot printing
library(reshape2)      #for melting
library(RCurl)         #for getting binary url
```


### Loading input data

```r
## ********  1) Checking if file exists in our wd*************
name1 <- "repdata_data_StormData.csv.bz2"
lname <- c(name1)
list_name <- dir(path=".")

verif1 <- lname %in% list_name

if (verif1[1] == TRUE ){
        print("Reading file")
        bunzip2(name1, destname="repdata_data_StormData.csv", overwrite=TRUE)
} else {
        print("file is missing, downloading")
        binData <-getBinaryURL("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", ssl.verifypeer=0L, followlocation=1L)
        destFileHandle <- file(name1, open="wb")
        writeBin(binData,destFileHandle)
        bunzip2(name1, destname="repdata_data_StormData.csv", overwrite=TRUE)
        stop()
}
```

```
## [1] "file is missing, downloading"
```

```
## Warning in file.remove(filename): cannot remove file
## 'repdata_data_StormData.csv.bz2', reason 'Permission denied'
```

```
## Error in eval(expr, envir, enclos):
```

```r
##----------------------------------------------------------
```


```r
##***********  2) Reading the file *************************
if (!"repdata_data_StormData.csv" %in% ls()) {
    dades <- read.csv(file= "./repdata_data_StormData.csv" , header=TRUE,sep=",")
}
##----------------------------------------------------------
```

### Refactoring input data
We now work with Fatalities results: we get all the fatalities depending on type events and then we regroup all that data into major groups. 

Those major groups are:

-1-Water_events (flood, heavy surf, heavy rain, high water, high seas, high swells, marine events, ripcurrent, high waves, mixed precip, tsunami)

-2-Wind_extreme (Tornados, hurricanes , typhoon, funnel cloud)

-3-Storms (blizzard, duust, storm, wind, microbusrt, lightning, hail)

-4-Low_temp (Cold, Snow, Freeze, frost, glaze, Fog, winter weather, hypothermia, Ice, low temperature, Icy, sleet, wintry mix)

-5-High_temp (Heat, fire, hyperthermia, warm weather, drought)


-6-Earth_event (Avalanche, Landslide)

When treating with this data set, we "match" all data in each major event group to avoid having the same events in different major groups.


```r
## 3-1) ****************  FATALITIES  *******************************************
### Reading Fatalities depending on type of events
fatal <- aggregate(FATALITIES ~EVTYPE, dades, sum)
fatal <- subset(fatal, FATALITIES >0)


### Regrouping events

#### Water_events (flood, heavy surf, heavy rain, high water, high seas, high swells, marine events, ripcurrent, high waves, mixed precip, tsunami)
sub1 <- fatal[grep("[Ff][Ll][Oo][Oo][Dd]|[Ss][Uu][Rr][Ff]|[Rr][Aa][Ii][Nn]|[Ww][Aa][Tt][Ee]|[Ss][Ee][Aa][Ss]|[Ss][Ww][Ee][Ll]|[Dd][Rr][Oo][Ww][Nn]|[Mm][Aa][Rr][Ii][Nn]|[Cc][Uu][Rr][Rr][Ee]|[Ww][Aa][Vv][Ee]|[Pp][Rr][Ee][Cc]|[Ss][Tt][Rr][Ee][Aa]|[Tt][Ss][Uu][Nn]", fatal$EVTYPE),]

#### Wind_extreme (Tornados + hurricanes)
sub11 <- fatal[grep("[Tt][Oo][Rr][Nn][aA][Dd][Oo]|[Hh][Uu][Rr][Rr][Ii][Cc][Aa]", fatal$EVTYPE),]

#### STORMS (blizzard, duust, storm, wind, microbusrt, lightning, hail)
sub21 <- fatal[grep("[Ss][Tt][Oo][Rr][Mm]|[Ww][Ii][Nn][Dd]|[Bb][Ll][Ii][Zz][Zz]|[Dd][Uu][Ss][Tt]|[Bb][Uu][Rr][Ss]|[Ll][Ii][Gg][Hh][Tt]|[Hh][Aa][Ii][Ll]", fatal$EVTYPE),]

#### Low_temp (Cold, Snow, Freeze, frost, glaze, Fog, winter weather, hypothermia, Ice, low temperature, Icy, sleet, wintry mix)
sub31 <- fatal[grep("[Ss][Nn][Oo][Ww]|[Cc][Oo][Ll][Dd]|[Ff][Rr][Ee][Ee][Zz]|[Ff][Rr][Oo][Ss]|[Gg][Ll][Aa][Zz]|[Ff][Oo][Gg]|[Ww][Ii][Nn][Tt][Ee]|[Hh][Yy][Pp][Oo][Tt][Hh][Ee][Rr]|[Ii][Cc][Ee]|[Ll][Oo][Ww]|[Ii][Cc][Yy]|[Ss][Ll][Ee][Ee][Tt]|[Ww][Ii][Nn][Tt][Rr]", fatal$EVTYPE),]

#### High_temp (Heat, fire, hyperthermia)
sub41 <- fatal[grep("[Hh][Ee][Aa][Tt]|[Ff][Ii][Rr][Ee]|[Hh][Yy][Pp][Ee][Rr][Tt][Hh][Ee][Rr]", fatal$EVTYPE),]

#### Earth (Avalanche, Landslide)
sub51 <- fatal[grep("[Aa][Vv][Aa][Ll]|[Ll][Aa][Nn][Dd][Ss][Ll]|[Mm][Uu][Dd][Ss]", fatal$EVTYPE),]



### removing repeated events in subgroups
#### (tornado + hurricane) vs Water events
list2 <- match(sub11$EVTYPE,sub1$EVTYPE, nomatch = 0)
sub1 <- sub1[-(list2),]

#### Storm vs Water events
list3 <- match(sub21$EVTYPE,sub1$EVTYPE, nomatch = 0)
sub1 <- sub1[-(list3),]

#### Cold vs water
list4  <- match(sub31$EVTYPE,sub1$EVTYPE, nomatch = 0)
sub1 <- sub1[-(list4),]

#### Cold vs storm
list5  <- match(sub21$EVTYPE,sub31$EVTYPE, nomatch = 0)
sub31 <- sub31[-(list5),]


#### Heat vs water
list6  <- match(sub41$EVTYPE,sub1$EVTYPE, nomatch = 0)
sub1 <- sub1[-(list6),]


#### Storm vs Tornados
list7 <- match(sub11$EVTYPE,sub21$EVTYPE, nomatch = 0)
sub21 <- sub21[-(list7),]
```

We proceed the same way with "injuries" data :

```r
## 3-2) ****************  INJURIES  *******************************************
### Reading INJURIES depending on type of events
injur <- aggregate(INJURIES ~EVTYPE, dades, sum)
injur <- subset(injur, INJURIES >0)

### Regrouping events

#### Water_events (flood, heavy surf, heavy rain, high water, high seas, high swells, marine events, ripcurrent, high waves, mixed precip, tsunami)
subi1 <- injur[grep("[Ff][Ll][Oo][Oo][Dd]|[Ss][Uu][Rr][Ff]|[Rr][Aa][Ii][Nn]|[Ww][Aa][Tt][Ee]|[Ss][Ee][Aa][Ss]|[Ss][Ww][Ee][Ll]|[Dd][Rr][Oo][Ww][Nn]|[Mm][Aa][Rr][Ii][Nn]|[Cc][Uu][Rr][Rr][Ee]|[Ww][Aa][Vv][Ee]|[Pp][Rr][Ee][Cc]|[Ss][Tt][Rr][Ee][Aa]|[Tt][Ss][Uu][Nn]", injur$EVTYPE),]

#### Wind_extreme (Tornados, hurricanes , typhoon, funnel cloud)
subi11 <- injur[grep("[Tt][Oo][Rr][Nn][aA][Dd][Oo]|[Hh][Uu][Rr][Rr][Ii][Cc][Aa]|[Tt][Yy][Pp][Hh][Oo]|[Ff][Uu][Nn][Nn][Ee]", injur$EVTYPE),]

#### STORMS (blizzard, duust, storm, wind, microbusrt, lightning, hail)
subi21 <- injur[grep("[Ss][Tt][Oo][Rr][Mm]|[Ww][Ii][Nn][Dd]|[Bb][Ll][Ii][Zz][Zz]|[Dd][Uu][Ss][Tt]|[Bb][Uu][Rr][Ss]|[Ll][Ii][Gg][Hh][Tt]|[Hh][Aa][Ii][Ll]", injur$EVTYPE),]

#### Low_temp (Cold, Snow, Freeze, frost, glaze, Fog, winter weather, hypothermia, Ice, low temperature, Icy, sleet, wintry mix)
subi31 <- injur[grep("[Ss][Nn][Oo][Ww]|[Cc][Oo][Ll][Dd]|[Ff][Rr][Ee][Ee][Zz]|[Ff][Rr][Oo][Ss]|[Gg][Ll][Aa][Zz]|[Ff][Oo][Gg]|[Ww][Ii][Nn][Tt][Ee]|[Hh][Yy][Pp][Oo][Tt][Hh][Ee][Rr]|[Ii][Cc][Ee]|[Ll][Oo][Ww]|[Ii][Cc][Yy]|[Ss][Ll][Ee][Ee][Tt]|[Ww][Ii][Nn][Tt][Rr]", injur$EVTYPE),]

#### High_temp (Heat, fire, hyperthermia, warm weather, drought)
subi41 <- injur[grep("[Hh][Ee][Aa][Tt]|[Ff][Ii][Rr][Ee]|[Hh][Yy][Pp][Ee][Rr][Tt][Hh][Ee][Rr]|[Ww][Aa][Rr][Mm]|[Dd][Rr][Oo][Uu][Gg][Hh][Tt]", injur$EVTYPE),]

#### Earth (Avalanche, Landslide)
subi51 <- injur[grep("[Aa][Vv][Aa][Ll]|[Ll][Aa][Nn][Dd][Ss][Ll]|[Mm][Uu][Dd][Ss]", injur$EVTYPE),]

#### Others (other, high)
subi61 <- injur[ injur$EVTYPE== c("HIGH", "OTHER"),]




### removing repeated events in subgroups
#### (tornado + hurricane) vs Water events
listi2 <- match(subi11$EVTYPE,subi1$EVTYPE, nomatch = 0)
subi1 <- subi1[-(listi2),]

#### Storm vs Water events
listi3 <- match(subi21$EVTYPE,subi1$EVTYPE, nomatch = 0)
subi1 <- subi1[-(listi3),]

#### Cold vs water
listi4  <- match(subi31$EVTYPE,subi1$EVTYPE, nomatch = 0)
subi1 <- subi1[-(listi4),]

#### Cold vs storm
listi5  <- match(subi21$EVTYPE,subi31$EVTYPE, nomatch = 0)
subi31 <- subi31[-(listi5),]


#### Heat vs water
listi6  <- match(subi41$EVTYPE,subi1$EVTYPE, nomatch = 0)
subi1 <- subi1[-(listi6),]

#### Earth vs Low temp
listi7  <- match(subi51$EVTYPE,subi21$EVTYPE, nomatch = 0)
subi21 <- subi21[-(listi7),]
```


The economic damage variables are : 

-PROPDMG -> Economic amount of property damage in orders of magnitude

-PROPDMGEXP -> Order of magnitude for property damage (e.g. K for thousands)

-CROPDMG -> Economic amount of crop damage in orders of magnitude

-CROPDMGEXP -> Order of magnitude for crop damage (e.g. M for milions)


We treat these variables in some steps:

1-We convert the magnitude (e.g. h, K, M, B) in numeric magnitudes (e.g. h=100, k=1000, etc).

2-We multiply this numeric magnitudes by the economic amount of damage

3-We regroup the economic amounts of damage depending on the 6 major groups of events  ("Extreme_Wind" , "High_temp", "Earth_events", "Low_temp", "Storms", "Water_events").



```r
#### Selecting variables from data set
dmg <- subset(dades, PROPDMG >0 & CROPDMG >0, select = c("EVTYPE", "PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP") ) 


#### Converting damamge in absolute amounts
listam <- c("H", "h", "k", "K", "M", "m", "B", "b")
listval <- as.numeric(c(100,100,1000,1000,1000000,1000000,1000000000,1000000000))
checkList <- data.frame(Symbol=listam, Amount=listval)

listing1 <- match(dmg$PROPDMGEXP,checkList$Symbol)
dmg$PROPDMGEXP <- checkList$Amount[listing1]
dmg$PROPDMG <- dmg$PROPDMG * dmg$PROPDMGEXP

listing2 <- match(dmg$CROPDMGEXP,checkList$Symbol)
dmg$CROPDMGEXP <- checkList$Amount[listing2]
dmg$CROPDMG <- dmg$CROPDMG * dmg$CROPDMGEXP


### Subsetting damage variables and types of events
dmg <- subset(dmg, select=c("EVTYPE", "PROPDMG", "CROPDMG"))
dmg <- subset(dmg, PROPDMG >10 | CROPDMG >10)
dmg <- aggregate(PROPDMG~(EVTYPE + CROPDMG), dmg, sum)

### Regrouping events

#### Water_events (flood, heavy surf, heavy rain, high water, high seas, high swells, marine events, ripcurrent, high waves, mixed precip, tsunami)
subd1 <- dmg[grep("[Ff][Ll][Oo][Oo][Dd]|[Ss][Uu][Rr][Ff]|[Rr][Aa][Ii][Nn]|[Ww][Aa][Tt][Ee]|[Ss][Ee][Aa][Ss]|[Ss][Ww][Ee][Ll]|[Dd][Rr][Oo][Ww][Nn]|[Mm][Aa][Rr][Ii][Nn]|[Cc][Uu][Rr][Rr][Ee]|[Ww][Aa][Vv][Ee]|[Pp][Rr][Ee][Cc]|[Ss][Tt][Rr][Ee][Aa]|[Tt][Ss][Uu][Nn]", dmg$EVTYPE),]

#### Wind_extreme (Tornados, hurricanes , typhoon, funnel cloud)
subd11 <- dmg[grep("[Tt][Oo][Rr][Nn][aA][Dd][Oo]|[Hh][Uu][Rr][Rr][Ii][Cc][Aa]|[Tt][Yy][Pp][Hh][Oo]|[Ff][Uu][Nn][Nn][Ee]", dmg$EVTYPE),]

#### STORMS (blizzard, duust, storm, wind, microbusrt, lightning, hail)
subd21 <- dmg[grep("[Ss][Tt][Oo][Rr][Mm]|[Ww][Ii][Nn][Dd]|[Bb][Ll][Ii][Zz][Zz]|[Dd][Uu][Ss][Tt]|[Bb][Uu][Rr][Ss]|[Ll][Ii][Gg][Hh][Tt]|[Hh][Aa][Ii][Ll]", dmg$EVTYPE),]

#### Low_temp (Cold, Snow, Freeze, frost, glaze, Fog, winter weather, hypothermia, Ice, low temperature, Icy, sleet, wintry mix)
subd31 <- dmg[grep("[Ss][Nn][Oo][Ww]|[Cc][Oo][Ll][Dd]|[Ff][Rr][Ee][Ee][Zz]|[Ff][Rr][Oo][Ss]|[Gg][Ll][Aa][Zz]|[Ff][Oo][Gg]|[Ww][Ii][Nn][Tt][Ee]|[Hh][Yy][Pp][Oo][Tt][Hh][Ee][Rr]|[Ii][Cc][Ee]|[Ll][Oo][Ww]|[Ii][Cc][Yy]|[Ss][Ll][Ee][Ee][Tt]|[Ww][Ii][Nn][Tt][Rr]", dmg$EVTYPE),]

#### High_temp (Heat, fire, hyperthermia, warm weather, drought)
subd41 <- dmg[grep("[Hh][Ee][Aa][Tt]|[Ff][Ii][Rr][Ee]|[Hh][Yy][Pp][Ee][Rr][Tt][Hh][Ee][Rr]|[Ww][Aa][Rr][Mm]|[Dd][Rr][Oo][Uu][Gg][Hh][Tt]", dmg$EVTYPE),]

#### Earth (Avalanche, Landslide)
subd51 <- dmg[grep("[Aa][Vv][Aa][Ll]|[Ll][Aa][Nn][Dd][Ss][Ll]|[Mm][Uu][Dd][Ss]", dmg$EVTYPE),]



### removing repeated events in subgroups
#### Storm vs Water events
listd3 <- match(subd21$EVTYPE,subd1$EVTYPE, nomatch = 0)
subd1 <- subd1[-(unique(listd3)),]

#### Cold vs water
listd4  <- match(subd31$EVTYPE,subd1$EVTYPE, nomatch = 0)
subd1 <- subd1[-(unique(listd4)),]

#### Cold vs storm
listd5  <- match(subd21$EVTYPE, subd31$EVTYPE, nomatch = 0)
subd21 <- subd21[-((listd5)),]


#### Heat vs water
listd6  <- match(subd41$EVTYPE,subd1$EVTYPE, nomatch = 0)
subd1 <- subd1[-(unique(listd6)),]


#### Heat vs Extreme_wind
listd7  <- match(subd31$EVTYPE,subd11$EVTYPE, nomatch = 0)
subd11 <- subd11[-(unique(listd7)),]
```

## Results

### Injuries vs Fatalities results
We now present the results for injury and fatality data:

1-Fatalities


```r
###Regrouping results for Fatalities
event <- c("Water_events", "Extreme_Wind", "Storms", "Low_temp", "High_temp", "Earth_events")
sum1 <- as.numeric(c(sum(sub1$FATALITIES),sum(sub11$FATALITIES),sum(sub21$FATALITIES),sum(sub31$FATALITIES),sum(sub41$FATALITIES),sum(sub51$FATALITIES)))
resu1 <- data.frame(EVTYPE=event, FATALITIES = sum1)

resu1
```

```
##         EVTYPE FATALITIES
## 1 Water_events       2497
## 2 Extreme_Wind       5796
## 3       Storms       2785
## 4     Low_temp        569
## 5    High_temp       3229
## 6 Earth_events        268
```

2-Injuries


```r
###Regrouping results for Injuries
event2 <- c("Water_events", "Extreme_Wind", "Storms", "Low_temp", "High_temp", "Earth_events")
sum2 <- as.numeric(c(sum(subi1$INJURIES),sum(subi11$INJURIES),sum(subi21$INJURIES),sum(subi31$INJURIES),sum(subi41$INJURIES),sum(subi51$INJURIES)))
resu2 <- data.frame(EVTYPE=event2, INJURIES = sum2)

resu2
```

```
##         EVTYPE INJURIES
## 1 Water_events     9942
## 2 Extreme_Wind    92743
## 3       Storms    23212
## 4     Low_temp     3541
## 5    High_temp    10855
## 6 Earth_events      226
```

We now plot both variables in one plot:


```r
g <- ggplot(resu1 , aes(EVTYPE,FATALITIES)) +
        geom_bar(stat="identity", aes(fill=(resu1$EVTYPE)))+
        theme_minimal()+
        theme(legend.position="none")+
        labs(y="Total number of Fatalities")+
        ggtitle(label="Fatalities & Injuries depending on type events in USA from 1950 to 2011")+
        theme(axis.title.x=element_blank(),axis.text.x=element_blank())+
        geom_text(aes(label=resu1$FATALITIES), vjust=1.5, size=3.2)
        

q<- ggplot(resu2 , aes(EVTYPE,INJURIES))+
        geom_bar(stat="identity", aes(fill=(resu2$EVTYPE)))+
        theme_minimal()+
        theme(axis.text.x=element_text(angle=-45, vjust = 0.9, hjust=0.1, size=14))+
        theme(legend.position="none")+
        labs(y="Total number of Injuries")+
        theme(axis.title.x=element_blank())+
        geom_text(aes(label=resu2$INJURIES), vjust=1.2, size=3.2)


grid.arrange(g, q, ncol = 1, heights = c(1.5, 2.2))
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

As we can see the events that are most harmful with respect to population health are the Extreme Wind events (mainly the Tornado events) and then the High Temperature events (Heat, warm weather, etc).


### Economic Damage

The following results present the Economic Damage (Property & Crop) due to natural events from 1950 to 2011 in USA. 


```r
###Regrouping results 
event3 <- c("Water_events", "Extreme_Wind", "Storms", "Low_temp", "High_temp", "Earth_events")
sum3 <- as.numeric(c(sum(subd1$CROPDMG),sum(subd11$CROPDMG),sum(subd21$CROPDMG),sum(subd31$CROPDMG),sum(subd41$CROPDMG),sum(subd51$CROPDMG)))
sum4 <- as.numeric(c(sum(subd1$PROPDMG),sum(subd11$PROPDMG),sum(subd21$PROPDMG),sum(subd31$PROPDMG),sum(subd41$PROPDMG),sum(subd51$PROPDMG)))
resu3 <- data.frame(EVTYPE=event3, CROPDMG = sum3, PROPDMG = sum4)

### Results in $Milions
resu3$CROPDMG <- resu3$CROPDMG /1000000 
resu3$PROPDMG <- resu3$PROPDMG /1000000

resu3
```

```
##         EVTYPE  CROPDMG     PROPDMG
## 1 Water_events 8208.072 130856.5422
## 2 Extreme_Wind 5374.423  40676.8742
## 3       Storms 7121.340   8139.7027
## 4     Low_temp 5297.537    418.6451
## 5    High_temp 1699.367   1712.9500
## 6 Earth_events   20.017     14.4140
```


We now plot Property damage and Crop damage in $milions


```r
### Melting results
meltResu3 <- melt(resu3, id="EVTYPE")

### Plotting results
t <- ggplot(meltResu3 , aes(EVTYPE, value)) +
        geom_bar(stat="identity", aes(fill=variable))+
        theme(axis.text.x=element_text(angle=-45, vjust = 0.9, hjust=0.1, size=12))+
        labs(fill="Type of damage")+
        ggtitle(label="Economic damages depending on type events in USA from 1950 to 2011")+
        labs(x="Type Of Event")+ labs(y="$Milions")

t
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)

As we can see the events that have the greatest economic consequences are the Water events (mainly the Flood events) and the Extreme wind events (Tornadoes, hurricanes, typhoons).



