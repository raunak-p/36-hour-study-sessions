#Importing data and 

library(plyr)


df <- read.table("201601.csv", header=FALSE, sep=",", fill=TRUE, na.strings = c("-"))
d1 <- df[ , c(1,2,3,9,11,17,20, 21, 23, 25)]
colnames(d1) <- c("macid", "uuid", "campus", "date and time", "connection length", "total traffic", "wifi", "device type", "manufacturor", "operating system")

df2 <- read.table("201602.csv", header=FALSE, sep=",", fill=TRUE, na.strings = c("-"))
d2 <- df2[ , c(1,2,3,9,11,17,20, 21, 23, 25)]
colnames(d2) <- c("macid", "uuid", "campus", "date and time", "connection length", "total traffic", "wifi", "device type", "manufacturor", "operating system")


df3 <- read.table("201603.csv", header=FALSE, sep=",", fill=TRUE, na.strings = c("-"))
d3 <- df3[ , c(1,2,3,9,11,17,20, 21, 23, 25)]
colnames(d3) <- c("macid", "uuid", "campus", "date and time", "connection length", "total traffic", "wifi", "device type", "manufacturor", "operating system")



df4 <- read.table("201604.csv", header=FALSE, sep=",", fill=TRUE, na.strings = c("-"))
d4 <- df3[ , c(1,2,3,9,11,17,20, 21, 23, 25)]
colnames(d4) <- c("macid", "uuid", "campus", "date and time", "connection length", "total traffic", "wifi", "device type", "manufacturor", "operating system")



df5 <- read.table("201605.csv", header=FALSE, sep=",", fill=TRUE, na.strings = c("-"))
d5 <- df5[ , c(1,2,3,18, 19, 25, 28, 29,31,33)]
colnames(d5) <- c("macid", "uuid", "campus", "date and time", "connection length", "total traffic", "wifi", "device type", "manufacturor", "operating system")



df6 <- read.table("201606.csv", header=FALSE, sep=",", fill=TRUE, na.strings = c("-"))
d6 <- df6[ , c(1,2,3,18, 19, 25, 28, 29,31,33)]
colnames(d6) <- c("macid", "uuid", "campus", "date and time", "connection length", "total traffic", "wifi", "device type", "manufacturor", "operating system")

#Combining dataframes

d <- rbind(d1,d2,d3,d4,d5, d6)

##Using a function to extract connection length data in numeric form







#Used the replace fuNction to clean up the campus data

d$campus <- as.character(d$campus)

d$campus[d$campus == "cuc"] <- "CUC"
d$campus[d$campus == "cuc.claremont.edu"] <- "CUC"

d$campus[d$campus == "cgu"] <- "CGU"
d$campus[d$campus == "cgu.edu"] <- "CGU"

d$campus[d$campus == "kgi"] <- "KGI"
d$campus[d$campus == "kgi.edu"] <- "KGI"

d$campus[d$campus == "hmc"] <- "HMC"
d$campus[d$campus == "hmc.edu"] <- "HMC"

d$campus[d$campus == "pom"] <- "POM"


d$campus[d$campus == "pit"] <- "PZ"
d$campus[d$campus == "PIT"] <- "PZ"
d$campus[d$campus == "pitzer.edu"] <- "PZ"




d$campus[d$campus == "scr"] <- "SCR"
d$campus[d$campus == "scrippscollege.edu"] <- "SCR"

d$campus[d$campus == "cmc"] <- "CMC"
d$campus[d$campus == "cmc.edu"] <- "CMC"




# Creating campus subsets

d_cgu <- subset(d, campus== "CGU")
d_kgi <- subset(d, campus== "KGI")
d_cuc <- subset(d, campus== "CUC")
d_pom <- subset(d, campus== "POM")
d_hmc <- subset(d, campus== "HMC")
d_scr <- subset(d, campus== "SCR")
d_pz <- subset(d, campus=="PZ")
d_cmc <- subset(d, campus == "CMC")



##changing column names so count can work. Removing the space in the name

colnames(d)[4] <- "date_time"
colnames(d)[5] <- "connection_length"
colnames(d)[6] <- "traffic"
colnames(d)[8] <- "device"
colnames(d)[10] <- "os"

colnames(d_cgu)[4] <- "date_time"
colnames(d_cgu)[5] <- "connection_length"
colnames(d_cgu)[6] <- "traffic"
colnames(d_cgu)[8] <- "device"
colnames(d_cgu)[10] <- "os"

colnames(d_kgi)[4] <- "date_time"
colnames(d_kgi)[5] <- "connection_length"
colnames(d_kgi)[6] <- "traffic"
colnames(d_kgi)[8] <- "device"
colnames(d_kgi)[10] <- "os"

colnames(d_cuc)[4] <- "date_time"
colnames(d_cuc)[5] <- "connection_length"
colnames(d_cuc)[6] <- "traffic"
colnames(d_cuc)[8] <- "device"
colnames(d)[10] <- "os"

colnames(d_hmc)[4] <- "date_time"
colnames(d_hmc)[5] <- "connection_length"
colnames(d_hmc)[6] <- "traffic"
colnames(d_hmc)[8] <- "device"
colnames(d_hmc)[10] <- "os"

colnames(d_pom)[4] <- "date_time"
colnames(d_pom)[5] <- "connection_length"
colnames(d_pom)[6] <- "traffic"
colnames(d_pom)[8] <- "device"
colnames(d_pom)[10] <- "os"

colnames(d_scr)[4] <- "date_time"
colnames(d_scr)[5] <- "connection_length"
colnames(d_scr)[6] <- "traffic"
colnames(d_scr)[8] <- "device"
colnames(d_scr)[10] <- "os"

colnames(d_pz)[4] <- "date_time"
colnames(d_pz)[5] <- "connection_length"
colnames(d_pz)[6] <- "traffic"
colnames(d_pz)[8] <- "device"
colnames(d_pz)[10] <- "os"

colnames(d_cmc)[4] <- "date_time"
colnames(d_cmc)[5] <- "connection_length"
colnames(d_cmc)[6] <- "traffic"
colnames(d_cmc)[8] <- "device"
colnames(d_cmc)[10] <- "os"

#Combining the individual school data

dx <- rbind(d_cgu, d_kgi, d_pom, d_hmc, d_scr, d_pz, d_cmc)

#Converting connection length into reasonable format. 1 day data is recorded as 1 minute. solution? If more than __ characters, then 24*60 minutes. _ hr 0 mins is also FUCKED

durationFirst <- sapply(strsplit(substr(dx$connection_length,1,20),"\\s+"), `[`, 1)
durationSecond <- sapply(strsplit(substr(dx$connection length,1,20),"\\s+"), `[`, 3)
durationThird <- sapply(strsplit(substr(dx$connection_length,1,20),"\\s+"), `[`, 5)


durationFirst[is.na(durationFirst)] <- 0
durationSecond[is.na(durationSecond)] <- 0.5



x = length(durationSecond)
minV <- durationFirst



for (i in 1:x){
  if (is.na(durationThird[i])){
    if (durationSecond[i] == "0.5"){
      minV[i] <- durationFirst[i]
    } else{
      minV[i] <- as.numeric(durationSecond[i]) + as.numeric(durationFirst[i])*60
    }
  } else {
    minV[i] <- 60*24
  }
}

# Screwing around with traffic concentration and
dx$c_time <- as.numeric(minV)
#Changing 0 minutes to 1 minute. tc = inf otherwise
dx$c_time <- gsub(0, 1, dx$c_time)
dx$c_time <- as.numeric(dx$c_time)
dx$traffic <- as.numeric(dx$traffic)
dx$tc <- dx$traffic/dx$c_time
dx$tc <- gsub("NaN", "NA", dx$tc)
dx$tc <- as.numeric(dx$tc)

mean(dx$tc, na.rm=TRUE)


# Creating campus subsets again. This is important.

d_cgu <- subset(dx, campus== "CGU")
d_kgi <- subset(dx, campus== "KGI")
d_cuc <- subset(dx, campus== "CUC")
d_pom <- subset(dx, campus== "POM")
d_hmc <- subset(dx, campus== "HMC")
d_scr <- subset(dx, campus== "SCR")
d_pz <- subset(dx, campus=="PZ")
d_cmc <- subset(dx, campus == "CMC")


#For some fucked up reason, cuc refuses to bind with the rest of this data. Use dx for further analyses

#creating wifi subsets. 99.7% are either claremont or wpa

d_cl <- subset(dx, wifi=="Claremont")
d_wpa <- subset(dx, wifi == "Claremont-WPA")





#EXPLORATORY ANALYSIS

#unique users per school

uu_kgi <- length(unique(d_kgi$uuid))
uu_cgu <- length(unique(d_cgu$uuid))
uu_hmc <- length(unique(d_hmc$uuid))
uu_pz <- length(unique(d_pz$uuid))
uu_scr <- length(unique(d_scr$uuid))
uu_cmc <- length(unique(d_cmc$uuid))
uu_pom <- length(unique(d_pom$uuid))

#unique devices per school

ud_kgi <- length(unique(d_kgi$macid))
ud_cgu <- length(unique(d_cgu$macid))
ud_hmc <- length(unique(d_hmc$macid))
ud_pz <- length(unique(d_pz$macid))
ud_scr <- length(unique(d_scr$macid))
ud_cmc <- length(unique(d_cmc$macid))
ud_pom <- length(unique(d_pom$macid))


## Average devices per user per school

ad_kgi <- ud_kgi/uu_kgi
ad_cgu <- ud_cgu/uu_cgu
ad_hmc <- ud_hmc/uu_hmc
ad_pz <- ud_pz/uu_pz
ad_scr <- ud_scr/uu_scr
ad_cmc <- ud_cmc/uu_cmc
ad_pom <- ud_pom/uu_pom 


#Average connection time per network

mean(d_cl$c_time)
mean(d_wpa$c_time)


#Average connection time per school

mean(d_kgi$c_time)
mean(d_cgu$c_time)
mean(d_hmc$c_time)
mean(d_cmc$c_time)
mean(d_scr$c_time)
mean(d_pom$c_time)
mean(d_pz$c_time)


#Average traffic concentration per network

mean(d_cl$tc, na.rm=TRUE)
mean(d_wpa$tc, na.rm=TRUE)

#Average traffic concentration per school

mean(d_kgi$tc, na.rm=TRUE)
mean(d_cgu$tc, na.rm=TRUE)
mean(d_hmc$tc, na.rm=TRUE)
mean(d_scr$tc, na.rm=TRUE)
mean(d_pz$tc, na.rm=TRUE)
mean(d_pom$tc, na.rm=TRUE)
mean(d_cmc$tc, na.rm=TRUE)




# STATISTICAL ANALYSIS


mean(dx$tc)

sd(dx$tc)

sd(subset(dx, traffic!=0)$tc)

sd(subset(dx, traffic!=0, c_time!=1441)$tc)

sd(subset(dx, c_time==1441)$tc)

plot(subset(dx, c_time=1441), subset(dx, traffic!=0, c_time!=1441))