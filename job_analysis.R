par(family='STXihei')
library(ggplot2)
library(stringr)
library(ggmap)
#####1.Read the data#####
data = read.csv("job_test_440.csv")
data = data[,-1]

##Clean useless n and spaces"
clean = function(str1){
  str_arr=c()
  str1 = as.character(str1)
  for (word in str1){
    str2 = str_replace(word,"\n","")
    str2 = str_replace_all(str2," ","")
    str_arr = c(str_arr,str2)
  }
  return(str_arr)
}

##Clean all columns 
for(i in 1:dim(data)[2]){
  data[,i] = clean(data[,i])
}

######2.Analysis of Industry#####
Industry = data[,2]
#Frequency Counting 
Industry_fre = data.frame(table(Industry))
Industry_fre = Industry_fre[order(Industry_fre$Freq, decreasing = TRUE),]

##Take top 10 to analyze 
Industry_fre_10 = head(Industry_fre,10)
#Industry top10 Visulization 
pct <- round(Industry_fre_10[,2]/sum(Industry_fre_10[,2])*100)
lbls <- paste(Industry_fre_10[,1], pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(Industry_fre_10[,2], labels = lbls, main="Top10 of Industries")
###Conclusion:
#1.移动互联网类的公司需求最多。
#2.行业的角度来说，移动互联网>金融>>O2O>电子商务。由此可见，这四个行业对于数据分析的需求最多。
#3.乙方公司的占比为10%，甲方为90%。可见，数据由于其隐私性和重要性，大多企业都愿意自己成立数据分析部门.
#Sum: 从数量的角度来说，应着重投移动互联网类(至少要有APP)的科技行业,金融行业,O2O行业,电子商务行业，非乙方。


########3.Analysis of people########
People = data[,3]
#Frequency Counting 
People_fre = data.frame(table(People))
People_fre = People_fre[order(People_fre$Freq, decreasing = TRUE),]
People_fre = People_fre[-7,] #delete the data which is not numeric
#People Visulization 
pct <- round(People_fre[,2]/sum(People_fre[,2])*100)
lbls <- paste(People_fre[,1], pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(People_fre[,2], labels = lbls, main="Distribution of People")
###Conclusion:
#1.主要需求分布在50人及以上的团队
#2.令人意外的是，150-500人的公司需求最多。不过其它三种规模的公司需求差不多。
#Sum:避免投人数小于50人的，150-500人的公司需求最旺，可能是因为正值发展期。


###4.Analysis of Salary####
Salary = data[,4]
#Frequency Counting 
Salary_fre = data.frame(table(Salary))
Salary_fre = Salary_fre[order(Salary_fre$Freq, decreasing = TRUE),]
#Find out the most frequenlty salary the company will offer 
Salary_fre_h = Salary_fre[Salary_fre [,2]>mean(Salary_fre[,2]),]
#Salary Visulization 
pct <- round(Salary_fre_h[,2]/sum(Salary_fre_h[,2])*100)
lbls <- paste(Salary_fre_h[,1], pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(Salary_fre_h[,2], labels = lbls, main="Distribution of Salary Range")
###Conclusion:
#1.3个最常出现的工资段：10-20k, 8-15k,15-30k
#2.由于工资段太广了，不好统计，且由于对于应届生来说，一般以工资段的低端去计算比较合理。
#2.1 Creat a function to calculate the mean salary
clean_sal = function(str1){
  str_arr=c()
  str1 = as.character(str1)
  for (word in str1){
    str2 = unlist(strsplit(word,"-"))[1]
    str2 = as.integer(unlist(strsplit(str2,"k","")))
    str_arr = c(str_arr,str2)
  }
  return(str_arr)
}
Salary_fre_h[,1] = clean_sal(Salary_fre_h[,1])
#2.3 平均工资是 8.790323
sum(Salary_fre_h[,1]*Salary_fre_h[,2])/sum(Salary_fre_h[,2])
#2.4 Visulization 
plot(x=Salary_fre_h[,1],y=Salary_fre_h[,2],type="h",xlab="Salary(Unit:K)",ylab="Frequency",
     main="Salary Distribution",lwd = 20,col="red")
#3. 建议工资为8k-10k，可以先从9k开始
###


######5.Analysis of Locations#####
#5.1Turn the address to lat and lon 
source('HeatMap_locations.R')
address = data[,5]
location  <- ldply(address, function(x) geoCode(x))
head(location)
names(location)  <- c("lat","lon","location_type", "formatted")
data =cbind(data,location[,1:2])

#5.2Plot the location
job_coord = location[,1:2]
#40 NA value
sum(is.na(job_coord)) 
#we have to delete na values, but it will not affect a lot
job_coord  <- job_coord [complete.cases(job_coord ),]
#Plot 
ggplot(job_coord)+geom_point(aes(lat, lon), ylim=range(job_coord$lon, finite=TRUE), xlim=range(job_coord$lat, finite=TRUE), color="blue")
##Get Plot of China
china <- get_map(location = 'china', zoom = 4)

#test with job_coord data
job_coord$test = c(rep(1:3, 100),rep(5,91))
for(i in 1:dim(job_coord)[2]){
  job_coord[i] = as.numeric(unlist(job_coord[i]))
}
ggmap(china)+geom_tile(data = job_coord , aes(x = lon, y = lat, alpha = test),fill = "red")+theme(axis.title.y = element_blank(), axis.title.x = element_blank())


ggmap(china)+
  geom_point(
  aes(x = lon, y = lat),
      data= job_coord,colour = "red", size = 3
)


##(1)getChina
china1 <- get_map(location = 'hubei', zoom = 5)
ggmap(china1) +
  geom_point(
    aes(x = lon, y = lat),
    data = job_coord, colour = "red", size = 3
  )
#Obviously, the 5 most popular city, we will read through them literatly.

#(2)Bejing,162, (41.4%)
Beijing <- get_map(location = 'Beijing', zoom = 11)
ggmap(Beijing)+
  geom_point(
    aes(x = lon, y = lat),
    data = job_coord, colour = "red", size = 3
)
##Con: 
#(a)1.集中在东边（朝阳区）; 2.西北（海淀区中关村）; 3.西北(清河地铁站附近的软件园);4.东北(望京);  
#(b)3和4的两个地名比较陌生，可能是初创企业或者是新的软件区，所以对于数据分析需求比较大。
#(c)北京企业的数量占比50%了，所以是第一大需求地

#(3)Shanghai,72,(18.41%)
Shanghai <- get_map(location = 'Shanghai', zoom = 11)

ggmap(Shanghai)+
  geom_point(
    aes(x = lon, y = lat),
    data = job_coord, colour = "red", size = 3
)
##Con: 
#(a)地点比较分散，并没有在一个区域内，但是都在地铁线上面，大部分在内环.

#(4)Shenzhen,37,(9.46%)
Shenzhen <- get_map(location = 'Shenzhen', zoom = 11)
ggmap(Shenzhen)+
  geom_point(
    aes(x = lon, y = lat),
    data = job_coord, colour = "red", size = 3
  )
##Con: 
#(a)大部分集中在南山区，少部分在福田区.因为南山区科技园比较多，所以多是正常.


#(5)Guangzhou,36,(9.21%)
Guangzhou <- get_map(location = 'Guangzhou', zoom = 11)
ggmap(Guangzhou)+
  geom_point(
    aes(x = lon, y = lat),
    data = job_coord, colour = "red", size = 3
  )
##Con: 
#(a)大部分集中在天河区，因为商业地段，因此办公室扎堆也是正常。
#(b)令人惊喜的是，虽然，近年广州的科技商业并没有收到重视，但是，从数据来看，广州互联网企业对于数据分析的需求还是多的。
#(c)相对于深圳，广州与深圳基本持平，深圳也并没有像外界夸大的那样，对于数据分析需求很大。

#5.3Plot the location with salary and Industry
###HeatMap could not be ploted, because the data is not enough, we cann't see any effects on the plot
ggmap(Guangzhou) + geom_tile(data = job_coord, aes(x = lon, y = lat, alpha = test),
                             fill = "red") + theme(axis.title.y = element_blank(), axis.title.x = element_blank())

####Summary of Location Analysis
##1.Beijing is a good place.:). Must go to Beijing!
##2.Guangzhou and ShenZhen are similar, both of them less popular than Beijing. 

#######Save some plots#######
# #China
# png(filename = "China.png", width = 800, height = 600, units = "px")
# ggmap(china1) +
#   geom_point(
#     aes(x = lon, y = lat),
#     data = job_coord, colour = "red", size = 3
# )
# dev.off()
# 
# #Beijing
# png(filename = "Beijing.png", width = 800, height = 600, units = "px")
# ggmap(Beijing)+
#   geom_point(
#     aes(x = lon, y = lat),
#     data = job_coord, colour = "red", size = 3
#   )
# dev.off()
# 
# #Shanghai
# png(filename = "Shanghai.png", width = 800, height = 600, units = "px")
# ggmap(Shanghai)+
#   geom_point(
#     aes(x = lon, y = lat),
#     data = job_coord, colour = "red", size = 3
#   )
# dev.off()
# 
# #Shenzhen
# png(filename = "Shenzhen.png", width = 800, height = 600, units = "px")
# ggmap(Shenzhen)+
#   geom_point(
#     aes(x = lon, y = lat),
#     data = job_coord, colour = "red", size = 3
#   )
# dev.off()

# #Guangzhou
# png(filename = "Guangzhou.png", width = 800, height = 600, units = "px")
# ggmap(Guangzhou)+
#   geom_point(
#     aes(x = lon, y = lat),
#     data = job_coord, colour = "red", size = 3
#   )
# dev.off()

#########6.Analysis of Financing#####
Financing = data[,6]
#Frequency Counting 
Financing_fre = data.frame(table(Financing))
Financing_fre = Financing_fre[order(Financing_fre$Freq, decreasing = TRUE),]
Financing_fre = Financing_fre[-9,] #delete the data which is not fanancing
#Financing Visulization 
pct <- round(Financing_fre [,2]/sum(Financing_fre [,2])*100)
lbls <- paste(Financing_fre [,1], pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(Financing_fre [,2], labels = lbls, main="Distribution of Financing")

###Conclusion:
#1.大体来说，主要需求分开为两层:(1)上市公司(20%) (2)D轮以下(55%)天使轮以及未融资以上
#2.可以这么猜想，上市公司需要拓展新业务，或者需要节流，因此对于数据分析多;D轮以下的公司是正在搏杀的阶段，因此数据分析称为扩展业务的利器
#3.天使轮以及未融资的企业和的需求比较少，可能是因为公司数量本身就不多，也可能是因为还没到使用数据分析的阶段。
#4.D轮及以上但未上市的企业需求少。这个点非常奇怪，作为接近IPO的企业，一般来说人员和机制比较成熟，流动不大，业务也比较成熟，所以，可能因此并没有很大的数据分析的需求。












