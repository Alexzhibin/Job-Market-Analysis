# Job-Market-Analysis
拉勾网数据分析
This is a case example displase the procedure of data analysis.<br />
Website: www.lagou.com
# Get data 
1.The setting of search will be: (1)Experience:1-3 year experience. (2)Key Word:数据分析师. (3)Location: China<br />
2.[Crawl all related links in a stack.](https://github.com/Alexzhibin/Job-Market-Analysis/blob/master/Link_Result.ipynb)<br />
---   [Result.](https://github.com/Alexzhibin/Job-Market-Analysis/blob/master/urls_list.txt)<br />

3.[Crawl the data from each link. ](https://github.com/Alexzhibin/Job-Market-Analysis/blob/master/Job_Market_Crawling.ipynb)<br />

# Clean data
1. Use R to clean the data frame.
2. Remove invalid rows

# Basic Analysis and Visulization 
1. Analysis of each column
2. [Visulization](https://github.com/Alexzhibin/Job-Market-Analysis/blob/master/job_analysis.R)

#Analysis of Location
1. [Use Google API to conver the address to coordinates.](https://github.com/Alexzhibin/Job-Market-Analysis/blob/master/HeatMap_locations.R)
2. Plot the distributions of companies on GGMAP. 

#Text Mining
1. Use Jieba package to split the words.
2. Filter the useless words.
3. [Word Count and Word Cloud.](https://github.com/Alexzhibin/Job-Market-Analysis/blob/master/Textming_jd.R)


# Summary 
All the summaries and analysis will be displayed on the PPT. 



