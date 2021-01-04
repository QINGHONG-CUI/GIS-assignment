library(tidyverse)
library(moderndive)
install.packages("skimr")
library(skimr)
install.packages("gapminder")
library(gapminder)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(tibble)
library(stringr)
library(forcats)
install.packages("moderndive")
library(moderndive)
install.packages("skimr")
library(skimr)
install.packages("ISLR")
library(ISLR)
library(sf)
install.packages("plotly")
library(plotly)
install.packages("maptools")
install.packages(c("classInt", "tmap"))
install.packages(c("RColorBrewer", "sp", "rgeos", 
                   "tmaptools", "sf", "downloader", "rgdal", 
                   "geojsonio"))
library(maptools)
library(RColorBrewer)
library(classInt)
library(sp)
library(rgeos)
library(tmap)
library(tmaptools)
library(sf)
library(rgdal)
library(geojsonio)
library(tmap)
library(tmaptools)
install.packages("broom")
library(broom)
install.packages("car")
library(car)
install.packages("spdep")
library(spdep)
library(spatialreg)
library(spgwr)
library(base)
library(broom)
library(grid)
#导入ward地图
Ward <- st_read(here::here("ESRI",
                           "London_Ward_CityMerged.shp")) %>%
  st_transform(., 27700)
qtm(Ward)
summary(Ward)
#导入数据3.1,3.2
data <- read_csv("3.1.csv",
                        na = c("", "NA", "n/a"), 
                        locale = locale(encoding = 'Latin1'), 
                        col_names = TRUE)
data2 <- read_csv("3.2.csv",
                 na = c("", "NA", "n/a"), 
                 locale = locale(encoding = 'Latin1'), 
                 col_names = TRUE)
#合并数据3.1，3.2
Alldata <- data %>%
  left_join(.,data2,
            by=c("New ward code"="New ward code"))
##整理数据，删去不用的
Alldata <- Alldata[,c(-5,-6)]
#检查数据
Datatypelist <- Alldata %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist
#对原始数据进行描述性统计
install.packages("stargazer")  
library(stargazer)
mydata <- Alldata
mydata <- as.data.frame(mydata)
stargazer(mydata,type = "text", title="Statistical description for all indicators",digits=3,out="statistical description3.txt",
          covariate.labels=c("CIP","HFP","ICB","UEP","CRM","LEP") )

#对列名进行修改
library(janitor)
Alldata <- Alldata %>%
     dplyr::rename(CIP='childrenpoverty',HFP='householdsfuelpoor',ICB='Incapacity Benefit rate - 2009',
                  UEP='Unemployment rate 2009',CRM='Crime rate - 2009',LEP='Life Expectancy 2005-2009')

Alldata <- Alldata %>%
  dplyr::rename(Ward_name='Ward.x',Ward_code='New ward code')
#对自变量进行熵权法系数估计
##选取所需的自变量列
independentdata <- Alldata[,c(3,4,5,6,7)]
##标准化处理数据
###归一化最大最小化处理
min.max.norm<-function(x){
  (x-min(x))/(max(x)-min(x))
}
max.min.norm<-function(x){
  (max(x)-x)/(max(x)-min(x))
}
###正向指标负向指标
positive <- apply(independentdata[,c(1,2,3,4,5)],2,min.max.norm) #正向

##求出所有样本对指标Xj的贡献总量
step1 <- function(data)
{
  x <- c(data)
  for(i in 1:length(data))
    x[i] = data[i]/sum(data[])
  return(x)
}
dataframe <- apply(positive,2,step1)
##将上步生成的矩阵每个元素变成每个元素与该ln（元素）的积并计算信息熵
step2 <- function(data)
{
  x <- c(data)
  for(i in 1:length(data)){
    if(data[i] == 0){
      x[i] = 0
    }else{
      x[i] = data[i] * log(data[i])
    }
  }
  return(x)
}
dataframe1 <- apply(dataframe,2,step2)

k <- 1/log(length(dataframe1[,1]))
d <- -k * colSums(dataframe1)
##计算冗余度
d <- 1-d
##计算各项指标的权重
weights1 <- d/sum(d)
weights1
#用权重结果计算各ward的social deprivation数值
positive <- as.data.frame(positive)
socialdeprivation <- positive %>%
  mutate(SD=0.2602*CIP+0.1308*HFP+0.1728*ICB+0.2695*UEP+0.1667*CRM)
code <- Alldata %>%
  dplyr::select(Ward_code)
SDdata <- cbind(code,socialdeprivation)
#地图制作
WardMap <- Ward %>%
  left_join(.,SDdata,
            by = c("GSS_CODE" = "Ward_code"))

library(tmap)
library(tmaptools)
tmap_mode("plot")
qtm(WardMap, 
    fill = "SD")
##更规范的地图
my_breaks <- c(0,0.2,0.4,0.6,0.8)

tmap_mode("plot")
tm1 <- tm_shape(WardMap) + 
  tm_polygons("SD", 
              breaks=my_breaks,
              palette="YlOrBr")+
  tm_legend(show=FALSE)+
  tm_layout(frame=FALSE)

legend <- tm_shape(WardMap) +
  tm_polygons("SD",
              palette="YlOrBr",position=c(0.01,0.1)) +
  tm_scale_bar(position=c(0.07,0.2), text.size=0.6)+
  tm_compass(north=0,type='8star',size=5,position=c(0.1,0.5))+
  tm_layout(legend.only = TRUE, legend.text.size=1,legend.position=c(0.1,0.3),asp=0.1)
###地图布局设置
grid.newpage()

pushViewport(viewport(layout=grid.layout(1,2)))
print(tm1, vp=viewport(layout.pos.col=1, layout.pos.row=1, height=5))
print(legend, vp=viewport(layout.pos.col=2, layout.pos.row=1, height=5))

##---------------------------第二部分-------------------------------------
##把寿命整合进去
life <- Alldata %>%
  dplyr::select(c(1,8))

WardMap <- WardMap %>%
  left_join(.,life,
            by = c("GSS_CODE" = "Ward_code"))

#检查变量分布的情况
histplot <- ggplot(data=WardMap, aes(x=SD))
histplot +geom_histogram() ##SD正态分布
histplot <- ggplot(data=WardMap, aes(x=LEP))
histplot +geom_histogram()  ##LEP正态分布
##二者大致分布
q <- qplot(x = `SD`, 
           y = `LEP`, 
           data=WardMap)
q + stat_smooth(method="lm", se=FALSE, size=1) + 
  geom_jitter()
##分布的不错，试试线性函数
OLSmodel <- WardMap %>%
  lm(LEP ~ SD,
     data=.)
summary(OLSmodel) #结果显著，R方为0.5032，自变量系数-11.31

tidy(OLSmodel)
##残差检验
WardMap$res_SD <- residuals(OLSmodel)
WardMap%>%
  dplyr::select(res_SD)%>%
  pull()%>%
  qplot()+ 
  geom_histogram() 
##检验残差的齐次方差
par(mfrow=c(2,2))  
plot(OLSmodel)  #完美
##测试残差是否相关by Durbin-Watson test
DW_LEP <- durbinWatsonTest(OLSmodel)
tidy(DW_LEP) #1.38轻微的正相关，问题不大
##检验空间自相关！重头戏
tmap_mode("view")
tm_shape(WardMap) +
  tm_polygons("res_SD",
              palette = "RdYlBu") #有一些空间自相关现象
##规范制图
my_breaks2 <- c(-6,-4,-2,0,2,4,6,8)

tmap_mode("plot")
tm2 <- tm_shape(WardMap) + 
  tm_polygons("res_SD", 
              breaks=my_breaks2,
              palette="RdYlBu")+
  tm_legend(show=FALSE)+
  tm_layout(frame=FALSE)

legend2 <- tm_shape(WardMap) +
  tm_polygons("res_SD",
              palette="RdYlBu",position=c(0.01,0.1)) +
  tm_scale_bar(position=c(0.07,0.2), text.size=0.6)+
  tm_compass(north=0,type='8star',size=5,position=c(0.1,0.6))+
  tm_layout(legend.only = TRUE, legend.text.size=1,legend.position=c(0.1,0.3),asp=0.1)
###地图布局设置
grid.newpage()

pushViewport(viewport(layout=grid.layout(1,2)))
print(tm2, vp=viewport(layout.pos.col=1, layout.pos.row=1, height=5))
print(legend2, vp=viewport(layout.pos.col=2, layout.pos.row=1, height=5))

##莫兰指数检验空间自相关

###各Ward质心的计算
coordsw <- WardMap%>%
  st_centroid()%>%
  st_geometry()
plot(coordsw)

LWard_nb <- WardMap %>%
  poly2nb(., queen=T)

knn_wards <-coordsw %>%
  knearneigh(., k=4)

LWard_knn <- knn_wards %>%
  knn2nb()
plot(LWard_nb, st_geometry(coordsw), col="red")
plot(LWard_knn, st_geometry(coordsw), col="blue")

Lward.queens_weight <- LWard_nb %>%
  nb2listw(., style="C")

Lward.knn_4_weight <- LWard_knn %>%
  nb2listw(., style="C")
##莫兰指数
Queen <- WardMap %>%
  st_drop_geometry()%>%
  dplyr::select(res_SD)%>%
  pull()%>%
  moran.test(., Lward.queens_weight)%>%
  tidy()
Nearest_neighbour <- WardMap %>%
  st_drop_geometry()%>%
  dplyr::select(res_SD)%>%
  pull()%>%
  moran.test(., Lward.knn_4_weight)%>%
  tidy()
####自相关检验结果整合
MoransIresults <- rbind(Queen,Nearest_neighbour) 
#######加个名称
testnames <- c('Queen','K-nearest')
testnames <- as.data.frame(testnames)
MoransIresults <- cbind(testnames,MoransIresults)
#-----------------------------第三部分，空间回归模型--------------------------------
##先用SLM模型
SLM_queen <- lagsarlm(LEP ~SD, 
                    data = WardMap, 
                    nb2listw(LWard_nb, style="C"), 
                    method = "eigen")
tidy(SLM_queen)
glance(SLM_queen)
summary(SLM_queen) ##发现空间滞后项Rho不显著？


SLM_knn4 <- lagsarlm(LEP ~ SD, 
                                data = WardMap, 
                                nb2listw(LWard_knn, 
                                         style="C"), 
                                method = "eigen")

tidy(SLM_knn4)
glance(SLM_knn4)
summary(SLM_knn4) ##发现SLM-knn4的效果要优于SLM--queen
###检验SLM_knn4残差看是否还在空间自相关
WardMap <- WardMap %>%
  mutate(SLM_knn_resids = residuals(SLM_knn4))

SLMMoran <- WardMap %>%
  st_drop_geometry()%>%
  dplyr::select(SLM_knn_resids)%>%
  pull()%>%
  moran.test(., Lward.knn_4_weight)%>%
  tidy()

SLMMoran  ##好多了哈哈

##再用SEM试试
SEM_model <- errorsarlm(LEP ~ SD, 
                         data = WardMap,
                         nb2listw(LWard_knn, style="C"), 
                         method = "eigen")

tidy(SEM_model)
summary(SEM_model)
glance(SEM_model)


##比较而言，最后选择SEM







