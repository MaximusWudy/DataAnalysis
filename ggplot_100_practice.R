#GGpplot实操100练
#qplot
library(ggplot2)
data(diamonds)
View(diamonds)
#抽取100个随机样本
set.seed(42)
#由于diamonds是dataframe，我们按照行来抽取数据集
dsmall<-diamonds[sample(nrow(diamonds),100),]
#基本的图形,先看看变量名称，在进行绘图
colnames(dsmall)
#qplot是先x后y,我们需要在函数最后的一个参数define数据集
qplot(carat,price,data=diamonds,margins = F)#发现margins参数，目前还不知道什么用途
#放大，在1.5-2的区间上去看，
qplot(carat,price,xlim=c(1.5,2),main='price vs. weight magnified',data=dsmall)
#这种相关关系似乎是指数型的
qplot(carat,price,log='xy',data=dsmall)#log是指which variables to log transform
#这个图就线性很多，**由于图中的元素有很大的重叠，在下结论时候要小心**

#向重量和价格的散点图添加颜色和切工的信息,参数是colour
levels(dsmall$color)#看到颜色的种类
qplot(carat,price,log='xy',colour=color,shape=cut,data=dsmall)
#颜色太深，设置统一的透明度
qplot(carat,price,log='xy',colour=color,shape=cut,alpha=I(1/5),data=dsmall)

#通过改变ggplot的几何对象几乎可以画出任何一类图形，geom=point,smooth,boxplot,path,line
#对于连续变量geom=histogram直方图,frepoly频率多边形，density;对于离散变量geom=bar条形图
qplot(carat,price,data=dsmall,geom=c('point','smooth'))#可以同时画出多个图
qplot(carat,price,data=diamonds,geom=c('point','smooth'),se=F)#取消绘制标准误
qplot(carat,price,data=dsmall,geom=c('point','smooth'),method='loess')#n较小时候是默认，局部回归法
#loess对于大数据的内存消耗是n^2
qplot(carat,price,data=dsmall,geom=c('point','smooth'),method='loess',span=0.5)#平滑程度0-1

#曲线拟合
qplot(carat,price,data=dsmall,geom=c('point','smooth'),method='lm',se=F)
#拟合一个二次多项式
qplot(carat,price,data=dsmall,geom=c('point','smooth'),method='lm',formula=y~poly(x,2),se=F)
#使用MGCV包里头的脊回归
library(mgcv)
qplot(carat,price,data=dsmall,geom=c('point','smooth'),ylim=c(0,20000),method='gam',formula=y~s(x,bs='cs'))
#splines包让我们使用自然样条，第二个参数是自由度，其取值越大曲线波动越大
library(splines)
qplot(carat,price,data=dsmall,geom=c('point','smooth'),ylim=c(0,20000),method='lm',formula=y~ns(x,5))
#加ylim控制图高方便前后两图的比较
#MASS包中更稳健的线性拟合算法
library(MASS)
qplot(carat,price,data=dsmall,geom=c('point','smooth'),ylim=c(0,20000),method='rlm')
#与最基本的拟合对比
qplot(carat,price,data=dsmall,geom=c('point','smooth'),ylim=c(0,20000),method='lm')

#箱线图和扰动点（geom='jitter')
qplot(color,price/carat,data=diamonds,geom='jitter',alpha=I(1/20),size=10,colour='red',shape='diamonds')
#扰动点图参数size colour shape
qplot(color,price/carat,data=diamonds,geom='boxplot',size=1.2,colour='red',fill='red')
#箱线图 colour外框颜色，fill内部颜色，size线的粗细

#直方图和密度曲线
attach(diamonds)
qplot(carat,data=diamonds,geom='histogram',binwidth=(max(carat)-min(carat))/20,xlim=c(0,4))
qplot(carat,data=diamonds,geom='density')#adjust取值越大，曲线越平滑，binwidth组距，breaks切分位置
#在绘图过程中应该尝试多种组距，组距较大可以显示总体特征，组距较小可以显示更多细节

#不同组之间的分布对比，只需要加上一个图形映射
qplot(carat,data=diamonds,geom='density',colour=color)
qplot(carat,data=diamonds,geom='histogram',fill=color,binwidth=(max(carat)-min(carat))/20)
#**当一个分类变量被映射到某个图形属性，几何对象会自动按照这个变量进行拆分**

#条形图(我知道了，color不管怎么设置都是红色= =)
qplot(color,data=diamonds,geom='bar',colour='red')
qplot(color,data=diamonds,geom='bar',colour='red',weight=carat)+scale_y_continuous('carat')
#按照重量加权，展示了每种钻石的总重量？？

#时间序列的线条图
data("economics")
qplot(date,unemploy/pop,data=economics,geom='line')

#散点图和路径图的重叠
year <- function(x) as.POSIXlt(x)$year+1900 #这个函数只提取年的数值,+1900表示绝对年份
qplot(unemploy/pop,uempmed,data=economics,geom=c('point','path'))
qplot(unemploy/pop,uempmed,data=economics,geom=c('path'),colour=year(date))
#可以用group图形属性映射到一个表示分组的变量之上，从而多个时间序列，每一个代表一个个体

#分面，使用.作为占位符
qplot(carat,data=diamonds,facets=color~.,geom='histogram',binwidth=0.1,xlim=c(0,3))
qplot(carat,..density..,data=diamonds,facets=color~.,geom='histogram',binwidth=0.1,xlim=c(0,3),colour=I('green'),fill=I('yellow'))
#新的语法，将密度而不是频数映射到y
#oh原来可以通过I('green')来修改颜色参数的！

#接下来我们要深入学习ggplot语法的精髓，代码可能是整合的，因此每一条都需要仔细研究
#一旦你掌握了ggplot2的语法，你就会发现根据具体问题所做的很多图形都没有特定的名字
#气泡图——点的大小映射给其他变量
#坐标的映射（标度变换scaling)使用笛卡尔坐标系；颜色的分类变量直接等距映射到色轮上
#标度训练步骤：根据所有小数据集里头的数据的范围得到整体数据的范围
#标度变换是在统计变换前执行的，坐标变换是在统计变换后执行的
qplot(displ,hwy,data=mpg,facets=.~year)+geom_smooth()

#图形对象options，专门用来储存特定的图形主题选项
#在循环过程中要显示图形:print()
#ggsave();summary()简单查看结构；save()保存缓存副本，可以直接用load重现。数据同时储存不会改变。
#save(p,file='plot.rdata');load('plot.rdata')
#ggsave('plot.png',width=5,height=5)

#--------------
#chap4用图层构建图像
#数据必须为dataframe,ggplot()主要的参数：数据和图形属性映射——作为绘图的默认参数
#每一个几何对象都对应着一个默认的统计变换和未知参数，每一个统计变化也对应着一个默认的几何对象参数
#mapping=aes()可选；data可以修改默认的数据集
#图层是普通的R对象，可以储存到变量里头，有利于简化代码。
bestfit<-geom_smooth(method='lm',se=F,colour=alpha('steelblue',0.5),size=2)
library(scales)
qplot(sleep_rem,sleep_total,data=msleep)+bestfit
#对于数据变化使用reshape2和plyr
p<-ggplot(msleep, aes(sleep_rem, sleep_total))
mtcars<-transform(mtcars,mpg=mpg^2)
p%+%mtcars#更新数据集
p+geom_point()+bestfit
#最好不要使用数据集以外的变量，这样无法将绘图用到的数据都封装到一个对象里
#图形对象p中默认的映射可以在新图层里进行扩充或者修改
p+geom_point(aes(colour=factor(cyl)))
#但是一个图层里设定的图形属性映射只对该图层有效,例如aes(y=NULL)
#设置为单一值
p+geom_point(colour='darkblue')
p+geom_point(aes(colour='darkblue'))#区别！！
#后者是先创建了一个只有darkblue字符的变量，然后将colour映射到这个新变量
#默认的颜色标度将用色轮上等距的颜色——选择了桃红色

#分组group
library(nlme)
data("Oxboys")
p<-ggplot(Oxboys,aes(age,height,group=Subject))+geom_line(colour='darkblue')
p<-ggplot(Oxboys,aes(age,height))+geom_line(colour='darkblue')
#没有分组就会出现奇怪的图形，没有意义
#aes(group=1)可以得到所有男孩的拟合直线
p<-ggplot(Oxboys,aes(age,height,group=1))+geom_smooth(method='lm',size=2,colour='black')
boysbox<-ggplot(Oxboys,aes(Occasion,height))+geom_boxplot()
#occation，9个不同时期
boysbox+geom_smooth(aes(group=Subject),colour='darkblue',se=F)#这里就一定要设置group=1
##R不能绘制不同线条类型相连的线条
#%%更复杂的系统，线段平稳地从一种图形属性变到另一种图形属性，
#这种方式对于连续型变量有效【大小、颜色】不适用于渐变的线条类型
#线性插值法
xgrid<-with(df,seq(min(x),max(x),length=50))
interp<-data.frame(
    x=xgrid,
    y=approx(df$x,df$y,xout=xgrid)$y,
    colour=approx(df$x,df$colour,xout=xgrid)$y
)
qplot(x,y,data=df,colour=colour,size=I(5))+geom_line(data=interp,size=2)

#当映射对象是离散型的变量时，它将默认将群组几何对象分解成更小的块
#适用于条形图和面积图的绘制
#一些几何对象例子
#area面积图 bin2d二维热图 blank不画 crossbar带有水平中心线的盒子图
#density2d二维密度等高线图 dotplot点直方图用点表示观测值个数 errorbar误差棒
#freqpoly频率多边形 hex六边形表示的二维热图 linerange一条代表一个区间的竖直线
#path按原始顺序连接各观测值 pointrangeyoga一条中间带点的竖直线代表一个区间
#polygon多边形，一个有填充的路径 quantile添加分位数回归线 raster高校的矩形瓦片
#rect二维矩形图 ribbon色带图，连续的x值对应的y范围 rug边际地毯图 segment线段或箭头
#step阶梯形式连接各个观测值 tile瓦片图 violin小提琴图 


#--------统计变换
#一个统计变换必须是一个位置尺度不变量
#bin2d计算矩形封箱内的观测值个数 contour三维数据的等高线 density2d二维密度估计
#function添加新函数 identity不对数据进行统计变换 qq计算qq图的相关值
#spoke将角度和半径转换成xend和yend？
#sum计算每个单一值的频数，为了解决散点图的图形重叠问题
#summary2d 对于二维矩形封箱设定函数 unique删除重复值 ydensity小提琴图，计算一维y轴方向的核密度函数估计值

#统计变换可以向原数据集中插入新的变量，比如density
ggplot(diamonds,aes(carat))+geom_histogram(aes(y=..density..),binwidth = 0.1)
#生成变量的名字必须用..围起来，防止原数据集中变量混淆

#位置调整：dodge并排放置；fill堆叠并将高度标准化为1；jitter添加扰动，stack图形元素堆叠起来
#显示已经计算过的统计量stat_identity()

#将不同的数据画在不同的图层上：不同的数据要画在同一个图上——拟合模型得出的
#预测值需要来扩充原数据集
require(nlme,quiet=T,warn.conflicts = F)
model<-lme(height~age,data=Oxboys,
           random=~1+age|Subject)
oplot<-ggplot(Oxboys,aes(age,height,group=Subject))+geom_line()

#加入预测值
age_grid<-seq(-1,1,length=10)
subjects<-unique(Oxboys$Subject)
preds<-expand.grid(age=age_grid,Subject=subjects)#Create a Data Frame from All Combinations of Factors
preds$height<-predict(model,preds)#The function invokes particular methods which depend on the class of the first argument.
#不同修改任何图形属性，只用修改数据集
oplot+geom_line(data=preds,colour='steelblue',size=0.4)
#仍然不能分辨细节，我们可以看一下模型拟合的残差
#1，拟合值和残差添加到原数据，2，更新数据集，3，默认的y图形属性改成resid4，整个数据添加一条光滑曲线
Oxboys$fitted<-predict(model)
Oxboys$resid<-with(Oxboys,fitted-height)
oplot%+%Oxboys+aes(y=resid)+geom_smooth((aes(group=1)))
#发现啊残差不是随机分布，模型有缺陷
#向模型中添加一个二次项，再次计算拟合值和残差
model2<-update(model,height~age+I(age^2))
#Sometimes it is useful to call update with only one argument, for example if the data frame has been corrected.
Oxboys$fitted2<-predict(model2)
Oxboys$resid2<-with(Oxboys,fitted2-height)
oplot%+%Oxboys+aes(y=resid2)+geom_smooth(aes(group=1))
#重新做了两次图而没有再次运行过oplot，ggplot2理念，使得反复拟合和评估模型变得轻松自然


#chap5利用不同的几何对象和统计变换来解决特定的可视化问题
#本章的每一节都将解决一个特定的作图问题
#展示数据本身可以帮助我们改善模型，而展示模型可以帮助我们解释数据的微妙之处
#geom_text唯一一个需要添加格外的图形属性——指定label参数，可以通过设置可选的图形属性
#hjust和vjust来控制文本的横纵，设置angle控制文本的旋转【附录B】
#示例
df<-data.frame(x=c(3,1,5),y=c(2,4,6),label=c('a','b','c'))
p<-ggplot(df,aes(x,y))+xlab(NULL)+ylab(NULL)#这个xylab是在加号级上的
p+geom_point()+labs(title='geom_point')
p+geom_bar(stat='identity')+labs(title='geom_bar(stat=\"identity\")')
#\"可以用来添加双引号
p+geom_line()+labs(title='geom_line')
p+geom_area()+labs(title='geom_area')
p+geom_path()+labs(title='geom_path')
p+geom_text(aes(label=label))+labs(title='geom_text')
p+geom_tile()+labs(title='geom_tile')
p+geom_polygon()+labs(title='geom_polygon')

#展示数据分布%%%
#直方图,多种方式可以用来进行分布的跨组比较：同时绘制多个小的直方图，facets=.~var
#频率多边形；条件密度图position='fill'
depth_dist<-ggplot(diamonds,aes(depth))+xlim(58,68)#xlimy也是与+同级的
depth_dist+geom_histogram(aes(y=..density..),binwidth=0.1)+facet_grid(cut~.)
depth_dist+geom_histogram(aes(fill=cut),binwidth = 0.1,position='fill')
depth_dist+geom_freqpoly(aes(y=..density..,colour=cut),binwidth=0.1)+labs(title='freq_poly')
#colour是在aes（）里头的,freqpoly也可以设置binwidth
#主要使用的是facet_grid(cut~.)
#都可以看出，随着钻石质量的提高，分布逐渐向左偏移而且越发对称

#箱线图：也可以对连续型变量取条件，前提是数据预先经过binning处理
library(plyr)#主要为了使用round_any
qplot(cut,depth,data=diamonds,geom='boxplot')
qplot(carat,depth,data=diamonds,geom='boxplot',group=round_any(carat,0.1,floor),xlim=c(0,3))+geom_smooth(aes(group=1))

#密度图：仅在已知潜在的密度分布是平滑、连续而且无界的时候使用这种密度图。
#adjust参数用于调整密度曲线的平滑程度
qplot(depth,data=diamonds,geom='density')
qplot(depth,data=diamonds,geom='density',group=cut,fill=cut)

#处理数据遮盖：可以使用shape='.'使得点的大小为像素级
#linetype:0 = blank, 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash
df<-data.frame(x=rnorm(2000),y=rnorm(2000))
norm<-ggplot(df,aes(x,y))
norm+geom_point(alpha=1/2)
norm+geom_point(shape=1)#中空
norm+geom_point(shape='k',size=3)#卧槽显示的全是K
norm+geom_point(shape='.')#单个像素点

#增加扰动，与透明度一起使用时候特别有效，默认下增加的扰动量是数据分辨率的40%
td<-ggplot(diamonds,aes(table,depth))+xlim(50,70)+ylim(50,70)
td+geom_point()
td+geom_jitter()
jit<-position_jitter(width=0.5)#设置常用参数
#The jitter is added in both positive and negative directions, 
#so the total spread is twice the value specified here.
td+geom_jitter(position=jit)
td+geom_jitter(position=jit,colour='black',alpha=1/15)

#遮盖绘制问题：将点分箱并统计每个箱中点的数量
#划分为正方形容易产生假象，可以使用六边形箱geom_hexagon,使用参数bins和binwidth控制箱的数量和大小
d<-ggplot(diamonds,aes(carat,price))+xlim(1,3)#+theme(legend.position='none'),可以不要legend
d+stat_bin2d(colour='white')
d+stat_bin2d(bins=20,colour='bisque')
library(hexbin)
d+stat_binhex(binwidth = c(0.02,200))+theme(legend.position='none')

theme(text=element_text(family="STKaiti",size=14))
#中文字体显示
#使用stat_density2d作二维密度估计，将等高线添加到散点图
d<-ggplot(diamonds,aes(carat,price))+xlim(1,3)+theme(legend.position='none')
d+geom_point()+geom_density2d()#作出的是等高线图+散点图
d+stat_density2d(geom='point',aes(size=..density..),
                 contour=F)+scale_size_area()#作出的是二维密度的散点图！
#命令当中的aes(size=..density..)与scale_size_area()十分重要！！
d+stat_density2d(geom='tile',aes(fill=..density..),contour=F)#一定要设置成F！！
last_plot()+scale_fill_gradient(limits=c(1e-5,8e-4))#在这里设置fill_gradient的limit！

#地图：使用地图数据2种原因：1，为空间数据图形添加参考轮廓线，2，在不同的区域填充颜色构建等值线图
#maps包：france italy newzealand county(USA) state(USA) usa(border) world
library(maps)
data("us.cities")
big_cities<-subset(us.cities,pop>500000)
qplot(long,lat,data=big_cities)+borders('state',size=0.5)#%%
tx_cities<-subset(us.cities,country.etc=='TX')
ggplot(tx_cities,aes(long,lat))+borders('county','texas',colour='grey70')+geom_point(shape='H',colour='black',size=3)

scale_shape(solid=FALSE) #with emply heart
scale_shape_manual(values=c(21,24))  # Shapes: Filled circle, triangle

#等值线图：将我们数据中的标识符和地图数据中的标识符完全匹配
#map_data()将地图数据转换为数据框，之后通过merge（）同我们的数据融合
#关键是我们的数据和地图数据要有一列可以相互匹配
library(maps)
states<-map_data('state')
head(states);class(states)
arrests<-USArrests
names(arrests)<-tolower(names(arrests))
arrests$region<-tolower(rownames(USArrests))
choro<-merge(states,arrests,by='region')#合并表格
#由于绘制多边形时设计顺序问题，且merge破坏了原始顺序，需要重新排序
choro<-choro[order(choro$order),]
qplot(long,lat,data=choro,group=group,fill=assault,geom='polygon')#polygon的fill使用的哪个点？

#计算郡的近似中心，并利用这些中心位置数据在地图上对其名称进行标注
library(plyr)#分割再合并
ia<-map_data('county','iowa')
mid_range<-function(x)mean(range(x,na.rm=T))
centres<-ddply(ia,.(subregion),colwise(mid_range,.(lat,long)))#plyr中的包
ggplot(ia,aes(long,lat))+geom_polygon(aes(groug=group),fill=NA,colour='grey60')+
    geom_text(aes(label=subregion),data=centres,size=2,angle=45)

#揭示不确定性，对于线性模型，effects包非常适合提取标准误等值
effectdf<-function(...){
    suppressWarnings(as.data.frame(effect(...)))
}
color<-effectdf('color',mod)
carat<-effectdf('lcarat',mod,default.levels=3)
#连续型：geom_ribbon仅展示区间，geom_smooth展示区间和中间值
#离散型：geom_errorbar;geom_linearange展示区间；geom_crossbar geom_pointrange展示区间和中间值
#如何提取边际效应和条件效应，代码较复杂先不看了P103

#注意，为这类型图添加题注时需要细致地描述其中所含置信区间的本质，并说明观察置信区间
#之间的重叠是否有意义（比较不同组时候如果区间没有重叠说明差异显著）
#要注意这些标准误是针对单组的均值的还是针对不同组间均值之差的。
#multcomp包和multcompView包非常有用：计算和展示这些标准误，同时在多重比较中
#可以正确地对自由度进行调整%%%

#先写出各个参数再添加数据集
fplot<-ggplot(mapping=aes(y=fit,ymin=lower,ymax=upper))+ylim(range(both2$lower,both2$upper))
fplot%+%color+aes(x=color)+geom_point()+geom_errorbar()

#统计摘要：stat_summary()--中位数曲线，median_hilow(),mean_cl_boot(),mean_cl_normal()
#离散：mean(),mean_cl_normal()均值点和误差棒，median_hilow()中位数点和值域，median_hilow()
midm<-function(x)mean(x,trim=0.5)
m2+stat_summary(aes(colour='trimmed'),fun.y=midm,geom='point')+
    stat_summary(aes(colour='raw'),fun.y=mean,geom='point')+
    scale_color_hue('Mean')#fun.y接受简单数值型摘要计算函数
#fun.data支持更复杂的摘要计算函数
#来自Hmisc包中的照耀计算函数，拥有专门的封装使得能够与stat_summary()共同使用
#mean_cl_normal()正态渐进所得标准误；mean_cl_boot()Bootstrap所得标准误
#mean_sdl()标准差的倍数；median_hilow()尾部面积相同的外分位点对

#添加图形注解
unemp<-qplot(data,unemploy,data=economics,geom='line',xlab='',ylab='No.unemplyed(1000s)')
presidential<-presidential[-(1:3)]
yrng<-range(economics$unemploy)
xrng<-range(economics$date)
unemp+geom_vline(aes(xintercept=as.numeric(start)),data=presidential)

#标记处最高点
highest<-subset(economics,unemploy==max(unemploy))
unemp+geom_point(data=highest, size=3,colour='red',alpha=0.5)
#geom_line,geom_path和geom_segment都可以添加直线，所有这些都有arrow参数
#也可以使用arrow（）函数绘制箭头，参数:angle,length,ends,type

#图像赋予权重:对于点和线，可以根据点的数量调整图形属性size
qplot(percwhite,percbelowpoverty,data=midwest)
qplot(percwhite,percbelowpoverty,data=midwest,size=poptotal/1e-6)+scale_size_area('Population\n(millions)',breaks=c(0.5,1,2,4))
#\n表示换行,scale_size_area ensures that a value of 0 is mapped to a size of 0.
#以人口数量为权重
qplot(percwhite,percbelowpoverty,data=midwest,size=area)+scale_size_area()
#以面积为权重
#对于更复杂的设计统计变换的情况，通过修改weight图形属性表现权重
#各种元素基本都支持权重的设定：各类平滑器、分位回归，箱线图，直方图，各类密度图
#可以做出以人口数量作为权重的最优拟合曲线
lm_smooth<-geom_smooth(method='lm',size=1)
qplot(percwhite,percbelowpoverty,data=midwest,weight=popdensity,size=popdensity)+lm_smooth
+scale_size_area()#使用这个参数只是让点点更小了一点
#当我们使用总人口作为权重去修改直方图或者密度图的时候，我们的视角将从对郡数量分布
#的观察转向对于人口数量分布的观察
qplot(percbelowpoverty,data=midwest,binwidth=1)
qplot(percbelowpoverty,data=midwest,weight=poptotal,binwidth=1)+ylab('population')
#qplot(poptotal,data=midwest,binwidth=10,ylim=c(0,1e+6))+ylab('real_population')

#Chap6标度、坐标轴和图例
#标度的值域包含了我们可以感知的，R能够理解的，是在的图形属性：位置、颜色、形状、大小、线条类型
#执行标度的过程：变换transformation、训练training、映射mapping
#标度的另一个重要角色，是生成一个允许读图者从图形属性空间到数据空间进行反向映射的引导元素guide
#方便从图中读出取值。
#我们基本无法直接控制坐标轴和图例的细节，引导元素的外观都是由标度的参数控制
#以哺乳动物睡眠数据为例
library(ggplot2)
data(msleep)
plot<-qplot(cty,hwy,data=mpg)
plot
plot+aes(x=drv)
plot+aes(x=drv)+scale_x_discrete()#同上面一样的

#改变图例的外观,breaks + labels
p<-qplot(sleep_total, sleep_cycle,data=msleep,colour=vore)
p+scale_color_hue('what does\nit eat?',
                  breaks=c('herbi','carni','omni',NA),
                  labels=c('plants','meat','both','don"t know'))
#不同的标度
p+scale_color_brewer(palette='Set1')#观察到色板不同

##四组标度：位置标度、颜色标度、手动标度、同一型标度
#手动标度是将离散型变量映射到我们选择的符号大小，线条类型
#形状或颜色，并创建对应的图例
#同一型：直接将变量绘制为图形属性，不去映射
#写数学表达式：？plotmath

#通用参数的使用
p<-qplot(cty,hwy,data=mpg,colour=displ)
p
p+scale_x_continuous('City map')#改了X轴
p+xlab("City map")
p+labs(x='City map',y='Highway',color='Displacement')
#这样才改了图例标题
p+xlab(expression(frac(miles,gallon)))#吼！分数表达式用frac
#??需要研究一下这个expression

#breaks控制坐标轴上应该显示哪些刻度线的值
#labels指定应在断点处显示的标签，即两者同时出现
#为指定时候使用formatter标签刷:comma, percent, dollar, scientific

#变换通常被用力啊修改位置标度，有各种变换器ans,exp,log10,log2,logit
scale_y_continuous(trans='log10')
scale_y_log10()#这两个是一样滴
#！！注意对标度进行对数变换和对数据进行对数变换坐标轴上的标签是不同的

#日期和时间
#支持date和POSIXct，major,minor,format控制外观和刻度的位置
#data_breaks()每隔两周放置一个主刻度
#labels=date_format()指定了刻度标签的格式14/10/1979 %d/%m/%y
library(scales)
plot<-qplot(date,psavert,data=economics,geom='line')+ylab('Personal savings rate')+ geom_hline(yintercept=0,colour='grey50')
plot+scale_x_date(breaks=date_breaks('10 years'))
plot+scale_x_date(limits=as.Date(c('2004-01-01','2005-01-01')),
                  labels=date_format('%Y-%m-%d'))

#颜色梯度，双色scale_colour_gradient() & scale_fill_gradient()
#设置high和low
#三色，scale_colour_gradient2() & scale_fill_gradient2()
#设置high、low和midpoint
#values让颜色不依照数据的范围均匀分布，rescale=T则values从0-1
#老忠泉数据为例，二维核密度估计表面
f2d<-with(faithful,MASS::kde2d(eruptions,waiting,h=c(1,10),n=50))
#WTF???
df<-with(f2d,cbind(expand.grid(x,y),as.vector(z)))
names(df)<-c('eruptions','waiting','density')
erupt<-ggplot(df,aes(waiting,eruptions,fill=density))+geom_tile()+scale_x_continuous(expand=c(0,0))+scale_y_continuous(expand=c(0,0))
erupt+scale_fill_gradient(limits=c(0,0.04))
#做一个小破瓦片图要这么多鬼东西！？
erupt+scale_fill_gradient(limits=c(0,0.04),low='white',high='black')
#这个好看
erupt+scale_fill_gradient2(limits=c(-0.04,0.04),midpoint=mean(df$density))
#创建自定义的颜色梯度，使用scale_colour_gradientn()
#也可以使用vcd包生成的调色板， Zeileis et al.(2008)
# library(vcd)
# fill_gradn<-function(pal){
#     scale_fill_gradientn(colours=pal(7),limites=c(0,0.04))
# }
# erupt+fill_gradn(rainbow_hcl)

#离散
#默认的scale_colour_hue()可以通过沿着hcl色轮选取均匀分布的色相来生成颜色
#对多至8种颜色时有较好效果
#但是默认配色素有颜色的明度和彩度都相同

#使用colorbrewer配色，对于类别型数据中的点，最感兴趣的调色板是Set1和Dark2
#对面积是Set2，Pastel1,Pastel2,Accent
#调出所有调色板：RColorBrewer::display.brewer.all
point<-qplot(brainwt,bodywt,data=msleep,log='xy',colour=vore)
point+scale_color_brewer(palette='Set1')
point+scale_color_brewer(palette='Set2')
point+scale_color_brewer(palette='Pastel1')
area<-qplot(log10(brainwt),data=msleep,fill=vore,binwidth=1)
area+scale_fill_brewer(palette='Set1')
area+scale_fill_brewer(palette='Set2')
area+scale_fill_brewer(palette='Pastel1')

#手动设置标度
plot<-qplot(bodywt,brainwt,data=msleep,log='xy')
colours<-c(carni='green','NA'='orange',insecti='yellow',herbi='blue',omni='black')
plot+aes(colour=vore)+scale_colour_manual(values=colours)
plot+aes(shape=vore)+scale_shape_manual(values=c(1,2,4,8,16))

#ggplot中，图例是标度绘制的，有时候标度不知道要为线条添加何种标签
#因此就可以直接在aes中的colour设置名称
huron<-data.frame(year=1875:1972,level=LakeHuron)
ggplot(huron,aes(year))+geom_line(aes(y=level-5),colour='blue')+geom_line(aes(y=level+5),colour='red')
#OR
ggplot(huron,aes(year))+geom_line(aes(y=level-5,colour='below'))+geom_line(aes(y=level+5,colour='above'))
#用scale_colour_manual进行修复
ggplot(huron,aes(year))+geom_line(aes(y=level-5,colour='below'))+geom_line(aes(y=level+5,colour='above'))+scale_colour_manual('Direction',values=c('below'='blue','above'='red'))

#网格线的外观是由panel.grid.major和panel.grid.minor两个主题设置来控制的
#其值为right,left,top,bottom,none,或一个表示位置的数值legend.justification=c(0,1)
#表示相对边角的位置，右上角c(1,1)左下角c(0,0)
#优秀参考资料Cleveland(1993,1985);Cleveland and McGill(1987)
#Tufte(1990,1997,2001,2006)
#Brewer(1994)
#Carr(1994,2002);Carr and Sun(1999)使用颜色的一般规律和方法

#Chap7定位——布局分面和坐标系如何工作？
#坐标系——控制两个独立的位置标度来生成一个2维的坐标系，最常见的是
#笛卡尔坐标系，还有其他种类。
#对于探索性数据分析来说，分面是一个强大的工具，快速分析出数据各子集模式的异同
#ggplot2提供网格型和封装型分面facet_grid & facet_wrap
library(ggplot2)
qplot(cty,hwy,data=mpg)+facet_grid(.~cyl)#有助于y位置的比较
qplot(cty,data=mpg,geom='histogram',binwidth=2)+facet_grid(cyl~.)
#有助于x位置的比较
##多个变量的多个水平在行或者列上或同时：.~a+b a+b~.

#使用参数margins来绘制边际图
#margins=T; margins=c('sex','age')
p<-qplot(displ,hwy,data=mpg)+geom_smooth(method='lm',se=F)
p+facet_grid(cyl~drv)
p+facet_grid(cyl~drv,margins = T)
p+facet_grid(cyl~drv,margins = 'drv')

qplot(displ,hwy,data=mpg)+geom_smooth(aes(colour=drv),method='lm',se=F)+facet_grid(cyl~drv,margins=T)
#对每个驱动轮类型添加了彩色平滑线
#相对于屏幕显示，具有许多分面和边际图的图形更适合印刷（72dpi）

#facet_wrap在处理单个多水平变量时非常有用。
qplot(rating,..density..,data=subset(movies,decade>1890),geom='histogram',binwidth=0.5)+facet_wrap(- decade, ncol=6)

#标度控制——对于两种分面，可以通过调整scales 来控制面板的位置标度是相同（固定）
#还是允许变化（自由）
scales='fixed'
scales='free'
scales='free_x'
scales='free_y'
#自由标度在展示不同量纲的时间序列时非常有用
library(reshape2)
em<-melt(economics,id='date')
qplot(date,value,data=em,geom='line',group=variable)+facet_grid(variable ~.,scale='free_y')
#真的区别非常大！！
#space='free'每行或者每列的宽度或者高度与该行或者列的标度范围成比例。
#这将使得所有面板的标度比例相同，对分类标度非常有用。
mpg2<-within(mpg,{model<-reorder(model,cty)
manufacturer<-reorder(manufacturer,-cty)})
models<-qplot(cty,model,data=mpg2)
models
models+facet_grid(manufacturer~.,scales='free',space='free')
+theme(strip.text.y=element_text())#这个主题设置用来旋转分面标签

#并列与分面——对分面图形中的标签做一些调整
qplot(color,data=diamonds,geom='bar',fill=cut,position='dodge')
qplot(cut,data=diamonds,geom='bar',fill=cut)+facet_grid(.~color)+theme(axis.text.x=element_text(angle=45,hjust=1,size=8,colour='grey50'))
#注意主题的设置(theme)

#连续型变量——首先需要转化为离散型，然后进行分面
#1. 分为n个长度相同的部分，cut_interval(x,n=10),cut_interval(x,length=1)
#2. 讲数据划分为n个有相同数目点的部分：cut_number(x,n=10)
library(ggplot2)
mpg$disp_ww<-cut_interval(mpg$displ,length=1)#每个面板长度为1
plot<-qplot(cty,hwy,data = mpg)+labs(x=NULL,y=NULL)#去掉坐标轴名字，图像更大
plot+facet_wrap(~ disp_ww,nrow=1)
head(mpg$disp_ww);class(mpg$disp_ww)

#坐标系：ggplot2包含了6种不同的坐标系
#coord_equal同尺度笛卡尔,coord_flip翻转的笛卡尔,coord_trans变换的笛卡尔,coord_map地图射影,coord_polar极坐标
#当设定坐标系的范围xlim & ylim时就想放大镜一样，只展示一小片图形区域
#这样在线段拟合的时候就不会因为重新拟合而出问题！！
p+coord_cartesian(xlim=c(325,500))
#翻转坐标轴
qplot(cty,displ,data=mpg)+geom_smooth()#相当于先翻转再对于x-y进行拟合
qplot(displ,cty,data=mpg)+geom_smooth()+coord_flip()#拟合初始数据再翻转输出结果
#coord_trans有变换器
qplot(carat,price,data=diamonds,log='xy')+geom_smooth(method='lm')
library(scales)
last_plot()+coord_trans(x=exp_trans(10),y=exp_trans(10))
#coord_equal使得x轴和y轴上的1cm代表相同的数据波动范围，可以修改
#ratio参数来更改两者的尺度比例

#Chap8
#精雕细琢之主题设置
#ggplot2在绘图时首先确定数据如何展示，之后再用主题系统对细节进行渲染
#重要的是通过自己的设置，获得自己独有的主题
#内置主题them_gray()淡灰色背景和白色网格线
#base_size参数用来控制基础字体的大小（轴标题，一般图形标题比其大20%，轴须标签小20%）
#全局设置theme_set(theme_grey());theme_set(theme_bw())
#theme_set（）返回先前的主题，可以储存后面用
previous_theme<-theme_set(theme_bw())
theme_set(previous_theme)
#局部设置 + theme_grey(),会覆盖默认的全局设置
#这些控制元素外观的函数被称为元素函数，通过对水平和竖直方向元素的不同设置
#内置元素函数有4个基础类型：文本，线条，矩形，空白，每个元素都有一系列控制外观的参数
#角度的改变可能对轴须标签很有用，需将hjust设置为0-1
hgram<-qplot(mpg,data=mtcars,binwidth=5)+theme_bw()
hgramt<-hgram+labs(title='This is a histogram')
hgramt+theme(plot.title=element_text(size=20,colour='red',hjust=0))
#hjust用来修改缩进0是左对齐
hgramt+theme(plot.title=element_text(size=20,face='bold'))
#用face参数来设置粗体
hgramt+theme(plot.title=element_text(size=20,angle=180))
#angle用来翻转

#element_line()绘制线条或者线段，控制colour，size，linetype
hgram+theme(panel.grid.major=element_line(colour='red'))
#Oh 注意panel.grid.major的用法啊！
theme_set(theme_grey())
hgram+theme(axis.line=element_line(size=0.7,colour='red'))
#这里是因为主题问题所以坐标轴颜色没有显示出来
#element_rect()绘制主要供背景使用的矩形
#控制fill,colour,size,linetype(边界)
#element_blank()表示空主题，对元素不分配相应的绘图空间（之间的colour=NA,fill=NA)
#是会占用空间的
last_plot()+theme(panel.grid.major=element_blank())
#这样就去除了网格线

#用theme_get()可以得到当前主题的设置
#theme_update()可以为后面图形的绘图进行全局性地修改
old_theme<-theme_update(
    plot.background=element_rect(fill='#3366FF'),
    panel.background=element_rect(fill='#003DF5'),
    axis.text.x=element_text(colour='#CCFF33'),
    axis.text.y=element_text(colour='#CCFF33',hjust=1),
    axis.title.x=element_text(colour='#CCFF33',face='bold'),
    axis.title.y=element_text(colour='#CCFF33',face='bold',angle=90)
)
qplot(cut,data=diamonds,geom='bar')+theme(panel.grid.major=element_blank())
#好丑好丑
qplot(cty,hwy,data=mpg)+theme(plot.title=element_text(size=20,face='bold',colour='green'),panel.grid.major=element_blank(),panel.grid.minor=element_blank())+labs(title='OK this is pretty ugly')
theme_set(old_theme)#这样就可以还原了！

#为保证新主题的整体连贯性可能需要调整一些标度和几何对象的默认设置
p<-qplot(mpg,wt,data=mtcars,colour=factor(cyl))
p
scale_color_discrete<-scale_colour_brewer
p

#update_geom_defualts()+update_stat_defaults()只改变设置后的图形
update_geom_defaults('point',aes(colour='darkblue'))
qplot(mpg,wt,data=mtcars)
update_stat_defaults('bin',aes(y=..density..))
qplot(mpg,data=mtcars,geom='histogram',binwidth=1)


#储存图形ggsave()
#width,height设置绝对尺寸大小，scale设置图形相对屏幕展示的尺寸大小，
#若空白则使用当前屏幕图形设备尺寸
#对于光栅图形dpi控制图形的分辨率，默认是300，可以修改为600（high）或者72（网页）
#如果想两幅图存在一个文件中
qplot(mpg,wt,data=mtcars)
pdf(file='output.pdf',width=6,height=6)
qplot(mpg,wt,data=mtcars)
qplot(wt,mpg,data=mtcars)
dev.off()
#代码要连续，中间不能间断，否则文件损坏

#半透明效果：用于展示置信区间，一般推荐使用png(600dpi)
#一页多图：grid的工作原理：视图窗口，显示设备的一个矩形子区域
#最简单的方式是创建图形并将图形赋成变量，再绘制出来
(a<-qplot(date,unemploy,data=economics,geom = 'line'))
(b<-qplot(date,unemploy,data=economics,geom = 'line'))
(c<-qplot(date,unemploy,data=economics,geom = 'line'))

#子图——将子图嵌入到主图的顶部是一种常见的图形布局。
#首先绘制主图，然后在更小的视图窗口绘制子图
#参数x,y,width,height控制视图窗口的大小和位置，默认单位'npc'0-1
#(0,0)左下角，(1,1)右上角，也可以使用unit(1,'cm')的绝对单位
library(grid)
vp1<-viewport(width=1,height=1,x=0.5,y=0.5)
vp1<-viewport()#都表示占据整个图形设备的视图窗口

asmall<-a+theme_gray(9)+labs(x=NULL,y=NULL)+theme(plot.margin=unit(rep(0,4),'lines'))
pdf('polishing-subplot-1.pdf',width=4,height=4)
subvp<-viewport(width=0.4,height=0.4,x=0.75,y=0.35)
b
print(asmall,vp=subvp)
dev.off()

#矩形网格：可以将3幅图放在一起
#一个更好的处理方式是grid.layout(),设置了一个任意高和宽的视图窗口网络
#需要一个个创建视图窗口，但是不用设置窗口位置和大小，只需设置布局的行数和列数
library(grid)
pdf('polishing-layout.pdf',width=8,height=6)
grid.newpage()#生成一个新页
pushViewport(viewport(layout=grid.layout(2,2)))
vplayout<-function(x,y)#图形占用网格位置设定函数
    viewport(layout.pos.row = x,layout.pos.col = y)

print(a,vp=vplayout(1,1:2))
print(b,vp=vplayout(2,1))
print(c,vp=vplayout(2,2))
dev.off()
#特别棒！一定要学会！
#Objects created by the viewport() function are only descriptions of a drawing context. 
#A viewport object must be 
#pushed onto the viewport tree before it has any effect on drawing.
#Viewports may be added to the tree using pushViewport() 
#and removed from the tree using popViewport().

#Chap9数据操作
#如何使用plyr包实现ggplot2内部的统计变换，如何用R对象作图，如何用ggplot2
#实现灵活的线性模型诊断
ddply()#能够同时在数据的多个子集上做统计汇总
#把复杂的数据分割成几个部分，分别对各个部分进行处理，
#最后把结果综合到一起**
#ddply根据行的取值，把数据框分解成结果子集，分别把各个子集输入某个函数
#最后把结果综合在一个数据框内
ddply(.data,.variables,.fun,...)#.data用来作图的数据
#.variables对数据取子集的分组变量.(var1,var2)包含所有分组变量和分面变量
#.fun是在各子集上运行的统计汇总函数，可以返回向量或数据框

#选取各个颜色里最小的钻石
library(plyr);library(ggplot2)
ddply(diamonds,.(color),subset,carat==min(carat))
#选取最小的两个钻石
ddply(diamonds,.(color),subset,order(carat)<=2)#order返回是排序i的在原数据集的第几个位置
#选出每组前1%的钻石
ddply(diamonds,.(color),subset,carat>quantile(carat,0.99))
#选出所有比组平均价值大的钻石
ddply(diamonds,.(color),subset,price>mean(price))

#transform函数——将几个时间序列数据调整到同一标度
#把每个颜色组里头的钻石的价格标准化，使均值为0，方差为1
p<-ddply(diamonds,.(color),transform,price=scale(price))#直接替换变量
head(p)

#colwise用来向量化一个普通函数
#把原本只接受向量输入的函数变成可以接受数据库输入
#返回一个新的函数而不是运行结果
#如nmissing()计算向量里缺失值的数目，用colwise()向量化
#尤其适用于那些自建的原本只适用于向量的函数
nmissing<-function(x)sum(is.na(x))
nmissing(msleep$name)
nmissing(msleep$brainwt)
nmissing_df<-colwise(nmissing)
nmissing_df(msleep)#哇这个简直太棒了！
#或者用
colwise(nmissing)(msleep)

#numcolwise设定只对数值类型的列操作
numcolwise(median)#只对每个数值类型的列计算中位数
#catcolwise()只对分类类型的列操作
numcolwise(median)(msleep,na.rm=T)

#!!以上这些函数与ddply一起可以对数据进行各种分组统计

#自建函数，计算秩相关系数和对数变换后的普通相关系数
my_summary<-function(df){
    with(df,data.frame(
        pc_cor=cor(price,carat,method='spearman'),
        lpc_cor=cor(log(price),log(carat))
    ))
}
ddply(diamonds,.(cut),my_summary)
#真是非常好

#下面介绍怎么使用plyr完成ggplot2的内置统计功能，某些情况下需要把统计结果
#储存或者用到其他数据集上，而且能够检查ggplot2的中间过程与我们的想法
#是否完全一致

#notably，在一张图中以每种颜色的钻石作为一组，分别画一条曲线很容易，
#但是想把价格和克拉两个变量作为两组，分别画一条曲线就很难
#所以我们需要把宽数据变成长数据——reshape--melt

#平行坐标图
#以variable变量为x轴表示变量名，以value为y轴表示变量取值，还需要一个分组
#变量把各个观测分组，每个观测分别对应一条线

#线性模型——ggplot2目前只有一种适合线性模型的fortify方法
#fortify()方法负责数据变形，ggplot2负责画图
#fortify()中的默认内置变量有.cooksd,.fitted,.hat,.resid,.sigma,.stdresid
mod<-lm(cty~displ,data=mpg)
basic<-ggplot(mod,aes(.fitted,.resid))+geom_hline(yintercept=0,colour='grey50',size=0.5)+geom_point()+geom_smooth(size=0.5,se=F)
basic
basic+aes(y=.stdresid)
basic+aes(size=.cooksd)+scale_size_area('Cook’s Distance')
#继续增补数据集，把原数据中没有放入回归模型的变量也添加进去，有助于
#发现那些变量能够改进模型
full<-basic%+% fortify(mod,mpg)#这个有待考虑
full+aes(colour=factor(cyl))
full+aes(displ,colour=factor(cyl))

#Chap10减少重复工作
last_plot()
#绘图模板
gradient_rb<-scale_colour_gradient(low='red',high='blue')
qplot(cty,hwy,data=mpg,colour=displ)+gradient_rb
#可以将ggplot2中的组建储存为list格式的列表，向某个图形中添加组建列表
#和将其中的组建按顺序逐个添加效果一样
xquiet<-scale_x_continuous('',breaks=NULL)
yquiet<-scale_y_continuous('',breaks=NULL)
quiet<-list(xquiet,yquiet)
qplot(mpg,wt,data=mtcars)+quiet

#在子健模板的过程中，要按照chap9的要求，将数据处理与绘图的命令分开！
#封装函数的一个很好的例子是qplot（），笔者强烈建议逐行认真阅读qplot（）
#函数的源代码，去理解它的工作机理
#统计图形的种类其实很多，困难的是如何用R语言去正确地构思并且实现
#基础图形中只有两种图是ggplot2不能代替的：filled.contour()和persp()
with(mpg,cdplot(displ,factor(cyl)))
qplot(displ,fill=factor(cyl),data=mpg,geom='density')
#注意设置fill=factor(cyl)
with(mpg,coplot(displ~cyl|a+b))
qplot(displ,cyl,facets=a~b)

#调色板
palette(rainbow(5))
plot(1:5,1:5,pch=19,cex=4)
qplot(1:5,1:5,col=factor(1:5),size=I(4))
last_plot()+scale_colour_manual(values=rainbow(5))
#连续色彩的调色板其中的色彩是由线性插值得到的
qplot(0:100,0:100,col=0:100,size=I(4))+scale_color_gradientn(colours=rainbow(7))
last_plot()+scale_color_gradientn(colours=terrain.colors(7))

#色盲友好型调色板
cb_col = c("#000000","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7")
scale_fill_manual(values=cb_col)
# 调用dichromat包

#一页多图
#视图窗口(viewport):显示设备的一个矩阵子区域。grid.layout()设置了一个
#任意高和宽的视图窗口布局。
library(grid)
pdf('myplot.pdf',width=9,height=9)
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2)))
vplayout=function(x,y){
    viewport(layout.pos.row=x, layout.pos.col=y)
}
print(p1,vp=vplayout(1,1))
print(p2,vp=vplayout(1,2))
dev.off()
# 默认的grid.layout()中，每个单元格的大小都相同，
# 可以设置widths和heights参数使得它们具有不同的大小。
