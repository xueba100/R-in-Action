# R in Action Chapter 4

# 4.1
##################################
# 代码清单4-1
##################################
manager <- c(1:5)
date <- c("10/24/08", "10/28/08", "10/01/08", "10/12/08", "05/01/09")
country <- c("US", "US", "UK", "UK", "UK")
gender <- c("M", "F", "F", "M", "F")
age <- c(32, 45, 25, 39, 99)
q1 <- c(5, 3, 3, 3, 2)
q2 <- c(4, 5, 5, 3, 2)
q3 <- c(5, 2, 5, 4, 1)
q4 <- c(5, 5, 5, NA, 2)
q5 <- c(5, 5, 2, NA, 1)
leadership0 <- data.frame(manager, date, country, gender, age, q1, q2, q3, q4, q5, stringsAsFactors = F)
##################################
# 代码清单4-1
##################################

# 4.2
##################################
# 代码清单4-2
##################################
mydata0 <- data.frame(x1 = c(2, 2, 6, 4), x2 = c(3, 4, 2, 8))
mydata <- mydata0
mydata

sumx <- x1 + x2 # 这里报错。因为x1不在ls()中，是mydata的一列。
sumx <- mydata$x1 + mydata$x2
meanx <- sumx /2
sumx
meanx # 这里算出的值不在dataframe中，是独立的变量。

# 把新的变量添加到dataframe里
ls()
rm(meanx, sumx)
# 法一
attach(mydata)
mydata$sumx <- x1 + x2 # attatch()避免了赋值号右侧的变量名过长，但是左边的变量名该有mydata$还是要有。不然就成为变量，与上面的方法无异。
mydata$meanx <- (x1 + x2)/2
detach(mydata)
sumx # 这里报错
mydata # 发现多了两列，正是sumx和meanx

# 法二
mydata <- mydata0
mydata
mydata <- transform(mydata, sumx = x1 + x2, meanx = (x1 + x2)/2)
mydata

# 4.3
# 值的替换，以age列为例
leadership <- leadership0
leadership$age[leadership$age == 99] <- NA # 将99岁的年龄编码为缺失值
                                           # 这里的模板是variable[condition] <- expression
                                           # 对满足condition = True的variable执行variable <- expression

# 将一个连续变量修改为一组类别值
# 基于分数线创建一个类别变量，以age为例
leadership$agecat[leadership$age > 75] <- 'Elder' # 根据age列产生新一列数据
leadership$agecat[leadership$age>=55 & leadership$age <=75] <- 'Middle'
leadership$agecat[leadership$age <= 55] <- 'Young'
leadership # 这里的agecat列NA有尖括号，别的NA没有，为什么？？？
class(leadership$agecat)
# 以上三行代码实现的功能还可以这样写
leadership$agecat <- NULL # 首先还原数据框
leadership <- within(data = leadership, {
                     agecat = NA
                     agecat[age > 75] <- 'Elder'
                     agecat[age <= 75 & age > 55] <- 'Middle'
                     agecat[age <= 55] <- 'Young'})# 注意这里先生成了agecat这列， 而且几个赋值语句之间没有逗号，本质上是用space分开的；赋值后直接显示了dataframe # expr这几个赋值语句要分行写或写在同一行并用分号隔开，否则报错
class(leadership$agecat)
# 比较一下with和within的输出
leadership <- with(data = leadership, 
                   expr = {extracol = NA; extracol <- c('a1', 'b2', 'c3', 'd4', 'e5') })
leadership # with函数的返回值是原语句的返回值。within跟with功能相同，但返回值不同，within会返回所有修改生效后的原始数据结构（列表、数据框等）
leadership <- leadership0
leadership$age[leadership$age == 99] <- NA
leadership$agecat <- NA
leadership$agecat[leadership$age > 75] <- 'Elder'
leadership$agecat[leadership$age <=75 & leadership$age > 55] <- 'Middle'
leadership$agecat[leadership$age <= 55] <- 'Young'
leadership
class(leadership$agecat)
leadership$agecat <- factor(leadership$agecat)
class(leadership$agecat)
leadership
# 使用cut函数，注意break给一个数字的时候它的边界会溢出0.1%，所以尽量不要这么用； 区间端点开闭用right=T(左开右闭)or right = F（左闭右开)调节。在目前这一具体例子里由于区间端点的开闭问题不适合用这个函数。但是仍然作一个演示。
agecat <- cut(leadership$age, breaks = c(0, 55, 75, Inf), labels = c('Young', 'Middle','Elder')) # 这里以向量给出的breaks参数给定了子区间端点；如果给出的是一个数字，则代表将最大/最小值之间的距离（稍有溢出）分成这些份
table(agecat)  # 把上一行的labels去掉还可以查看区间数值和区间内数据点的数量

# 4.4 
# 法一：使用{base}函数
names(leadership)
names(leadership)[2] <- "testDate"
names(leadership)
names(leadership)[6:10] <- c('item1', 'item2', 'item3', 'item4', 'item5')
leadership
# 法二：使用包函数 rename(dataframe, c(oldname = "newname", …)) 注意引号
leadership <- leadership0
library(plyr)
leadership <- rename(leadership, c(manager = 'managerID', date = 'testDate'))
leadership

# 4.5
# 缺失值用 NA 表示，空值用 NULL 表示
# NaN：无意义的数，比如sqrt(-2)， 0/0。
# Inf：正无穷大   -Inf：负无穷大
y <- c(1, 2, 3, NA)
is.na(y) # NA值处显示TRUE，其他显示F

##################################
# 代码清单4-3 使用is.na()函数
##################################

is.na(leadership[,6:10])
leadership[,6:10]
##################################
# 代码清单4-3 使用is.na()函数
##################################

# 4.5.1
#模板：variable[expr] <- NA

# 4.5.2
x <- c(1, 2, NA, 3)
y <- x[1] + x[2] + x[3] +x[4]
z <- sum(x)
cat('y', y, 'z', z)
y <- sum(x, na.rm = T)
y
##################################
# 代码清单4-4 使用na.omit()删除不完整的观测
##################################
# 还原数据
leadership
names(leadership)
names(leadership)[1:2] <- c("manager", 'date')
leadership$age[leadership$age == 99] <- NA
leadership
# 删除不完整的观测
newdata <- na.omit(leadership)
newdata

# 4.6
mydates <- as.Date((c("2007-06-22", "2004-02-13")), format = "%Y/%m/%d")
# 还原数据
leadership <- leadership0
leadership$age[leadership$age == 99] <- NA
# 修改日期格式
leadership$date <- as.Date(leadership$date, "%m/%d/%y") #格式要用双引号，斜线记得打上
leadership

# 当天日期
Sys.Date()
# 当前日期和时间
date()

# 输出指定格式的日期值，提取其中的某部分
today <- Sys.Date()
format(today, "%A/%d")
format(today, "%m %d, %A")

# 执行日期上的运算
# 法一
startdate <- as.Date("2004-02-13")
enddate <- as.Date("2011-01-22")
days <- enddate - startdate
days
# 法二
today <- Sys.Date()
dob <- as.Date("1993-10-12")
difftime(time1 = today, time2 = dob, units = 'day')

# 4.6.1
as.character(Sys.Date())
# 4.6.2
help("as.Date")
help("strftime")
help("ISOdatetime")
#可能有用的包： lubridate, timeDate等

# 4.7
# 模板： 
# 判断数据类型 is.datatype()
# 转换数据类型 as.datatype()
# 可用函数：
# is.numeric()       as.numeric() 
# is.character()     as.character() 
# is.vector()        as.vector() 
# is.matrix()        as.matrix() 
# is.data.frame()    as.data.frame() 
# is.factor()        as.factor() 
# is.logical()       as.logical()
##################################
# 代码清单4-5 转换数据类型
##################################

ls()
rm(dob, enddate, startdate, x, y, z, today, date, days, mydates)
ls()
a <- c(1, 2, 3)
is.numeric(a)
is.vector(a)
a <- as.character(a)
is.numeric(a)
is.vector(a)
is.character(a)
##################################
# 代码清单4-5 转换数据类型
##################################

# 4.8
ls()
newdata <- leadership[order(leadership$age), ] #按照age列升序排序；注意逗号
newdata
newdata <- leadership[order(leadership$gender, leadership$age),] # 先按性别从女到男排序，同性别按年龄升序排序
newdata
newdata <- leadership[order(leadership$gender, -leadership$age),] # 先性别，在年龄降序
newdata

# 4.9
# 4.9.1 向dataframe添加行/横向合并： merge(), cbind()
# 以某一列为共有变量进行联结，使用merge()
# 不需指定公共索引，使用cbind()也可。 cbind()联结的两个df应该行数相同，顺序相同
a <- data.frame(ID = c(1, 2, 3), name = c("AA", "BB", "CC"))
b <- data.frame(ID = c(2, 1, 3), name =c("bb", 'aa', 'cc'))
c1 <- merge(a, b, by = 'ID') # by 的参数值要有引号
c1
c2 <- cbind(a, b)
c2

#合并的两个df行数不同会怎样
rbind(a, a)
merge(a, rbind(a, a))
cbind(a, rbind(a, a))

# 4.9.2 向dataframe添加行/横向合并：rbind() 要求列名相同，但顺序不一定一样
ls()
a
b
rbind(a,b)

# 列名相同，顺序不同，可以合并
a1 <- data.frame(name = a$name, ID = a$ID)
a1
a
rbind(a, a1)

# 列名不同，不可合并
colnames(b) <- c('id', 'Name')
b
a
rbind(a,b)

# df列向量不同，如何处理
a
b
colnames(b) <- colnames(a)
b
a$age <- c(11, 12, 13)
a
rbind(a,b)
b$age <- NA # 缺少列变量的补齐为NA
b
rbind(a,b)
rm(a, b, a1)
# 4.10 数据集取子集
# 4.10.1 选入（保留）列
# 使用序号或列名
newdata <- leadership[, 6:10] # 用序号 dataframe[row indices, col indices]
newdata 

newdata <- NULL
newdata <- leadership[c('q1', 'q2', 'q3', 'q4', 'q5')] # 使用列名 dataframe[vector_of_colnames] 像这样形式上有规律的元素可以通过paste('q',1:5, sep='')产生
newdata

# 用逻辑值
myvars <- c(TRUE, TRUE, TRUE, TRUE, FALSE) # 只要第一到第四列
newdata<- newdata[myvars]
newdata
# 4.10.2 剔除列
# 法一 指定数据赋值为NULL
# 使用列名或列序号删除
newdata <- leadership[c('q1', 'q2', 'q3', 'q4', 'q5')]
newdata$newcol <- c('a', 'b', 'c', 'd', 'e') # 目标是删除这一列
newdata
newdata$newcol <- NULL
newdata
newdata$q6 <- c(1, 2, 3, 4, 5)
newdata
newdata[,6] <- NULL
newdata

# 法二 用逻辑值 
# 以删除q3 q4为例
newdata <- leadership[c('q1', 'q2', 'q3', 'q4', 'q5')]
myvars <- names(newdata) %in% c('q3', 'q4')
myvars
newdata <- newdata[!myvars] # !myvars 是反选
newdata

# 4.10.3 选入行
##################################
# 代码清单4-6 选入观测
##################################

# 法一 根据行序号选择
leadership <- leadership0
newdata <- leadership[1:3, ]
newdata
# 法二 按照一定标准筛选观测值符合条件的行
newdata <- leadership[leadership$gender == 'M' & leadership$age >30]
newdata
##################################
# 代码清单4-6 选入观测
##################################

# 选取日期符合条件的行
leadership$date <- as.Date(leadership$date, format = "%m/%d/%y")
startdate <- as.Date("2009-01-01")
enddate <- as.Date("2009-10-31")
newdata <- leadership[which(leadership$date >= startdate & leadership$date<= enddate), ]
newdata

# 4.10.4 subset()
leadership <- leadership0
leadership
newdata <- subset(x = leadership, # 子集所在的object
                  subset = age >= 35 | age < 24, #行的筛选条件，表达式的值应该是logical类型 NA会直接被移除
                  select = c(q1, q2, q3, q4)) # 列的筛选条件 负号表示不要
newdata

# 注意，age = NA的行会被直接移除
leadership$age[leadership$age == 99] <- NA
leadership
newdata <- subset(x = leadership, 
                  subset = age >= 35 | age < 24, 
                  select = c(q1, q2, q3, q4)) 
newdata

# 反选列
newdata <- subset(x = leadership, 
                  subset = age >= 35 | age < 24, 
                  select = -c(country, gender)) # 负号表示不要
newdata

# 4.10.5
leadership <- leadership0
myexample <- leadership[sample(1:nrow(leadership),  #数据行数
                               3,  # 抽取几个
                               replace = F)] # 无放回抽样 
myexample
