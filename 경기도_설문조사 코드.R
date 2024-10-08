#pakages
install.packages("ggplot2")
install.packages("gt")
install.packages("dplyr")
install.packages("formattable")
install.packages("readxl")
devtools::install_github("renkun-ken/formattable")

library(formattable)
library(dplyr)
library(gt)
library(ggplot2)
library(readxl)

#데이터 불러오기
data1 <- read_excel("C:/Users/syj90/OneDrive/바탕 화면/경기도_설문조사 완료_.xlsx")

#응답자 기본사항_나이_plot
x = data1$나이
age<-ifelse(x<=29,1,
       ifelse(x<=39,2,
              ifelse(x<=49,3,4)))
name<-c("20세이상 29세이하","30세이상 39세이하","40세이상 49세이하","50세이상 59세이하") 
tmp_value = age%>% unique()
non_value = c(1:4)[!(1:4 %in% tmp_value)]
count_vec = c(table(age))
names(count_vec)[!(1:4 %in% tmp_value)] = non_value

count_dat = data.frame(count = count_vec, tmp_name = names(count_vec), name = name)
count_dat = count_dat %>% mutate(per = round(count/sum(count)*100, 2), 
                                 누적퍼센트 = base::cumsum(per))
plot01 <- ggplot(count_dat, aes(x = tmp_name)) + 
  geom_bar(fill="#102b6a", aes(y = count), width=0.7, stat = "identity") + 
  labs(x="",y="")+
  theme_bw()+
  theme(text=element_text(size=15),axis.text.x = element_text(size = 13)) + 
  scale_x_discrete(labels = count_dat$name)
plot01
#응답자 기본사항_나이_table
values<-1:5
a[grepl("NA", a)] <- 5
a <- ifelse(is.na(a), 5 , as.numeric(a))
result1 <- table(factor(a, levels = values))
name<-c("20세이상 29세이하","30세이상 39세이하","40세이상 49세이하","50세이상 59세이하","결측")
df1 <- data.frame(result1)
count_dat1 = data.frame(count = result1, tmp_name = names(result1), name = name)
count_dat1 = count_dat1 %>% mutate(per = round(count.Freq/sum(count.Freq)*100, 2), 
                                   누적퍼센트 = base::cumsum(per))
formattable(count_dat1 %>% rename(퍼센트 = per, 빈도 = count.Freq, 항목 = name) %>% select(항목, 빈도, 퍼센트, 누적퍼센트), 
            list(빈도 = color_bar("#BDBDBD"),퍼센트 = color_bar("#7D9C36"),항목=formatter("span",style=~style(font.weight="bold","font-size"="17px")),
                 누적퍼센트 = formatter("span", x ~ formatC(x, format = "f", digits = 1), style = ~ style(font.weight = "bold","font-size"="17px"))))

#응답자 기본사항_담당 업무_plot
data1 <- read_excel("C:/Users/syj90/OneDrive/바탕 화면/경기도_설문조사 완료_.xlsx")
View(data1)
x=data1$담당업무
split_data <- strsplit(x, ",")
df<-data.frame(matrix(unlist(split_data),ncol=1))
colnames(df)<-c("value")
print(df)
table(df)
name<-c("중앙지원 생활SOC 사업","지자체 추진 생활SOC 사업","신도시사업 추진 중","기타")

tmp_value = df %>% unique()

non_value = c(1:4)[!(1:4 %in% tmp_value)]

count_vec = c(table(df))
names(count_vec)[!(1:4 %in% tmp_value)] = non_value

count_dat = data.frame(count = count_vec, tmp_name = names(count_vec), name = name)
count_dat = count_dat %>% mutate(per = round(count/sum(count)*100, 2), 
                                 누적퍼센트 = base::cumsum(per))
plot02 <- ggplot(count_dat, aes(x = tmp_name)) + 
  geom_bar(fill="#102b6a", aes(y = count), width=0.7, stat = "identity") + 
  labs(x="",y="")+
  theme_bw()+
  theme(text=element_text(size=15),axis.text.x = element_text(size = 9)) + 
  scale_x_discrete(labels = count_dat$name)
plot02

#응답자 기본사항_담당 업무_table
newdf<-lapply(df,function(df) ifelse(is.na(df),5,df))
newdf <- lapply(newdf, as.numeric)
newdf2 <- data.frame(newdf)
values <- 1:5
result <- table(factor(newdf2$value, levels = values))
result

name<-c("중앙지원 생활SOC 사업","지자체 추진 생활SOC 사업","신도시사업 추진 중","기타","결측")

df1 <- data.frame(result)
count_dat1 = data.frame(count = result, tmp_name = names(result), name = name)
count_dat1 = count_dat1 %>% mutate(per = round(count.Freq/sum(count.Freq)*100, 2), 
                                   누적퍼센트 = base::cumsum(per))
formattable(count_dat1 %>% rename(퍼센트 = per, 빈도 = count.Freq, 항목 = name) %>% select(항목, 빈도, 퍼센트, 누적퍼센트), 
            list(빈도 = color_bar("#BDBDBD"),퍼센트 = color_bar("#7D9C36"),항목=formatter("span",style=~style(font.weight="bold","font-size"="17px")),
                 누적퍼센트 = formatter("span", x ~ formatC(x, format = "f", digits = 1), style = ~ style(font.weight = "bold","font-size"="17px"))))


#응답자 기본사항_담당지역_plot
data <- read_excel("C:/Users/syj90/OneDrive/바탕 화면/경기도_설문조사 완료_.xlsx",sheet="Sheet2")
data$Q300[is.na(data$Q300)] <- 0
name<-c("가평군","고양시","과천시","광명시","광주시", "구리시","군포시", "김포시", "남양주시", "동두천시","부천시", "성남시",
        "수원시", "시흥시", "안산시", "안성시", "안양시","양주시", "양평군", "여주시","연천군","오산시","용인시", "의왕시",
        "의정부시","이천시", "파주시",  "평택시", "포천시", "하남시", "화성시")
values <- 1:31
result1 <- table(factor(data[data$Q300!= 0,]$Q300, levels = values))
df1 <- data.frame(result1)
plot03 <- ggplot(df1, aes(x = Var1, y = Freq)) + 
  geom_bar(fill="#102b6a", aes(y = Freq), width=0.7, stat = "identity") + 
  labs(x="",y="")+
  theme_bw()+
  theme(text=element_text(size=15),axis.text.x = element_text(size = 11,angle=90,vjust=0.3)) + 
  scale_x_discrete(labels = name)
print(plot03)
#응답자 기본사항_담당지역_table
options(digits = 2)
values <- 1:32
result1 <- table(factor(data$Q300, levels = values))
df1 <- data.frame(result1)
perq1 <- round((table(factor(data[data$Q300!= 0,]$Q300, levels = values)) / sum(df1$Freq)) * 100,2)
cuperq1 <- cumsum(perq1)
cuperq1 
name1<-c("가평군","고양시","과천시","광명시","광주시", "구리시","군포시", "김포시", "남양주시", "동두천시","부천시", "성남시",
         "수원시", "시흥시", "안산시", "안성시", "안양시","양주시", "양평군", "여주시","연천군","오산시","용인시", "의왕시",
         "의정부시","이천시", "파주시",  "평택시", "포천시", "하남시", "화성시","결측")
df1.1<-cbind(name1,df1)
df1.1<-cbind(df1.1,perq1)
df1.1<-cbind(df1.1,cuperq1)
df1.1 <- df1.1[,c(-2,-4)]
rownames(df1.1) = NULL
name2<- c("항목","빈도", "퍼센트", "누적퍼센트")
colnames(df1.1) <- name2

formattable(df1.1, list(빈도 = color_bar("#BDBDBD"), 퍼센트 = color_bar("#7D9C36"),항목=formatter("span",style=~style(font.weight="bold","font-size"="17px")),
                        누적퍼센트 = formatter("span", x ~ formatC(x, format = "f", digits = 1), style = ~ style(font.weight = "bold","font-size"="17px"))))


#Q1_plot
name<- c("매우 적절하다", "비교적 적절하다", "보통이다", "적절하지 않다", "매우 적절치 않다")

require(dplyr)
x = data1$Q1
x
tmp_value = x %>% unique()

non_value = c(1:5)[!(1:5 %in% tmp_value)]

count_vec = c(0, table(x))
names(count_vec)[!(1:5 %in% tmp_value)] = non_value

count_dat = data.frame(count = count_vec, tmp_name = names(count_vec), name = name)
count_dat = count_dat %>% mutate(per = round(count/sum(count)*100, 2), 
                                 누적퍼센트 = base::cumsum(per))
plot1 <- ggplot(count_dat, aes(x = tmp_name)) + 
  geom_bar(fill="#102b6a", aes(y = count), width=0.7, stat = "identity") + 
  labs(x="",y="")+
  theme_bw()+
  theme(text=element_text(size=15),axis.text.x = element_text(size = 13)) + 
  scale_x_discrete(labels = count_dat$name)
plot1
#Q1_table
values <- 1:6

result1 <- table(factor(x, levels = values))
#esult1 <- table(factor(data[data$Q1!= 0,]$Q1, levels = values))
name<- c("매우 적절하다", "비교적 적절하다", "보통이다", "적절하지 않다", "매우 적절치 않다", "결측")
df1 <- data.frame(result1)
count_dat1 = data.frame(count = result1, tmp_name = names(result1), name = name)
count_dat1 = count_dat1 %>% mutate(per = round(count.Freq/sum(count.Freq)*100, 2), 
                                 누적퍼센트 = base::cumsum(per))

require(formattable)
formattable(count_dat1 %>% rename(퍼센트 = per, 빈도 = count.Freq, 항목 = name) %>% select(항목, 빈도, 퍼센트, 누적퍼센트), 
            list(빈도 = color_bar("#BDBDBD"),퍼센트 = color_bar("#7D9C36"),항목=formatter("span",style=~style(font.weight="bold","font-size"="17px")),
                누적퍼센트 = formatter("span", x ~ formatC(x, format = "f", digits = 1), style = ~ style(font.weight = "bold","font-size"="17px"))))


#Q2_plot
name<- c("매우 그렇다", "약간 그렇다", "보통이다", "그렇지 않다", "매우 그렇지 않다")

require(dplyr)
x = data1$Q2
x
tmp_value = x %>% unique()

non_value = c(1:5)[!(1:5 %in% tmp_value)]

count_vec = c(table(x),0)
names(count_vec)[!(1:5 %in% tmp_value)] = non_value

count_dat = data.frame(count = count_vec, tmp_name = names(count_vec), name = name)
count_dat = count_dat %>% mutate(per = round(count/sum(count)*100, 2), 
                                 누적퍼센트 = base::cumsum(per))
plot2 <- ggplot(count_dat, aes(x = tmp_name)) + 
  geom_bar(fill="#102b6a", aes(y = count), width=0.7, stat = "identity") + 
  labs(x="",y="")+
  theme_bw()+
  theme(text=element_text(size=15),axis.text.x = element_text(size = 13)) + 
  scale_x_discrete(labels = count_dat$name)
plot2
#Q2_table
values <- 1:6

result1 <- table(factor(x, levels = values))
name<- c("매우 그렇다", "약간 그렇다", "보통이다", "그렇지 않다", "매우 그렇지 않다","결측")
df1 <- data.frame(result1)
count_dat1 = data.frame(count = result1, tmp_name = names(result1), name = name)
count_dat1 = count_dat1 %>% mutate(per = round(count.Freq/sum(count.Freq)*100, 2), 
                                   누적퍼센트 = base::cumsum(per))
require(formattable)
formattable(count_dat1 %>% rename(퍼센트 = per, 빈도 = count.Freq, 항목 = name) %>% select(항목, 빈도, 퍼센트, 누적퍼센트), 
            list(빈도 = color_bar("#BDBDBD"),퍼센트 = color_bar("#7D9C36"),항목=formatter("span",style=~style(font.weight="bold","font-size"="17px")),
                 누적퍼센트 = formatter("span", x ~ formatC(x, format = "f", digits = 1), style = ~ style(font.weight = "bold","font-size"="17px"))))


#Q3_plot
name<- c("매우 그렇다", "약간 그렇다", "보통이다", "그렇지 않다", "매우 그렇지 않다")

require(dplyr)
x = data1$Q3
x
tmp_value = x %>% unique()

non_value = c(1:5)[!(1:5 %in% tmp_value)]

count_vec = c(table(x))
names(count_vec)[!(1:5 %in% tmp_value)] = non_value

count_dat = data.frame(count = count_vec, tmp_name = names(count_vec), name = name)
count_dat = count_dat %>% mutate(per = round(count/sum(count)*100, 2), 
                                 누적퍼센트 = base::cumsum(per))
plot3 <- ggplot(count_dat, aes(x = tmp_name)) + 
  geom_bar(fill="#102b6a", aes(y = count), width=0.7, stat = "identity") + 
  labs(x="",y="")+
  theme_bw()+
  theme(text=element_text(size=15),axis.text.x = element_text(size = 13)) + 
  scale_x_discrete(labels = count_dat$name)
plot3
#Q3_table
values <- 1:6

result1 <- table(factor(x, levels = values))
name<- c("매우 그렇다", "약간 그렇다", "보통이다", "그렇지 않다", "매우 그렇지 않다","결측")
df1 <- data.frame(result1)
count_dat1 = data.frame(count = result1, tmp_name = names(result1), name = name)
count_dat1 = count_dat1 %>% mutate(per = round(count.Freq/sum(count.Freq)*100, 2), 
                                   누적퍼센트 = base::cumsum(per))
formattable(count_dat1 %>% rename(퍼센트 = per, 빈도 = count.Freq, 항목 = name) %>% select(항목, 빈도, 퍼센트, 누적퍼센트), 
            list(빈도 = color_bar("#BDBDBD"),퍼센트 = color_bar("#7D9C36"),항목=formatter("span",style=~style(font.weight="bold","font-size"="17px")),
                 누적퍼센트 = formatter("span", x ~ formatC(x, format = "f", digits = 1), style = ~ style(font.weight = "bold","font-size"="17px"))))


#Q4_plot
name<- c("매우 그렇다", "약간 그렇다", "보통이다", "그렇지 않다", "매우 그렇지 않다")

x = data1$Q4
tmp_value = x %>% unique()

non_value = c(1:5)[!(1:5 %in% tmp_value)]

count_vec = c(0,table(x))
names(count_vec)[!(1:5 %in% tmp_value)] = non_value

count_dat = data.frame(count = count_vec, tmp_name = names(count_vec), name = name)
count_dat = count_dat %>% mutate(per = round(count/sum(count)*100, 2), 
                                 누적퍼센트 = base::cumsum(per))
plot4 <- ggplot(count_dat, aes(x = tmp_name)) + 
  geom_bar(fill="#102b6a", aes(y = count), width=0.7, stat = "identity") + 
  labs(x="",y="")+
  theme_bw()+
  theme(text=element_text(size=15),axis.text.x = element_text(size = 13)) + 
  scale_x_discrete(labels = count_dat$name)
plot4
#Q4_table
values <- 1:6

result1 <- table(factor(x, levels = values))
name<- c("매우 그렇다", "약간 그렇다", "보통이다", "그렇지 않다", "매우 그렇지 않다","결측")
df1 <- data.frame(result1)
count_dat1 = data.frame(count = result1, tmp_name = names(result1), name = name)
count_dat1 = count_dat1 %>% mutate(per = round(count.Freq/sum(count.Freq)*100, 2), 
                                   누적퍼센트 = base::cumsum(per))
formattable(count_dat1 %>% rename(퍼센트 = per, 빈도 = count.Freq, 항목 = name) %>% select(항목, 빈도, 퍼센트, 누적퍼센트), 
            list(빈도 = color_bar("#BDBDBD"),퍼센트 = color_bar("#7D9C36"),항목=formatter("span",style=~style(font.weight="bold","font-size"="17px")),
                 누적퍼센트 = formatter("span", x ~ formatC(x, format = "f", digits = 1), style = ~ style(font.weight = "bold","font-size"="17px"))))


#Q5_plot
name<- c("지역 내 다양성 확보"," 지역 주민수요","접근성이 좋은 입지","질 좋은 \n 프로그램 도입","수익성(운영비)\n 확보","예산 확보의 용이성","기타")
x = data1$Q5
x[grepl(",", x)] <- NA
tmp_value = x %>% unique()

non_value = c(1:7)[!(1:7 %in% tmp_value)]

count_vec = c(table(x),0)
names(count_vec)[!(1:7 %in% tmp_value)] = non_value

count_dat = data.frame(count = count_vec, tmp_name = names(count_vec), name = name)
count_dat = count_dat %>% mutate(per = round(count/sum(count)*100, 2), 
                                 누적퍼센트 = base::cumsum(per))
plot5 <- ggplot(count_dat, aes(x = tmp_name)) + 
  geom_bar(fill="#102b6a", aes(y = count), width=0.7, stat = "identity") + 
  labs(x="",y="")+
  theme_bw()+
  theme(text=element_text(size=15),axis.text.x = element_text(size = 13)) + 
  scale_x_discrete(labels = count_dat$name)
plot5
#Q5_table 
values<-1:8
x[grepl(",", x)] <- NA
x <- ifelse(is.na(x), 8, as.numeric(x))
result1 <- table(factor(x, levels = values))
name<- c("지역 내 다양성 확보", " 지역 주민수요","접근성이 좋은 입지","질 좋은 프로그램 도입","수익성(운영비) 확보","예산 확보의 용이성","기타","결측")
df1 <- data.frame(result1)
count_dat1 = data.frame(count = result1, tmp_name = names(result1), name = name)
count_dat1 = count_dat1 %>% mutate(per = round(count.Freq/sum(count.Freq)*100, 2), 
                                   누적퍼센트 = base::cumsum(per))
formattable(count_dat1 %>% rename(퍼센트 = per, 빈도 = count.Freq, 항목 = name) %>% select(항목, 빈도, 퍼센트, 누적퍼센트), 
            list(빈도 = color_bar("#BDBDBD"),퍼센트 = color_bar("#7D9C36"),항목=formatter("span",style=~style(font.weight="bold","font-size"="16px")),
                 누적퍼센트 = formatter("span", x ~ formatC(x, format = "f", digits = 1), style = ~ style(font.weight = "bold","font-size"="16px"))))


#Q6_plot
name<-c("지역 수요와\n 맞지 않음","유사시설 중복공급","대도시 중심\n 설치","인구구조를 고려하지\n 않은 시설배치","지역 주민과의\n 의견 충돌", "접근성(인프라)부족","기타")

x = data1$Q6
x[grepl(",", x)] <- NA
tmp_value = x %>% unique()

non_value = c(1:7)[!(1:7 %in% tmp_value)]

count_vec = c(table(x),0)
names(count_vec)[!(1:7 %in% tmp_value)] = non_value

count_dat = data.frame(count = count_vec, tmp_name = names(count_vec), name = name)
count_dat = count_dat %>% mutate(per = round(count/sum(count)*100, 2), 
                                 누적퍼센트 = base::cumsum(per))
plot6 <- ggplot(count_dat, aes(x = tmp_name)) + 
  geom_bar(fill="#102b6a", aes(y = count), width=0.7, stat = "identity") + 
  labs(x="",y="")+
  theme_bw()+
  theme(text=element_text(size=15),axis.text.x = element_text(size = 9)) + 
  scale_x_discrete(labels = count_dat$name)
plot6
#Q6_table
values<-1:8
x[grepl(",", x)] <- 7
x[grepl("NA", x)] <- 8
x <- ifelse(is.na(x), 8 , as.numeric(x))

result1 <- table(factor(x, levels = values))
name<-c("지역 수요와 맞지 않음","유사시설 중복공급","대도시 중심 설치","인구구조를 고려하지 않은 시설배치","지역 주민과의 의견 충돌", "접근성(인프라)부족","기타","결측")
df1 <- data.frame(result1)
count_dat1 = data.frame(count = result1, tmp_name = names(result1), name = name)
count_dat1 = count_dat1 %>% mutate(per = round(count.Freq/sum(count.Freq)*100, 2), 
                                   누적퍼센트 = base::cumsum(per))
formattable(count_dat1 %>% rename(퍼센트 = per, 빈도 = count.Freq, 항목 = name) %>% select(항목, 빈도, 퍼센트, 누적퍼센트), 
            list(빈도 = color_bar("#BDBDBD"),퍼센트 = color_bar("#7D9C36"),항목=formatter("span",style=~style(font.weight="bold","font-size"="17px")),
                 누적퍼센트 = formatter("span", x ~ formatC(x, format = "f", digits = 1), style = ~ style(font.weight = "bold","font-size"="17px"))))


#Q7_plot 
name<-c("지역 불균형\n공급","지역 주민과의\n의견 충돌","접근성(인프라)\n 부족","가이드라인\n 미흡","사업추진 속도의\n미비함","경제적\n 타당성 검토","계획수립시 전문가\n확보의 어려움","지역 특성\n미반영","예산 확보","기타")

x = data1$Q71
x[grepl(",", x)] <- NA
tmp_value = x %>% unique()

non_value = c(0:9)[!(0:9 %in% tmp_value)]

count_vec = table(factor(x , levels = c(0:9)))

names(count_vec)[!(0:9 %in% tmp_value)] = non_value

count_dat = data.frame(count = count_vec, tmp_name = names(count_vec) , name = name)
count_dat = count_dat %>% mutate(per = round(count.Freq/sum(count.Freq)*100, 2), 
                                 누적퍼센트 = base::cumsum(per))

plot7 <- ggplot(count_dat, aes(x = tmp_name)) + 
  geom_bar(fill="#102b6a", aes(y = count.Freq), width=0.7, stat = "identity") + 
  labs(x="",y="")+
  theme_bw()+
  theme(text=element_text(size=15),axis.text.x = element_text(size = 9)) + 
  scale_x_discrete(labels = count_dat$name)
plot7
#Q7_table
values<-0:10
x[grepl(",", x)] <- 10
x[grepl("NA", x)] <- 10
x <- ifelse(is.na(x), 10 , as.numeric(x))

result1 <- table(factor(x, levels = values))
name<-c("지역 불균형 공급","지역 주민과의 의견 충돌","접근성(인프라) 부족","가이드라인 미흡","사업추진 속도의 미비함","경제적 타당성 검토","계획수립시 전문가 확보의 어려움","지역 특성 미반영","예산 확보","기타","결측")
df1 <- data.frame(result1)
count_dat1 = data.frame(count = result1, tmp_name = names(result1), name = name)
count_dat1 = count_dat1 %>% mutate(per = round(count.Freq/sum(count.Freq)*100, 2), 
                                   누적퍼센트 = base::cumsum(per))
formattable(count_dat1 %>% rename(퍼센트 = per, 빈도 = count.Freq, 항목 = name) %>% select(항목, 빈도, 퍼센트, 누적퍼센트), 
            list(빈도 = color_bar("#BDBDBD"),퍼센트 = color_bar("#7D9C36"),항목=formatter("span",style=~style(font.weight="bold","font-size"="17px")),
                 누적퍼센트 = formatter("span", x ~ formatC(x, format = "f", digits = 1), style = ~ style(font.weight = "bold","font-size"="17px"))))


#Q8_plot
data1 <- read_excel("C:/Users/syj90/OneDrive/바탕 화면/경기도_설문조사 완료_.xlsx")
x=data1$Q8
split_data <- strsplit(x, ",")
df<-data.frame(matrix(unlist(split_data),ncol=1))
colnames(df)<-c("value")
print(df)
table(df)

name<-c("의료형 생활SOC\n(보건소, 약국,\n치매요양, 장애인시설 등)","교육,학습형 생활SOC\n(공공유치원, 지역아동센터,\n도서관 등)","(고령자)\n복지형 생활SOC\n(노인센터, 경로당 등)","(아동, 청소년)\n복지형 생활SOC\n(어린이집, 돌봄센터 등)","문화, 체육형 생활SOC\n(박물관, 공연장,\n체육시설 등)","생활편의형 생활SOC\n(택배함, 도시공원,\n주차장 등)")

tmp_value = df %>% unique()

non_value = c(1:6)[!(1:6 %in% tmp_value)]

count_vec = c(table(df))
names(count_vec)[!(1:6 %in% tmp_value)] = non_value

count_dat = data.frame(count = count_vec, tmp_name = names(count_vec), name = name)
count_dat = count_dat %>% mutate(per = round(count/sum(count)*100, 2), 
                                 누적퍼센트 = base::cumsum(per))
plot8 <- ggplot(count_dat, aes(x = tmp_name)) + 
  geom_bar(fill="#102b6a", aes(y = count), width=0.7, stat = "identity") + 
  labs(x="",y="")+
  theme_bw()+
  theme(text=element_text(size=15),axis.text.x = element_text(size = 9)) + 
  scale_x_discrete(labels = count_dat$name)
plot8
#Q8_table
values<-1:7
newdf<-lapply(df, function(df) ifelse(is.na(df), 7, df))
res<-table(newdf)
name<-c("의료형 생활SOC(보건소, 약국, 치매요양, 장애인시설 등)","교육,학습형 생활SOC(공공유치원, 지역아동센터, 도서관 등)","(고령자) 복지형 생활SOC(노인센터, 경로당 등)","(아동, 청소년) 복지형 생활SOC(어린이집, 돌봄센터 등)","문화, 체육형 생활SOC(박물관, 공연장, 체육시설 등)","생활편의형 생활SOC(택배함, 도시공원, 주차장 등)","결측")

df1 <- data.frame(newdf)
count_dat1 = data.frame(count = res, tmp_name = names(result1), name = name)
count_dat1 = count_dat1 %>% mutate(per = round(count.Freq/sum(count.Freq)*100, 2), 
                                   누적퍼센트 = base::cumsum(per))
formattable(count_dat1 %>% rename(퍼센트 = per, 빈도 = count.Freq, 항목 = name) %>% select(항목, 빈도, 퍼센트, 누적퍼센트), 
            list(빈도 = color_bar("#BDBDBD"),퍼센트 = color_bar("#7D9C36"),항목=formatter("span",style=~style(font.weight="bold","font-size"="17px")),
                 누적퍼센트 = formatter("span", x ~ formatC(x, format = "f", digits = 1), style = ~ style(font.weight = "bold","font-size"="17px"))))



# Q9_plot
data <- read_excel("C:/Users/syj90/OneDrive/바탕 화면/경기도_설문조사 완료_.xlsx")
data$Q91 <- ifelse(is.na(data$Q91), 0 , data$Q91)
data$Q92 <- ifelse(is.na(data$Q92), 0 , data$Q92)
data$Q93 <- ifelse(is.na(data$Q93), 0 , data$Q93)
data$Q94 <- ifelse(is.na(data$Q94), 0 , data$Q94)
data$Q95 <- ifelse(is.na(data$Q95), 0 , data$Q95)
data$Q96 <- ifelse(is.na(data$Q96), 0 , data$Q96)

values <- c(0:6)
q91t <- table(factor(data$Q91, levels = values))
q92t <- table(factor(data$Q92, levels = values))
q93t <- table(factor(data$Q93, levels = values))
q94t <- table(factor(data$Q94, levels = values))
q95t <- table(factor(data$Q95, levels = values))
q96t <- table(factor(data$Q96, levels = values))

df91 <- data.frame(q91t)
df92 <- data.frame(q92t)
df93 <- data.frame(q93t)
df94 <- data.frame(q94t)
df95 <- data.frame(q95t)
df96 <- data.frame(q96t)

df91['score1'] <- df91$Freq * 6
df92['score2'] <- df92$Freq * 5
df93['score3'] <- df93$Freq * 4
df94['score4'] <- df94$Freq * 3

df95['score5'] <- df95$Freq * 2
df96['score6'] <- df96$Freq * 1

df9 <- data.frame(c(df91,df92,df93,df94,df95,df96))
df9 <- df9[,c(1,2,3,6,9,12,15,18)]
df9 <- df9[-1,]
df9['sumscore'] <- df9$score1 + df9$score2 + df9$score3 + df9$score4 + df9$score5 + df9$score6

name <- c("계획검토\n전문가 파견","운영관리 컨설팅","재정지원","인허가 절차\n간소화","설립타당성\n검토 컨설팅","설계검토\n(공공건축가)")
plot9 <- ggplot(df9, aes(x = Var1, y = sumscore)) + 
  geom_bar(fill="#102b6a", aes(y = sumscore), width=0.7, stat = "identity") + 
  labs(x="",y="")+
  theme_bw()+
  theme(text=element_text(size=15),axis.text.x = element_text(size = 13)) +
  scale_x_discrete(labels = name)
print(plot9)
#Q9_table
name_r <- c("계획검토 전문가 파견","운영관리 컨설팅","재정지원","인허가 절차 간소화","설립타당성 검토 컨설팅","설계검토(공공건축가)","결측")
name_c <- c("항목","점수")

df91 <- data.frame(q91t)
df92 <- data.frame(q92t)
df93 <- data.frame(q93t)
df94 <- data.frame(q94t)
df95 <- data.frame(q95t)
df96 <- data.frame(q96t)

df91['score1'] <- df91$Freq * 6
df92['score2'] <- df92$Freq * 5
df93['score3'] <- df93$Freq * 4
df94['score4'] <- df94$Freq * 3
df95['score5'] <- df95$Freq * 2
df96['score6'] <- df96$Freq * 1

df9 <- data.frame(c(df91,df92,df93,df94,df95,df96))
df9['sumscore'] <- df9$score1 + df9$score2 + df9$score3 + df9$score4 + df9$score5 + df9$score6
df9t <- df9[,c(1,19)]

df9t <- rbind(df9t, df9t[1,])
df9t <- df9t[-1,]

df9t[7,2] <- 0
df9t[,1] <- name_r

rownames(df9t) <- NULL
colnames(df9t) <- name_c
formattable(df9t, list(점수 = color_bar("#7D9C36"),
                       항목=formatter("span",style=~style(font.weight="bold","font-size"="17px"))))


#Q11_plot
name<-c("지자체 공무원","지역 내 협의체","관련분야 공공기관","재단조성","전문민간 위탁관리","기타")

x = data1$Q11
x[grepl(",", x)] <- NA
tmp_value = x %>% unique()

non_value = c(1:6)[!(1:6 %in% tmp_value)]

count_vec = c(table(x),0)
names(count_vec)[!(1:6 %in% tmp_value)] = non_value

count_dat = data.frame(count = count_vec, tmp_name = names(count_vec), name = name)
count_dat = count_dat %>% mutate(per = round(count/sum(count)*100, 2), 
                                 누적퍼센트 = base::cumsum(per))
plot11 <- ggplot(count_dat, aes(x = tmp_name)) + 
  geom_bar(fill="#102b6a", aes(y = count), width=0.7, stat = "identity") + 
  labs(x="",y="")+
  theme_bw()+
  theme(text=element_text(size=15),axis.text.x = element_text(size = 9)) + 
  scale_x_discrete(labels = count_dat$name)
plot11
#Q11_table 
values<-1:7
x[grepl(",", x)] <-  7
x[grepl("NA", x)] <- 7
x <- ifelse(is.na(x), 7 , as.numeric(x))

result1 <- table(factor(x, levels = values))
name<-c("지자체 공무원","지역 내 협의체","관련분야 공공기관","재단조성","전문민간 위탁관리","기타","결측")

df1 <- data.frame(result1)
count_dat1 = data.frame(count = result1, tmp_name = names(result1), name = name)
count_dat1 = count_dat1 %>% mutate(per = round(count.Freq/sum(count.Freq)*100, 2), 
                                   누적퍼센트 = base::cumsum(per))
formattable(count_dat1 %>% rename(퍼센트 = per, 빈도 = count.Freq, 항목 = name) %>% select(항목, 빈도, 퍼센트, 누적퍼센트), 
            list(빈도 = color_bar("#BDBDBD"),퍼센트 = color_bar("#7D9C36"),항목=formatter("span",style=~style(font.weight="bold","font-size"="17px")),
                 누적퍼센트 = formatter("span", x ~ formatC(x, format = "f", digits = 1), style = ~ style(font.weight = "bold","font-size"="17px"))))



#Q12_plot
name<-c("지자체의 정기적인\n운영관리 조사","지속적인 예산 지원","정기적인 지역주민\n만족도 조사","정기적인 시설\n유지보수","기타")
x = data1$Q12
x[grepl(",", x)] <- NA
tmp_value = x %>% unique()

non_value = c(1:5)[!(1:5 %in% tmp_value)]

count_vec = c(table(x))
names(count_vec)[!(1:5 %in% tmp_value)] = non_value

count_dat = data.frame(count = count_vec, tmp_name = names(count_vec), name = name)
count_dat = count_dat %>% mutate(per = round(count/sum(count)*100, 2), 
                                 누적퍼센트 = base::cumsum(per))
plot12 <- ggplot(count_dat, aes(x = tmp_name)) + 
  geom_bar(fill="#102b6a", aes(y = count), width=0.7, stat = "identity") + 
  labs(x="",y="")+
  theme_bw()+
  theme(text=element_text(size=15),axis.text.x = element_text(size = 9)) + 
  scale_x_discrete(labels = count_dat$name)
plot12
#Q12_table 
values<-1:6
x[grepl(",", x)] <-  6
x[grepl("NA", x)] <- 6
x <- ifelse(is.na(x), 6 , as.numeric(x))

result1 <- table(factor(x, levels = values))
name<-c("지자체의 정기적인 운영관리 조사","지속적인 예산 지원","정기적인 지역주민 만족도 조사","정기적인 시설 유지보수","기타","결측")

df1 <- data.frame(result1)
count_dat1 = data.frame(count = result1, tmp_name = names(result1), name = name)
count_dat1 = count_dat1 %>% mutate(per = round(count.Freq/sum(count.Freq)*100, 2), 
                                   누적퍼센트 = base::cumsum(per))
formattable(count_dat1 %>% rename(퍼센트 = per, 빈도 = count.Freq, 항목 = name) %>% select(항목, 빈도, 퍼센트, 누적퍼센트), 
            list(빈도 = color_bar("#BDBDBD"),퍼센트 =color_bar("#7D9C36"),항목=formatter("span",style=~style(font.weight="bold","font-size"="17px")),
                 누적퍼센트 = formatter("span", x ~ formatC(x, format = "f", digits = 1), style = ~ style(font.weight = "bold","font-size"="17px"))))


#Q13_plot
name<-c("운영 주체의\n선정","운영 프로그램에 대한\n지역 주민과의 의견 충돌","수익구조 조성의\n어려움","활용에 적합하지\n 않은 공간","운영인력확보","시설 홍보 및\n정보공유","운영,유지관리를\n위한 재원마련","지역주민 참여유도","기타")
      
x = data1$Q13

values<-1:9
result1 <- table(factor(x, levels = values))
count_dat = data.frame(count = result1, tmp_name = names(result1), name = name)
count_dat = count_dat %>% mutate(per = round(result1/sum(result1)*100, 2), 
                                 누적퍼센트 = base::cumsum(per))
plot13 <- ggplot(count_dat, aes(x = tmp_name)) + 
  geom_bar(fill="#102b6a", aes(y = result1), width=0.7, stat = "identity") + 
  labs(x="",y="")+
  theme_bw()+
  theme(text=element_text(size=15),axis.text.x = element_text(size = 7)) + 
  scale_x_discrete(labels = count_dat$name)
plot13
#Q13_table 
values<-1:10
x[grepl(",", x)] <-  10
x[grepl("NA", x)] <- 10
x <- ifelse(is.na(x), 10 , as.numeric(x))

result1 <- table(factor(x, levels = values))
name<-c("운영 주체의 선정","운영 프로그램에 대한 지역 주민과의 의견 충돌","수익구조 조성의 어려움","활용에 적합하지 않은 공간","운영인력확보","시설 홍보 및 정보공유","운영,유지관리를 위한 재원마련","지역주민 참여유도","기타","결측")

df1 <- data.frame(result1)
count_dat1 = data.frame(count = result1, tmp_name = names(result1), name = name)
count_dat1 = count_dat1 %>% mutate(per = round(count.Freq/sum(count.Freq)*100, 2), 
                                   누적퍼센트 = base::cumsum(per))
formattable(count_dat1 %>% rename(퍼센트 = per, 빈도 = count.Freq, 항목 = name) %>% select(항목, 빈도, 퍼센트, 누적퍼센트), 
            list(빈도 = color_bar("#BDBDBD"),퍼센트 =color_bar("#7D9C36"),항목=formatter("span",style=~style(font.weight="bold","font-size"="17px")),
                 누적퍼센트 = formatter("span", x ~ formatC(x, format = "f", digits = 1), style = ~ style(font.weight = "bold","font-size"="17px"))))


#Q14_plot
data1$Q141 <- ifelse(is.na(data1$Q141), 0 , data1$Q141)
data1$Q142 <- ifelse(is.na(data1$Q142), 0 , data1$Q142)
data1$Q143 <- ifelse(is.na(data1$Q143), 0 , data1$Q143)
data1$Q144 <- ifelse(is.na(data1$Q144), 0 , data1$Q144)

values <- c(0:4)
q141t <- table(factor(data1$Q141, levels = values))
q142t <- table(factor(data1$Q142, levels = values))
q143t <- table(factor(data1$Q143, levels = values))
q144t <- table(factor(data1$Q144, levels = values))

df141 <- data.frame(q141t)
df142 <- data.frame(q142t)
df143 <- data.frame(q143t)
df144 <- data.frame(q144t)
df141['score1'] <- df141$Freq * 4
df142['score2'] <- df142$Freq * 3
df143['score3'] <- df143$Freq * 2
df144['score4'] <- df144$Freq * 1


df14 <- data.frame(c(df141,df142,df143,df144))
df14 <- df14[,c(1,2,3,6,9,12)]
df14 <- df14[-1,]
df14['sumscore'] <- df14$score1 + df14$score2 + df14$score3 + df14$score4

name <- c("재정적 부담 완화","여러 시설의 접근편의성","시설 간 상호보완 가능","기존의 좋은 입지 확보")
plot14 <- ggplot(df14, aes(x = Var1, y = sumscore)) + 
  geom_bar(fill="#102b6a", aes(y = sumscore), width=0.7, stat = "identity") + 
  labs(x="",y="")+
  theme_bw()+
  theme(text=element_text(size=15),axis.text.x = element_text(size = 13)) +
  scale_x_discrete(labels = name)
print(plot14)

#Q14_table
name_r <- c("재정적 부담 완화","여러 시설의 접근편의성","시설 간 상호보완 가능","기존의 좋은 입지 확보","결측")
name_c <- c("항목","점수")
df141 <- data.frame(q141t)
df142 <- data.frame(q142t)
df143 <- data.frame(q143t)
df144 <- data.frame(q144t)
df141['score1'] <- df141$Freq * 4
df142['score2'] <- df142$Freq * 3
df143['score3'] <- df143$Freq * 2
df144['score4'] <- df144$Freq * 1

df14 <- data.frame(c(df141,df142,df143,df144))
df14['sumscore'] <- df14$score1 + df14$score2 + df14$score3 + df14$score4

df14t <- df14[,c(1,13)]
df14t <- rbind(df14t, df14t[1,])
df14t <- df14t[-1,]
df14t[5,2] <- 0
df14t[,1] <- name_r

rownames(df14t) <- NULL
colnames(df14t) <- name_c
formattable(df14t, list(점수 = color_bar("#7D9C36"),항목=formatter("span",style=~style(font.weight="bold","font-size"="17px"))))

#Q15_plot
data1$Q151 <- ifelse(is.na(data1$Q151), 0 , data1$Q151)
data1$Q152 <- ifelse(is.na(data1$Q152), 0 , data1$Q152)
data1$Q153 <- ifelse(is.na(data1$Q153), 0 , data1$Q153)
data1$Q154 <- ifelse(is.na(data1$Q154), 0 , data1$Q154)
data1$Q155 <- ifelse(is.na(data1$Q155), 0 , data1$Q155)

values <- c(0:5)
q151t <- table(factor(data1$Q151, levels = values))
q152t <- table(factor(data1$Q152, levels = values))
q153t <- table(factor(data1$Q153, levels = values))
q154t <- table(factor(data1$Q154, levels = values))
q155t <- table(factor(data1$Q155, levels = values))

df151 <- data.frame(q151t)
df152 <- data.frame(q152t)
df153 <- data.frame(q153t)
df154 <- data.frame(q154t)
df155 <- data.frame(q155t)

df151['score1'] <- df151$Freq * 5
df152['score2'] <- df152$Freq * 4
df153['score3'] <- df153$Freq * 3
df154['score4'] <- df154$Freq * 2
df155['score5'] <- df155$Freq * 1

df15 <- data.frame(c(df151,df152,df153,df154,df155))
df15 <- df15[,c(1,2,3,6,9,12,15)]
df15 <- df15[-1,]
df15<-df15[,-2]
df15['sumscore'] <- df15$score1 + df15$score2 + df15$score3 + df15$score4 + df15$score5

name <- c("안전 및 보안 문제","복합시설에 따른\n동선의 어려움","주체 간 갈등 발생","전문성 저하 우려","복수주체에 따른\n운영혼란")
plot15 <- ggplot(df15, aes(x = Var1, y = sumscore)) + 
  geom_bar(fill="#102b6a", aes(y = sumscore), width=0.7,stat = "identity") + 
  labs(x="",y="")+
  theme_bw()+
  theme(text=element_text(size=15),axis.text.x = element_text(size = 13)) +
  scale_x_discrete(labels = name)
print(plot15)

#Q15_table
name_r <- c("안전 및 보안 문제","복합시설에 따른 동선의 어려움","주체 간 갈등 발생","전문성 저하 우려","복수주체에 따른 운영혼란", "결측")
name_c <- c("항목","점수")
df151 <- data.frame(q151t)
df152 <- data.frame(q152t)
df153 <- data.frame(q153t)
df154 <- data.frame(q154t)
df155 <- data.frame(q155t)
df151['score1'] <- df151$Freq * 5
df152['score2'] <- df152$Freq * 4
df153['score3'] <- df153$Freq * 3
df154['score4'] <- df154$Freq * 2
df155['score5'] <- df155$Freq * 1

df15 <- data.frame(c(df151,df152,df153,df154,df155))
df15['sumscore'] <- df15$score1 + df15$score2 + df15$score3 + df15$score4 + df15$score5
df15t <- df15[,c(1,16)]

df15t <- rbind(df15t, df15t[1,])
df15t <- df15t[-1,]
df15t[6,2] <- 0
df15t[,1] <- name_r

rownames(df15t) <- NULL
colnames(df15t) <- name_c
formattable(df15t, list(점수 = color_bar("#7D9C36"),항목=formatter("span",style=~style(font.weight="bold","font-size"="17px"))))


#Q16_plot
data1 <- read_excel("C:/Users/syj90/OneDrive/바탕 화면/경기도_설문조사 완료_.xlsx")
x=data1$Q16
split_data <- strsplit(x, ",")
df<-data.frame(matrix(unlist(split_data),ncol=1))
colnames(df)<-c("value")
print(df)
table(df)

name<-c("교통시설\n(역세권)","공공공지\n(광장, 녹지, 공원)","유통,공급시설\n(시장,업무 등)","체육시설","문화시설","교육시설","커뮤니티 시설","기타")

tmp_value = df %>% unique()

non_value = c(1:8)[!(1:8 %in% tmp_value)]

count_vec = c(table(df),0)
names(count_vec)[!(1:8 %in% tmp_value)] = non_value

count_dat = data.frame(count = count_vec, tmp_name = names(count_vec), name = name)
count_dat = count_dat %>% mutate(per = round(count/sum(count)*100, 2), 
                                 누적퍼센트 = base::cumsum(per))
plot16 <- ggplot(count_dat, aes(x = tmp_name)) + 
  geom_bar(fill="#102b6a", aes(y = count), width=0.7, stat = "identity") + 
  labs(x="",y="")+
  theme_bw()+
  theme(text=element_text(size=15),axis.text.x = element_text(size = 10)) + 
  scale_x_discrete(labels = count_dat$name)
plot16
#Q16_table
newdf<-lapply(df,function(df) ifelse(is.na(df),9,df))
newdf <- lapply(newdf, as.numeric)
newdf2 <- data.frame(newdf)
values <- 1:9
result <- table(factor(newdf2$value, levels = values))
result

name<-c("교통시설(역세권)","공공공지(광장, 녹지, 공원)","유통,공급시설(시장,업무 등)","체육시설","문화시설","교육시설","커뮤니티 시설","기타","결측")

df1 <- data.frame(result)
count_dat1 = data.frame(count = result, tmp_name = names(result), name = name)
count_dat1 = count_dat1 %>% mutate(per = round(count.Freq/sum(count.Freq)*100, 2), 
                                   누적퍼센트 = base::cumsum(per))
formattable(count_dat1 %>% rename(퍼센트 = per, 빈도 = count.Freq, 항목 = name) %>% select(항목, 빈도, 퍼센트, 누적퍼센트), 
            list(빈도 = color_bar("#BDBDBD"),퍼센트 = color_bar("#7D9C36"),항목=formatter("span",style=~style(font.weight="bold","font-size"="17px")),
                 누적퍼센트 = formatter("span", x ~ formatC(x, format = "f", digits = 1), style = ~ style(font.weight = "bold","font-size"="17px"))))


#Q17_plot
x=data1$Q17
split_data <- strsplit(x, ",")
df<-data.frame(matrix(unlist(split_data),ncol=1))
colnames(df)<-c("value")
print(df)
table(df)
name<-c("고령자\n편의시설","육아지원시설","청년지원시설","복지지원시설","평생교육시설","의료지원시설","문화지원시설","생활편의\n지원시설","기타")

tmp_value = df %>% unique()

non_value = c(1:9)[!(1:9 %in% tmp_value)]

count_vec = c(table(df))
names(count_vec)[!(1:9 %in% tmp_value)] = non_value

count_dat = data.frame(count = count_vec, tmp_name = names(count_vec), name = name)
count_dat = count_dat %>% mutate(per = round(count/sum(count)*100, 2), 
                                 누적퍼센트 = base::cumsum(per))
plot17 <- ggplot(count_dat, aes(x = tmp_name)) + 
  geom_bar(fill="#102b6a", aes(y = count), width=0.7, stat = "identity") + 
  labs(x="",y="")+
  theme_bw()+
  theme(text=element_text(size=15),axis.text.x = element_text(size = 9)) + 
  scale_x_discrete(labels = count_dat$name)
plot17
#Q17_table
newdf<-lapply(df,function(df) ifelse(is.na(df),10,df))
newdf <- lapply(newdf, as.numeric)
newdf2 <- data.frame(newdf)
values <- 1:10
result <- table(factor(newdf2$value, levels = values))
result

name<-c("고령자 편의시설","육아지원시설","청년지원시설","복지지원시설","평생교육시설","의료지원시설","문화지원시설","생활편의 지원시설","기타","결측")

df1 <- data.frame(result)
count_dat1 = data.frame(count = result, tmp_name = names(result), name = name)
count_dat1 = count_dat1 %>% mutate(per = round(count.Freq/sum(count.Freq)*100, 2), 
                                   누적퍼센트 = base::cumsum(per))
formattable(count_dat1 %>% rename(퍼센트 = per, 빈도 = count.Freq, 항목 = name) %>% select(항목, 빈도, 퍼센트, 누적퍼센트), 
            list(빈도 = color_bar("#BDBDBD"),퍼센트 = color_bar("#7D9C36"),항목=formatter("span",style=~style(font.weight="bold","font-size"="17px")),
                 누적퍼센트 = formatter("span", x ~ formatC(x, format = "f", digits = 1), style = ~ style(font.weight = "bold","font-size"="17px"))))



#Q18_plot
data1 <- read_excel("C:/Users/syj90/OneDrive/바탕 화면/경기도_설문조사 완료_.xlsx")
x=data1$Q18
split_data <- strsplit(x, ",")
df<-data.frame(matrix(unlist(split_data),ncol=1))
colnames(df)<-c("value")
print(df)
table(df)
name<-c("교통시설\n(역세권)","공공공지\n(광장, 녹지, 공원)","유통,공급시설\n(시장,업무 등)","체육시설","문화시설","교육시설","커뮤니티 시설","기타")

tmp_value = df %>% unique()

non_value = c(1:8)[!(1:8 %in% tmp_value)]

count_vec = c(table(df),0)
names(count_vec)[!(1:8 %in% tmp_value)] = non_value

count_dat = data.frame(count = count_vec, tmp_name = names(count_vec), name = name)
count_dat = count_dat %>% mutate(per = round(count/sum(count)*100, 2), 
                                 누적퍼센트 = base::cumsum(per))
plot18 <- ggplot(count_dat, aes(x = tmp_name)) + 
  geom_bar(fill="#102b6a", aes(y = count), width=0.7, stat = "identity") + 
  labs(x="",y="")+
  theme_bw()+
  theme(text=element_text(size=15),axis.text.x = element_text(size = 9)) + 
  scale_x_discrete(labels = count_dat$name)
plot18
#Q18_table
newdf<-lapply(df,function(df) ifelse(is.na(df),9,df))
newdf <- lapply(newdf, as.numeric)
newdf2 <- data.frame(newdf)
values <- 1:9
result <- table(factor(newdf2$value, levels = values))
result

name<-c("교통시설(역세권)","공공공지(광장, 녹지, 공원)","유통,공급시설(시장,업무 등)","체육시설","문화시설","교육시설","커뮤니티 시설","기타","결측")

df1 <- data.frame(result)
count_dat1 = data.frame(count = result, tmp_name = names(result), name = name)
count_dat1 = count_dat1 %>% mutate(per = round(count.Freq/sum(count.Freq)*100, 2), 
                                   누적퍼센트 = base::cumsum(per))
formattable(count_dat1 %>% rename(퍼센트 = per, 빈도 = count.Freq, 항목 = name) %>% select(항목, 빈도, 퍼센트, 누적퍼센트), 
            list(빈도 = color_bar("#BDBDBD"),퍼센트 = color_bar("#7D9C36"),항목=formatter("span",style=~style(font.weight="bold","font-size"="17px")),
                 누적퍼센트 = formatter("span", x ~ formatC(x, format = "f", digits = 1), style = ~ style(font.weight = "bold","font-size"="17px"))))


#Q19_plot
data1 <- read_excel("C:/Users/syj90/OneDrive/바탕 화면/경기도_설문조사 완료_.xlsx")
x=data1$Q19
split_data <- strsplit(x, ",")
df<-data.frame(matrix(unlist(split_data),ncol=1))
colnames(df)<-c("value")
print(df)
table(df)
name<-c("고령자\n편의시설","육아지원시설","청년지원시설","복지지원시설","평생교육시설","의료지원시설","문화지원시설","생활편의\n지원시설","기타")

tmp_value = df %>% unique()

non_value = c(1:9)[!(1:9 %in% tmp_value)]

count_vec = c(table(df))
names(count_vec)[!(1:9 %in% tmp_value)] = non_value

count_dat = data.frame(count = count_vec, tmp_name = names(count_vec), name = name)
count_dat = count_dat %>% mutate(per = round(count/sum(count)*100, 2), 
                                 누적퍼센트 = base::cumsum(per))
plot19 <- ggplot(count_dat, aes(x = tmp_name)) + 
  geom_bar(fill="#102b6a", aes(y = count), width=0.7, stat = "identity") + 
  labs(x="",y="")+
  theme_bw()+
  theme(text=element_text(size=15),axis.text.x = element_text(size = 9)) + 
  scale_x_discrete(labels = count_dat$name)
plot19
#Q19_table
newdf<-lapply(df,function(df) ifelse(is.na(df),10,df))
newdf <- lapply(newdf, as.numeric)
newdf2 <- data.frame(newdf)
values <- 1:10
result <- table(factor(newdf2$value, levels = values))
result

name<-c("고령자 편의시설","육아지원시설","청년지원시설","복지지원시설","평생교육시설","의료지원시설","문화지원시설","생활편의 지원시설","기타","결측")

df1 <- data.frame(result)
count_dat1 = data.frame(count = result, tmp_name = names(result), name = name)
count_dat1 = count_dat1 %>% mutate(per = round(count.Freq/sum(count.Freq)*100, 2), 
                                   누적퍼센트 = base::cumsum(per))
formattable(count_dat1 %>% rename(퍼센트 = per, 빈도 = count.Freq, 항목 = name) %>% select(항목, 빈도, 퍼센트, 누적퍼센트), 
            list(빈도 = color_bar("#BDBDBD"),퍼센트 = color_bar("#7D9C36"),항목=formatter("span",style=~style(font.weight="bold","font-size"="17px")),
                 누적퍼센트 = formatter("span", x ~ formatC(x, format = "f", digits = 1), style = ~ style(font.weight = "bold","font-size"="17px"))))


#Q20_plot
data1 <- read_excel("C:/Users/syj90/OneDrive/바탕 화면/경기도_설문조사 완료_.xlsx")
name<-c("공간활용성","인구특성 반영","방문객 수","수익성","교통접근성","주민수요 적합성","기타")
x = data1$Q20
x[grepl(",", x)] <- NA
tmp_value = x %>% unique()

non_value = c(1:7)[!(1:7 %in% tmp_value)]

count_vec = c(table(x),0)
names(count_vec)[!(1:7 %in% tmp_value)] = non_value

count_dat = data.frame(count = count_vec, tmp_name = names(count_vec), name = name)
count_dat = count_dat %>% mutate(per = round(count/sum(count)*100, 2), 
                                 누적퍼센트 = base::cumsum(per))
plot20 <- ggplot(count_dat, aes(x = tmp_name)) + 
  geom_bar(fill="#102b6a", aes(y = count), width=0.7, stat = "identity") + 
  labs(x="",y="")+
  theme_bw()+
  theme(text=element_text(size=15),axis.text.x = element_text(size = 13)) + 
  scale_x_discrete(labels = count_dat$name)
plot20
#Q20_table 
values<-1:8
x[grepl(",", x)] <-  8
x[grepl("NA", x)] <- 8
x <- ifelse(is.na(x), 8 , as.numeric(x))

result1 <- table(factor(x, levels = values))
name<-c("공간활용성","인구특성 반영","방문객 수","수익성","교통접근성","주민수요 적합성","기타","결측")

df1 <- data.frame(result1)
count_dat1 = data.frame(count = result1, tmp_name = names(result1), name = name)
count_dat1 = count_dat1 %>% mutate(per = round(count.Freq/sum(count.Freq)*100, 2), 
                                   누적퍼센트 = base::cumsum(per))
formattable(count_dat1 %>% rename(퍼센트 = per, 빈도 = count.Freq, 항목 = name) %>% select(항목, 빈도, 퍼센트, 누적퍼센트), 
            list(빈도 = color_bar("#BDBDBD"),퍼센트 =color_bar("#7D9C36"),항목=formatter("span",style=~style(font.weight="bold","font-size"="17px")),
                 누적퍼센트 = formatter("span", x ~ formatC(x, format = "f", digits = 1), style = ~ style(font.weight = "bold","font-size"="17px"))))


#Q2_plot
data1 <- read_excel("C:/Users/syj90/OneDrive/바탕 화면/경기도_설문조사 완료_.xlsx",sheet="Sheet3")
name<-c("신축형","리모델링형")
x = data1$Q23
tmp_value = x %>% unique()

non_value = c(1:2)[!(1:2 %in% tmp_value)]

count_vec = c(table(x))
names(count_vec)[!(1:2 %in% tmp_value)] = non_value

count_dat = data.frame(count = count_vec, tmp_name = names(count_vec), name = name)
count_dat = count_dat %>% mutate(per = round(count/sum(count)*100, 2), 
                                 누적퍼센트 = base::cumsum(per))
plot23 <- ggplot(count_dat, aes(x = tmp_name)) + 
  geom_bar(fill="#102b6a", aes(y = count), width=0.7, stat = "identity") + 
  labs(x="",y="")+
  theme_bw()+
  theme(text=element_text(size=15),axis.text.x = element_text(size = 15)) + 
  scale_x_discrete(labels = count_dat$name)
plot23
#Q2_table 
values<-1:3
x[grepl(",", x)] <-  3
x[grepl("NA", x)] <- 3
x <- ifelse(is.na(x), 3 , as.numeric(x))

result1 <- table(factor(x, levels = values))
name<-c("신축형","리모델링형","결측")

df1 <- data.frame(result1)
count_dat1 = data.frame(count = result1, tmp_name = names(result1), name = name)
count_dat1 = count_dat1 %>% mutate(per = round(count.Freq/sum(count.Freq)*100, 2), 
                                   누적퍼센트 = base::cumsum(per))
formattable(count_dat1 %>% rename(퍼센트 = per, 빈도 = count.Freq, 항목 = name) %>% select(항목, 빈도, 퍼센트, 누적퍼센트), 
            list(빈도 = color_bar("#BDBDBD"),퍼센트 =color_bar("#7D9C36"),항목=formatter("span",style=~style(font.weight="bold","font-size"="17px")),
                 누적퍼센트 = formatter("span", x ~ formatC(x, format = "f", digits = 1), style = ~ style(font.weight = "bold","font-size"="17px"))))


#Q3_plot
data1 <- read_excel("C:/Users/syj90/OneDrive/바탕 화면/경기도_설문조사 완료_.xlsx",sheet="Sheet3")
name<-c("미진행","추진 예정","공모 진행","계획수립","건립 중","준공완료")
x = data1$Q24
tmp_value = x %>% unique()

non_value = c(1:6)[!(1:6 %in% tmp_value)]

count_vec = c(table(x))
names(count_vec)[!(1:6 %in% tmp_value)] = non_value

count_dat = data.frame(count = count_vec, tmp_name = names(count_vec), name = name)
count_dat = count_dat %>% mutate(per = round(count/sum(count)*100, 2), 
                                 누적퍼센트 = base::cumsum(per))
plot24 <- ggplot(count_dat, aes(x = tmp_name)) + 
  geom_bar(fill="#102b6a", aes(y = count), width=0.7, stat = "identity") + 
  labs(x="",y="")+
  theme_bw()+
  theme(text=element_text(size=15),axis.text.x = element_text(size = 15)) + 
  scale_x_discrete(labels = count_dat$name)
plot24
#Q3_table 
values<-1:7
x[grepl(",", x)] <- 7
x[grepl("NA", x)] <- 7
x <- ifelse(is.na(x), 7 , as.numeric(x))

result1 <- table(factor(x, levels = values))
name<-c("미진행","추진 예정","공모 진행","계획수립","건립 중","준공완료","결측")

df1 <- data.frame(result1)
count_dat1 = data.frame(count = result1, tmp_name = names(result1), name = name)
count_dat1 = count_dat1 %>% mutate(per = round(count.Freq/sum(count.Freq)*100, 2), 
                                   누적퍼센트 = base::cumsum(per))
formattable(count_dat1 %>% rename(퍼센트 = per, 빈도 = count.Freq, 항목 = name) %>% select(항목, 빈도, 퍼센트, 누적퍼센트), 
            list(빈도 = color_bar("#BDBDBD"),퍼센트 =color_bar("#7D9C36"),항목=formatter("span",style=~style(font.weight="bold","font-size"="17px")),
                 누적퍼센트 = formatter("span", x ~ formatC(x, format = "f", digits = 1), style = ~ style(font.weight = "bold","font-size"="17px"))))


#Q4_plot
data1 <- read_excel("C:/Users/syj90/OneDrive/바탕 화면/경기도_설문조사 완료_.xlsx",sheet="Sheet3")
name<-c("공공 재정투자방식","민간투자방식","민관협력투자방식","기타")
x = data1$Q25
tmp_value = x %>% unique()

non_value = c(1:4)[!(1:4 %in% tmp_value)]

count_vec = c(table(x))
names(count_vec)[!(1:4 %in% tmp_value)] = non_value

count_dat = data.frame(count = count_vec, tmp_name = names(count_vec), name = name)
count_dat = count_dat %>% mutate(per = round(count/sum(count)*100, 2), 
                                 누적퍼센트 = base::cumsum(per))
plot25 <- ggplot(count_dat, aes(x = tmp_name)) + 
  geom_bar(fill="#102b6a", aes(y = count), width=0.7, stat = "identity") + 
  labs(x="",y="")+
  theme_bw()+
  theme(text=element_text(size=15),axis.text.x = element_text(size = 13)) + 
  scale_x_discrete(labels = count_dat$name)
plot25
#Q4_table 
values<-1:5
x[grepl(",", x)] <- 5
x[grepl("NA", x)] <- 5
x <- ifelse(is.na(x), 5 , as.numeric(x))

result1 <- table(factor(x, levels = values))
name<-c("공공 재정투자방식","민간투자방식","민관협력투자방식","기타","결측")

df1 <- data.frame(result1)
count_dat1 = data.frame(count = result1, tmp_name = names(result1), name = name)
count_dat1 = count_dat1 %>% mutate(per = round(count.Freq/sum(count.Freq)*100, 2), 
                                   누적퍼센트 = base::cumsum(per))
formattable(count_dat1 %>% rename(퍼센트 = per, 빈도 = count.Freq, 항목 = name) %>% select(항목, 빈도, 퍼센트, 누적퍼센트), 
            list(빈도 = color_bar("#BDBDBD"),퍼센트 =color_bar("#7D9C36"),항목=formatter("span",style=~style(font.weight="bold","font-size"="17px")),
                 누적퍼센트 = formatter("span", x ~ formatC(x, format = "f", digits = 1), style = ~ style(font.weight = "bold","font-size"="17px"))))


#Q5_plot
data1 <- read_excel("C:/Users/syj90/OneDrive/바탕 화면/경기도_설문조사 완료_.xlsx",sheet="Sheet3")
name<-c("국비","도비","지자체","민간","혼합 구성")

x = data1$Q26

values<-1:5
result1 <- table(factor(x, levels = values))
count_dat = data.frame(count = result1, tmp_name = names(result1), name = name)
count_dat = count_dat %>% mutate(per = round(result1/sum(result1)*100, 2), 
                                 누적퍼센트 = base::cumsum(per))
plot26 <- ggplot(count_dat, aes(x = tmp_name)) + 
  geom_bar(fill="#102b6a", aes(y = result1), width=0.7, stat = "identity") + 
  labs(x="",y="")+
  theme_bw()+
  theme(text=element_text(size=15),axis.text.x = element_text(size = 15)) + 
  scale_x_discrete(labels = count_dat$name)
plot26
#Q5_table 
values<-1:6
x[grepl("NA", x)] <- 6
x <- ifelse(is.na(x), 6 , as.numeric(x))

result1 <- table(factor(x, levels = values))
name<-c("국비","도비","지자체","민간","혼합 구성","결측")

df1 <- data.frame(result1)
count_dat1 = data.frame(count = result1, tmp_name = names(result1), name = name)
count_dat1 = count_dat1 %>% mutate(per = round(count.Freq/sum(count.Freq)*100, 2), 
                                   누적퍼센트 = base::cumsum(per))
formattable(count_dat1 %>% rename(퍼센트 = per, 빈도 = count.Freq, 항목 = name) %>% select(항목, 빈도, 퍼센트, 누적퍼센트), 
            list(빈도 = color_bar("#BDBDBD"),퍼센트 =color_bar("#7D9C36"),항목=formatter("span",style=~style(font.weight="bold","font-size"="17px")),
                 누적퍼센트 = formatter("span", x ~ formatC(x, format = "f", digits = 1), style = ~ style(font.weight = "bold","font-size"="17px"))))

