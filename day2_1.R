# 파생변수생성
cars
cars <- cars

## 변수이름 확인
colnames(cars)
colnames(cars) <- c("속도", "거리")
head(cars)
tail(cars)

# 파생변수 생성
cars$속도x거리 <- cars$속도 * cars$거리
head(cars)

# 조건에 따라 변수의 값을 새로 생성
cars$속도정도 <- ifelse(cars$속도 > 7, "빠름", "느림")
head(cars)

# hflights 패키지 설치
#install.packages("hflights")
require(hflights)
hflights <- hflights
summary(hflights)

# Select
#install.packages("dplyr")
require(dplyr)
str(hflights)
newhflights <- select(hflights, Origin, Distance, TailNum)

# Filter
head(
  filter(hflights, Month==2) 
)
hflight1 <- filter(hflights, Month==2)
nrow(hflight1)

head(
  filter(hflights, Month==8)
)
hflight2 <- filter(hflights, Month==8)
nrow(hflight2)

hflight3 <- filter(hflights, Month==2 | Month==8)
nrow(hflight3)

hflight4 <- filter(hflights, Month==2 | Month=="DWF")
nrow(hflight4)

# 정렬 Arrange
hflight5 <- arrange(hflights, Month)
head(hflight5)
tail(hflight5)

hflight6 <- arrange(hflights, desc(Month))
head(hflight6)

hflight7 <- arrange(hflights, desc(Month), DayOfWeek)
head(hflight7)

# 열의 조작
head(mutate(hflights, gain = ArrDelay - DepDelay), gain_per_hour = gain / (Airtime/60))

# Summary
summarise(hflights, delay = mean(DepDelay, na.rm = TRUE))

# 그룹별 요약(다시) using %>%
hflights %>%
  group_by(TailNum) %>%
  select(Month, DayofMonth, DayOfWeek) %>%
  summarise(AveMonth = mean(Month, na.rm = TRUE), AveMonth = mean(Month, na.rm = TRUE), AveDayofMonth = mean(DayofMonth, na.rm = TRUE), AveDayofWeek = mean(DayOfWeek, na.rm = TRUE))

# 연습문제
head(
  hflights %>%
    filter(TailNum != "") %>%
    select(FlightNum, TailNum, AirTime, Distance, Origin) %>%
    group_by(TailNum) %>%
    filter(Origin == "HOU" || Distance > 1000) %>%
    summarise(AveAirTime = mean(AirTime, na.rm=TRUE), AveDistance = mean(Distance, na.rm=TRUE))
)

# ggplot2
## 패키지 설치
#install.packages("ggplot2")
require(ggplot2)

## 데이터 불러오기
data("midwest", package="ggplot2")
summary(midwest)

options(scipen=999)
ggplot(midwest, aes(x=area, y=poptotal))

## Scatter Plot생성
midwest %>%
  ggplot(aes(x=area, y=poptotal)) + geom_point()

## 리그레션 라인 추가
midwest %>%
  ggplot(aes(x=area, y=poptotal)) + geom_point() + geom_smooth(method="lm")

# 그래프의 범위 조정
g <- midwest %>%
  ggplot(aes(x=area, y=poptotal)) + geom_point() + geom_smooth(method="lm")
g + xlim(c(0,0.1)) + ylim(c(0,1000000))

# 줌
g1 <- g + coord_cartesian(xlim=c(0, 0.75), ylim=c(0, 500000))
g1

g1 <- g + xlim(c(0,0.1)) + ylim(c(0,1000000))

# 제목과 축 이름
g1
g1 + labs(title="Area VS Population", subtitle = "midwest", y="Population", x="area", caption="Midwest Population")

# 색변경
g <- midwest %>%
  ggplot(aes(x=area, y=poptotal)) + geom_point(col="red", size=1) + geom_smooth(method="lm")
g + xlim(c(0,0.1)) + ylim(c(0,1000000))

# 색에 데이터 정보 반영
g <- midwest %>%
  ggplot(aes(x=area, y=poptotal)) + geom_point(aes(col=state), size=1) + geom_smooth(method="lm")
g + xlim(c(0,0.1)) + ylim(c(0,1000000))

# 그래프 분할
g <- midwest %>%
  ggplot(aes(x=area, y=poptotal)) + geom_point() + geom_smooth(method="lm")
g + facet_wrap(~state, nrow=2)

midwest
hist(midwest$popdensity)
midwest$degreeofdensity <- ifelse(midwest$popdensity >= 10000, "high", "low")
table(midwest$degreeofdensity)

g <- midwest %>%
  ggplot(aes(x=area, y=poptotal)) + geom_point(aes(col=degreeofdensity)) + geom_smooth(method="lm")
g + facet_wrap(~state, nrow=2)

# ggplot 실습
mtcars <- mtcars
summary(mtcars)
plot(mtcars)

# mtcars데이터를 활용해서 두개의 연속형 변수를 선택한 후 scatter plot을 그려보세요
# 점에 카테고리컬 변수의 정보를 활용해서 점의 색깔에 반영해 보십시요.
prac <- mtcars %>%
  ggplot(aes(x=disp, y=hp)) + geom_point(aes(col=as.factor(wt))) + ggtitle('scatter plot practice') + xlab('Displacement') + ylab('Gross horsepower')
prac

mtcars$names <- rownames(mtcars)

gg <- mtcars %>%
  ggplot(aes(x=wt, y=mpg)) + geom_point(aes(col=as.factor(vs))) + geom_smooth(method = "lm") + geom_text(aes(label=names), size=3)
gg
