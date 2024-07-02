# ----------------------------------------------------------------------------------
# seed를 고정한 후, 군집분석 시행
set.seed(123)
data <- read.csv("C:/Amazon/amazon_preprocessed.csv")

# ----  1번째 분석 -> 구매 물품 종류 Purchase category별로 고객 데이터를 나누어 클러스터링.
data <- subset(data, select = -Improvement_Areas)
data <- subset(data, select = -Purchase_Frequency)
data <- subset(data, select = -Browsing_Frequency)
data <- subset(data, select = -Personalized_Recommendation_Frequency_Purchase)
data <- subset(data, select = -Personalized_Recommendation_Frequency)
data <- subset(data, select = -Recommendation_Helpfulness)
data <- subset(data, select = -Shopping_Satisfaction)
data <- subset(data, select = -X)

# 데이터의 표준화 => 삭제 : 더미변수화 된 변수는 표준화하지 않습니다
# data_scaled <- data.frame(scale(data))

# 적절한 군집 수를 추정(엘보우 기법) => Elbow Method를 수행하는 패키지가 있어 이를 추가하였습니다
library(factoextra)
fviz_nbclust(data, kmeans, method = "wss")

# 3 => 4 새로운 패키지 결과에 따라 군집수 4가 적절하다 판단되어 수정하였습니다
km <- kmeans(data, centers=4, nstart=10)

# 군집 당 사이즈
km$size
# 각각의 열이 어떤 군집에 할당되었는지를 보여 준다.
km$cluster
# 각 군집 중앙값
km$centers

# dplyr 패키지 로드
library(dplyr)

# 군집 결과를 데이터 프레임에 추가.
data_clustered <- data.frame(data, Cluster = km$cluster)

# 원래의 데이터 프레임을 다시 불러와 'Improvement_Areas' 열을 추가.
original_data <- read.csv("C:/Amazon/amazon_preprocessed.csv")
data_with_improvement <- cbind(data_clustered, Improvement_Areas = original_data$Improvement_Areas)

# 각 군집 내에서 'Improvement_Areas' 값의 빈도수를 계산
improvement_freq <- data_with_improvement %>%
  group_by(Cluster, Improvement_Areas) %>%
  summarise(Frequency = n()) %>%
  arrange(Cluster, desc(Frequency))

# 각 군집 내에서 가장 빈번한 'Improvement_Areas' 값 ?
most_frequent_improvement <- improvement_freq %>%
  group_by(Cluster) %>%
  slice(which.max(Frequency))

# 결과 분석
print(most_frequent_improvement)

#####################################################################################
# 엑셀파일 만드는 코드입니다.
# library(writexl)
# write_xlsx(data_with_improvement, path = "C:/Amazon/1data_with_improvement.xlsx")
#####################################################################################

# -----------------------------------------------------------------------------------



# ----  2번째 분석 -> 추천 내용이 도움이 된 경험 별로 고객 데이터를 나누어 클러스터링
# seed를 고정한 후, 군집분석 시행
set.seed(123)
data <- read.csv("C:/Amazon/amazon_preprocessed.csv")

data <- subset(data, select = -Improvement_Areas)
data <- subset(data, select = -age)
data <- subset(data, select = -Gender_Male)
data <- subset(data, select = -Gender_Female)
data <- subset(data, select = -Gender_None)
data <- subset(data, select = -Purchase_Categories_Beauty_and_Personal_Care)
data <- subset(data, select = -Purchase_Categories_Clothing_and_Fashion)
data <- subset(data, select = -Purchase_Categories_Groceries_and_Gourmet_Food)
data <- subset(data, select = -Purchase_Categories_Home_and_Kitchen)
data <- subset(data, select = -X)

# 데이터의 표준화
data_scaled <- data.frame(scale(data))

# 적절한 군집 수를 추정(엘보우 기법) --> 4개의 군집 => 마찬가지로 패키지 함수로 수정하였으나 군집수에는 차이가 없습니다
library(factoextra)
fviz_nbclust(data, kmeans, method = "wss")

km <- kmeans(data, centers=4, nstart=10)

# 군집 당 사이즈 
km$size
# 각각의 열이 어떤 군집에 할당되었는지를 보여 준다.
km$cluster
# 각 군집 중앙값
km$centers

# dplyr 패키지 로드
library(dplyr)

# 군집 결과를 데이터 프레임에 추가.
data_clustered <- data.frame(data, Cluster = km$cluster)

# 원래의 데이터 프레임을 다시 불러와 'Improvement_Areas' 열을 추가.
original_data <- read.csv("C:/Amazon/amazon_preprocessed.csv")
data_with_improvement <- cbind(data_clustered, Improvement_Areas = original_data$Improvement_Areas)

# 각 군집 내에서 'Improvement_Areas' 값의 빈도수를 계산
improvement_freq <- data_with_improvement %>%
  group_by(Cluster, Improvement_Areas) %>%
  summarise(Frequency = n()) %>%
  arrange(Cluster, desc(Frequency))

# 각 군집 내에서 가장 빈번한 'Improvement_Areas' 값 ?
most_frequent_improvement <- improvement_freq %>%
  group_by(Cluster) %>%
  slice(which.max(Frequency))

# 결과
print(most_frequent_improvement)


# write_xlsx(data_with_improvement, path = "C:/Amazon/2data_with_improvement.xlsx")
# -----------------------------------------------------------------------------------



# ----  3번째 분석 -> 상품 추천이 유용하지 않은 사람을 대상으로 클러스터링하여 마케팅 및 추천 알고리즘 향상이 필요한 제품 카테고리 식별
# seed를 고정한 후, 군집분석 시행
set.seed(123)
data <- read.csv("C:/Amazon/amazon_preprocessed.csv")

data <- subset(data, select = -Improvement_Areas)
data <- subset(data, select = -age)
data <- subset(data, select = -Gender_Male)
data <- subset(data, select = -Gender_Female)
data <- subset(data, select = -Gender_None)
data <- subset(data, select = -Browsing_Frequency)
data <- subset(data, select = -X)

# 더미변수화된 변수는 스케일링하지 않습니다
onehotencoded.col = c("Purchase_Categories_Beauty_and_Personal_Care",
                      "Purchase_Categories_Clothing_and_Fashion",
                      "Purchase_Categories_Groceries_and_Gourmet_Food",
                      "Purchase_Categories_Home_and_Kitchen")
# 서열변수는 스케일링을 수행합니다
data.onehotencdoed.col = data[, onehotencoded.col]
scaling.col = c("Purchase_Frequency",
                "Personalized_Recommendation_Frequency_Purchase","Personalized_Recommendation_Frequency",
                "Recommendation_Helpfulness","Shopping_Satisfaction")
data.scaling.col = data[, scaling.col]
data.scaling.col.scaled = scale(data.scaling.col)

# 데이터의 표준화
data_scaled = data.frame(data.onehotencdoed.col, data.scaling.col.scaled)

# 마찬가지로 패키지 함수로 수정하였으나 군집수에는 차이가 없습니다
library(factoextra)
fviz_nbclust(data, kmeans, method = "wss")

km <- kmeans(data, centers=5, nstart=10)

# 군집 당 사이즈
km$size
# 각각의 열이 어떤 군집에 할당되었는지를 보여 준다.
km$cluster
# 각 군집 중앙값
km$centers

# dplyr 패키지 로드
library(dplyr)

# 군집 결과를 데이터 프레임에 추가.
data_clustered <- data.frame(data, Cluster = km$cluster)

# 원래의 데이터 프레임을 다시 불러와 'Improvement_Areas' 열을 추가.
original_data <- read.csv("C:/Amazon/amazon_preprocessed.csv")
data_with_improvement <- cbind(data_clustered, Improvement_Areas = original_data$Improvement_Areas)

# 각 군집 내에서 'Improvement_Areas' 값의 빈도수를 계산
improvement_freq <- data_with_improvement %>%
  group_by(Cluster, Improvement_Areas) %>%
  summarise(Frequency = n()) %>%
  arrange(Cluster, desc(Frequency))

# 각 군집 내에서 가장 빈번한 'Improvement_Areas' 값 ?
most_frequent_improvement <- improvement_freq %>%
  group_by(Cluster) %>%
  slice(which.max(Frequency))

# 결과
print(most_frequent_improvement)

# 밑에는 엑셀파일 만드는 코드입니다.
# write_xlsx(data_with_improvement, path = "C:/Amazon/3data_with_improvement.xlsx")
# ------------------------------------------------------------------------------------- 



# ----  4번째 분석 -> [인구통계학적 특성]만을 고려하여 군집을 나누고, 어떤 영역의 향상을 원하는지를 식별
# seed를 고정한 후, 군집분석 시행
set.seed(123)
data <- read.csv("C:/Amazon/amazon_preprocessed.csv")
# ??????> data <- read.csv("D:/대학/강의/4-1/조사방법론/tf2/amazon_preprocessed.csv")

data <- subset(data, select = -Purchase_Categories_Beauty_and_Personal_Care)
data <- subset(data, select = -Purchase_Categories_Clothing_and_Fashion)
data <- subset(data, select = -Purchase_Categories_Groceries_and_Gourmet_Food)
data <- subset(data, select = -Purchase_Categories_Home_and_Kitchen)
data <- subset(data, select = -Browsing_Frequency)
data <- subset(data, select = -Purchase_Frequency)
data <- subset(data, select = -Personalized_Recommendation_Frequency_Purchase)
data <- subset(data, select = -Personalized_Recommendation_Frequency)
data <- subset(data, select = -Recommendation_Helpfulness)
data <- subset(data, select = -Shopping_Satisfaction)
data <- subset(data, select = -Improvement_Areas)
data <- subset(data, select = -X)

# 데이터의 표준화 => 성별에는 scaling을 수행하지 않으며 연령에 대하여만 수행합니다
data$age <- data.frame(scale(data$age))
data_scaled = data

# dist_eucl <- dist(data_scaled, method="euclidean")

# 마찬가지로 패키지 함수로 수정하였으며 군집수는 5가 적절한 것으로 보입니다
library(factoextra)
fviz_nbclust(data, kmeans, method = "wss")


km <- kmeans(data, centers=5, nstart=10)

# 군집 당 사이즈
km$size
# 각각의 열이 어떤 군집에 할당되었는지를 보여 준다.
km$cluster
# 각 군집 중앙값
km$centers

# dplyr 패키지 로드
library(dplyr)

# 군집 결과를 데이터 프레임에 추가.
data_clustered <- data.frame(data, Cluster = km$cluster)

# 원래의 데이터 프레임을 다시 불러와 'Improvement_Areas' 열을 추가.
original_data <- read.csv("C:/Amazon/amazon_preprocessed.csv")
data_with_improvement <- cbind(data_clustered, Improvement_Areas = original_data$Improvement_Areas)

# 각 군집 내에서 'Improvement_Areas' 값의 빈도수를 계산
improvement_freq <- data_with_improvement %>%
  group_by(Cluster, Improvement_Areas) %>%
  summarise(Frequency = n()) %>%
  arrange(Cluster, desc(Frequency))

# 각 군집 내에서 가장 빈번한 'Improvement_Areas' 값 ?
most_frequent_improvement <- improvement_freq %>%
  group_by(Cluster) %>%
  slice(which.max(Frequency))

# 결과
print(most_frequent_improvement)












# 하단의 코드는 제가 코드를 모두 해석할 수 없어 임의로 작성한 코드입니다
# 혹여나 참고하실 때 도움이 될까 하여 첨부합니다
data <- read.csv("C:/Amazon/amazon_preprocessed.csv")
set.seed(123)
head(data, 1)
unique(data$Recommendation_Helpfulness)
#========================1 Purchase Categories==================================
data <- read.csv("C:/Amazon/amazon_preprocessed.csv")
# clustering 대상 col
pur.categories = c("Purchase_Categories_Beauty_and_Personal_Care",
                   "Purchase_Categories_Clothing_and_Fashion",
                   "Purchase_Categories_Groceries_and_Gourmet_Food",
                   "Purchase_Categories_Home_and_Kitchen")
data.pur.categories = data[, pur.categories]
# interpret 대상 col
personal = c("age", "Gender_Male", "Gender_Female", "Gender_None", "Purchase_Frequency", "Shopping_Satisfaction")
data.personal = data[,personal]

library(factoextra)
fviz_nbclust(data.pur.categories, kmeans, method = "wss")

km = kmeans(data.pur.categories, centers = 5, nstart = 100)

library(cluster)
clusplot(x=data.pur.categories, clus=km$cluster, color=T, shade=T, labels=5, lines=0)

km$size
km$centers;
# km$cluster

data.analysis = data.frame(data.personal, data.pur.categories, km$cluster)

cluster_means = data.analysis %>%
  group_by(km.cluster) %>%
  summarise(across(everything(), mean, na.rm = TRUE))

cluster_means
