# パッケージの読み込み
library(rstan)
library(bayesplot)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#判定コード別に分類
age_a1 <- file_kenshin_dataage[file_kenshin_dataage$code=="A1",]
age_a2 <- file_kenshin_dataage[file_kenshin_dataage$code=="A2",]
age_b <- file_kenshin_dataage[file_kenshin_dataage$code=="B",]
age_c <- file_kenshin_dataage[file_kenshin_dataage$code=="C",]
nrow(file_kenshin_dataage)
nrow(age_a1)+nrow(age_a2)+nrow(age_c)+nrow(age_b)
#判定コードA1と年齢のヒストグラム----------------------------------------------------------------
ggplot(data = age_a1, 
       mapping = aes(x = age, y = ..density.., 
                     color = code, fill = code)) +
  geom_histogram(alpha = 0.5, position = "identity")+
  geom_density(alpha = 0.5, size = 0)
#判定コードA2と年齢のヒストグラム----------------------------------------------------------------
ggplot(data = age_a2, 
       mapping = aes(x = age, y = ..density.., 
                     color = code, fill = code)) +
  geom_histogram(alpha = 0.5, position = "identity")+
  geom_density(alpha = 0.5, size = 0)
#判定コードBと年齢のヒストグラム----------------------------------------------------------------
ggplot(data = age_b, 
       mapping = aes(x = age, y = ..density.., 
                     color = code, fill = code)) +
  geom_histogram(alpha = 0.5, position = "identity")+
  geom_density(alpha = 0.5, size = 0)
#判定コードCと年齢のヒストグラム----------------------------------------------------------------
ggplot(data = age_c, 
       mapping = aes(x = age, y = ..density.., 
                     color = code, fill = code)) +
  geom_histogram(alpha = 0.5, position = "identity")+
  geom_density(alpha = 0.5, size = 0)

#判定コード全てと年齢のヒストグラム----------------------------------------------------------------
ggplot(data = file_kenshin_dataage, 
       mapping = aes(x = age, y = ..density.., 
                     color = code, fill = code)) +
  geom_histogram(alpha = 0.5, position = "identity")+
  geom_density(alpha = 0.5, size = 0)


temp_liver <- select(wide_varis_liver,weight_change_rate)
ggplot(data = temp_liver, 
       mapping = aes(x = weight_change_rate, y = ..density.., )) +
  geom_histogram(alpha = 0.5, position = "identity")+
  geom_density(alpha = 0.5, size = 0)
