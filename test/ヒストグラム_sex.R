# パッケージの読み込み
library(rstan)
library(bayesplot)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#判定コード別に分類
head(file_kenshin_data)
file_bmi <- file_kenshin_data[,-1]

head(file_bmi)


sex_a1 <- file_kenshin_datasex[file_kenshin_datasex$code=="A1",]
sex_a2 <- file_kenshin_datasex[file_kenshin_datasex$code=="A2",]
sex_b <- file_kenshin_datasex[file_kenshin_datasex$code=="B",]
sex_c <- file_kenshin_datasex[file_kenshin_datasex$code=="C",]
nrow(file_kenshin_datasex)
nrow(sex_a1)+nrow(sex_a2)+nrow(sex_c)+nrow(sex_b)
#判定コードA1と年齢のヒストグラム----------------------------------------------------------------
ggplot(data = sex_a1, 
       mapping = aes(x = sex, y = ..density.., 
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
