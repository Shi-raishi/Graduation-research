# パッケージの読み込み
library(rstan)
library(bayesplot)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#判定コード別に分類
bmi_a1 <- file_bmi[file_bmi$code=="A1",]
bmi_a2 <- file_bmi[file_bmi$code=="A2",]
bmi_b <- file_bmi[file_bmi$code=="B",]
bmi_c <- file_bmi[file_bmi$code=="C",]
nrow(file_bmi)
nrow(bmi_a1)+nrow(bmi_a2)+nrow(bmi_c)+nrow(bmi_b)
#判定コードA1と年齢のヒストグラム----------------------------------------------------------------
ggplot(data = bmi_a1, 
       mapping = aes(x = BMI, y = ..density.., 
                     color = code, fill = code)) +
  geom_histogram(alpha = 0.5, position = "identity")+
  geom_density(alpha = 0.5, size = 0)
#判定コードA2とBMIのヒストグラム----------------------------------------------------------------
ggplot(data = bmi_a2, 
       mapping = aes(x = BMI, y = ..density.., 
                     color = code, fill = code)) +
  geom_histogram(alpha = 0.5, position = "identity")+
  geom_density(alpha = 0.5, size = 0)
#判定コードBとBMIのヒストグラム----------------------------------------------------------------
ggplot(data = bmi_b, 
       mapping = aes(x = BMI, y = ..density.., 
                     color = code, fill = code)) +
  geom_histogram(alpha = 0.5, position = "identity")+
  geom_density(alpha = 0.5, size = 0)
#判定コードCとBMIのヒストグラム----------------------------------------------------------------
ggplot(data = bmi_c, 
       mapping = aes(x = BMI, y = ..density.., 
                     color = code, fill = code)) +
  geom_histogram(alpha = 0.5, position = "identity")+
  geom_density(alpha = 0.5, size = 0)

#判定コード全てBMI齢のヒストグラム----------------------------------------------------------------
ggplot(data = file_bmi, 
       mapping = aes(x = BMI, y = ..density.., 
                     color = code, fill = code)) +
  geom_histogram(alpha = 0.5, position = "identity")+
  geom_density(alpha = 0.5, size = 0)
