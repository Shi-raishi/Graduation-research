# パッケージの読み込み
library(rstan)
library(bayesplot)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# 歩数列と体重列を抽出、manualとRenobodyの２種類に統一
data_frame_na <- kenpos[,5:7]
data_frame <- data_frame_na[!is.na(data_frame_na$体重),]
data_frame3 <- data_frame[!is.na(data_frame$歩数),]
data_frame3[2] <- lapply(data_frame3[2],gsub, pattern="手入力", replacement="manual")
data_frame3[2] <- lapply(data_frame3[2],gsub, pattern="アプリ", replacement="Renobody")
head(data_frame3)

# 区別
hosu_manual <- data_frame3[data_frame3$歩数入力元=="manual",]
hosu_app <- data_frame3[data_frame3$歩数入力元=="Renobody",]

# 歩数入力元列を削除
hosu_manual2 <- hosu_manual[,-2]
hosu_app2 <- hosu_app[,-2]

# 列名を英語表記に変更
colnames(hosu_manual2)[1] <- "steps"
colnames(hosu_app2)[1] <- "steps"
colnames(hosu_manual2)[2] <- "weight"
colnames(hosu_app2)[2] <- "weight"

hosuapp2_size <- nrow(hosu_app2)
hosumanu2_size <- nrow(hosu_manual2)


#以降参考書の方法
# 先にRenobody--------------------------------------------------

#図示
ggplot(hosu_app2, aes(x = weight, y = steps))+
  geom_point() +
  labs(title = "weight and steps(app)")

# listにまとめる
hosu_app2_list <- list(
  N = hosuapp2_size,
  steps = hosu_app2$steps, 
  weight = hosu_app2$weight
)
# 乱数の生成
mcmc_result_app <- stan(
  file = "単回帰test2.stan",
  data = hosu_app2_list,
  seed = 1
)
# 結果の表示
print(mcmc_result_app,  probs = c(0.025, 0.5, 0.975))
# MCMCサンプルの抽出
mcmc_sample_app <- rstan::extract(mcmc_result_app, permuted = FALSE)


# 事後分布の図示 

# トレースプロットと事後分布
mcmc_combo(
  mcmc_sample_app, 
  pars = c("Intercept", "beta", "sigma")
)


# 以降manual----------------------------------------


ggplot(hosu_manual2, aes(x = weight, y = steps))+
  geom_point() +
  labs(title = "weight and steps(manual)")

# listにまとめる
hosu_manu2_list <- list(
  N = hosumanu2_size,
  steps = hosu_manual2$steps, 
  weight = hosu_manual2$weight
)
# 乱数の生成
mcmc_result_manual <- stan(
  file = "単回帰test2.stan",
  data = hosu_manu2_list,
  seed = 1
)
# 結果の表示
print(mcmc_result_manual,  probs = c(0.025, 0.5, 0.975))
# MCMCサンプルの抽出
mcmc_sample <- rstan::extract(mcmc_result, permuted = FALSE)


# 事後分布の図示 

# トレースプロットと事後分布
mcmc_combo(
  mcmc_sample, 
  pars = c("Intercept", "beta", "sigma")
)
