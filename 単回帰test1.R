# パッケージの読み込み
library(rstan)
library(bayesplot)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#歩数列の抽出と歩数列のNA除去
hosu_na <- kenpos[,5]
hosu <- hosu_na[!is.na(hosu_na$歩数),]
size_hosu <- nrow(hosu)

# is.na()で空データを削除できないので、is_blank関数
# 歩数入力元の列の抽出の空白の除去
how_hosu_na <- kenpos[,6]
is_blank <- function(x) {is.na(x) | x == ""}
how_hosu <- how_hosu_na[!is_blank(how_hosu_na$歩数入力元),]

# 歩数入力元の手入力をmanual,アプリをRenobodyの置き換え
how_hosu[1] <-lapply(how_hosu[1],gsub, pattern="手入力", replacement="manual")
how_hosu[1] <-lapply(how_hosu[1],gsub, pattern="アプリ", replacement="Renobody")
size_how_hosu <- nrow(how_hosu)

# 体重列の抽出とNAの除去
taiju_na <- kenpos[,7]
taiju <- taiju_na[!is.na(taiju$体重),]
size_taiju <- nrow(taiju)

# 体重入力元列の抽出と空白の除去
how_taiju_na <- kenpos[,9]
how_taiju <- how_taiju_na[!is_blank(how_taiju_na$体重入力元),]
size_how_taiju <- nrow(how_taiju)

# サイズ確認
size_hosu
size_how_hosu
size_taiju
size_how_taiju

#　kenposから歩数、歩数入力元、体重列を抽出
df <- kenpos[,5:7]
# 歩数入力元列を削除
df2 <- df[,-2]
# 歩数列にNAのある行を削除
df3 <-df2[!is.na(df2$歩数),]
# 体重列に空データのある行を削除
df4 <-df3[!is_blank(df3$体重),]
df4_size <- nrow(df4)

# 図示
ggplot(df4, aes(x = 体重, y = 歩数))+
  geom_point() +
  labs(title = "体重と歩数の関係")

#　df4のデータサイズが大きくmcmcが終わらないのでデータサイズを前半の半分に
df5 <- df4[1:50000,]
df5_size <- nrow(df5)

# 図示
ggplot(df5, aes(x = 体重, y = 歩数))+
  geom_point() +
  labs(title = "体重と歩数の関係")

#以降参考書の方法

# listにまとめる
kenpos_data_list <- list(
  N = df5_size,
  hosu = df5$歩数, 
  taiju = df5$体重
)
# 乱数の生成
mcmc_result <- stan(
  file = "単回帰test1.stan",
  data = kenpos_data_list,
  seed = 1
)
# 結果の表示
print(mcmc_result,  probs = c(0.025, 0.5, 0.975))
# MCMCサンプルの抽出
mcmc_sample <- rstan::extract(mcmc_result, permuted = FALSE)


# 事後分布の図示 ----------------------------------------------------

# トレースプロットと事後分布
mcmc_combo(
  mcmc_sample, 
  pars = c("Intercept", "beta", "sigma")
)


