# パッケージの読み込み
library(rstan)
library(brms)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# NA削除-------------------------------------------
is_blank <- function(x) {is.na(x) | x == ""}
steps_method <- kenpos[,5:6]
steps_method2 <- steps_method[!is.na(steps_method$歩数),]
steps_method3 <- steps_method2[!is_blank(steps_method2$歩数入力元),]

#統一
steps_method3[2] <- lapply(steps_method3[2],gsub,  pattern="手入力", replacement="manual")
steps_method3[2] <- lapply(steps_method3[2],gsub,  pattern="アプリ", replacement="Renobody")
steps_method3[2] <- lapply(steps_method3[2],gsub,  pattern="Renobody", replacement="renobody")

colnames(steps_method3)[1] <- "steps"
colnames(steps_method3)[2] <- "method"

# データの要約
summary(steps_method3)

# 図示
ggplot(data = steps_method3, mapping = aes(x = method, y = steps)) +
  geom_violin() +
  geom_point(aes(color = method)) +
  labs(title = "steps and method")

# brmsによる分散分析モデルの推定 -------------------------------------------------------------------

# 分散分析モデルを作る
anova_brms <- brm(
  formula = steps ~ method,  # modelの構造を指定
  family = gaussian(),        # 正規分布を使う
  data = steps_method3,       # データ
  seed = 1,                   # 乱数の種
  prior = c(set_prior("", class = "Intercept"),
            set_prior("", class = "sigma"))
)

# MCMCの結果の確認
anova_brms

# 推定された天気別の平均売り上げのグラフ
eff <- conditional_effects()
plot(eff, points = FALSE)
