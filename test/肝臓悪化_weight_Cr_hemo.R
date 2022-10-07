
# ダミー変数と分散分析モデル｜RとStanではじめる ベイズ統計モデリングによるデータ分析入門
# 3-6-ダミー変数と分散分析モデルより

#肝臓判定値悪化のデータに絞って分析
#目的変数(2020)：血清クレアチニン、説明変数(2020):アプリ使用＋血色素量+体重

# 分析の準備 -------------------------------------------------------------------

# パッケージの読み込み
library(rstan)
library(brms)
library(dplyr)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


#肝臓判定に関して-----------------------------------------
#NAと空白を削除
liver <- kenshin[!is.na(kenshin$肝臓_19),]
liver <- liver[!is_blank(liver$肝臓_19),]
liver <- liver[!is.na(liver$肝臓_20),]
liver <- liver[!is_blank(liver$肝臓_20),]

#判定値悪化を探す
deterA1 <- liver[liver$肝臓_19=="A1",]
deterA1 <- deterA1[deterA1$肝臓_20 != "A1",]
summary(deterA1$肝臓_19)
summary(deterA1$肝臓_20)

deterA2 <- liver[liver$肝臓_19=="A2",]
deterA2 <- deterA2[deterA2$肝臓_20 != "A1",]
deterA2 <- deterA2[deterA2$肝臓_20 != "A2",]
summary(deterA2$肝臓_19)
summary(deterA2$肝臓_20)

deterB <- liver[liver$肝臓_19 == "B",]
deterB <- deterB[deterB$肝臓_20 != "A1",]
deterB <- deterB[deterB$肝臓_20 != "A2",]
deterB <- deterB[deterB$肝臓_20 != "B",]
summary(deterB$肝臓_19)
summary(deterB$肝臓_20)

# 判定値Cを含める場合（肝臓判定値上昇してないグループの場合）
# deterC <- liver[liver$肝臓_19 == "C",]
# deterC <- deterC[deterC$肝臓_20 != "A1",]
# deterC <- deterC[deterC$肝臓_20 != "A2",]
# deterC <- deterC[deterC$肝臓_20 != "B",]
# summary(deterC$肝臓_19)
# summary(deterC$肝臓_20)

deterA1_A2 <- rbind(deterA1,deterA2)
deterA1_A2_B <- rbind(deterA1_A2,deterB)

# 判定値Cを含める場合（肝臓判定値上昇してないグループの場合）
# deterA1_A2_B <- rbind(deterA1_A2_B,deterC)

nrow(deterA1_A2_B)

# データの準備 -------------------------------------------------------------
user_id <- intersect(kenshin["No"],kenpos["No"])

#変数app_usageを追加（アプリ利用者の行にuser、不使用者の行にnon user）
deterA1_A2_B <- within(deterA1_A2_B,{
  app_usage <- "non user"
  app_usage[!(deterA1_A2_B$No %in% user_id$No)] <- "non user"
  app_usage[deterA1_A2_B$No %in% user_id$No] <- "user"
})

#factorに変換
deterA1_A2_B["app_usage"] <- as.factor(deterA1_A2_B$app_usage)
summary(deterA1_A2_B$app_usage)


#変数を抽出
#体重、血清クレアチニン(2020)、血色素量（2020）、アプリ使用有無の列を抽出
Liv_Cr_hemo_weight19 <- select(deterA1_A2_B,637,643,644,app_usage)

#変数名変更とNAを含む行の削除
colnames(Liv_Cr_hemo_weight19)[1] <- "weight"
colnames(Liv_Cr_hemo_weight19)[2] <- "Cr"
colnames(Liv_Cr_hemo_weight19)[3] <- "hemo"
Liv_Cr_hemo_weight19 <- Liv_Cr_hemo_weight19[!is.na(Liv_Cr_hemo_weight19$Cr),]
Liv_Cr_hemo_weight19 <- Liv_Cr_hemo_weight19[!is.na(Liv_Cr_hemo_weight19$hemo),]

#異常値？削除
Liv_Cr_hemo_weight19 <- Liv_Cr_hemo_weight19[!(Liv_Cr_hemo_weight19$Cr > 8),]
Liv_Cr_hemo_weight19 <- Liv_Cr_hemo_weight19[!(Liv_Cr_hemo_weight19$hemo > 20),]

# 分析対象のデータ
head(Liv_Cr_hemo_weight19,3)

# データの要約
summary(Liv_Cr_hemo_weight19)

# 図示
ggplot(data = Liv_Cr_hemo_weight19, mapping = aes(x = hemo, y = Cr)) +
  geom_point(aes(color = app_usage)) +
  labs(title = "血清クレアチニン~血中素量+体重")+
  theme_gray(base_family = "HiraginoSans-W3")
ggplot(data = Liv_Cr_hemo_weight19, mapping = aes(x = weight, y = Cr)) +
  geom_point(aes(color = app_usage)) +
  labs(title = "血清クレアチニン~血中素量+体重")+
  theme_gray(base_family = "HiraginoSans-W3")

# brmsによる分散分析モデルの推定 -------------------------------------------------------------------

# 分散分析モデルを作る
lm_brms_Liv_Cr_hemo_weight19 <- brm(
  formula = Cr ~ hemo + weight + app_usage,  # modelの構造を指定
  family = gaussian(),        # 正規分布を使う
  data = Liv_Cr_hemo_weight19,       # データ
  seed = 1,                   # 乱数の種
  prior = c(set_prior("", class = "Intercept"),
            set_prior("", class = "sigma"))
)

# MCMCの結果の確認
lm_brms_Liv_Cr_hemo_weight19

# 推定された天気別の平均売り上げのグラフ
eff <- conditional_effects(lm_brms_Liv_Cr_hemo_weight19,  effects = "weight:app_usage")
plot(eff, points = TRUE)

eff <- conditional_effects(lm_brms_Liv_Cr_hemo_weight19,  effects = "hemo:app_usage")
plot(eff, points = TRUE)

# 補足：分散分析モデルのデザイン行列 ----------------------------------------------------------

# デザイン行列の作成
formula_anova_Liv_Cr_hemo_weight19 <- formula(Cr ~ app_usage)
design_mat_Liv_Cr_hemo_weight19 <- model.matrix(formula_anova_Liv_Cr_hemo_weight19, Liv_Cr_hemo_weight19)

# stanに渡すlistの作成
data_list_Liv_Cr_hemo_weight19 <- list(
  N = nrow(Liv_Cr_hemo_weight19), # サンプルサイズ
  K = 2,                   # デザイン行列の列数
  Y = Liv_Cr_hemo_weight19$Cr, # 応答変数
  X = design_mat_Liv_Cr_hemo_weight19           # デザイン行列
)
# Stanに渡すデータの表示
data_list_Liv_Cr_hemo_weight19



# 補足：brmsを使わない分散分析モデルの推定 -----------------------------------------------------

# rstanで分散分析モデルを実行
anova_stan_Liv_Cr_hemo_weight19 <- stan(
  file = "3-4-1-lm-design-matrix.stan",
  data = data_list_Liv_Cr_hemo_weight19,
  seed = 1
)

# 結果の確認
print(anova_stan_Liv_Cr_hemo_weight19, probs = c(0.025, 0.5, 0.975))

#anova_brms_Liv_Cr_hemo_weight19
