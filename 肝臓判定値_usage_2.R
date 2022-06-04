
# データ <- kenshinデータ
# 調整変数 <- app_usage:アプリ使用の有無
# 説明変数を2019年のものに絞る

# 分析の準備 

#変化率を計算する関数
calc_change_rate <- function(x,y){
  return ((y-x)/x*100)
}

# パッケージの読み込み
library(bayesplot)
library(rstan)
library(brms)
library(dplyr)
library(tidyr)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# app_usageを追加 --------------------------------------------------------------

# kenshinとkenposの共通IDを取得
user_id <- intersect(kenshin["No"],kenpos["No"])

#変数app_usageを追加（アプリ利用者の行にuser、不使用者の行にnon user）
kenshin_usage <- within(kenshin,{
  app_usage <- "non user"
  app_usage[!(kenshin$No %in% user_id$No)] <- "non user"
  app_usage[kenshin$No %in% user_id$No] <- "user"
})

#factorに変換
kenshin_usage["app_usage"] <- as.factor(kenshin_usage$app_usage)
summary(kenshin_usage$app_usage)

#kenshinの肝臓判定値,体重に欠測値を含む行を削除----
data_liver <- kenshin_usage[!is.na(kenshin_usage$肝臓_19),]
data_liver <- data_liver[!is_blank(data_liver$肝臓_19),]
data_liver <- data_liver[!is.na(data_liver$肝臓_20),]
data_liver <- data_liver[!is_blank(data_liver$肝臓_20),]

data_liver <- data_liver[!is.na(data_liver$体重-19),]
data_liver <- data_liver[!is_blank(data_liver$体重_19),]
data_liver <- data_liver[!is.na(data_liver$体重_20),]
data_liver <- data_liver[!is_blank(data_liver$体重_20),]

#変数：体重の変化率を作成--------------------------------------------------
data_liver["weight_change_rate"] <- calc_change_rate(data_liver$体重_19,data_liver$体重_20)

# 肝臓判定値悪化のデータを取得-----------------------------------------
liver_deterA1 <- data_liver[data_liver$肝臓_19=="A1",]
liver_deterA1 <- liver_deterA1[liver_deterA1$肝臓_20 != "A1",]

liver_deterA2 <- data_liver[data_liver$肝臓_19=="A2",]
liver_deterA2 <- liver_deterA2[liver_deterA2$肝臓_20 != "A1",]
liver_deterA2 <- liver_deterA2[liver_deterA2$肝臓_20 != "A2",]

liver_deterB <- data_liver[data_liver$肝臓_19 == "B",]
liver_deterB <- liver_deterB[liver_deterB$肝臓_20 != "A1",]
liver_deterB <- liver_deterB[liver_deterB$肝臓_20 != "A2",]
liver_deterB <- liver_deterB[liver_deterB$肝臓_20 != "B",]

liver_deter_all <- rbind(liver_deterA1,liver_deterA2)
liver_deter_all <- rbind(liver_deter_all,liver_deterB)

# 肝臓判定値維持のデータを取得----
liver_keepA1 <- data_liver[data_liver$肝臓_19 == "A1",]
liver_keepA1 <- liver_keepA1[liver_keepA1$肝臓_20 == "A1",]

liver_keepA2 <- data_liver[data_liver$肝臓_19 == "A2",]
liver_keepA2 <- liver_keepA2[liver_keepA2$肝臓_20 == "A2",]

liver_keepB <- data_liver[data_liver$肝臓_19 == "B",]
liver_keepB <- liver_keepB[liver_keepB$肝臓_20 == "B",]

liver_keepC <- data_liver[data_liver$肝臓_19 == "C",]
liver_keepC <- liver_keepC[liver_keepC$肝臓_20 == "C",]

liver_keep_all <- rbind(liver_keepA1,liver_keepA2)
liver_keep_all <- rbind(liver_keep_all,liver_keepB)
liver_keep_all <- rbind(liver_keep_all,liver_keepC)

# 肝臓判定値改善----
liver_impA1 <- data_liver[data_liver$肝臓_19=="C",]
liver_impA1 <- liver_impA1[liver_impA1$肝臓_20 != "C",]
summary(liver_impA1$肝臓_19)
summary(liver_impA1$肝臓_20)

liver_impA2 <- data_liver[data_liver$肝臓_19=="B",]
liver_impA2 <- liver_impA2[liver_impA2$肝臓_20 != "B",]
liver_impA2 <- liver_impA2[liver_impA2$肝臓_20 != "C",]
summary(liver_impA2$肝臓_19)
summary(liver_impA2$肝臓_20)

liver_impB <- data_liver[data_liver$肝臓_19 == "A2",]
liver_impB <- liver_impB[liver_impB$肝臓_20 != "A2",]
liver_impB <- liver_impB[liver_impB$肝臓_20 != "B",]
liver_impB <- liver_impB[liver_impB$肝臓_20 != "C",]
summary(liver_impB$肝臓_19)
summary(liver_impB$肝臓_20)

liver_imp_all <- rbind(liver_impA1,liver_impA2)
liver_imp_all <- rbind(liver_imp_all,liver_impB)

# それぞれのデータ数----
nrow(liver_imp_all)
nrow(liver_keep_all)
nrow(liver_deter_all)
nrow(data_liver)
#グループ全体の体重変化率のヒストグラム(n=2081,user=134)----
nrow(data_liver[data_liver$app_usage=="user",])
ggplot(data_liver, aes(x = weight_change_rate, fill = app_usage,color = app_usage)) +                       # Draw overlaying histogram
  geom_histogram(position = "identity", alpha = 0.2, bins = 50)+
  labs(title = "肝臓判定グループ(n=2081)")+
  theme_gray(base_family = "HiraginoSans-W3")

ggplot(liver_imp_all, aes(x = weight_change_rate, fill = app_usage,color = app_usage)) +                       # Draw overlaying histogram
  geom_histogram(position = "identity", alpha = 0.2, bins = 50)+
  labs(title = "肝臓判定改善")+
  theme_gray(base_family = "HiraginoSans-W3")

ggplot(liver_keep_all, aes(x = weight_change_rate, fill = app_usage,color = app_usage)) +                       # Draw overlaying histogram
  geom_histogram(position = "identity", alpha = 0.2, bins = 50)+
  labs(title = "肝臓判定維持")+
  theme_gray(base_family = "HiraginoSans-W3")

ggplot(liver_deter_all, aes(x = weight_change_rate, fill = app_usage,color = app_usage)) +                       # Draw overlaying histogram
  geom_histogram(position = "identity", alpha = 0.2, bins = 50)+
  labs(title = "肝臓判定悪化")+
  theme_gray(base_family = "HiraginoSans-W3")
# 変数の抽出----
wide_varis_liver2 <- select(data_liver,1,"age19" = 4,
                              "BMI19" = 6,"code19" = 9,"cigarettes19" = 91,
                              "alcho19" = 94,"vege19" = 97,"weight19" = 252,app_usage,weight_change_rate)
wide_varis_livdeter2 <- select(liver_deter_all,1,"age19" = 4,
                              "BMI19" = 6,"code19" = 9,"cigarettes19" = 91,
                              "alcho19" = 94,"vege19" = 97,app_usage,weight_change_rate)
wide_varis_livkeep2 <- select(liver_keep_all,1,"age19" = 4,
                             "BMI19" = 6,"code19" = 9,"cigarettes19" = 91,
                             "alcho19" = 94,"vege19" = 97,app_usage,weight_change_rate)
wide_varis_livimp2 <- select(liver_imp_all,1,"age19" = 4,
                            "BMI19" = 6,"code19" = 9,"cigarettes19" = 91,
                            "alcho19" = 94,"vege19" = 97,app_usage,weight_change_rate)

# 空白とNAを含む行の削除----
#全体
wide_varis_liver2 <- wide_varis_liver2[!is_blank(wide_varis_liver2$code19),]
wide_varis_liver2 <- wide_varis_liver2[!is_blank(wide_varis_liver2$cigarettes19),]
wide_varis_liver2 <- wide_varis_liver2[!is_blank(wide_varis_liver2$alcho19),]
wide_varis_liver2 <- wide_varis_liver2[!is_blank(wide_varis_liver2$vege19),]

wide_varis_liver2 <- wide_varis_liver2 %>% drop_na(everything())

wide_varis_liver2$alcho19 <- sub("１合未満","0~1",wide_varis_liver2$alcho19)
wide_varis_liver2$alcho19 <- sub("１～２合未満","1~2",wide_varis_liver2$alcho19)
wide_varis_liver2$alcho19 <- sub("２～３未満","2~3",wide_varis_liver2$alcho19)
wide_varis_liver2$alcho19 <- sub("３合以上","3~",wide_varis_liver2$alcho19)

wide_varis_liver2$cigarettes19 <- sub("10本以内","0~10",wide_varis_liver2$cigarettes19)
wide_varis_liver2$cigarettes19 <- sub("11～20本","11~20",wide_varis_liver2$cigarettes19)
wide_varis_liver2$cigarettes19 <- sub("21～30本","21~30",wide_varis_liver2$cigarettes19)
wide_varis_liver2$cigarettes19 <- sub("31～40本","31~40",wide_varis_liver2$cigarettes19)
wide_varis_liver2$cigarettes19 <- sub("41本以上","40~",wide_varis_liver2$cigarettes19)

wide_varis_liver2$vege19 <- sub("なし","no",wide_varis_liver2$vege19)
wide_varis_liver2$vege19 <- sub("あり","yes",wide_varis_liver2$vege19)

wide_varis_liver2$alcho19 <- as.factor(wide_varis_liver2$alcho19)
wide_varis_liver2$cigarettes19 <- as.factor(wide_varis_liver2$cigarettes19)
wide_varis_liver2$vege19 <- as.factor(wide_varis_liver2$vege19)


# 判定悪化グループ
wide_varis_livdeter2 <- wide_varis_livdeter2[!is_blank(wide_varis_livdeter2$code19),]
wide_varis_livdeter2 <- wide_varis_livdeter2[!is_blank(wide_varis_livdeter2$cigarettes19),]
wide_varis_livdeter2 <- wide_varis_livdeter2[!is_blank(wide_varis_livdeter2$alcho19),]
wide_varis_livdeter2 <- wide_varis_livdeter2[!is_blank(wide_varis_livdeter2$vege19),]

wide_varis_livdeter2 <- wide_varis_livdeter2 %>% drop_na(everything())


# 維持グループ
wide_varis_livkeep2 <- wide_varis_livkeep2[!is_blank(wide_varis_livkeep2$code19),]
wide_varis_livkeep2 <- wide_varis_livkeep2[!is_blank(wide_varis_livkeep2$cigarettes19),]
wide_varis_livkeep2 <- wide_varis_livkeep2[!is_blank(wide_varis_livkeep2$alcho19),]
wide_varis_livkeep2 <- wide_varis_livkeep2[!is_blank(wide_varis_livkeep2$vege19),]

wide_varis_livkeep2 <- wide_varis_livkeep2 %>% drop_na(everything())


# 改善グループ
wide_varis_livimp2 <- wide_varis_livimp2[!is_blank(wide_varis_livimp2$code19),]
wide_varis_livimp2 <- wide_varis_livimp2[!is_blank(wide_varis_livimp2$cigarettes19),]
wide_varis_livimp2 <- wide_varis_livimp2[!is_blank(wide_varis_livimp2$alcho19),]
wide_varis_livimp2 <- wide_varis_livimp2[!is_blank(wide_varis_livimp2$vege19),]

wide_varis_livimp2 <- wide_varis_livimp2 %>% drop_na(everything())

# 変数を絞った肝臓グループ全体(n=558)----
nrow(wide_varis_livimp2)
nrow(wide_varis_livkeep2)
nrow(wide_varis_livdeter2)
nrow(wide_varis_liver2)

# 変数を絞った後の図示(n=558, user=40)------------------------------------------
nrow(wide_varis_liver2[wide_varis_liver2$app_usage=="user",])
ggplot(wide_varis_liver2, aes(x = weight_change_rate, fill = app_usage,color = app_usage)) +                       # Draw overlaying histogram
  geom_histogram(position = "identity", alpha = 0.2, bins = 50)+
  theme_gray(base_family = "HiraginoSans-W3")

ggplot(wide_varis_livimp2, aes(x = weight_change_rate, fill = app_usage,color = app_usage)) +                       # Draw overlaying histogram
  geom_histogram(position = "identity", alpha = 0.2, bins = 50)+
  labs(title = "肝臓判定改善")+
  theme_gray(base_family = "HiraginoSans-W3")

ggplot(wide_varis_livkeep2, aes(x = weight_change_rate, fill = app_usage,color = app_usage)) +                       # Draw overlaying histogram
  geom_histogram(position = "identity", alpha = 0.2, bins = 50)+
  labs(title = "肝臓判定維持")+
  theme_gray(base_family = "HiraginoSans-W3")

ggplot(wide_varis_livdeter2, aes(x = weight_change_rate, fill = app_usage,color = app_usage)) +                       # Draw overlaying histogram
  geom_histogram(position = "identity", alpha = 0.2, bins = 50)+
  labs(title = "肝臓判定悪化")+
  theme_gray(base_family = "HiraginoSans-W3")

# 分析対象のデータ----
# データの要約
summary(wide_varis_livdeter2)
summary(wide_varis_livkeep2)
summary(wide_varis_livimp2)
summary(wide_varis_liver2)

# brmsによる分散分析モデルの推定 -------------------------------------------------------------------
# 分散分析モデルを作る(女性数が０のため性別を説明変数から削除)
lm_brms_wide_varis_liver2 <- brm(
  formula = weight_change_rate ~ app_usage + BMI19 + code19 + cigarettes19 + alcho19 + vege19 + age19,  # modelの構造を指定
  family = gaussian(),        # 正規分布を使う
  data = wide_varis_liver2,       # データ
  seed = 1,                   # 乱数の種
  prior = c(set_prior("", class = "Intercept"),
            set_prior("", class = "sigma"))
)
lm_brms_wide_varis_livdeter2 <- brm(
  formula = weight_change_rate ~ app_usage + BMI19 + code19 + cigarettes19 + alcho19 + vege19 + age19,  # modelの構造を指定
  family = gaussian(),        # 正規分布を使う
  data = wide_varis_livdeter2,       # データ
  seed = 1,                   # 乱数の種
  prior = c(set_prior("", class = "Intercept"),
            set_prior("", class = "sigma"))
)
lm_brms_wide_varis_livkeep2 <- brm(
  formula = weight_change_rate ~ app_usage + BMI19 + code19 + cigarettes19 + alcho19 + vege19 + age19,  # modelの構造を指定
  family = gaussian(),        # 正規分布を使う
  data = wide_varis_livkeep2,       # データ
  seed = 1,                   # 乱数の種
  prior = c(set_prior("", class = "Intercept"),
            set_prior("", class = "sigma"))
)
lm_brms_wide_varis_livimp2 <- brm(
  formula = weight_change_rate ~ app_usage + BMI19 + code19 + cigarettes19 + alcho19 + vege19 + age19,  # modelの構造を指定
  family = gaussian(),        # 正規分布を使う
  data = wide_varis_livimp2,       # データ
  seed = 1,                   # 乱数の種
  prior = c(set_prior("", class = "Intercept"),
            set_prior("", class = "sigma"))
)

# MCMCの結果の確認----
lm_brms_wide_varis_liver2
lm_brms_wide_varis_livimp2
lm_brms_wide_varis_livkeep2
lm_brms_wide_varis_livdeter2

#可視化

eff <- conditional_effects(lm_brms_wide_varis_liver2,effects = "app_usage")
plot(eff, points = F)

plot(lm_brms_wide_varis_liver2)

mcmc_plot(lm_brms_wide_varis_liver2)

# brms implement stan 
stancode(lm_brms_wide_varis_liver2)

brms_stan_liv <- stan(
  file = "brms_stan1.stan",
  data = wide_varis_liver2,
  seed = 1
)








# n=558のうち体重減少のグループ(n=241,user:nonuser=23:218)、体重増加グループ(n=317,user:nonuser=17:300)で分析------
wide_varis_liver2_weight_loss <-  wide_varis_liver2[wide_varis_liver2$weight_change_rate<0,]
wide_varis_liver2_weight_gain <-  wide_varis_liver2[!(wide_varis_liver2$weight_change_rate<0),]

# データの確認
summary(wide_varis_liver2_weight_loss)
summary(wide_varis_liver2_weight_gain)

lm_brms_wide_varis_liver2_weight_loss <- brm(
  formula = weight_change_rate ~ app_usage + BMI19 + code19 + cigarettes19 + alcho19 + vege19 + age19,  # modelの構造を指定
  family = gaussian(),        # 正規分布を使う
  data = wide_varis_liver2_weight_loss,       # データ
  seed = 1,                   # 乱数の種
  prior = c(set_prior("", class = "Intercept"),
            set_prior("", class = "sigma"))
)
lm_brms_wide_varis_liver2_weight_gain <- brm(
  formula = weight_change_rate ~ app_usage + BMI19 + code19 + cigarettes19 + alcho19 + vege19 + age19,  # modelの構造を指定
  family = gaussian(),        # 正規分布を使う
  data = wide_varis_liver2_weight_gain,       # データ
  seed = 1,                   # 乱数の種
  prior = c(set_prior("", class = "Intercept"),
            set_prior("", class = "sigma"))
)

# 結果の表示------

# 減量者の計算結果とグラフ
lm_brms_wide_varis_liver2_weight_loss
ggplot(wide_varis_liver2_weight_loss, aes(x = weight_change_rate, fill = app_usage,color = app_usage)) +                       # Draw overlaying histogram
  geom_histogram(position = "identity", alpha = 0.2, bins = 50)+
  labs(title = "肝臓判定グループ＿変数限定＿体重減少者")+
  theme_gray(base_family = "HiraginoSans-W3")

# 増量者の計算結果とグラフ
lm_brms_wide_varis_liver2_weight_gain
ggplot(wide_varis_liver2_weight_gain, aes(x = weight_change_rate, fill = app_usage,color = app_usage)) +                       # Draw overlaying histogram
  geom_histogram(position = "identity", alpha = 0.2, bins = 50)+
  labs(title = "肝臓判定グループ＿変数限定＿体重増加者")+
  theme_gray(base_family = "HiraginoSans-W3")

#可視化したい
#減量者
eff <- conditional_effects(lm_brms_wide_varis_liver2_weight_loss,effects = "app_usage")
plot(eff, points = F)

plot(lm_brms_wide_varis_liver2_weight_loss)

mcmc_plot(lm_brms_wide_varis_liver2_weight_loss)
#増量者　----
eff <- conditional_effects(lm_brms_wide_varis_liver2_weight_gain,effects = "app_usage")
plot(eff, points = F)

plot(lm_brms_wide_varis_liver2_weight_gain)

mcmc_plot(lm_brms_wide_varis_liver2_weight_gain)










wide_varis_livimp2
# n=558のうち肝臓判定値改善者の体重減少のグループ(n=58,user:nonuser=6:52)、
wide_varis_livimp2_weight_loss <-  wide_varis_livimp2[wide_varis_livimp2$weight_change_rate<0,]

# データの確認
summary(wide_varis_livimp2_weight_loss)

lm_brms_wide_varis_livimp2_weight_loss <- brm(
  formula = weight_change_rate ~ app_usage + BMI19 + code19 + cigarettes19 + alcho19 + vege19 + age19,  # modelの構造を指定
  family = gaussian(),        # 正規分布を使う
  data = wide_varis_livimp2_weight_loss,       # データ
  seed = 1,                   # 乱数の種
  prior = c(set_prior("", class = "Intercept"),
            set_prior("", class = "sigma"))
)


# 結果の表示------

# 減量者の計算結果とグラフ
lm_brms_wide_varis_livimp2_weight_loss
ggplot(wide_varis_livimp2_weight_loss, aes(x = weight_change_rate, fill = app_usage,color = app_usage)) +                       # Draw overlaying histogram
  geom_histogram(position = "identity", alpha = 0.2, bins = 50)+
  labs(title = "肝臓判定グループ＿変数限定＿体重減少者")+
  theme_gray(base_family = "HiraginoSans-W3")

#可視化したい
eff <- conditional_effects(lm_brms_wide_varis_liver2_weight_loss,effects = "app_usage")
plot(eff, points = F)






