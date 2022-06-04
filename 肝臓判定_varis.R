
# ダミー変数と分散分析モデル｜RとStanではじめる ベイズ統計モデリングによるデータ分析入門
# 3-6-ダミー変数と分散分析モデルより

#目的変数：体重、説明変数：アプリ使用状況＋12個

# 分析の準備 -------------------------------------------------------------------

# パッケージの読み込み
library(rstan)
library(brms)
library(dplyr)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# 使用者IDデータの準備 -------------------------------------------------------------
user_id <- intersect(kenshin["No"],kenpos["No"])

#変数app_usageを追加（アプリ利用者の行にuser、不使用者の行にnon user）
data_kenshin <- within(kenshin,{
  app_usage <- "non user"
  app_usage[!(kenshin$No %in% user_id$No)] <- "non user"
  app_usage[kenshin$No %in% user_id$No] <- "user"
})

#factorに変換
data_kenshin["app_usage"] <- as.factor(data_kenshin$app_usage)

#肝臓判定値別に分類-----------------------------------------
#NAと空白を削除
data_liver <- data_kenshin[!is.na(data_kenshin$肝臓_19),]
data_liver <- data_liver[!is_blank(data_liver$肝臓_19),]
data_liver <- data_liver[!is.na(data_liver$肝臓_20),]
data_liver <- data_liver[!is_blank(data_liver$肝臓_20),]

#判定値悪化
liver_deterA1 <- data_liver[data_liver$肝臓_19=="A1",]
liver_deterA1 <- liver_deterA1[liver_deterA1$肝臓_20 != "A1",]
summary(liver_deterA1$肝臓_19)
summary(liver_deterA1$肝臓_20)

liver_deterA2 <- data_liver[data_liver$肝臓_19=="A2",]
liver_deterA2 <- liver_deterA2[liver_deterA2$肝臓_20 != "A1",]
liver_deterA2 <- liver_deterA2[liver_deterA2$肝臓_20 != "A2",]
summary(liver_deterA2$肝臓_19)
summary(liver_deterA2$肝臓_20)

liver_deterB <- data_liver[data_liver$肝臓_19 == "B",]
liver_deterB <- liver_deterB[liver_deterB$肝臓_20 != "A1",]
liver_deterB <- liver_deterB[liver_deterB$肝臓_20 != "A2",]
liver_deterB <- liver_deterB[liver_deterB$肝臓_20 != "B",]
summary(liver_deterB$肝臓_19)
summary(liver_deterB$肝臓_20)

liver_deter_all <- rbind(liver_deterA1,liver_deterA2)
liver_deter_all <- rbind(liver_deter_all,liver_deterB)
nrow(liver_deter_all)

# 肝臓判定値維持
liver_keepA1 <- data_liver[data_liver$肝臓_19 == "A1",]
liver_keepA1 <- liver_keepA1[liver_keepA1$肝臓_20 == "A1",]
summary(liver_keepA1$肝臓_19)
summary(liver_keepA1$肝臓_20)

liver_keepA2 <- data_liver[data_liver$肝臓_19 == "A2",]
liver_keepA2 <- liver_keepA2[liver_keepA2$肝臓_20 == "A2",]
summary(liver_keepA2$肝臓_19)
summary(liver_keepA2$肝臓_20)

liver_keepB <- data_liver[data_liver$肝臓_19 == "B",]
liver_keepB <- liver_keepB[liver_keepB$肝臓_20 == "B",]
summary(liver_keepB$肝臓_19)
summary(liver_keepB$肝臓_20)

liver_keepC <- data_liver[data_liver$肝臓_19 == "C",]
liver_keepC <- liver_keepC[liver_keepC$肝臓_20 == "C",]
summary(liver_keepC$肝臓_19)
summary(liver_keepC$肝臓_20)

liver_keep_all <- rbind(liver_keepA1,liver_keepA2)
liver_keep_all <- rbind(liver_keep_all,liver_keepB)
liver_keep_all <- rbind(liver_keep_all,liver_keepC)
nrow(liver_keep_all)

# 肝臓判定値改善
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
nrow(liver_imp_all)

#変数の抽出----
wide_varis_livdeter <- select(liver_deter_all,"sex" = 2,"age" = 4,
                              "BMI19" = 6,"code19" = 9,"cigarettes19" = 91,
                              "alcho19" = 94,"vege19" = 97,"conscious" = 166,
                              "weight19" = 252,"BMI20" = 391,"code20" = 394,
                              "cigarettes20"= 476,"alcho20" = 479,"vege20" = 482,
                              "weight20" = 637,app_usage)
wide_varis_livkeep <- select(liver_keep_all,"sex" = 2,"age" = 4,
                              "BMI19" = 6,"code19" = 9,"cigarettes19" = 91,
                              "alcho19" = 94,"vege19" = 97,"conscious" = 166,
                              "weight19" = 252,"BMI20" = 391,"code20" = 394,
                              "cigarettes20"= 476,"alcho20" = 479,"vege20" = 482,
                              "weight20" = 637,app_usage)
wide_varis_livimp <- select(liver_imp_all,"sex" = 2,"age" = 4,
                              "BMI19" = 6,"code19" = 9,"cigarettes19" = 91,
                              "alcho19" = 94,"vege19" = 97,"conscious" = 166,
                              "weight19" = 252,"BMI20" = 391,"code20" = 394,
                              "cigarettes20"= 476,"alcho20" = 479,"vege20" = 482,
                              "weight20" = 637,app_usage)

# NAを含む行の削除----
# 判定悪化グループ
wide_varis_livdeter <- wide_varis_livdeter[!is_blank(wide_varis_livdeter$conscious),]
wide_varis_livdeter <- wide_varis_livdeter[!is_blank(wide_varis_livdeter$code19),]
wide_varis_livdeter <- wide_varis_livdeter[!is_blank(wide_varis_livdeter$code20),]
wide_varis_livdeter <- wide_varis_livdeter[!is_blank(wide_varis_livdeter$cigarettes19),]
wide_varis_livdeter <- wide_varis_livdeter[!is_blank(wide_varis_livdeter$cigarettes20),]
wide_varis_livdeter <- wide_varis_livdeter[!is_blank(wide_varis_livdeter$alcho19),]
wide_varis_livdeter <- wide_varis_livdeter[!is_blank(wide_varis_livdeter$alcho20),]
wide_varis_livdeter <- wide_varis_livdeter[!is_blank(wide_varis_livdeter$vege19),]
wide_varis_livdeter <- wide_varis_livdeter[!is_blank(wide_varis_livdeter$vege20),]

wide_varis_livdeter <- wide_varis_livdeter[!is.na(wide_varis_livdeter$weight19),]
wide_varis_livdeter <- wide_varis_livdeter[!is.na(wide_varis_livdeter$weight20),]
wide_varis_livdeter <- wide_varis_livdeter[!is.na(wide_varis_livdeter$BMI19),]
wide_varis_livdeter <- wide_varis_livdeter[!is.na(wide_varis_livdeter$BMI20),]
wide_varis_livdeter <- wide_varis_livdeter[!is.na(wide_varis_livdeter$age),]

# 維持グループ
wide_varis_livkeep <- wide_varis_livkeep[!is_blank(wide_varis_livkeep$conscious),]
wide_varis_livkeep <- wide_varis_livkeep[!is_blank(wide_varis_livkeep$code19),]
wide_varis_livkeep <- wide_varis_livkeep[!is_blank(wide_varis_livkeep$code20),]
wide_varis_livkeep <- wide_varis_livkeep[!is_blank(wide_varis_livkeep$cigarettes19),]
wide_varis_livkeep <- wide_varis_livkeep[!is_blank(wide_varis_livkeep$cigarettes20),]
wide_varis_livkeep <- wide_varis_livkeep[!is_blank(wide_varis_livkeep$alcho19),]
wide_varis_livkeep <- wide_varis_livkeep[!is_blank(wide_varis_livkeep$alcho20),]
wide_varis_livkeep <- wide_varis_livkeep[!is_blank(wide_varis_livkeep$vege19),]
wide_varis_livkeep <- wide_varis_livkeep[!is_blank(wide_varis_livkeep$vege20),]

wide_varis_livkeep <- wide_varis_livkeep[!is.na(wide_varis_livkeep$weight19),]
wide_varis_livkeep <- wide_varis_livkeep[!is.na(wide_varis_livkeep$weight20),]
wide_varis_livkeep <- wide_varis_livkeep[!is.na(wide_varis_livkeep$BMI19),]
wide_varis_livkeep <- wide_varis_livkeep[!is.na(wide_varis_livkeep$BMI20),]
wide_varis_livkeep <- wide_varis_livkeep[!is.na(wide_varis_livkeep$age),]

# 改善グループ
wide_varis_livimp <- wide_varis_livimp[!is_blank(wide_varis_livimp$conscious),]
wide_varis_livimp <- wide_varis_livimp[!is_blank(wide_varis_livimp$code19),]
wide_varis_livimp <- wide_varis_livimp[!is_blank(wide_varis_livimp$code20),]
wide_varis_livimp <- wide_varis_livimp[!is_blank(wide_varis_livimp$cigarettes19),]
wide_varis_livimp <- wide_varis_livimp[!is_blank(wide_varis_livimp$cigarettes20),]
wide_varis_livimp <- wide_varis_livimp[!is_blank(wide_varis_livimp$alcho19),]
wide_varis_livimp <- wide_varis_livimp[!is_blank(wide_varis_livimp$alcho20),]
wide_varis_livimp <- wide_varis_livimp[!is_blank(wide_varis_livimp$vege19),]
wide_varis_livimp <- wide_varis_livimp[!is_blank(wide_varis_livimp$vege20),]

wide_varis_livimp <- wide_varis_livimp[!is.na(wide_varis_livimp$weight19),]
wide_varis_livimp <- wide_varis_livimp[!is.na(wide_varis_livimp$weight20),]
wide_varis_livimp <- wide_varis_livimp[!is.na(wide_varis_livimp$BMI19),]
wide_varis_livimp <- wide_varis_livimp[!is.na(wide_varis_livimp$BMI20),]
wide_varis_livimp <- wide_varis_livimp[!is.na(wide_varis_livimp$age),]


# データとその型を変換した他場合（喫煙本数を数値、飲酒量を数値、食事嗜好を因子、性別を因子）----
wide_varis_livdeter_changed <- wide_varis_livdeter
wide_varis_livkeep_changed <- wide_varis_livkeep
wide_varis_livimp_changed <- wide_varis_livimp


wide_varis_livdeter_changed$"vege20"<- sub("なし", 0,wide_varis_livdeter_changed$"vege20")
wide_varis_livdeter_changed$"vege20"<- sub("あり", 1,wide_varis_livdeter_changed$"vege20")

wide_varis_livdeter_changed$"vege19"<- sub("なし", 0,wide_varis_livdeter_changed$"vege19")
wide_varis_livdeter_changed$"vege19"<- sub("あり", 1,wide_varis_livdeter_changed$"vege19")

wide_varis_livdeter_changed$"alcho19"<- sub("１合未満", 0.5,wide_varis_livdeter_changed$"alcho19")
wide_varis_livdeter_changed$"alcho19"<- sub("１～２合未満", 1.5,wide_varis_livdeter_changed$"alcho19")
wide_varis_livdeter_changed$"alcho19"<- sub("２～３未満", 2.5,wide_varis_livdeter_changed$"alcho19")
wide_varis_livdeter_changed$"alcho19"<- sub("３合以上", 3.5,wide_varis_livdeter_changed$"alcho19")

wide_varis_livdeter_changed$"alcho20"<- sub("飲酒無し", 0,wide_varis_livdeter_changed$"alcho20")
wide_varis_livdeter_changed$"alcho20"<- sub("１合未満", 0.5,wide_varis_livdeter_changed$"alcho20")
wide_varis_livdeter_changed$"alcho20"<- sub("１～２合未満", 1.5,wide_varis_livdeter_changed$"alcho20")
wide_varis_livdeter_changed$"alcho20"<- sub("２～３未満", 2.5,wide_varis_livdeter_changed$"alcho20")
wide_varis_livdeter_changed$"alcho20"<- sub("３合以上", 3.5,wide_varis_livdeter_changed$"alcho20")

wide_varis_livdeter_changed$"cigarettes19"<- sub("10本以内", 5,wide_varis_livdeter_changed$"cigarettes19")
wide_varis_livdeter_changed$"cigarettes19"<- sub("11～20本", 15.5,wide_varis_livdeter_changed$"cigarettes19")
wide_varis_livdeter_changed$"cigarettes19"<- sub("21～30本", 25.5,wide_varis_livdeter_changed$"cigarettes19")
wide_varis_livdeter_changed$"cigarettes19"<- sub("31～40本", 35.5,wide_varis_livdeter_changed$"cigarettes19")
wide_varis_livdeter_changed$"cigarettes19"<- sub("41本以上", 45.5,wide_varis_livdeter_changed$"cigarettes19")

wide_varis_livdeter_changed$"cigarettes20"<- sub("10本以内", 5,wide_varis_livdeter_changed$"cigarettes20")
wide_varis_livdeter_changed$"cigarettes20"<- sub("11～20本", 15.5,wide_varis_livdeter_changed$"cigarettes20")
wide_varis_livdeter_changed$"cigarettes20"<- sub("21～30本", 25.5,wide_varis_livdeter_changed$"cigarettes20")
wide_varis_livdeter_changed$"cigarettes20"<- sub("31～40本", 35.5,wide_varis_livdeter_changed$"cigarettes20")
wide_varis_livdeter_changed$"cigarettes20"<- sub("41本以上", 45.5,wide_varis_livdeter_changed$"cigarettes20")

wide_varis_livdeter_changed$sex <- sub("男", 1,wide_varis_livdeter_changed$sex)
wide_varis_livdeter_changed$sex <- sub("女", 0,wide_varis_livdeter_changed$sex)

wide_varis_livdeter_changed$sex <- as.factor(wide_varis_livdeter_changed$sex)
wide_varis_livdeter_changed$cigarettes19 <- as.numeric(wide_varis_livdeter_changed$cigarettes19)
wide_varis_livdeter_changed$cigarettes20 <- as.numeric(wide_varis_livdeter_changed$cigarettes20)
wide_varis_livdeter_changed$alcho19 <- as.numeric(wide_varis_livdeter_changed$alcho19)
wide_varis_livdeter_changed$alcho20 <- as.numeric(wide_varis_livdeter_changed$alcho20)
wide_varis_livdeter_changed$vege19 <- as.factor(wide_varis_livdeter_changed$vege19)
wide_varis_livdeter_changed$vege20 <- as.factor(wide_varis_livdeter_changed$vege20)

# 分析対象のデータ----
head(wide_varis_livdeter,3)
head(wide_varis_livkeep,3)
head(wide_varis_livimp,3)

head(wide_varis_livdeter_changed)

# データの要約
summary(wide_varis_livdeter)
summary(wide_varis_livkeep)
summary(wide_varis_livimp)

summary(wide_varis_livdeter_changed)

# brmsによる分散分析モデルの推定 -------------------------------------------------------------------

# 分散分析モデルを作る

#wide_varis_livdeter：変数抽出と欠測データの削除
lm_brms_wide_varis_livdeter <- brm(
  formula = weight20 ~ app_usage + age + BMI19 + code19 + cigarettes19 + alcho19 + vege19 + weight19 + BMI20 + code20 + cigarettes20 + alcho20 + vege20,  # modelの構造を指定
  family = gaussian(),        # 正規分布を使う
  data = wide_varis_livdeter,       # データ
  seed = 1,                   # 乱数の種
  prior = c(set_prior("", class = "Intercept"),
            set_prior("", class = "sigma"))
)
lm_brms_wide_varis_livkeep <- brm(
  formula = weight20 ~ app_usage + age + BMI19 + code19 + cigarettes19 + alcho19 + vege19 + weight19 + BMI20 + code20 + cigarettes20 + alcho20 + vege20,  # modelの構造を指定
  family = gaussian(),        # 正規分布を使う
  data = wide_varis_livkeep,       # データ
  seed = 1,                   # 乱数の種
  prior = c(set_prior("", class = "Intercept"),
            set_prior("", class = "sigma"))
)
lm_brms_wide_varis_livimp <- brm(
  formula = weight20 ~ app_usage + age + BMI19 + code19 + cigarettes19 + alcho19 + vege19 + weight19 + BMI20 + code20 + cigarettes20 + alcho20 + vege20,  # modelの構造を指定
  family = gaussian(),        # 正規分布を使う
  data = wide_varis_livimp,       # データ
  seed = 1,                   # 乱数の種
  prior = c(set_prior("", class = "Intercept"),
            set_prior("", class = "sigma"))
)
# # wide_varis_livdeter_changed：変数抽出と欠測データの削除、データとその型の変換----
# lm_brms_wide_varis_livdeter_changed <- brm(
#   formula = weight20 ~ app_usage + age + BMI19 + code19 + cigarettes19 + alcho19 + vege19 + weight19 + BMI20 + code20 + cigarettes20 + alcho20 + vege20,  # modelの構造を指定
#   family = gaussian(),        # 正規分布を使う
#   data = wide_varis_livdeter_changed,       # データ
#   seed = 1,                   # 乱数の種
#   prior = c(set_prior("", class = "Intercept"),
#             set_prior("", class = "sigma"))
# )
# lm_brms_wide_varis_livdeter_changed

# MCMCの結果の確認----
lm_brms_wide_varis_livdeter
lm_brms_wide_varis_livkeep
lm_brms_wide_varis_livimp

# 推定されたアプリ使用状況別の平均体重のグラフ
# eff <- conditional_effects(lm_brms_wide_varis_livdeter,  effects = "weight19:app_usage")
# plot(eff, points = TRUE)


