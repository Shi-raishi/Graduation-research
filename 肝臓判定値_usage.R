# 本番

# データ <- kenshinデータ
# 調整変数 <- app_usage:アプリ使用の有無

# 分析の準備 

# パッケージの読み込み
library(rstan)
library(brms)
library(dplyr)

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



#kenshinの肝臓判定値にNAと空白を含む行を削除----
data_liver <- kenshin_usage[!is.na(kenshin_usage$肝臓_19),]
data_liver <- data_liver[!is_blank(data_liver$肝臓_19),]
data_liver <- data_liver[!is.na(data_liver$肝臓_20),]
data_liver <- data_liver[!is_blank(data_liver$肝臓_20),]

# 肝臓判定値悪化のデータを取得-----------------------------------------
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

# 肝臓判定値維持のデータを取得----
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



# 分析対象のデータ----
head(wide_varis_livdeter,3)
head(wide_varis_livkeep,3)
head(wide_varis_livimp,3)

# データの要約
summary(wide_varis_livdeter)
summary(wide_varis_livkeep)
summary(wide_varis_livimp)

# brmsによる分散分析モデルの推定 -------------------------------------------------------------------

# 分散分析モデルを作る
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

# MCMCの結果の確認----
lm_brms_wide_varis_livimp
lm_brms_wide_varis_livkeep
lm_brms_wide_varis_livdeter
