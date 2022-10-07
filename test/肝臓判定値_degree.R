# 本番

# データ <- kenshinデータ
# 調整変数 <- app_degree：アプリ使用の濃さ、程度
# データ少なすぎてMCMCがうまくいかない

# パッケージの読み込み
library(rstan)
library(brms)
library(dplyr)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# app_degreeを追加-------------------------------------------------

# kenposのNo列、実績日列を取得
days_kenpos <- select(kenpos,"No",measuring_day = "実績日")

# No各々における実績日の日数を取得（同日に複数回計測していない前提）
days_kenpos <-
  days_kenpos %>% 
  group_by(No) %>% 
  summarise( num_access = length(measuring_day))

# アクセス日数（実績日の日数）の分布を確認
summary(as.factor(days_kenpos$num_access))

# アプリ使用歴1年以内の個体のIDを取得
within_year <- days_kenpos[days_kenpos$num_access <= 365,]

# kenshinのアプリ使用群のデータにapp_degreeを追加
kenshin_degree <- merge(kenshin,user_id)
kenshin_degree <- within(kenshin_degree,{
  app_degree <- "over year"
  app_degree[!(kenshin_degree$No %in% within_year$No)] <- "over year"
  app_degree[(kenshin_degree$No %in% within_year$No)] <- "within year"
})

#kenshinの肝臓判定値にNAと空白を含む行を削除----
data_liver_deg <- kenshin_degree[!is.na(kenshin_degree$肝臓_19),]
data_liver_deg <- data_liver_deg[!is_blank(data_liver_deg$肝臓_19),]
data_liver_deg <- data_liver_deg[!is.na(data_liver_deg$肝臓_20),]
data_liver_deg <- data_liver_deg[!is_blank(data_liver_deg$肝臓_20),]

# 肝臓判定値悪化のデータを取得-----------------------------------------
liver_deterA1_deg <- data_liver_deg[data_liver_deg$肝臓_19=="A1",]
liver_deterA1_deg <- liver_deterA1_deg[liver_deterA1_deg$肝臓_20 != "A1",]
summary(liver_deterA1_deg$肝臓_19)
summary(liver_deterA1_deg$肝臓_20)

liver_deterA2_deg <- data_liver_deg[data_liver_deg$肝臓_19=="A2",]
liver_deterA2_deg <- liver_deterA2_deg[liver_deterA2_deg$肝臓_20 != "A1",]
liver_deterA2_deg <- liver_deterA2_deg[liver_deterA2_deg$肝臓_20 != "A2",]
summary(liver_deterA2_deg$肝臓_19)
summary(liver_deterA2_deg$肝臓_20)

liver_deterB_deg <- data_liver_deg[data_liver_deg$肝臓_19 == "B",]
liver_deterB_deg <- liver_deterB_deg[liver_deterB_deg$肝臓_20 != "A1",]
liver_deterB_deg <- liver_deterB_deg[liver_deterB_deg$肝臓_20 != "A2",]
liver_deterB_deg <- liver_deterB_deg[liver_deterB_deg$肝臓_20 != "B",]
summary(liver_deterB_deg$肝臓_19)
summary(liver_deterB_deg$肝臓_20)

liver_deter_all_deg <- rbind(liver_deterA1_deg,liver_deterA2_deg)
liver_deter_all_deg <- rbind(liver_deter_all_deg,liver_deterB_deg)
nrow(liver_deter_all_deg)

# 肝臓判定値維持のデータを取得----
liver_keepA1_deg <- data_liver_deg[data_liver_deg$肝臓_19 == "A1",]
liver_keepA1_deg <- liver_keepA1_deg[liver_keepA1_deg$肝臓_20 == "A1",]
summary(liver_keepA1_deg$肝臓_19)
summary(liver_keepA1_deg$肝臓_20)

liver_keepA2_deg <- data_liver_deg[data_liver_deg$肝臓_19 == "A2",]
liver_keepA2_deg <- liver_keepA2_deg[liver_keepA2_deg$肝臓_20 == "A2",]
summary(liver_keepA2_deg$肝臓_19)
summary(liver_keepA2_deg$肝臓_20)

liver_keepB_deg <- data_liver_deg[data_liver_deg$肝臓_19 == "B",]
liver_keepB_deg <- liver_keepB_deg[liver_keepB_deg$肝臓_20 == "B",]
summary(liver_keepB_deg$肝臓_19)
summary(liver_keepB_deg$肝臓_20)

liver_keepC_deg <- data_liver_deg[data_liver_deg$肝臓_19 == "C",]
liver_keepC_deg <- liver_keepC_deg[liver_keepC_deg$肝臓_20 == "C",]
summary(liver_keepC_deg$肝臓_19)
summary(liver_keepC_deg$肝臓_20)

liver_keep_all_deg <- rbind(liver_keepA1_deg,liver_keepA2_deg)
liver_keep_all_deg <- rbind(liver_keep_all_deg,liver_keepB_deg)
liver_keep_all_deg <- rbind(liver_keep_all_deg,liver_keepC_deg)
nrow(liver_keep_all_deg)

# 肝臓判定値改善----
liver_impA1_deg <- data_liver_deg[data_liver_deg$肝臓_19=="C",]
liver_impA1_deg <- liver_impA1_deg[liver_impA1_deg$肝臓_20 != "C",]
summary(liver_impA1_deg$肝臓_19)
summary(liver_impA1_deg$肝臓_20)

liver_impA2_deg <- data_liver_deg[data_liver_deg$肝臓_19=="B",]
liver_impA2_deg <- liver_impA2_deg[liver_impA2_deg$肝臓_20 != "B",]
liver_impA2_deg <- liver_impA2_deg[liver_impA2_deg$肝臓_20 != "C",]
summary(liver_impA2_deg$肝臓_19)
summary(liver_impA2_deg$肝臓_20)

liver_impB_deg <- data_liver_deg[data_liver_deg$肝臓_19 == "A2",]
liver_impB_deg <- liver_impB_deg[liver_impB_deg$肝臓_20 != "A2",]
liver_impB_deg <- liver_impB_deg[liver_impB_deg$肝臓_20 != "B",]
liver_impB_deg <- liver_impB_deg[liver_impB_deg$肝臓_20 != "C",]
summary(liver_impB_deg$肝臓_19)
summary(liver_impB_deg$肝臓_20)

liver_imp_all_deg <- rbind(liver_impA1_deg,liver_impA2_deg)
liver_imp_all_deg <- rbind(liver_imp_all_deg,liver_impB_deg)
nrow(liver_imp_all_deg)
#変数の抽出----
wide_varis_livdeter_deg <- select(liver_deter_all_deg,"sex" = 2,"age" = 4,
                              "BMI19" = 6,"code19" = 9,"cigarettes19" = 91,
                              "alcho19" = 94,"vege19" = 97,"conscious" = 166,
                              "weight19" = 252,"BMI20" = 391,"code20" = 394,
                              "cigarettes20"= 476,"alcho20" = 479,"vege20" = 482,
                              "weight20" = 637,app_degree)
wide_varis_livkeep_deg <- select(liver_keep_all_deg,"sex" = 2,"age" = 4,
                             "BMI19" = 6,"code19" = 9,"cigarettes19" = 91,
                             "alcho19" = 94,"vege19" = 97,"conscious" = 166,
                             "weight19" = 252,"BMI20" = 391,"code20" = 394,
                             "cigarettes20"= 476,"alcho20" = 479,"vege20" = 482,
                             "weight20" = 637,app_degree)
wide_varis_livimp_deg <- select(liver_imp_all_deg,"sex" = 2,"age" = 4,
                            "BMI19" = 6,"code19" = 9,"cigarettes19" = 91,
                            "alcho19" = 94,"vege19" = 97,"conscious" = 166,
                            "weight19" = 252,"BMI20" = 391,"code20" = 394,
                            "cigarettes20"= 476,"alcho20" = 479,"vege20" = 482,
                            "weight20" = 637,app_degree)

# NAを含む行の削除----
# 判定悪化グループ
wide_varis_livdeter_deg <- wide_varis_livdeter_deg[!is_blank(wide_varis_livdeter_deg$conscious),]
wide_varis_livdeter_deg <- wide_varis_livdeter_deg[!is_blank(wide_varis_livdeter_deg$code19),]
wide_varis_livdeter_deg <- wide_varis_livdeter_deg[!is_blank(wide_varis_livdeter_deg$code20),]
wide_varis_livdeter_deg <- wide_varis_livdeter_deg[!is_blank(wide_varis_livdeter_deg$cigarettes19),]
wide_varis_livdeter_deg <- wide_varis_livdeter_deg[!is_blank(wide_varis_livdeter_deg$cigarettes20),]
wide_varis_livdeter_deg <- wide_varis_livdeter_deg[!is_blank(wide_varis_livdeter_deg$alcho19),]
wide_varis_livdeter_deg <- wide_varis_livdeter_deg[!is_blank(wide_varis_livdeter_deg$alcho20),]
wide_varis_livdeter_deg <- wide_varis_livdeter_deg[!is_blank(wide_varis_livdeter_deg$vege19),]
wide_varis_livdeter_deg <- wide_varis_livdeter_deg[!is_blank(wide_varis_livdeter_deg$vege20),]

wide_varis_livdeter_deg <- wide_varis_livdeter_deg[!is.na(wide_varis_livdeter_deg$weight19),]
wide_varis_livdeter_deg <- wide_varis_livdeter_deg[!is.na(wide_varis_livdeter_deg$weight20),]
wide_varis_livdeter_deg <- wide_varis_livdeter_deg[!is.na(wide_varis_livdeter_deg$BMI19),]
wide_varis_livdeter_deg <- wide_varis_livdeter_deg[!is.na(wide_varis_livdeter_deg$BMI20),]
wide_varis_livdeter_deg <- wide_varis_livdeter_deg[!is.na(wide_varis_livdeter_deg$age),]

# 維持グループ
wide_varis_livkeep_deg <- wide_varis_livkeep_deg[!is_blank(wide_varis_livkeep_deg$conscious),]
wide_varis_livkeep_deg <- wide_varis_livkeep_deg[!is_blank(wide_varis_livkeep_deg$code19),]
wide_varis_livkeep_deg <- wide_varis_livkeep_deg[!is_blank(wide_varis_livkeep_deg$code20),]
wide_varis_livkeep_deg <- wide_varis_livkeep_deg[!is_blank(wide_varis_livkeep_deg$cigarettes19),]
wide_varis_livkeep_deg <- wide_varis_livkeep_deg[!is_blank(wide_varis_livkeep_deg$cigarettes20),]
wide_varis_livkeep_deg <- wide_varis_livkeep_deg[!is_blank(wide_varis_livkeep_deg$alcho19),]
wide_varis_livkeep_deg <- wide_varis_livkeep_deg[!is_blank(wide_varis_livkeep_deg$alcho20),]
wide_varis_livkeep_deg <- wide_varis_livkeep_deg[!is_blank(wide_varis_livkeep_deg$vege19),]
wide_varis_livkeep_deg <- wide_varis_livkeep_deg[!is_blank(wide_varis_livkeep_deg$vege20),]

wide_varis_livkeep_deg <- wide_varis_livkeep_deg[!is.na(wide_varis_livkeep_deg$weight19),]
wide_varis_livkeep_deg <- wide_varis_livkeep_deg[!is.na(wide_varis_livkeep_deg$weight20),]
wide_varis_livkeep_deg <- wide_varis_livkeep_deg[!is.na(wide_varis_livkeep_deg$BMI19),]
wide_varis_livkeep_deg <- wide_varis_livkeep_deg[!is.na(wide_varis_livkeep_deg$BMI20),]
wide_varis_livkeep_deg <- wide_varis_livkeep_deg[!is.na(wide_varis_livkeep_deg$age),]

# 改善グループ
wide_varis_livimp_deg <- wide_varis_livimp_deg[!is_blank(wide_varis_livimp_deg$conscious),]
wide_varis_livimp_deg <- wide_varis_livimp_deg[!is_blank(wide_varis_livimp_deg$code19),]
wide_varis_livimp_deg <- wide_varis_livimp_deg[!is_blank(wide_varis_livimp_deg$code20),]
wide_varis_livimp_deg <- wide_varis_livimp_deg[!is_blank(wide_varis_livimp_deg$cigarettes19),]
wide_varis_livimp_deg <- wide_varis_livimp_deg[!is_blank(wide_varis_livimp_deg$cigarettes20),]
wide_varis_livimp_deg <- wide_varis_livimp_deg[!is_blank(wide_varis_livimp_deg$alcho19),]
wide_varis_livimp_deg <- wide_varis_livimp_deg[!is_blank(wide_varis_livimp_deg$alcho20),]
wide_varis_livimp_deg <- wide_varis_livimp_deg[!is_blank(wide_varis_livimp_deg$vege19),]
wide_varis_livimp_deg <- wide_varis_livimp_deg[!is_blank(wide_varis_livimp_deg$vege20),]

wide_varis_livimp_deg <- wide_varis_livimp_deg[!is.na(wide_varis_livimp_deg$weight19),]
wide_varis_livimp_deg <- wide_varis_livimp_deg[!is.na(wide_varis_livimp_deg$weight20),]
wide_varis_livimp_deg <- wide_varis_livimp_deg[!is.na(wide_varis_livimp_deg$BMI19),]
wide_varis_livimp_deg <- wide_varis_livimp_deg[!is.na(wide_varis_livimp_deg$BMI20),]
wide_varis_livimp_deg <- wide_varis_livimp_deg[!is.na(wide_varis_livimp_deg$age),]



# 分析対象のデータ----
head(wide_varis_livdeter_deg,3)
head(wide_varis_livkeep_deg,3)
head(wide_varis_livimp_deg,3)

# データの要約
summary(wide_varis_livdeter_deg)
summary(wide_varis_livkeep_deg)
summary(wide_varis_livimp_deg)

# brmsによる分散分析モデルの推定 -------------------------------------------------------------------

# 分散分析モデルを作る
lm_brms_wide_varis_livdeter_deg <- brm(
  formula = weight20 ~ app_degree + age + BMI19 + code19 + cigarettes19 + alcho19 + vege19 + weight19 + BMI20 + code20 + cigarettes20 + alcho20 + vege20,  # modelの構造を指定
  family = gaussian(),        # 正規分布を使う
  data = wide_varis_livdeter_deg,       # データ
  seed = 1,                   # 乱数の種
  prior = c(set_prior("", class = "Intercept"),
            set_prior("", class = "sigma"))
)
lm_brms_wide_varis_livkeep_deg <- brm(
  formula = weight20 ~ app_degree + age + BMI19 + code19 + cigarettes19 + alcho19 + vege19 + weight19 + BMI20 + code20 + cigarettes20 + alcho20 + vege20,  # modelの構造を指定
  family = gaussian(),        # 正規分布を使う
  data = wide_varis_livkeep_deg,       # データ
  seed = 1,                   # 乱数の種
  prior = c(set_prior("", class = "Intercept"),
            set_prior("", class = "sigma"))
)
lm_brms_wide_varis_livimp_deg <- brm(
  formula = weight20 ~ app_degree + age + BMI19 + code19 + cigarettes19 + alcho19 + vege19 + weight19 + BMI20 + code20 + cigarettes20 + alcho20 + vege20,  # modelの構造を指定
  family = gaussian(),        # 正規分布を使う
  data = wide_varis_livimp_deg,       # データ
  seed = 1,                   # 乱数の種
  prior = c(set_prior("", class = "Intercept"),
            set_prior("", class = "sigma"))
)

# MCMCの結果の確認----
lm_brms_wide_varis_livdeter_deg
lm_brms_wide_varis_livkeep_deg
lm_brms_wide_varis_livimp_deg
