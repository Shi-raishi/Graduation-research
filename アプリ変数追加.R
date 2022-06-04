
library(dplyr)

# kenshin_usage(n=9613)：kenshinにアプリの使用有無を表す変数を追加したもの
# app_usage :アプリ使用の有無

# kenpos_degree(m=482752), kenshin_degree(n=527)：kenshin, kenpos各々にアプリ使用の程度を表す変数を追加したもの
# app_degree：アプリの使用の濃さ（アクセスの日数が365日以下 or 365日より多い）


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

# kenposにapp_degreeを追加
kenpos_degree <- within(kenpos,{
  app_degree <- "over year"
  app_degree[!(kenpos$No %in% within_year$No)] <- "over year"
  app_degree[(kenpos$No %in% within_year$No)] <- "within year"
})
  
  
  