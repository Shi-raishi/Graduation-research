#データの準備ーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーー

#アプリ利用者に１、不使用者に０
data_kenshin <- within(kenshin,{
  app_factor <- "non user"
  app_factor[!(test$No %in% userid$No)] <- "non user"
  app_factor[test$No %in% userid$No] <- "user"
})
data_kenshin["app_factor"] <- as.factor(data_kenshin$app_factor)


#適当な変数を抽出ーーーーーーーーーーーーーーーーーーーーーーーーーー
hensuu <- select(.data = data_kenshin,2,4,6,9,86,90,91,93,94,107,251,252,256,260,772)

#変数名の変更
names(hensuu) <- c("sex","age","bmi","code","time_sleeping","year_smoking",
                   "daily_smoking","freq_drinking","daily_drinking","time_vdt"
                   ,"height","weight","body_fat","blood_cells","app_usage")

# #ーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーー
# #必要ならデータを英文字に変更
# hensuu["sex"] <- lapply(hensuu["sex"], gsub,pattern="",replacement="")
# 
# hensuu["time_sleeping"] <- lapply(hensuu["time_sleeping"], gsub,pattern="",replacement="")
# 
# hensuu["daily_smoking"] <- lapply(hensuu["daily_smoking"], gsub,pattern="",replacement="")
# 
# hensuu["freq_drinking"] <- lapply(hensuu["freq_drinking"], gsub,pattern="",replacement="")
# 
# hensuu["daily_drinking"] <- lapply(hensuu["daily_drinking"], gsub,pattern="",replacement="")
# 
# hensuu["app_usage"] <- lapply(hensuu["app_usage"],gsub,pattern="",replacement="")
# #ーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーー
