rm(list=setdiff(ls(), "m2"))

install.packages("neuralnet")
library("neuralnet")
library(randomForest)
library(reshape2)
setwd("C:/Users/Kamal/Desktop/DOTA")
t9<-read.csv("train9.csv",stringsAsFactors = F)
t1<-read.csv("train1.csv",stringsAsFactors = F)

# internal validation set
p9<-read.csv("test9.csv",stringsAsFactors = F)
p1<-read.csv("test1.csv",stringsAsFactors = F)



# uncomment
# p9<-read.csv("test9.csv",stringsAsFactors = F)
# p1<-read.csv("test1.csv",stringsAsFactors = F)

# tb<-rbind(t9_w,t1_w)
# 
# tb<-tb[order(tb$user_id),]
# tb$whichtest<-1:10
# t1<-NULL
# t9<-NULL
# for(i in 1:10)
# {
#   ti9<-tb[tb$whichtest!=i,]
#   ti9$user_id<-paste(ti9$user_id,"_",i,sep ="")
#   ti1<-tb[tb$whichtest==i,]
#   ti1$user_id<-paste(ti1$user_id,"_",i,sep ="")
#   t1<-rbind(t1,ti1)
#   t9<-rbind(t9,ti9)
# }


temp<-tapply(rbind(t9,p9)$num_wins,rbind(t9,p9)$hero_id,sum)/tapply(rbind(t9,p9)$num_games,rbind(t9,p9)$hero_id,sum)
hero_temp<-as.data.frame(cbind(names(temp),temp),row.names = F)
colnames(hero_temp)<-c("hero_id","char_win_perc_act")
t1<-merge(t1,hero_temp,by = "hero_id")
p1<-merge(p1,hero_temp,by = "hero_id")

temp<-tapply(rbind(t9,p9)$kda_ratio,rbind(t9,p9)$hero_id,mean)
hero_temp<-as.data.frame(cbind(names(temp),temp),row.names = F)
colnames(hero_temp)<-c("hero_id","char_win_perc")
t1<-merge(t1,hero_temp,by = "hero_id")
p1<-merge(p1,hero_temp,by = "hero_id")
hero_temp$char_win_perc<-as.numeric(as.character(hero_temp$char_win_perc))

t1$char_win_perc<-as.numeric(as.character(t1$char_win_perc))
t1$char_win_perc_act<-as.numeric(as.character(t1$char_win_perc_act))

p1$char_win_perc<-as.numeric(as.character(p1$char_win_perc))
p1$char_win_perc_act<-as.numeric(as.character(p1$char_win_perc_act))



h<-read.csv("hero_data.csv",stringsAsFactors = F)
h$tot_<-h$base_armor+h$base_health
t9n<-merge(t9,h,by="hero_id")
t1n<-merge(t1,h,by="hero_id")
p1n<-merge(p1,h,by="hero_id")
p9n<-merge(p9,h,by="hero_id")
#--------train------------------
same_temp<-merge(t1n,t9n,by="user_id")
same_temp1<-same_temp[same_temp$primary_attr.x==same_temp$primary_attr.y,]
same_temp2<-same_temp[same_temp$attack_type.x==same_temp$attack_type.y,]
temp2<-tapply(same_temp1$num_wins.y,same_temp1$user_id,sum)/tapply(same_temp1$num_games.y,same_temp1$user_id,sum)
temp3<-tapply(same_temp1$kda_ratio.y,same_temp1$user_id,mean)
hero_temp2<-as.data.frame(cbind(names(temp2),temp2,temp3),row.names = F)
colnames(hero_temp2)<-c("user_id","same_prim_attack_win_perc","same_prim_attack_kda")
t1n<-merge(t1n,hero_temp2,by="user_id",all.x = T)

t1n$same_prim_attack_win_perc<-ifelse(is.na(t1n$same_prim_attack_win_perc),-99,t1n$same_prim_attack_win_perc)
t1n$same_prim_attack_kda<-ifelse(is.na(t1n$same_prim_attack_kda),-99,t1n$same_prim_attack_kda)

temp2<-tapply(same_temp2$num_wins.y,same_temp2$user_id,sum)/tapply(same_temp2$num_games.y,same_temp2$user_id,sum)
temp3<-tapply(same_temp2$kda_ratio.y,same_temp2$user_id,mean)
hero_temp2<-as.data.frame(cbind(names(temp2),temp2,temp3),row.names = F)
colnames(hero_temp2)<-c("user_id","same_attack_type_win_perc","same_attack_type_kda")
t1n<-merge(t1n,hero_temp2,by="user_id",all.x = T)

t1n$same_attack_type_win_perc<-ifelse(is.na(t1n$same_attack_type_win_perc),-99,t1n$same_attack_type_win_perc)
t1n$same_attack_type_kda<-ifelse(is.na(t1n$same_attack_type_kda),-99,t1n$same_attack_type_kda)

#---prediction-----
same_temp<-merge(p1n,p9n,by="user_id")
same_temp1<-same_temp[same_temp$primary_attr.x==same_temp$primary_attr.y,]
same_temp2<-same_temp[same_temp$attack_type.x==same_temp$attack_type.y,]
temp2<-tapply(same_temp1$num_wins,same_temp1$user_id,sum)/tapply(same_temp1$num_games.y,same_temp1$user_id,sum)
temp3<-tapply(same_temp1$kda_ratio,same_temp1$user_id,mean)
hero_temp2<-as.data.frame(cbind(names(temp2),temp2,temp3),row.names = F)
colnames(hero_temp2)<-c("user_id","same_prim_attack_win_perc","same_prim_attack_kda")
p1n<-merge(p1n,hero_temp2,by="user_id",all.x = T)

p1n$same_prim_attack_win_perc<-ifelse(is.na(p1n$same_prim_attack_win_perc),-99,p1n$same_prim_attack_win_perc)
p1n$same_prim_attack_kda<-ifelse(is.na(p1n$same_prim_attack_kda),-99,p1n$same_prim_attack_kda)

temp2<-tapply(same_temp2$num_wins,same_temp2$user_id,sum)/tapply(same_temp2$num_games.y,same_temp2$user_id,sum)
temp3<-tapply(same_temp2$kda_ratio,same_temp2$user_id,mean)
hero_temp2<-as.data.frame(cbind(names(temp2),temp2,temp3),row.names = F)
colnames(hero_temp2)<-c("user_id","same_attack_type_win_perc","same_attack_type_kda")
p1n<-merge(p1n,hero_temp2,by="user_id",all.x = T)

p1n$same_attack_type_win_perc<-ifelse(is.na(p1n$same_attack_type_win_perc),-99,p1n$same_attack_type_win_perc)
p1n$same_attack_type_kda<-ifelse(is.na(p1n$same_attack_type_kda),-99,p1n$same_attack_type_kda)





t9n$win_perc<-t9n$num_wins/t9n$num_games
t1n$win_perc<-t1n$num_wins/t1n$num_games
t9n$attack_type<-as.factor(t9n$attack_type)
t9n$primary_attr<-as.factor(t9n$primary_attr)
t1n$attack_type<-as.factor(t1n$attack_type)
t1n$primary_attr<-as.factor(t1n$primary_attr)

p9n$win_perc<-p9n$num_wins/p9n$num_games
#p1n$win_perc<-p1n$num_wins/p1n$num_games
p9n$attack_type<-as.factor(p9n$attack_type)
p9n$primary_attr<-as.factor(p9n$primary_attr)
p1n$attack_type<-as.factor(p1n$attack_type)
p1n$primary_attr<-as.factor(p1n$primary_attr)

t9n<-t9n[order(t9n$user),]
t1n<-t1n[order(t1n$user),]
p9n<-p9n[order(p9n$user),]
p1n<-p1n[order(p1n$user),]


cn<-colnames(t9n)[grepl("base",colnames(t9n))]
t9n[,paste0("iwin_",cn,sep = "")]<-t9n[,colnames(t9n)[grepl("base",colnames(t9n))]]*t9n$num_games
t1n[,paste0("iwin_",cn,sep = "")]<-t1n[,colnames(t1n)[grepl("base",colnames(t1n))]]*t1n$num_games

p9n[,paste0("iwin_",cn,sep = "")]<-p9n[,colnames(p9n)[grepl("base",colnames(p9n))]]*p9n$num_games
p1n[,paste0("iwin_",cn,sep = "")]<-p1n[,colnames(p1n)[grepl("base",colnames(p1n))]]*p1n$num_games

t9n[,paste0("twin_",cn,sep = "")]<-t9n[,colnames(t9n)[grepl("base",colnames(t9n)) & !grepl("iwin",colnames(t9n))]]*tapply(t9n$num_games,t9n$user_id,sum)
t1n[,paste0("twin_",cn,sep = "")]<-t1n[,colnames(t1n)[grepl("base",colnames(t1n)) & !grepl("iwin",colnames(t1n))]]*tapply(t9n$num_games,t9n$user_id,sum)

p9n[,paste0("twin_",cn,sep = "")]<-p9n[,colnames(p9n)[grepl("base",colnames(p9n)) & !grepl("iwin",colnames(p9n))]]*tapply(p9n$num_games,p9n$user_id,sum)
p1n[,paste0("twin_",cn,sep = "")]<-p1n[,colnames(p1n)[grepl("base",colnames(p1n)) & !grepl("iwin",colnames(p1n))]]*tapply(p9n$num_games,p9n$user_id,sum)




rm(t9,t1)
rm(p1,p9)

roles<-unique(unlist(unique(strsplit(t9n$roles,":"))))

for(i in 1:9)
{
  t9n[,roles[i]]<-grepl(roles[i],t9n$roles)*1
}
for(i in 1:9)
{
  t1n[,roles[i]]<-grepl(roles[i],t1n$roles)*1
}

for(i in 1:9)
{
  p9n[,roles[i]]<-grepl(roles[i],p9n$roles)*1
}
for(i in 1:9)
{
  p1n[,roles[i]]<-grepl(roles[i],p1n$roles)*1
}


data_manip_9<-function(x)
{
  x<-x[order(-x$num_games),]
  x$hero_id<-1:9
  newx<-reshape(x,idvar="user_id",timevar = "hero_id",direction = "wide",drop = c("id"))
  return(newx)
}


data_manip_10<-function(x)
{
  #x<-x[order(-x$num_games),]
  x$hero_id<-10
  newx<-reshape(x,idvar="user_id",timevar = "hero_id",direction = "wide",drop = c("id"))
  return(newx)
}


splitted<-split(t9n,t9n$user_id)

train9<-lapply(splitted,data_manip_9)
train9<-do.call("rbind",train9)

splitted<-split(t1n,t1n$user_id)

train1<-lapply(splitted,data_manip_10)
train1<-do.call("rbind",train1)

train<-data.frame(train9,train1)

splitted<-split(p9n,p9n$user_id)

test9<-lapply(splitted,data_manip_9)
test9<-do.call("rbind",test9)

splitted<-split(p1n,p1n$user_id)
test1<-lapply(splitted,data_manip_10)
test1<-do.call("rbind",test1)

test<-data.frame(test9,test1)

train$rank_game<-apply(data.frame(train$num_games.1,train$num_games.2,train$num_games.3,train$num_games.4,train$num_games.5,train$num_games.6,train$num_games.7,train$num_games.8,train$num_games.9,train$num_games.10),1,rank)[10,]
train$tot_win_perc<-rowSums(train[,grepl("num_wins",colnames(train))][,1:9])/rowSums(train[,grepl("num_games",colnames(train))][,1:9])
train$avg_kda<-rowSums(train[,grepl("kda_ratio",colnames(train))][,1:9])/9
#train$char_win_perc.10<-as.numeric(as.character(train$char_win_perc.10))

test$rank_game<-apply(data.frame(test$num_games.1,test$num_games.2,test$num_games.3,test$num_games.4,test$num_games.5,test$num_games.6,test$num_games.7,test$num_games.8,test$num_games.9,test$num_games.10),1,rank)[10,]
test$tot_win_perc<-rowSums(test[,grepl("num_wins",colnames(test))][,1:9])/rowSums(test[,grepl("num_games",colnames(test))][,1:9])
test$avg_kda<-rowSums(test[,grepl("kda_ratio",colnames(test))][,1:9])/9
#train$char_win_perc.10<-as.numeric(as.character(train$char_win_perc.10))


train$user_rating_index_kda<-train$avg_kda/mean(c(train$avg_kda,test$avg_kda))
train$user_rating_index_wp<-train$tot_win_perc/(sum(c(rowSums(test[,grepl("num_wins",colnames(test))][,1:9]),rowSums(train[,grepl("num_wins",colnames(train))][,1:9])))/sum(c(rowSums(test[,grepl("num_games",colnames(test))][,1:9]),rowSums(train[,grepl("num_games",colnames(train))][,1:9]))))
train$char_rating_index<-train$char_win_perc.10/mean(hero_temp$char_win_perc)

train$user_char_index<-train$user_rating_index_kda*train$char_rating_index
#train$perc_10_games<-NULL
train$perc_10_games<-train[,grepl("num_games",colnames(train))][,10]/rowSums(train[,grepl("num_games",colnames(train))])
train$user_char_order_index<-train$user_char_index*train$perc_10_games


test$user_rating_index_kda<-test$avg_kda/mean(c(train$avg_kda,test$avg_kda))
test$user_rating_index_wp<-test$tot_win_perc/(sum(c(rowSums(test[,grepl("num_wins",colnames(test))][,1:9]),rowSums(train[,grepl("num_wins",colnames(train))][,1:9])))/sum(c(rowSums(test[,grepl("num_games",colnames(test))][,1:9]),rowSums(train[,grepl("num_games",colnames(train))][,1:9]))))
test$char_rating_index<-test$char_win_perc.10/mean(hero_temp$char_win_perc)
test$user_char_index<-test$user_rating_index_kda*test$char_rating_index
#train$perc_10_games<-NULL
test$perc_10_games<-test[,grepl("num_games",colnames(test))][,10]/rowSums(test[,grepl("num_games",colnames(test))])
test$user_char_order_index<-test$user_char_index*test$perc_10_games




f<-as.formula(paste0("kda_ratio.10~",paste0(colnames(train)[(sapply(train,class) %in% c("integer","numeric","factor")) & !(colnames(train) %in% c("user_id","kda_ratio.10","num_wins.10","win_perc.10"))],collapse = "+"),collapse = ""))
m<-randomForest(f,train)

m1<-gbm(f,train,cv.folds = 5, distribution = "gaussian",interaction.depth = 3,n.trees=2000)

install.packages("h2o")
library(h2o)

h2o.init()

train1<-as.h2o(train)
y<-"kda_ratio.10"
x<-c(colnames(train)[(sapply(train,class) %in% c("integer","numeric","factor")) & !(colnames(train) %in% c("user_id","kda_ratio.10","num_wins.10","win_perc.10"))])

m2<-h2o.automl(x=x,y=y,training_frame = train1,max_runtime_secs = 100,stopping_metric = "RMSE")





test1<-as.h2o(test)
pred<-as.vector(h2o.predict(m2@leader,test1))
cheat_sub<-data.frame(cbind(as.character(p1$id),as.numeric(pred)))
colnames(cheat_sub)<-c("id","kda_ratio")
write.csv(cheat_sub,"cheat_sub_2nd.csv",row.names = F)
sum(test$user_id==p1$user_id)
test$perc_10_games<-test[,grepl("num_games",colnames(test))][,10]/rowSums(test[,grepl("num_games",colnames(test))])


rf1<-predict(m,test)
gb1<-predict(m1,test)
rf1<-data.frame(p1$id,rf1)
colnames(rf1)<-c("id","kda_ratio")
gb1<-data.frame(p1$id,gb1)
colnames(gb1)<-c("id","kda_ratio")

write.csv(rf1,"rf1.csv",row.names=F)
write.csv(gb1,"gb1.csv",row.names=F)

read.csv("")