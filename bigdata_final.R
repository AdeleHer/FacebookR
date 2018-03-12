require("Rfacebook")
fb_oauth = fbOAuth (app_id = "122464385012526",app_secret = "d01c9521bd7208fd963e395ce55c9bdb" )

### data crawler and get useful data ###
celebrities <- read.csv("./FBcrawler/fb_celebrity.csv")
## show popular man's impact ##
transfer_data <- data.frame(user_page=character(),page_likes=character(),good_posts=integer(),active_level=double())
## cluster data ##
all_data <- data.frame(from_id=character(),from_name=character(),created_time=numeric(),type=character(),likes_count=integer(),comments_count=integer(),shares_count=integer())
tryCatch({
  for(i in 1:nrow(celebrities)){
    celebrity = celebrities[i,]
    clikes <- gsub(",", "", as.character(celebrity$likes))
    page <- getPage(page=celebrity$user_page,token=fb_oauth, n=1000)
    page$created_time <- gsub("+0000", "", as.character(page$created_time))
    page$created_time <- as.Date(page$created_time, "%Y-%m-%dT%H:%M:%S+")
    page$created_time <- as.numeric(page$created_time)
    filter_like <- as.integer(clikes)/100
    ind <- which(page$likes_count>round(filter_like))
    filter_page <- page[ind, ]
    transfer_row <- data.frame(user_page=celebrity$page_name,page_likes=clikes,good_posts=nrow(filter_page),active_level=as.double(nrow(filter_page)/nrow(page)))
    transfer_data <- rbind(transfer_data,transfer_row)
    simple_row <- data.frame(from_id=page$from_id,from_name = page$from_name,created_time = page$created_time, type = page$type, likes_count = page$likes_count, comments_count = page$comments_count, shares_count = page$shares_count)
    all_data <- rbind(all_data,simple_row)
  }
},error = function(e){
  conditionMessage(e)
})

##### show popular man's impact #####
library(ggplot2)
transfer_data$page_likes <- as.integer(as.character(transfer_data$page_likes))
transfer_data <- transform(transfer_data, new_page_likes = (page_likes/1000))
transfer_data <-transform(transfer_data, point_size = (active_level*25))
ggplot(transfer_data, aes(x = new_page_likes , y = good_posts ,color = user_page, label = user_page)) +
  geom_point(size = transfer_data$point_size,alpha=0.6)+ geom_text()+ geom_hline(yintercept = 500,colour="red")+ geom_vline(xintercept = 1000,colour="red")+theme(legend.position="none")

##### cluster data #####
require(factoextra)
drops <- c("from_id","from_name","type")
tdata <- all_data[,!(names(all_data) %in% drops)]
sindex <- sample(nrow(tdata),10000)
sample_data <- tdata[sindex,]
c_data <- all_data[sindex,]
fviz_nbclust(sample_data, 
             FUNcluster = hcut,  # hierarchical clustering
             method = "wss",     # total within sum of square
             k.max = 20          # max number of clusters to consider
)+labs(title="Elbow Method for HC")+geom_vline(xintercept = 4,       # 在 X=3的地方 
                                               linetype = 2)
kmeans.result <- kmeans(sample_data, 4)
c_table <- table(c_data$from_name, kmeans.result$cluster)
ggplot(sample_data, aes(created_time, likes_count, color =kmeans.result$cluster)) + geom_point()
plot(sample_data[c("created_time", "likes_count")], col = kmeans.result$cluster) # 顏色所對應的名稱
legend(-1,1,col = kmeans.result$cluster)
kmeans.result$cluster
fviz_cluster(kmeans.result,       # 分群結果
             data = sample_data,        # 資料
             geom = c("point"),     # 點 (point)
             frame.type = "norm",
             class = c_data$from_name)   # 框架型態
result_data1 <- as.data.frame.matrix(c_table)
result_data2 <- result_data1
result_data2[,1] <- result_data1[,1]/ rowSums(result_data1[,c(1,2,3,4)])
result_data2[,2] <- result_data1[,2]/ rowSums(result_data1[,c(1,2,3,4)])
result_data2[,3] <- result_data1[,3]/ rowSums(result_data1[,c(1,2,3,4)])
result_data2[,4] <- result_data1[,4]/ rowSums(result_data1[,c(1,2,3,4)])
good_man <- result_data2[which((result_data2[,3]>0.8) & (result_data2[,3]<1)) ,]

good_post_man <- transfer_data[which((transfer_data$good_posts>0.8) | (transfer_data$page_likes>500000)) ,]
really_good_man <- good_man[good_post_man$user_page,]
really_good_man <- na.omit(really_good_man)

##### compare data #####

## good at facebook ##
#眾量級
crowd_page <- all_data[which(all_data$from_name == celebrities$page_name[15]),] 
fasion_page <- all_data[which(all_data$from_name == celebrities$page_name[11]),] 
han_page <- all_data[which(all_data$from_name == celebrities$page_name[17]),] 

## good at youtube ##
#阿滴英文
raydu_page <- all_data[which(all_data$from_name == celebrities$page_name[1]),]
saint_page <- all_data[which(all_data$from_name == celebrities$page_name[10]),] 
zyn_page <- all_data[which(all_data$from_name == celebrities$page_name[8]),] 

ggplot(crowd_page,aes(created_time))+geom_line(aes(y = likes_count, colour = "likes_count")) + 
         geom_line(aes(y = comments_count, colour = "comments_count")) +  geom_line(aes(y = shares_count, colour = "shares_count"))+geom_smooth(method = "lm",aes(y = likes_count))
ggplot(crowd_page,aes(created_time))+geom_line(aes(y = likes_count, colour = type)) + geom_smooth(method = "lm",aes(y = likes_count))
ggplot(crowd_page,aes(x=type)) + geom_bar()

ggplot(fasion_page,aes(created_time))+geom_line(aes(y = likes_count, colour = "likes_count")) + 
  geom_line(aes(y = comments_count, colour = "comments_count")) +  geom_line(aes(y = shares_count, colour = "shares_count"))+geom_smooth(method = "lm",aes(y = likes_count))
ggplot(fasion_page,aes(created_time))+geom_line(aes(y = likes_count, colour = type)) + geom_smooth(method = "lm",aes(y = likes_count))
ggplot(fasion_page,aes(x=type)) + geom_bar()

ggplot(han_page,aes(created_time))+geom_line(aes(y = likes_count, colour = "likes_count")) + 
  geom_line(aes(y = comments_count, colour = "comments_count")) +  geom_line(aes(y = shares_count, colour = "shares_count"))+geom_smooth(method = "lm",aes(y = likes_count))
ggplot(han_page,aes(created_time))+geom_line(aes(y = likes_count, colour = type)) + geom_smooth(method = "lm",aes(y = likes_count))
ggplot(han_page,aes(x=type)) + geom_bar()

ggplot(raydu_page,aes(created_time))+geom_line(aes(y = likes_count, colour = "likes_count")) + 
  geom_line(aes(y = comments_count, colour = "comments_count")) +  geom_line(aes(y = shares_count, colour = "shares_count"))+geom_smooth(method = "lm",aes(y = likes_count))
ggplot(raydu_page,aes(created_time))+geom_line(aes(y = likes_count, colour = type)) + geom_smooth(method = "lm",aes(y = likes_count))
ggplot(raydu_page,aes(x=type)) + geom_bar()

ggplot(saint_page,aes(created_time))+geom_line(aes(y = likes_count, colour = "likes_count")) + 
  geom_line(aes(y = comments_count, colour = "comments_count")) +  geom_line(aes(y = shares_count, colour = "shares_count"))+geom_smooth(method = "lm",aes(y = likes_count))
ggplot(saint_page,aes(created_time))+geom_line(aes(y = likes_count, colour = type)) + geom_smooth(method = "lm",aes(y = likes_count))
ggplot(saint_page,aes(x=type)) + geom_bar()

ggplot(zyn_page,aes(created_time))+geom_line(aes(y = likes_count, colour = "likes_count")) + 
  geom_line(aes(y = comments_count, colour = "comments_count")) +  geom_line(aes(y = shares_count, colour = "shares_count"))+geom_smooth(method = "lm",aes(y = likes_count))
ggplot(zyn_page,aes(created_time))+geom_line(aes(y = likes_count, colour = type)) + geom_smooth(method = "lm",aes(y = likes_count))
ggplot(zyn_page,aes(x=type)) + geom_bar()
