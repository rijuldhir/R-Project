library("rvest")
library("tm")
library("igraph")
urls = "https://indianexpress.com/latest-news/"
Page = read_html(urls)
# mix = html_nodes(Page,"href")
# URLs = html_attr(html_nodes(Page,".bg a"),"href")
rank_data_html = html_nodes(Page,'.title a')
rank_data = html_attr(rank_data_html,'href')
#typeof(URLs)
#rank_data = html_text(rank_data_html)
#head(rank_data)
#rank_data=gsub(" ","",rank_data)
rank_data
n = length(rank_data)
n = 9
for(i in 1:n)
{
  newsPage<- read_html(rank_data[i])
  write.table(rank_data[i],file=paste("F:/RLab/url.csv",sep =""),append=TRUE, sep="\n\n", row.name=FALSE, col.name=FALSE)
  x = html_nodes(newsPage,'p')
  x = html_text(x);
  # matterHeading<- newsPage %>% html_nodes("p") %>% html_text()
  write.table(x, file=paste("F:/assg/article",paste(as.character(i),".txt",sep = ""),sep =""), append=TRUE, sep="\n\n", row.name=FALSE, col.name=FALSE)
  remove(newsPage,x)
}
# rank_data = html_text(mix)
# rank_data
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
docs = Corpus(DirSource("F:/assg"))
#toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ",x))})
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs,content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, c("the", "and",stopwords("english")))
dtm <- DocumentTermMatrix(docs,control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE),wordLengths=c(4, 20),bounds = list(global = c(1 , n-1))))
tf_idf <- as.matrix(dtm)
tf_idf
transpose = t(tf_idf)
dococ = tf_idf %*% transpose
dococ = round(dococ,8)
dococ
mi = max(as.numeric(unlist(dococ)))
mi
thres = mi*0.008
thres
for(i in 1:n)
{
  for( j in 1:n)
  {
    if(dococ[i,j]>thres)
      dococ[i,j]=1
    else
      dococ[i,j]=0
  }
}
dococ
actual_label = c(1,1,1,1,0,0,1,0,0,1,1,0,0,0,0,0,0,1,1,0,1,1,0,0,1,0,0,1,0,1,1,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,1,0,1,0,1,1,0,0,1,0,0,0,0,0,0,0,1,0,1,0,0,1,0,0,0,0,0,0,1)
new_label = matrix(actual_label,nrow=9,ncol=9)
new_label
ans = matrix(0,nrow=2,ncol=2)
ans
for(i in 1:n)
{
  for(j in 1:n)
  {
    if(new_label[i,j]==dococ[i,j] && new_label[i,j]==1)
    {
      ans[1,1] = ans[1,1]+1
    }
    else if(new_label[i,j]==dococ[i,j] && new_label[i,j]==0)
    {
      ans[2,2] = ans[2,2]+1
    }
    else if(new_label[i,j]!=dococ[i,j] && new_label[i,j]==0)
    {
      ans[2,1] = ans[2,1]+1
    }
    else if(new_label[i,j]!=dococ[i,j] && new_label[i,j]==1)
    {
      ans[1,2] = ans[1,2]+1
    }
  }
}
ans
accuracy = (ans[1,1]+ans[2,2])/sum(ans)
accuracy
write.csv(dococ,file=paste("F:/RLab/actual_table.csv",sep =""))
write.csv(new_label,file=paste("F:/RLab/Manual_table.csv",sep =""))
write.csv(ans,file=paste("F:/RLab/confusion_matrix.csv",sep =""))
write.table(accuracy,file=paste("F:/RLab/accuracy.txt",sep =""),append=TRUE, sep="\n\n", row.name=FALSE, col.name=FALSE)
# write.matrix(format(dococ, scientific=FALSE),file = paste("F:/RLab/1501CS37_2.csv",sep=""))