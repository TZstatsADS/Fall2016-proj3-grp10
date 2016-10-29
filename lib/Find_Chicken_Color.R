library(jpeg)
library(EBImage)
library(gbm)
setwd("/Users/caikezhi/Desktop/images")

color.blocks=matrix(0,nrow=36000,ncol=3)
for (i in 1:1000)
{
  if (i<10) 
    file.name=sprintf("chicken_000%s.jpg",i)
  else if (i<100)
    file.name=sprintf("chicken_00%s.jpg",i)
  else if (i<1000)
    file.name=sprintf("chicken_0%s.jpg",i)
  else file.name=sprintf("chicken_%s.jpg",i)
  arr=readJPEG(file.name)
  arr=resize(arr,6,6)
  for(j in 1:6)
    for (k in 1:6)
    {
      color.blocks[i*36-36+j*6-6+k,]=arr[j,k,]
    }
  print(i)
}

#amn=(nrow(color.blocks)-1)*sum(apply(color.blocks,2,var))
#for(i in 2:15) amn[i]=sum(kmeans(color.blocks,centers=i)$withinss) 
#plot(1:15,amn)

fit=kmeans(color.blocks,2)
sum.RGB=c(sum(fit$centers[1,]),sum(fit$centers[2,]))
if(sum.RGB[1]>sum.RGB[2]) 
{
  base.chicken.color=fit$centers[2,]
}else 
    { base.chicken.color=fit$centers[1,] }
chicken.color.identifier=c(base.chicken.color[1]/base.chicken.color[2]*0.75,
    base.chicken.color[1]/base.chicken.color[2]*1.25,base.chicken.color[2]/base.chicken.color[3]*0.75)

#######################################################################################################
Make_color_feature=function(color.blocks)
{
  for(i in 1:1000)
  {
    cc.count=0
    for(j in 1:36)
    {
      k=i*36-36+j
      if(color.blocks[k,2]==0)OoO1=FALSE
      else OoO1=((color.blocks[k,1]/color.blocks[k,2])>chicken.color.identifier[1])
      if(color.blocks[k,2]==0)OoO2=FALSE
      else OoO2=((color.blocks[k,1]/color.blocks[k,2])<chicken.color.identifier[2])
      if(color.blocks[k,3]==0)OoO3=FALSE
      else OoO3=((color.blocks[k,2]/color.blocks[k,3])>chicken.color.identifier[3])
      if(OoO1&OoO2&OoO3) cc.count=cc.count+1
    }
    cc[i]=cc.count
  }
  return(cc)
}
###################################################################################################
cc=array()
cc[1:1000]=Make_color_feature(color.blocks)

###################################################################################################
dog.color.blocks=matrix(0,nrow=36000,ncol=3)
for (i in 1:1000)
{
  if (i<10) 
    file.name=sprintf("dog_000%s.jpg",i)
  else if (i<100)
    file.name=sprintf("dog_00%s.jpg",i)
  else if (i<1000)
    file.name=sprintf("dog_0%s.jpg",i)
  else file.name=sprintf("dog_%s.jpg",i)
  arr=readJPEG(file.name)
  arr=resize(arr,6,6)
  for(j in 1:6)
    for (k in 1:6)
    {
      dog.color.blocks[i*36-36+j*6-6+k,]=arr[j,k,]
    }
  print(i)
}

cc[1001:2000]=Make_color_feature(dog.color.blocks)

dd=cc/(max(cc)-min(cc))

