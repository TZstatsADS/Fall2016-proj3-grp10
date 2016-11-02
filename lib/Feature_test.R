library(jpeg)
library(EBImage)
library(radiomics)
setwd("/Users/caikezhi/Desktop/images")

Make_color_feature=function(color.blocks)
{
    cc.count=0
    for(j in 1:36)
    {
      k=j
      if(color.blocks[k,2]==0)OoO1=FALSE
      else OoO1=((color.blocks[k,1]/color.blocks[k,2])>chicken.color.identifier[1])
      if(color.blocks[k,2]==0)OoO2=FALSE
      else OoO2=((color.blocks[k,1]/color.blocks[k,2])<chicken.color.identifier[2])
      if(color.blocks[k,3]==0)OoO3=FALSE
      else OoO3=((color.blocks[k,2]/color.blocks[k,3])>chicken.color.identifier[3])
      if(OoO1&OoO2&OoO3) cc.count=cc.count+1
    }
  return(cc.count)
}

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
fit=kmeans(color.blocks,2)
sum.RGB=c(sum(fit$centers[1,]),sum(fit$centers[2,]))
if(sum.RGB[1]>sum.RGB[2]) 
{
  base.chicken.color=fit$centers[2,]
}else 
{ base.chicken.color=fit$centers[1,] }
chicken.color.identifier=c(base.chicken.color[1]/base.chicken.color[2]*0.75,
                           base.chicken.color[1]/base.chicken.color[2]*1.25,base.chicken.color[2]/base.chicken.color[3]*0.75)


Get_feature=function(cord,num)
{
  the.feature=matrix(0,nrow=35)
  for (i in 1:num)
  {
    if (i<10) {
      file.name=sprintf("%s_000%s.jpg",cord,i) 
      }else if (i<100) {
      file.name=sprintf("%s_00%s.jpg",cord,i)
      }else if (i<1000) { file.name=sprintf("%s_0%s.jpg",cord,i)
      }else file.name=sprintf("%s_%s.jpg",cord,i)
      
    print(file.name)
    arr=readJPEG(file.name)
    arr66=resize(arr,6,6)
    array36=matrix(0,nrow=36,ncol=3)
    for(j in 1:6)
      for (k in 1:6)
        array36[j*6+k-6,]=arr66[j,k,]
    ccc=Make_color_feature(array36)            
    
    #######################################################################    
    bss=resize(arr,100,100)
    #bss=medianFilter(bss,2)
    bnw=bss[,,1]*0.2989+bss[,,2]*0.5870+bss[,,3]*0.1140
    bss=normalize(bnw)
    #ccc=Get_texture(bss,kmeans.fit)
    cm=glcm(bss,angle=0,d=1)
    ccc=cbind(ccc,calc_features(bss),calc_features(cm))
    the.feature=cbind(the.feature,t(ccc))
    print(i)
  }
  cai=the.feature
  return(cai)
}
###########################################################################
setwd("/Users/caikezhi/Desktop/images_test")
a=Sys.time()
#output=cbind(Get_feature("chicken",1000)[,2:1001],Get_feature("dog",1000)[,2:1001])
output=Get_feature("image",2020)
output=output[,2:dim(output)[2]]
b=Sys.time()
print(b-a)
for ( i in 1:35) output[i,]=(output[i,]-min(output[i,]))/(max(output[i,])-min(output[i,]))
output=output[-c(10,11),]
#write.csv(output,file="final.csv")
sift.feature=read.csv(file="sift features_test.csv")
sift.feature=rbind(as.matrix(sift.feature),as.matrix(output))
write.csv(sift.feature,file="final.csv")

