library(EBImage)
library(tidyverse)
library(magick)

######## Bone Fractures Detection

##################### traing set of images ######################

# load images
train_img_names<- list.files(path ="D:/Desktop/R for Statistical Data Science Project/Bone Fractures Detection/train/images",pattern = "*.jpg",full.names = TRUE )
train_images<-lapply(train_img_names,readImage)

#dim of images
dimensions<-data.frame(unique(lapply(train_images,dim)))

# load txt files 
train_txt_names<-list.files("D:/Desktop/R for Statistical Data Science Project/Bone Fractures Detection/train/labels",pattern="*.txt",full.names = TRUE)
train_txtfiles<-lapply(train_txt_names,read.table)

#### creating list for fractures

#***check images and txt files are indexed correctly*#

dir_strip<-function(x){tools::file_path_sans_ext(basename(x))}
unique(sapply(train_txt_names,dir_strip)==sapply(train_img_names,dir_strip))



#**adding image number to each instance of a fracture*

n<-1
while (n<length(train_txt_names)+1) {
  train_txtfiles[[n]]<-cbind(train_txtfiles[[n]],"Image"=array(n,c(dim(train_txtfiles[[n]])[1],1)))
n<-n+1}



#**creating list + adding number to each fracture*


n<-2

train_Fracture_list<-train_txtfiles[[1]]

while (n<length(train_txt_names)+1) {
  train_Fracture_list<-rbind(train_Fracture_list,train_txtfiles[[n]])
n<-n+1}

train_Fracture_list$Fracture_Number<-c(1:dim(train_Fracture_list)[1])

head(train_Fracture_list)



##################### validation set of images ######################

# load images
valid_img_names<- list.files(path ="D:/Desktop/R for Statistical Data Science Project/Bone Fractures Detection/valid/images",pattern = "*.jpg",full.names = TRUE )
valid_images<-lapply(valid_img_names,readImage)

#dim of images
dimensions<-data.frame(unique(lapply(valid_images,dim)))

# load txt files 
valid_txt_names<-list.files("D:/Desktop/R for Statistical Data Science Project/Bone Fractures Detection/valid/labels",pattern="*.txt",full.names = TRUE)
valid_txtfiles<-lapply(valid_txt_names,read.table)

#### creating list for fractures

#***check images and txt files are indexed correctly*#

dir_strip<-function(x){tools::file_path_sans_ext(basename(x))}
unique(sapply(valid_txt_names,dir_strip)==sapply(valid_img_names,dir_strip))



#**adding image number to each instance of a fracture*

n<-1
while (n<length(valid_txt_names)+1) {
  valid_txtfiles[[n]]<-cbind(valid_txtfiles[[n]],"Image"=array(n,c(dim(valid_txtfiles[[n]])[1],1)))
  n<-n+1}



#**creating list + adding number to each fracture*


n<-2

valid_Fracture_list<-valid_txtfiles[[1]]

while (n<length(valid_txt_names)+1) {
  valid_Fracture_list<-rbind(valid_Fracture_list,valid_txtfiles[[n]])
  n<-n+1}

valid_Fracture_list$Fracture_Number<-c(1:dim(valid_Fracture_list)[1])

head(valid_Fracture_list)





##################### cropping images of train fractures ###################

crop_Fr<-function(n){as_EBImage(image_read(train_img_names[[train_Fracture_list[n,6]]])%>%image_crop(geometry = geometry_area(
  width = train_Fracture_list[n,4]*640,
  height = train_Fracture_list[n,5]*640,
  x_off = (train_Fracture_list[n,2]*640)-(train_Fracture_list[n,4]*640)/2,
  y_off = (train_Fracture_list[n,3]*640)-(train_Fracture_list[n,5]*640)/2)))}


Fracture_Images<-list()
n<-1
while (n<length(train_Fracture_list)) {Fracture_Images[[n]]<-crop_Fr(n)
n<-n+1}

display(train_images[[1]])
display(Fracture_Images[[1]])
display(Fracture_Images[[2]])

display(channel(train_images[[1]],"Gray"))




