library(BiocManager)
library(EBImage)
library(tidyverse)
library(magick)

######## Bone Fractures Detection
##################### traing set of images ######################

jpgFractures_names<- list.files(path ="D:/Desktop/R for Statistical Data Science Project/Bone Fractures Detection/train/images",pattern = "*.jpg",full.names = TRUE )
images_Fr<-lapply(jpgFractures_names,readImage)

dimensions<-data.frame(unique(lapply(images_Fr,dim)))
dimensions
display(images_Fr[[1]])



txtFractures_names<-list.files("D:/Desktop/R for Statistical Data Science Project/Bone Fractures Detection/train/labels",pattern="*.txt",full.names = TRUE)
txtfiles_Fr<-lapply(txtFractures_names,read.table)



#### check images and txt files are indexed correctly

jpgFractures_names[1:3]
txtFractures_names[1:3]

#str_sub(txtFractures_names[1],1,nchar(txtFractures_names[1])-4)
#names_ref<-function(x){str_sub(x,1,nchar(x)-4)}
#image_names<-sapply(jpgFractures_names,names_ref)
#txt_names<-sapply(txtFractures_names,names_ref)
#unique(image_names==txt_names)

basename(jpgFractures_names[1])
tools::file_path_sans_ext(basename(jpgFractures_names[1]))


dir_strip<-function(x){tools::file_path_sans_ext(basename(x))}
image_names<-sapply(jpgFractures_names,dir_strip)
txt_names<-sapply(txtFractures_names,dir_strip)

unique(txt_names==image_names)


##########creating dataframe for txt file info


####adding image number to each instance of a fracture
length(txtFractures_names)
txtfiles_Fr[1]
c(dim(txtfiles_Fr[[1]])[1],1)
array(1,c(dim(txtfiles_Fr[[1]])[1],1))



n<-1
while (n<length(txtFractures_names)+1) {
  txtfiles_Fr[[n]]<-cbind(txtfiles_Fr[[n]],"Image"=array(n,c(dim(txtfiles_Fr[[n]])[1],1)))
n<-n+1}


txtfiles_Fr[[2]]


###### creating list + adding number to each fracture
n<-2
Fracture_list<-txtfiles_Fr[[1]]
while (n<length(txtFractures_names)+1) {Fracture_list<-rbind(Fracture_list,txtfiles_Fr[[n]])
n<-n+1}

Fracture_list$Fracture_Number<-c(1:length(Fracture_list))

head(Fracture_list)

##################### cropping images of fractures

crop_Fr<-function(n){as_EBImage(image_read(jpgFractures_names[[Fracture_list[n,6]]])%>%image_crop(geometry = geometry_area(
  width = Fracture_list[n,4]*640,
  height = Fracture_list[n,5]*640,
  x_off = (Fracture_list[n,2]*640)-(Fracture_list[n,4]*640)/2,
  y_off = (Fracture_list[n,3]*640)-(Fracture_list[n,5]*640)/2)))}


Fracture_Images<-list()
n<-1
while (n<length(Fracture_list)) {Fracture_Images[[n]]<-crop_Fr(n)
n<-n+1}

display(images_Fr[[1]])
display(Fracture_Images[[1]])
display(Fracture_Images[[2]])
