#set Python before loading keras3
Sys.setenv(
 RETICULATE_PYTHON =
    "C:/Users/valcl/Documents/.virtualenvs/r-keras311/Scripts/python.exe"
)

library(keras3)
library(EBImage)

image_size<-256
grid_size<-16
num_outputs<-5

## putting yolo info into array for cnn to train on

y_train<-array(0,dim=c(length(train_img_names),grid_size,grid_size,num_outputs))


#filling arrays
for (i in seq_len(nrow(train_Fracture_list))){

img_id<-train_Fracture_list$Image[i]
x<-train_Fracture_list$V2[i]
y<-train_Fracture_list$V3[i]
width<-train_Fracture_list$V4[i]
height<-train_Fracture_list$V5[i]

#find cell containing centre
grid_x<-min(floor(x*grid_size)+1,grid_size)
grid_y<-min(floor(y*grid_size)+1,grid_size)


#1:fracture present
y_train[img_id,grid_y,grid_x,1]<-1

#2/3:centre coordinates relative to the grid cell
y_train[img_id,grid_y,grid_x,2]<-x*grid_size-floor(x*grid_size)
y_train[img_id,grid_y,grid_x,3]<-y*grid_size-floor(y*grid_size)

#4/5:normalised width and height
y_train[img_id,grid_y,grid_x,4]<-width
y_train[img_id,grid_y,grid_x,5]<-height
}


dim(y_train)

#putting yolo info into array for cnn to validate on
y_valid <- array(0,dim = c(length(valid_img_names),grid_size,grid_size,num_outputs))

#filing arrays
for (i in seq_len(nrow(valid_Fracture_list))){

img_id<-valid_Fracture_list$Image[i]
x<-valid_Fracture_list$V2[i]
y<-valid_Fracture_list$V3[i]
width<-valid_Fracture_list$V4[i]
height<-valid_Fracture_list$V5[i]

grid_x<-min(floor(x*grid_size)+1,grid_size)
grid_y<-min(floor(y*grid_size)+1,grid_size)

y_valid[img_id,grid_y,grid_x,1]<-1
y_valid[img_id,grid_y,grid_x,2]<-x*grid_size-floor(x*grid_size)
y_valid[img_id,grid_y,grid_x,3]<-y*grid_size-floor(y*grid_size)
y_valid[img_id,grid_y,grid_x,4]<-width
y_valid[img_id,grid_y,grid_x,5]<-height
}





#function to resize images and put as array

prepare_image<-function(file){
img<-readImage(file)
img<-resize(img,w=image_size,h=image_size)
img<-as.array(img)
img
}

#Prepare training images
X_train<-array(0,dim=c(length(train_img_names),image_size,image_size,3))

for(i in seq_along(train_img_names)){
  
X_train[i,,,]<-prepare_image(train_img_names[i])

if(i%%100==0) {cat("Training images processed:", i, "\n")}

}

#Prepare validation images
X_valid<-array(0,dim=c(length(valid_img_names),image_size,image_size,3))

for (i in seq_along(valid_img_names)){
X_valid[i,,,]<-prepare_image(valid_img_names[i])

if(i%%50==0){
cat("Validationimagesprocessed:",i,"\n")
 }
}

# Check dimensions and pixel range
dim(X_train)
dim(X_valid)

range(X_train)
range(X_valid)



model<-keras_model_sequential(input_shape=c(image_size,image_size,3))|>

layer_conv_2d(filters=32,kernel_size=3,padding="same",activation="relu")|>
layer_max_pooling_2d(pool_size=2)|>

layer_conv_2d(filters=64,kernel_size=3,padding="same",activation="relu")|>
layer_max_pooling_2d(pool_size=2)|>

layer_conv_2d(filters=128,kernel_size=3,padding="same",activation="relu")|>
layer_max_pooling_2d(pool_size=2)|>

layer_conv_2d(filters=128,kernel_size=3,padding="same",activation="relu")|>
layer_max_pooling_2d(pool_size=2)|>

layer_conv_2d(filters=5,kernel_size=1,padding="same",activation="sigmoid")

model|>summary()


#########loss function
  
fracture_loss<-function(y_true,y_pred){

#true and predicted boxes
box_true<-y_true[,,,2:5,drop=FALSE]
box_pred<-y_pred[,,,2:5,drop=FALSE]

#centre coords and dimensions
tx<-box_true[,,,1,drop=FALSE]
ty<-box_true[,,,2,drop=FALSE]
tw<-box_true[,,,3,drop=FALSE]*grid_size
th<-box_true[,,,4,drop=FALSE]*grid_size

px<-box_pred[,,,1,drop=FALSE]
py<-box_pred[,,,2,drop=FALSE]
pw<-box_pred[,,,3,drop=FALSE]*grid_size
ph<-box_pred[,,,4,drop=FALSE]*grid_size

#box boundaries
tx1<-tx-tw/2
ty1<-ty-th/2
tx2<-tx+tw/2
ty2<-ty+th/2

px1<-px-pw/2
py1<-py-ph/2
px2<-px+pw/2
py2<-py+ph/2

#Intersection
ix1<-op_maximum(tx1,px1)
iy1<-op_maximum(ty1,py1)
ix2<-op_minimum(tx2,px2)
iy2<-op_minimum(ty2,py2)

iw<-op_maximum(ix2-ix1,0)
ih<-op_maximum(iy2-iy1,0)

intersection<-iw*ih

#Union
true_area<-tw*th
pred_area<-pw*ph
union_area<-true_area+pred_area-intersection

#IoUloss
iou<-intersection/(union_area)
iou_loss<-1-iou

}


####compile model
model|>compile(optimizer="adam",loss=fracture_loss)

history<-model|>fit(
x=X_train,
y=y_train,
validation_data=list(X_valid,y_valid),
epochs=15,
batch_size=16,
)

plot(history)


y_pred<-predict(model,X_valid,batch_size = 16)





