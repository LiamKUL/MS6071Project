Just wanted to put a little message here about how to run the code since we have many different files.

Running order
Reticulate stuff.R
Image Loading.R
Model codes(will cover separately)
app.R

Classification CNN.R is all that needs to run for the main pipeline and is the only model used in the app.

For detection stuff (non app pipeline)
Detection_CNN_Data_Prep.R
Detection_CNN.R
Detection_CNN_Box_Testing.R
Detection_CNN_Object_Presence_Testing.R


I wouldn't advise running the detection stuff at the same time as the regular pipeline. Its not really computationally feasible, its horrendously demanding.
