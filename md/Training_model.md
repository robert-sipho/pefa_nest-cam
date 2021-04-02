# Workflow

![](https://github.com/nuwcru/krmp_image-class/blob/master/docs/Training_workflow.jpg)

# Local

## 1. Raw Images
The first step is to gather your images. The number of images will depend on your specific circumstances. More different types of classes in the model will require more classes. Classes should be somewhat equally well represented in the data set. That being said, the images should also be representative of your original dataset. You should not expect machine learning to be able to correctly identify an image that is completely unlike any it has seen in the training data. For fairly good results, something like 600 images per class should be used.

## 2. Cropped Images
To save time on evaluation, we'll crop the top and bottom sections of the reconyx images. This is information that RECONYX applies to every image; it doesn't help our models figure out if there's an object of interest (OOI) present.

Images are cropped using the ```cropout.py``` script. 

## 3. Add Bounding Boxes
To train our model, we need to first locate the OOI's within each image so the model can learn. We have forked the repository and renamed it krmp_OpenLabeling. Use the instructions located within this repository to add bounding boxes (which produces txt files) to the image folder. 

# Web - Google

## 4. Upload to GDrive
Upload the images plus their bounding box txt files to the appropriate folder in GDrive. If this is your first time training the model, the images will go into the ```/img``` folder. If you're retraining a model, we'd like to keep track of the images used for re-training efforts so for now, put the images used to train the model a second time in the folder ```/img2```. If it's your third time training the model, the images will go into the folder ```/img3``` and so on. 

##. 5. Training and Test data sets
Create a txt file that points our model to training and test data sets. Use the ```splitTrainAndTest.py``` script to do this. It will dump the resulting txt files into the folder darknet_ad_nestlings (or whatever model you're training). 

## 6. Train the model


Now the easy part. We need to set up the YOLOV3 and tell it where to find everything. The exact configuration will depend on what computational resources you are using to train. We used colab to do the training. This requires setting up a google drive folder and a colab script. You can just copy the format of this google drive that was used for training a 2 class problem. 

* [Adults and Nestlings](https://drive.google.com/open?id=1kiITPSkY_QhI1M0IIRzbwZTDpM4ZrpyU)
* [eggs](https://drive.google.com/drive/u/1/folders/1SPUQt3tXjHEKutn0gUeHlkFPwcl5MGp1)
* [bands](https://drive.google.com/open?id=1UbLpFufrEpIJRfrZjDJE--boA-od1d6v)

### /backup folder
This starts empty. The program uses this folder to put the trained weights periodically. If you are using colab, you only have 12 hours of computation at a time so the weights need to be saved periodically as you go.

### /bin folder
This is where the yolo material is placed after being pulled using the colab script

### /cfg folder 
This is where the yolov3.cfg script is placed. Most of the file can remain unchanged. You should alter several things. 

Search for 'classes' (lines:610, 696, 783) and replace the number with the number of classes you have. As you are doing that change the number of filters in the paragraph above the classes, i.e. (lines:603, 689, 776). To (((# classes)+5)*3) for example I had 2 classes so ((2+5)*3)=21. Notice that there are many lines where filters are defined; you only want to change it in the three places. Also note that the other filters are always a power of 2. Don't change those. 

### /cuDNN folder
Legally speaking, you should download your own version of this file from the Nvidia website (https://developer.nvidia.com/cuda-zone). You'll just need to extract this and put it in the folder. 

### /img folder
You'll put all your images and all the .txt files that came from labelImg. Make sure everything is in here. For example, it should definitely have an even number of files in here. 

### obj.data file
It tells the program the location of the different folders and files. You should be able to use the same file if you are using colab, and change the paths to match what you have if you are doing it locally. 

### obj.names file
just the names of the classes that you have. 

### test.txt and train.txt files
These files you'll need to generate using splitTrainAndTest.py. First alter lines 26 and 28 to match with the paths that you will have in the drive (or folder if not suing colab). Put splitTrainAndTest.py outside the folder that you have all your images in. Then run it using the command python3 splitTrainAndTest.py "folderwimages". This will generate the two txt files. Right now it is set up to put 10% in the test set and 90% in the train set randomly. You can change this by changing line 17 to the ratio you want. 10% is probably good for most things. Maybe if you are low on images you might put fewer images in the test set? 

### Unititled.ipynb file
This is the colab script to set up everything. Note some of the things you only need to run the first time. And the statement that runs the yolo script will depend on whether you are running it the first time or not. 

## Training 
If you are using colab or just running locally once the program starts running it will take some time. Depending on the number of images and number of classes, as well as the speed of the hardware you are using this might take sometime around 12 hours to get a solution that is behaving sensibly. Files with names like yolov3_1000.weights will start showing up in that folder. These are the files that can be used to evaluate new images. The details of the evaluation of the files will be covered in Evaluate.md

# Training folder structure

File structure is the same for all three model training exercises (adults/nestlings, eggs, bands).

```

├── Untitled1.ipynb          <- Filed used to train model using google colab (google resources)
|
├── Untitled10.ipynb         <- 
|
├── train.txt				 <- a file pointing to the images used for training
|
├── test.txt 				 <- a file pointing to the images used for testing
|
├── obj.names                <- indicates the classes
|
├── obj.data                 <-
|
├── darknet53.conv.74        <- file used to train the model locally
|
├── /backup
│   ├── yolov3_8000.weights. <- weight files generated in the training process
│   ├── yolov3_9000.weights
│   ├── yolov3_10000.weights
│   └── ...
│
├── /cfg                     
│   └── yolov3.cfg           <- model file generated in the training process
|
|── /img
|   ├── cluster 31777.JPG    <- image used to train model
|   ├── cluster 31777.txt    <- corresponding txt file that indicates where the object is in the photo
|   ├── cluster 31778.JPG   
|   ├── cluster 31778.txt   
|   └── ...                 
|
└── /img2
    ├── x.JPG				 <- potentially empty folder only to be used for additional training rounds (followed by /img3 etc.)
    ├── x.txt
    └── ...

```


