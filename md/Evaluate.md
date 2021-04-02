## Background 
If you want to train a model for a specific feature you are in the wrong place. Please consult Training_model.md for instructions on how to do that. If you have previously trained a model you are in the right place. You should have at least one file that has a name like yolov3_1000.weights. You will also need some photos to test on. 
These files are really large and .git can't handle them. They are hosted on google drive.

#### Eggs
https://drive.google.com/file/d/1-1WPrNn0cqi6ph3chQDHA06hyKbKTSLt/view?usp=sharing
* additional weight files can be found [here](https://drive.google.com/drive/u/1/folders/1gGrq3xjn7Ijtz4cS_2Ij-3jCIhdjPodY)


#### Adult and nestling 
https://drive.google.com/file/d/1tYo5F_TYR4O_JREAHSo3xJgpjVxEEdSM/view?usp=sharing

* additional weight files can be found [here](https://drive.google.com/drive/u/1/folders/1g7NWYS8ChNhraLKw9GHyaBBNX5L4dPcF)

#### Bands
https://drive.google.com/file/d/1-6DlJlsi5NYMuYl8kWpJaQw5M1Lkz8HU/view?usp=sharing
* additional weight files can be found [here](https://drive.google.com/drive/u/1/folders/1fXWEdX2gNYYUq8bzOhSTZBv2t0eoT-VO)



# Step 1
Test to make sure it is kinda making some sense. Take a simple test image and place it in a folder with the weights file and object_detection_yolo.py. The test image should hopefully be a simple image that is one of the 'easier' images. You will also need the .cfg file from training and the .names file you used for training. You will need to alter line 35 so that corresponds to the name of the weights file you are using. Make sure line 26,34,35 all are correct and those files are in the same folder. Now run the file using the command python3 object_detection_yolo.py --image=filename
Where filename is the name of the image of interest. 

# Step 2 
Hopefully when you ran that command it output an image in the same folder with the same name with 'out' at the end of the name. Look at this image. It will hopefully have bounding boxes drawn around the objects of interest. If this is not the case, the solution might not have converged yet and you might need to train more to get a good model. 

# Step 3 

Assuming that step 2 was succesfull and it seems to be starting to make sense, then you can move on to use this model to a larger degree. Open file my_object_detection.py and change lines 10, 25, 33, 34, 68. Line 10 is the filename of your output .csv file. Line 25, 33, 34 should correspond to the file names as before. Line 68 is the name of the folder that you want to look at. You can also change line 73. Right now it only looks at the first 1000 images in the folder. Just change that to whatever. It might take a second per image so just be cautious before running something on a huge image set. Now run this using the stament 'python3 my_object_detection.py'. This script will now produce a .csv file with the details of the images. 

## CSV file

The csv file has some columns. Each row corresponds to a image. The first column is the name of the image file. There are then 6x6 columns after. Each group of 6 corresponds to a possible detection event. 1: class of detection. 2: percent likelyhood that it is infact that object. 3,4 Top corner location 5,6 height and width of the image. This is repeated 6 times. This was done because the csv file needs to be a square matrix. If you want more than 6 possible detections you'll need to change line 84 of od_functions.py to something else. If less than 6 objects are detected the object label is '-1'. 

### Finish

Hopefully, the final csv file can be used for further processing. 
