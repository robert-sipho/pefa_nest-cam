# lines to check
    # 20 - name of the csv out
    # 35 - class file name
    # 43 - model file name
    # 44 - weights file name
    # 78 - directory containing the images 
    # 85 - number of images to be processed within the dir



import cv2 as cv
import argparse
import sys
import numpy as np
import os.path
import pdb
from PIL import Image
from od_functions import getOutputsNames, drawPred, postprocess_visual
import random
import shutil
fileID=open('AN_98_weights2000.csv','w')

# Initialize the parameters
confThreshold = 0.3  #Confidence threshold
nmsThreshold = 0.3  #Non-maximum suppression threshold
#pdb.set_trace()
inpWidth = 768#780#768     #Width of network's input image width=2048 height=1536
inpHeight =768#780#68#608     #Height of network's input image

#parser = argparse.ArgumentParser(description='Object Detection using YOLO in OPENCV')
#parser.add_argument('--image', help='Path to image file.')
#parser.add_argument('--video', help='Path to video file.')
#args = parser.parse_args()
        
# Load names of classes
classesFile = "obj.names";

classes = None
with open(classesFile, 'rt') as f:
    classes = f.read().rstrip('\n').split('\n')

# Give the configuration and weight files for the model and load the network using them.

modelConfiguration = "yolov3.cfg";
modelWeights = "yolov3_2000.weights";

net = cv.dnn.readNetFromDarknet(modelConfiguration, modelWeights)
net.setPreferableBackend(cv.dnn.DNN_BACKEND_OPENCV)
net.setPreferableTarget(cv.dnn.DNN_TARGET_CPU)

# Process inputs
#winName = 'Deep learning object detection in OpenCV'
#cv.namedWindow(winName, cv.WINDOW_NORMAL)

#outputFile = "yolo_out_py.avi"
#if (args.image):
#    # Open the image file
#    if not os.path.isfile(args.image):
#        print("Input image file ", args.image, " doesn't exist")
#        sys.exit(1)
#    cap = cv.VideoCapture(args.image)
#    outputFile = args.image[:-4]+'_yolo_out_py.jpg'
#elif (args.video):
#    # Open the video file
#    if not os.path.isfile(args.video):
#        print("Input video file ", args.video, " doesn't exist")
#        sys.exit(1)
#    cap = cv.VideoCapture(args.video)
#    outputFile = args.video[:-4]+'_yolo_out_py.avi'
#else:
#    # Webcam input
#    cap = cv.VideoCapture(0)

# Get the video writer initialized to save the output video
#if (not args.image):
#    vid_writer = cv.VideoWriter(outputFile, cv.VideoWriter_fourcc('M','J','P','G'), 30, (round(cap.get(cv.CAP_PROP_FRAME_WIDTH)),round(cap.get(cv.CAP_PROP_FRAME_HEIGHT))))

#while cv.waitKey(1) < 0:

image_dir='/media/robert/My Passport/Reconyx/Rankin 2017/My Site'
outputdir='output/'
path, dirs, files = next(os.walk(image_dir))

n=0

listoffiles=list()
for (dirpath,dirnames,filenames) in os.walk(path):
    listoffiles+=[os.path.join(dirpath,file) for file in filenames]

numbfilesneeded=100

while n<=numbfilesneeded:
    f=listoffiles[random.randint(0,len(listoffiles))]
    #pdb.set_trace()

#pdb.set_trace()
#for f in sorted(os.listdir(image_dir)): 
    if f.startswith('.'):   
        continue
    n=n+1
    #if(n>10972):
    #    fileID.close()
    #    break
    # get frame from the video
    #hasFrame, frame = cap.read()
    
    # Stop the program if reached end of video
    #if not hasFrame:
    #    print("Done processing !!!")
    #    print("Output file is stored as ", outputFile)
    #    cv.waitKey(3000)
    #    break

    # Create a 4D blob from a frame.
    #pdb.set_trace()
    #img=Image.open('img1.JPG')
    #img = cv.imread(path+'/'+f)
    img = cv.imread(f)
    fileID.write('\n'+f)
    blob = cv.dnn.blobFromImage(img, 1/255, (inpWidth, inpHeight), [0,0,0], 1, crop=False)

    # Sets the input to the network
    net.setInput(blob)

    # Runs the forward pass to get output of the output layers
    outs = net.forward(getOutputsNames(net))

    # Remove the bounding boxes with low confidence
    postprocess_visual(img, outs,classes,confThreshold,nmsThreshold,fileID)
    
    #fileID.close()
    # Put efficiency information. The function getPerfProfile returns the overall time for inference(t) and the timings for each of the layers(in layersTimes)
    #t, _ = net.getPerfProfile()
    #label = 'Inference time: %.2f ms' % (t * 1000.0 / cv.getTickFrequency())
    ##cv.putText(frame, label, (0, 15), cv.FONT_HERSHEY_SIMPLEX, 0.5, (0, 0, 255))

    ## Write the frame with the detection boxes
    #if (args.image):
    #pdb.set_trace()
    #outputFile='out'+f
    #temp=f[f.find('2017',0,len(f)):]
    temp=f[f.find('2017',55,len(f)):]
    #sitenumber=f[f.find('My Site/Site',0,len(f))+13]
    sitenumber=f[f.find('2017',50,len(f))-3:f.find('2017',50,len(f))-1]
    outputFile=sitenumber+'_'+temp
    cv.imwrite(outputdir+outputFile, img.astype(np.uint8));
    shutil.copy2(f,'originals')
    
    #else:
    #    vid_writer.write(frame.astype(np.uint8))
    #
    #cv.imshow(winName, frame)

fileID.close()
