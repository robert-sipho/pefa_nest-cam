import random
import os
import subprocess
import sys
import pdb

def split_data_set(image_dir):

    f_val = open("test.txt", 'w')
    f_train = open("train.txt", 'w')
    #pdb.set_trace()
    
    path, dirs, files = next(os.walk(image_dir))
    data_size = len(files)

    ind = 0
    data_test_size = int(0.1 * data_size)
    test_array = random.sample(range(data_size), k=data_test_size)
    
    for f in os.listdir(image_dir):
        if(f.split(".")[1] == "jpg" or f.split(".")[1] == "JPG"):
            ind += 1
            
            if ind in test_array:
                #pdb.set_trace()
                f_val.write('/Volumes/GoogleDrive/My Drive/nuwcru git resources/krmp_image-class/darknet_wildlife/img'+f+'\n')
            else:
                f_train.write('/Volumes/GoogleDrive/My Drive/nuwcru git resources/krmp_image-class/darknet_wildlife/img'+f+'\n')


split_data_set(sys.argv[1])
