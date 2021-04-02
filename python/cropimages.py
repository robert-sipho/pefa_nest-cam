
import random
import os
import subprocess
import sys
import pdb
from PIL import Image

def split_data_set(image_dir):

    #output = open("cropout.txt", 'w')
    #f_train = open("peregrine_train.txt", 'w')
    
    path, dirs, files = next(os.walk(image_dir))
    data_size = len(files)

    #f=open('')
    newpath = 'cropout' 
    if not os.path.exists(newpath):
        os.makedirs(newpath)

    area=(1,31,2048,1505)
    for f in os.listdir(image_dir):
        #pdb.set_trace()
        img=Image.open(path+'/'+f)
        img.crop(area).save('cropout'+'/'+f)

    #ind = 0
    #data_test_size = int(0.1 * data_size)
    #test_array = random.sample(range(data_size), k=data_test_size)
    
    #for f in os.listdir(image_dir):
    #    if(f.split(".")[1] == "jpg" or f.split(".")[1] == "JPG"):
    #        ind += 1
            
    #        if ind in test_array:
     #           f_val.write('peregrine/'+image_dir+'/'+f+'\n')
    #        else:
    #            f_train.write('peregrine/'+image_dir+'/'+f+'\n')


split_data_set(sys.argv[1])