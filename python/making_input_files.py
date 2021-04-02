import random
import os
import subprocess
import sys
import pdb
from datetime import datetime
import argparse


def main(argv):
    parser=argparse.ArgumentParser(description='not sure')
    parser.add_argument("--i", help='place to look for files')
    parser.add_argument("--o", help='place to put output text files')
    args=parser.parse_args()
    inputlocation=args.i
    outputlocation=args.o
    startTime = datetime.now()

    image_dir = inputlocation#'/media/robert/My Passport/Reconyx/Rankin 2017/My Site'
    save_path=outputlocation#'~/Document/darknetpaths/'
    # fileID=open('myfilepaths.txt','w')
    # pdb.set_trace()
    # dirnames=os.listdir(image_dir)
    n = 0
    #pdb.set_trace()
    for r, subdirs, fout in os.walk(image_dir):
        for folder in subdirs:
            n=0
            #pdb.set_trace()
            for a,b,c in os.walk(image_dir+'/'+folder):
                print('hey')
                #pdb.set_trace()
                mfilename = folder.replace(' ','')+'_'+str(n)+'.txt'
                fileID = open(save_path+mfilename, 'w')
        
                m = 0
                for myfile in c:
                    if(myfile.split(".")[1] == "jpg" or myfile.split(".")[1] == "JPG"):
                        #pdb.set_trace()
                        fileID.write(os.path.join(a, myfile)+'\n')
                        m = m+1
                    if(m > 10000):
                        n = n+1
                        m = 0
                        fileID.close()
                        mfilename = folder.replace(' ','')+'_'+str(n)+'.txt'
                        fileID = open(save_path+mfilename, 'w')
                        # fileID.write(image_dir+'/'+myfile+'\n')
    fileID.close()
    print(datetime.now()-startTime)

if __name__=="__main__":
    main(sys.argv[1:])    


# Explanation
# this file takes in a input directory and makes a file with all the jpeg files in it to be used for yolo. It makes batches of 10k images to save time potentially on analysis
# you need to tell the program where to find the starting directory and it will go down to all the subfolders inside that folder (maybe). You can run this program with something like 
# python3 making_input_files.py --i /media/robert/My Passport/Reconyx/Rankin 2017/My Site --o /home/robert/documents/place_where_you_want_the_output_files 
