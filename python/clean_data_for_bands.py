import pdb
import pandas as pd
import os
import re
from imganlayze import imagename, classname,percentname,leftxname,topyname1,width1name,height1name,datename,gettotalsize

#fileID=open("F:\Site1_2.txtout","r")
#t=1
mfolder='/home/robert/darknet/tinyeggs/'
#mfolder='/home/robert/Documents/darknetpaths/testing_bands/'
#fileID9=open('90plus.csv','w')
#fileID8=open('80plus.csv','w')
#fileID7=open('70plus.csv','w')
#fileID6=open('60plus.csv','w')
#fileID5=open('50plus.csv','w')
for t in range(1,150):
    
    #fileID5=open(str(t)+'_50plus.csv','w')
    #pdb.set_trace()
    mlist=[]
    
    #pdb.set_trace()
    #regex=re.compile(*.txtout)
    for root, dirs,files in os.walk(mfolder):
        #pdb.set_trace()
        for mfile in files:
            #pdb.set_trace()
            if os.path.splitext(mfile)[1]=='.txtout' and int(mfile[4:mfile.find('_')])==t:
                mlist.append(mfile)

        #mlist.append(regex.match(files))
    #pdb.set_trace()
    if not mlist:
        continue
    fileID9=open(str(t)+'_90plus.csv','w')
    fileID8=open(str(t)+'_80plus.csv','w')
    fileID7=open(str(t)+'_70plus.csv','w')
    fileID6=open(str(t)+'_60plus.csv','w')
    #fileID=open(str(t)+'out.csv','w')
    #presize=gettotalsize(mlist)
    #pdb.set_trace()
    #fileID.write("\"Image_name\",\"Class1\",\"Percent1\",\"leftx1\",\"topy1\",\"width1\",\"height1\",\"Class2\",\"Percent2\",\"leftx2\",\"topy2\",\"width2\",\"height2\",\"Class3\",\"Percent3\",\"leftx3\",\"topy3\",\"width3\",\"height3\",\"Class4\",\"Percent4\",\"leftx4\",\"topy4\",\"width4\",\"height4\",\"Class5\",\"Percent5\",\"leftx5\",\"topy5\",\"width5\",\"height5\",\"Class6\",\"Percent6\",\"leftx6\",\"topy6\",\"width6\",\"height6\"")
    #database=pd.read_csv(r'tempplate.csv',dtype={'Image_name':str,'Class1':str,'Percent1':float,'leftx1':float,'topy1':float,'width1':float,'height1':float,'Class2':str,'Percent2':float,'leftx2':float,'topy2':float,'width2':float,'height2':float,'Class3':str,'Percent3':float,'leftx3':float,'topy3':float,'width3':float,'height3':float,'Class4':str,'Percent4':float,'leftx4':float,'topy4':float,'width4':float,'height4':float,'Class5':str,'Percent5':float,'leftx5':float,'topy5':float,'width5':float,'height5':float,'Class6':str,'Percent6':float,'leftx6':float,'topy6':float,'width6':float,'height6':float})
    #pdb.set_trace()
    for mfile in mlist:

        #pdb.set_trace()
        n=0
        with open(mfolder+mfile,"r") as f:
            for _ in range(4):
                next(f)
            mystart=0    
            for line in f:
                #pdb.set_trace()
                if not line[0:5]=="Enter":
                    img_name=imagename(previousline)
                    #date_name=datename(previousline)
                    class_name=classname(line)
                    percent_name=percentname(line)
                    leftx_name=leftxname(line)
                    topy1_name=topyname1(line)
                    width1_name=width1name(line)
                    height1_name=height1name(line) 
                    if int(percent_name)>90:
                        fileID9.write('\n'+img_name+','+percent_name)                       
                    elif int(percent_name)>80:
                        fileID8.write('\n'+img_name+','+percent_name)    
                    elif int(percent_name)>70:
                        fileID7.write('\n'+img_name+','+percent_name)
                    elif int(percent_name)>60:
                        fileID6.write('\n'+img_name+','+percent_name)
                    #elif int(percent_name)>50:
                        #fileID5.write('\n'+img_name+','+percent_name)

                else:
                    if n==0 and mystart==1:
                        img_name=imagename(previousline)
                        #fileID.write('\n'+img_name)
                        #database=database.append({'Image_name':img_name},ignore_index=True)
                    n=0
                    mystart=1
                    previousline=line
        #pdb.set_trace() 


    fileID9.close()
    fileID8.close()
    fileID7.close()
    fileID6.close()
    #fileID5.close()
    #outputfilename="compiledout"+str(t)+".csv"
    #database.to_csv(outputfilename,index=False)

    