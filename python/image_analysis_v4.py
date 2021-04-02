import pdb
#import pandas as pd
import os
import re
from imganlayze_v4 import imagename, classname,percentname,leftxname,topyname1,width1name,height1name,datename,gettotalsize
import getopt
import sys
import argparse
#fileID=open("F:\Site1_2.txtout","r")
#t=1

def main(argv):
    for t in range(1,150):
        #pdb.set_trace()
        mlist=[]
        
        #mfrom=''
        #mgoing=''
        #try:
        #    mfrom,mgoing=getopt.getopt(argv,"hi:o",["ifile=","ofile="])
        #except getopt.GetoptError:
        #    print('-i <inputfile -o outputlocation')    
        #pdb.set_trace()

        parser=argparse.ArgumentParser(description='not sure')
        parser.add_argument("--i", help='this is the folder location of the input files')
        parser.add_argument("--o", help='this is the folder location of the output files')
        args=parser.parse_args()
        inputfolder=args.i
        outputfolder=args.o

        #regex=re.compile(*.txtout)
        for root, dirs,files in os.walk(inputfolder):
            #pdb.set_trace()
            for mfile in files:
                #pdb.set_trace()
                try:
                    if os.path.splitext(mfile)[1]=='.txtout' and int(mfile[4:mfile.find('_')])==t:
                        mlist.append(mfile)
                except: 
                    try:    
                        if os.path.splitext(mfile)[1]=='.txtout' and int(mfile[4:mfile.find('(')])==t:   
                            mlist.append(mfile)
                    except:
                        continue
            #mlist.append(regex.match(files))
        #pdb.set_trace()
        if not mlist:
            continue
        fileID=open(outputfolder+str(t)+'out.csv','w')
        #presize=gettotalsize(mlist)
        #pdb.set_trace()
        fileID.write("\"Image_name\",\"Class1\",\"Percent1\",\"leftx1\",\"topy1\",\"width1\",\"height1\",\"Class2\",\"Percent2\",\"leftx2\",\"topy2\",\"width2\",\"height2\",\"Class3\",\"Percent3\",\"leftx3\",\"topy3\",\"width3\",\"height3\",\"Class4\",\"Percent4\",\"leftx4\",\"topy4\",\"width4\",\"height4\",\"Class5\",\"Percent5\",\"leftx5\",\"topy5\",\"width5\",\"height5\",\"Class6\",\"Percent6\",\"leftx6\",\"topy6\",\"width6\",\"height6\"")
        #database=pd.read_csv(r'tempplate.csv',dtype={'Image_name':str,'Class1':str,'Percent1':float,'leftx1':float,'topy1':float,'width1':float,'height1':float,'Class2':str,'Percent2':float,'leftx2':float,'topy2':float,'width2':float,'height2':float,'Class3':str,'Percent3':float,'leftx3':float,'topy3':float,'width3':float,'height3':float,'Class4':str,'Percent4':float,'leftx4':float,'topy4':float,'width4':float,'height4':float,'Class5':str,'Percent5':float,'leftx5':float,'topy5':float,'width5':float,'height5':float,'Class6':str,'Percent6':float,'leftx6':float,'topy6':float,'width6':float,'height6':float})
        #pdb.set_trace()
        for mfile in mlist:

            #pdb.set_trace()
            n=0
            with open(inputfolder+mfile,"r") as f:
                for _ in range(8):
                    next(f)
                mystart=0 
                iterf=iter(f)   
                for line in iterf:
                    #pdb.set_trace()
                    if line[0:5]=="Enter":
                        if len(line)>20:
                            n=0
                            for _ in range(3):
                                line=next(iterf)
                            img_name=imagename(line)
                            #pdb.set_trace()
                        else:
                            break

                            
                    else:
                        
                        if n==0:
                            #pdb.set_trace()
                            #img_name=imagename(previousline)
                            #date_name=datename(previousline)
                            class_name=classname(line)
                            percent_name=percentname(line)
                            leftx_name=leftxname(line)
                            topy1_name=topyname1(line)
                            width1_name=width1name(line)
                            height1_name=height1name(line)  
                            #pdb.set_trace()  
                            fileID.write('\n'+img_name+','+class_name+','+percent_name+','+leftx_name+','+topy1_name+','+width1_name+','+height1_name)       
                            #df=pd.DataFrame({'Image_name':img_name,'Class1':class_name,"Percent1":percent_name,"leftx1":leftx_name,"topy1":topy1_name,"width1":width1_name,"height1":height1_name})       
                            #database=pd.concat([database,df],ignore_index=True)
                            n=1
                        elif n==1:
                            #img_name=imagename(previousline)
                            class_name=classname(line)
                            percent_name=percentname(line)
                            leftx_name=leftxname(line)
                            topy1_name=topyname1(line)
                            width1_name=width1name(line)
                            height1_name=height1name(line)  
                            #pdb.set_trace()        
                            fileID.write(','+class_name+','+percent_name+','+leftx_name+','+topy1_name+','+width1_name+','+height1_name)       
                            #database.at[len(database)-1,'Class2']=class_name
                            #database.at[len(database)-1,"Percent2"]=percent_name
                            #database.at[len(database)-1,"leftx2"]=leftx_name
                            #database.at[len(database)-1,"topy2"]=topy1_name
                            #database.at[len(database)-1,"width2"]=width1_name
                            #database.at[len(database)-1,"height2"]=height1_name
                            n=2   
                        elif n==2:
                            #img_name=imagename(previousline)
                            class_name=classname(line)
                            percent_name=percentname(line)
                            leftx_name=leftxname(line)
                            topy1_name=topyname1(line)
                            width1_name=width1name(line)
                            height1_name=height1name(line)  
                            #pdb.set_trace() 
                            fileID.write(','+class_name+','+percent_name+','+leftx_name+','+topy1_name+','+width1_name+','+height1_name)              
                            #database.at[len(database)-1,'Class3']=class_name
                            #database.at[len(database)-1,"Percent3"]=percent_name
                            #database.at[len(database)-1,"leftx3"]=leftx_name
                            #database.at[len(database)-1,"topy3"]=topy1_name
                            #database.at[len(database)-1,"width3"]=width1_name
                            #database.at[len(database)-1,"height3"]=height1_name
                            n=3  

                        elif n==3:
                            #img_name=imagename(previousline)
                            class_name=classname(line)
                            percent_name=percentname(line)
                            leftx_name=leftxname(line)
                            topy1_name=topyname1(line)
                            width1_name=width1name(line)
                            height1_name=height1name(line)  
                            #pdb.set_trace()  
                            fileID.write(','+class_name+','+percent_name+','+leftx_name+','+topy1_name+','+width1_name+','+height1_name)             
                            #database.at[len(database)-1,'Class3']=class_name
                            #database.at[len(database)-1,"Percent3"]=percent_name
                            #database.at[len(database)-1,"leftx3"]=leftx_name
                            #database.at[len(database)-1,"topy3"]=topy1_name
                            #database.at[len(database)-1,"width3"]=width1_name
                            #database.at[len(database)-1,"height3"]=height1_name
                            n=4
                        elif n==4:
                            #img_name=imagename(previousline)
                            class_name=classname(line)
                            percent_name=percentname(line)
                            leftx_name=leftxname(line)
                            topy1_name=topyname1(line)
                            width1_name=width1name(line)
                            height1_name=height1name(line)  
                            #pdb.set_trace()  
                            fileID.write(','+class_name+','+percent_name+','+leftx_name+','+topy1_name+','+width1_name+','+height1_name)             
                            #database.at[len(database)-1,'Class4']=class_name
                            #database.at[len(database)-1,"Percent4"]=percent_name
                            #database.at[len(database)-1,"leftx4"]=leftx_name
                            #database.at[len(database)-1,"topy4"]=topy1_name
                            #database.at[len(database)-1,"width4"]=width1_name
                            #database.at[len(database)-1,"height4"]=height1_name
                            n=5
                        elif n==5:
                            #img_name=imagename(previousline)
                            class_name=classname(line)
                            percent_name=percentname(line)
                            leftx_name=leftxname(line)
                            topy1_name=topyname1(line)
                            width1_name=width1name(line)
                            height1_name=height1name(line)  
                            #pdb.set_trace()              
                            fileID.write(','+class_name+','+percent_name+','+leftx_name+','+topy1_name+','+width1_name+','+height1_name) 
                            #database.at[len(database)-1,'Class5']=class_name
                            #database.at[len(database)-1,"Percent5"]=percent_name
                            #database.at[len(database)-1,"leftx5"]=leftx_name
                            #database.at[len(database)-1,"topy5"]=topy1_name
                            #database.at[len(database)-1,"width5"]=width1_name
                            #database.at[len(database)-1,"height5"]=height1_name
                            n=6
                        elif n==6:
                            #img_name=imagename(previousline)
                            class_name=classname(line)
                            percent_name=percentname(line)
                            leftx_name=leftxname(line)
                            topy1_name=topyname1(line)
                            width1_name=width1name(line)
                            height1_name=height1name(line)  
                            #pdb.set_trace()              
                            fileID.write(','+class_name+','+percent_name+','+leftx_name+','+topy1_name+','+width1_name+','+height1_name) 
                            #database.at[len(database)-1,'Class6']=class_name
                            #database.at[len(database)-1,"Percent6"]=percent_name
                            #database.at[len(database)-1,"leftx6"]=leftx_name
                            #database.at[len(database)-1,"topy6"]=topy1_name
                            #database.at[len(database)-1,"width6"]=width1_name
                            #database.at[len(database)-1,"height6"]=height1_name
                            n=7  
                        elif n==7:
                            #img_name=imagename(previousline)
                            class_name=classname(line)
                            percent_name=percentname(line)
                            leftx_name=leftxname(line)
                            topy1_name=topyname1(line)
                            width1_name=width1name(line)
                            height1_name=height1name(line)               
                            fileID.write(','+class_name+','+percent_name+','+leftx_name+','+topy1_name+','+width1_name+','+height1_name)  
                            n==8   
                        elif n==8:
                            #img_name=imagename(previousline)
                            class_name=classname(line)
                            percent_name=percentname(line)
                            leftx_name=leftxname(line)
                            topy1_name=topyname1(line)
                            width1_name=width1name(line)
                            height1_name=height1name(line)               
                            fileID.write(','+class_name+','+percent_name+','+leftx_name+','+topy1_name+','+width1_name+','+height1_name)  
                            n==9 
                        elif n==9:
                            #img_name=imagename(previousline)
                            class_name=classname(line)
                            percent_name=percentname(line)
                            leftx_name=leftxname(line)
                            topy1_name=topyname1(line)
                            width1_name=width1name(line)
                            height1_name=height1name(line)               
                            fileID.write(','+class_name+','+percent_name+','+leftx_name+','+topy1_name+','+width1_name+','+height1_name)  
                            n==10 
                        elif n==10:
                            #img_name=imagename(previousline)
                            class_name=classname(line)
                            percent_name=percentname(line)
                            leftx_name=leftxname(line)
                            topy1_name=topyname1(line)
                            width1_name=width1name(line)
                            height1_name=height1name(line)               
                            fileID.write(','+class_name+','+percent_name+','+leftx_name+','+topy1_name+','+width1_name+','+height1_name)  
                            n==11         




                        elif n==11:
                            #pdb.set_trace()
                            print("11 objects somehow")    

                    #else:
                    #    if n==0 and mystart==1:
                    #        img_name=imagename(previousline)
                    #        fileID.write('\n'+img_name)
                    #        #database=database.append({'Image_name':img_name},ignore_index=True)
                    #    for _ in range(0,6-n):
                    #        fileID.write(',,,,,,')    
                    #    n=0
                    #    mystart=1
                    #    previousline=line
            #pdb.set_trace() 


        fileID.close()
        #outputfilename="compiledout"+str(t)+".csv"
        #database.to_csv(outputfilename,index=False)


if __name__=="__main__":
    main(sys.argv[1:])    

    