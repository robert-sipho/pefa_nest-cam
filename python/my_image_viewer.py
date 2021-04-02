import pdb
import cv2
import argparse
import sys
#from PIL import Image

#pdb.set_trace()
#mfilename="/media/robert/My Passport/Reconyx/Rankin 2017/My Site/Site 100/2017-07-17 20-32-22.JPG"
#myimage=cv2.imread(mfilename)


##pdb.set_trace()
#myimage=cv2.resize(myimage,(1024,768),interpolation=cv2.INTER_AREA)
#cv2.imshow('image',myimage)
#cv2.waitKey(0)
#cv2.destroyAllWindows()
##myimage.show()

def main(argv):
    parser=argparse.ArgumentParser(description='not sure')
    parser.add_argument("--i", help='this is the folder location of the input files')
    parser.add_argument("--o", help='this is the folder location of the output files')
    parser.add_argument("--images", help='this is the folder location of the output files')
    args=parser.parse_args()
    inputfolder=args.i
    outputfolder=args.o
    fileIDout=open(outputfolder,'a')
    image_locations=args.images
    with open(inputfolder,"r") as f:
        for _ in range(1):
                next(f)
        nnnn=0
        for line in f:
            nnnn=nnnn+1
            mlist=line.split(",")
            #pdb.set_trace()
            #print(nnnn)
            if not mlist[1] or int(mlist[2])<90:
                continue
            myimage=cv2.imread(image_locations+mlist[0])
            myimage=cv2.resize(myimage,(1024,768),interpolation=cv2.INTER_AREA)
            if mlist[1]:
                #pdb.set_trace()
                start=(int(0.5*(int(mlist[3]))),int(0.5*(int(mlist[4]))))
                end=(int(0.5*(int(mlist[3])+int(mlist[5]))),int(0.5*(int(mlist[6])+int(mlist[4]))))                
                cv2.rectangle(myimage,start,end,(0, 0, 255), 2)
                cv2.putText(myimage,mlist[1]+','+mlist[2],start,cv2.FONT_HERSHEY_SIMPLEX,0.75,(0,0,0),2)
                if mlist[7]:
                    madd=6
                    start=(int(0.5*(int(mlist[3+madd]))),int(0.5*(int(mlist[4+madd]))))
                    end=(int(0.5*(int(mlist[3+madd])+int(mlist[5+madd]))),int(0.5*(int(mlist[6+madd])+int(mlist[4+madd]))))
                    myimage=cv2.resize(myimage,(1024,768),interpolation=cv2.INTER_AREA)
                    cv2.rectangle(myimage,start,end,(0, 0, 255), 2)
                    cv2.putText(myimage,mlist[1+madd]+','+mlist[2+madd],start,cv2.FONT_HERSHEY_SIMPLEX,0.75,(0,0,0),2)
                if mlist[13]:
                    madd=12
                    start=(int(0.5*(int(mlist[3+madd]))),int(0.5*(int(mlist[4+madd]))))
                    end=(int(0.5*(int(mlist[3+madd])+int(mlist[5+madd]))),int(0.5*(int(mlist[6+madd])+int(mlist[4+madd]))))
                    myimage=cv2.resize(myimage,(1024,768),interpolation=cv2.INTER_AREA)
                    cv2.rectangle(myimage,start,end,(0, 0, 255), 2)
                    cv2.putText(myimage,mlist[1+madd]+','+mlist[2+madd],start,cv2.FONT_HERSHEY_SIMPLEX,0.75,(0,0,0),2)
                if mlist[19]:
                    madd=18
                    start=(int(0.5*(int(mlist[3+madd]))),int(0.5*(int(mlist[4+madd]))))
                    end=(int(0.5*(int(mlist[3+madd])+int(mlist[5+madd]))),int(0.5*(int(mlist[6+madd])+int(mlist[4+madd]))))
                    myimage=cv2.resize(myimage,(1024,768),interpolation=cv2.INTER_AREA)
                    cv2.rectangle(myimage,start,end,(0, 0, 255), 2)
                    cv2.putText(myimage,mlist[1+madd]+','+mlist[2+madd],start,cv2.FONT_HERSHEY_SIMPLEX,0.75,(0,0,0),2)
                if mlist[25]:
                    madd=12
                    start=(int(0.5*(int(mlist[3+madd]))),int(0.5*(int(mlist[4+madd]))))
                    end=(int(0.5*(int(mlist[3+madd])+int(mlist[5+madd]))),int(0.5*(int(mlist[6+madd])+int(mlist[4+madd]))))
                    myimage=cv2.resize(myimage,(1024,768),interpolation=cv2.INTER_AREA)
                    cv2.rectangle(myimage,start,end,(0, 0, 255), 2)
                    cv2.putText(myimage,mlist[1+madd]+','+mlist[2+madd],start,cv2.FONT_HERSHEY_SIMPLEX,0.75,(0,0,0),2)
                if mlist[31]:
                    madd=12
                    start=(int(0.5*(int(mlist[3+madd]))),int(0.5*(int(mlist[4+madd]))))
                    end=(int(0.5*(int(mlist[3+madd])+int(mlist[5+madd]))),int(0.5*(int(mlist[6+madd])+int(mlist[4+madd]))))
                    myimage=cv2.resize(myimage,(1024,768),interpolation=cv2.INTER_AREA)
                    cv2.rectangle(myimage,start,end,(0, 0, 255), 2)
                    cv2.putText(myimage,mlist[1+madd]+','+mlist[2+madd],start,cv2.FONT_HERSHEY_SIMPLEX,0.75,(0,0,0),2)
                
            cv2.imshow('image',myimage)
            mykey=cv2.waitKey()
            #pdb.set_trace()
            if mykey==116: #key t
                fileIDout.write(image_locations+mlist[0]+'\n')
                #sys.exit()
            #if nnnn>99:
            #    fileIDout.close()
            #    sys.exit()
            if mykey==113:# 113 is q
                fileIDout.close()
                sys.exit()
            #cv2.destroyAllWindows()



if __name__=="__main__":
    main(sys.argv[1:])    

