import pdb
def imagename(previousline):
    #out=
    return previousline[72:(previousline.find("Pre")-2)]

def classname(line):
    return line[0:line.find(':')].strip()    

def percentname(line):
    return line[line.find(':')+1:line.find('%')].strip()


def leftxname(line):
    return line[line.find('left_x')+7:line.find('top_y')].strip()

def topyname1(line):
    return line[line.find('top_y')+6:line.find('width')].strip()

def width1name(line):
    return line[line.find('width')+6:line.find('height')].strip()

def height1name(line):            
    return line[line.find('height')+7:line.find(')')].strip()

def datename(line):
    
    newline=line[line.find("My Site")+8:]
    newline=newline[newline.find('/')+1:newline.find(':')]
    print(newline)
    pdb.set_trace()
    return "hey"

def file_len(fname):
    with open("/home/robert/darknet/tinyeggs/"+fname) as f:
        for i,l in enumerate(f):
            pass
        return i-3

def gettotalsize(mlist):
    n=0
    for mfile in mlist:
        n=n+file_len(mfile)
    return n    