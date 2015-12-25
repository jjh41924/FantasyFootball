
def getCredentials(site):
    f = open('/home/ff/REF','r')
    i =0
    names = ""
    for i in range(0,100):
        l = f.readline()
        if(l==""): break
        l2 = l.split(",")
        if(site==l2[0]):
            return [l2[1], l2[2][:-1]]
        names = names +","+ l2[0]
    raise Exception("No credentials for site you requested. Sites Present: "+names)
        #print printCredentials(l2)

#print getCredentials("ESPN")[0]