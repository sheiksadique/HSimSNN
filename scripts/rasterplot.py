import numpy as np
import pylab

def loadHData(fname='../input.txt'):
    data = []
    with open(fname) as f:
        zs = f.readlines()
    for z in zs:
        if z != "EmptySpikeTrain":
            data.append([x.strip('[()]') for x in z.split(',')])
    
    data = np.array(data)
    data.flatten()
    data = data.astype('float').reshape((-1,2))
    return data

def plotHData(fname='../input.txt'):
    data = loadHData(fname)
    pylab.plot(data[:,1], data[:,0], '.')
    pylab.xlabel('t (ms)')
    pylab.ylabel('Neuron idx')



if __name__=='__main__':
    plotHData(fname='input.txt')
    plotHData(fname='output.txt')
    pylab.legend(['Input','Output'])
    pylab.show()

