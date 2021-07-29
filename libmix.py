
# coding: utf-8

# In[ ]:

import networkx as nx
import numpy as np
import matplotlib.pyplot as plt
import pylab
import random
import math as mt


# In[ ]:

'''Klasa reprezentująca wierzchołek grafu. Zawiera pola: 
neighbours lista wierzchołków, z którymi połączony jest dany wierzchołek
name - unikalny numer wierzchołka
visits - liczba odwiedzin wierzchołka przez proces stochastyczny
Zawiera metodę: addNeighbour odpowiedzialną za dodawania nowych sąsiadów do listy neighbours'''
class vertex:
    neighbours = []
    name = 0
    visits = 0
    def addNeighbour(v):
        self.neighbours.append(v)


# In[ ]:

'''Funkcja służąca do tworzenia wysp w grafach typu archipelag.
Return: lista nOfIslands klik rozmiaru nOfVertices, które są połączone między dobą bridges krawędziami.'''
def islandGraph(nOfVertices,nOfIslands,bridges):
    islandSize=nOfVertices/nOfIslands
    islands = []
    print islandSize
    for i in range(0,nOfIslands):
        island = []
        for vert in range(0,islandSize):
            island.append(vertex())
        for vert in island: #wyspa jest grafem pełnym
            vert.neighbours = island[:]
            vert.neighbours.remove(vert)
        islands.append(island)
    #każda wyspa za pomocą mostów łączy się z innymi wyspami
    shift = 0
    for isl in range(0,nOfIslands):
        tmpIsl1 = islands[isl]
        for dest in range(isl+1,nOfIslands):
            shift = shift + 1      
            tmpIsl2 = islands[dest]
            for i in range(0,bridges): #most jest jednokierunkowy
                tmpIsl1[(i+shift)%islandSize].neighbours.append(tmpIsl2[(i+shift)%islandSize])
                tmpIsl2[(i+shift)%islandSize].neighbours.append(tmpIsl1[(i+shift)%islandSize])
    return islands


# In[ ]:

''' Funkcję przyjmująca jako argument listę wysp zwracaną przez funkcję islandGraph. 
Zwraca listę wierzchołków, którą interpretuje się jako graf.
'''
def islands2graph(islands):
    graph = []
    rec = 0
    for isl in islands:
        for vert in isl:
            vert.name = rec
            rec = rec+1
            graph.append(vert)
    return graph


# In[ ]:

'''Funkcja służąca do generowania klik rozmiaru N
Return: lista wierzchołków takich, że każdy wierzchołek posiada na swojej 
liście neighbours wszystkie pozostałe wierzchołki'''
def fullGraph(N):
    vertices = []
    for i in range(N):
        vertices.append(vertex())
        vertices[i].neighbours = []
    for i in range(N):
        temp = vertices[:]
        temp.remove(vertices[i])
        vertices[i].neighbours.extend(temp)
        vertices[i].name=i
    return vertices


# In[1]:

'''Funkcja generująca grafy cykliczne rozmiaru N. Tworzymy wierzchołki 1,2,3...,N, 
a następnie łączymy między sobą wszystkie wierzchołki i,i+1 oraz wierzchołki 1,N.
'''
def circleGraph(N):
    vertices = []
    for i in range(N):
        vertices.append(vertex())
        vertices[i].neighbours = []
    for i in range(N):
        vertices[i].neighbours.append(vertices[(i+1)%N])
        vertices[i].neighbours.append(vertices[(i-1)%N])
        vertices[i].name=i
    return vertices


# In[ ]:

''' Funkcja realizująca algorytm tworzenia grafów Barabasi-Alberta 
N rozmiar grafu
'''
def barabasiGraph(N,function = 'standard',nomPower=1.0,denomPower=1.0):
    vertices = []
    for i in range(N):
        vertices.append(vertex())
        vertices[i].neighbours=[]
        vertices[i].name=i
    #inicjalizacja
    vertices[0].neighbours.append(vertices[1])
    vertices[1].neighbours.append(vertices[0])
    #dodawanie nowych krawedzi
    for i in range(2,N):
        while not vertices[i].neighbours: #dodatkowy warunek zmiejszający szansę na niespójność grafu
            for j in range(i):
                if function == 'standard':
                    deg=mt.pow(1.0*len(vertices[j].neighbours),nomPower)
                elif function == 'log':
                    deg = mt.pow(mt.log(1.0*len(vertices[j].neighbours)),nomPower)
                denominator = sum([ mt.pow(1.0*len(v.neighbours),denomPower) for v in vertices[0:i]])
                probability = 1.0*deg/denominator
                rnd = random.uniform(0,1)
                if rnd<probability:
                    vertices[i].neighbours.append(vertices[j])
                    vertices[j].neighbours.append(vertices[i])
    return vertices


# In[ ]:

'''Funkcja obliczająca Total Variation Distance między wektorem visits, 
którego i-ty element reprezentuje liczbę odwiedzin przez proces w i-tym
wierzchołku, a rozkładem stacjonarnym.
'''
def TVdistanceB(visits):
    setSize = len(visits)#liczba wierzchołków
    totalMoves = sum(visits)#łączna liczba ruchów
    stationary = 1.0/float(setSize)
    distribution = [float(visits[i])/float(totalMoves) for i in range(setSize)]
    m = 0.0
    for i in xrange(setSize):
        m = m+(abs(distribution[i]-stationary))
    return m/2.0


# In[ ]:

'''Funkcja obliczająca Total Variation Distance między wektorem visits, 
którego i-ty element reprezentuje liczbę odwiedzin przez proces w i-tym
wierzchołku, a rozkładem stacjonarnym stationary.
'''
def TVdistance(visits,stationary):
    setSize = len(visits)#liczba wierzchołków
    totalMoves = sum(visits)#łączna liczba ruchów
    distribution = [float(visits[i])/float(totalMoves) for i in range(setSize)]
    m = 0.0
    for i in xrange(setSize):
        m = m+(abs(distribution[i]-stationary[i]))
    return m/2.0


# In[ ]:

'''Funkcja zamieniająca graf na listę z bieżącą 
liczbą odwiedzeń dla każdego wierzchołka'''
def graph2visits(graph):
    vis = []
    for vert in graph:
        vis.append(vert.visits)
    return vis


# In[ ]:

'''Funkcja, która przyjmuje jako argument wierzchołek grafu i zwraca losowy wierzchołek z listy jego sąsiadów'''
def jump(vertex):
    return random.choice(vertex.neighbours)


# In[2]:

'''Funkcja zwrcajacąca wektor liczby krawędzi wierzchołków.'''
def edgeDistribution(graph):
    edgeNumbers = []
    counts = []
    for v in graph:
        edgeNumbers.append(len(v.neighbours))
    for i in range(len(graph)):
        counts.append(edgeNumbers.count(i))
    return counts


# In[ ]:

'''Funkcja zerująca liczbę odwiedzin w każdym z wierzchołków grafu graph'''
def clearGraph(graph):
    size = len(graph)
    for i in range(size):
        graph[i].visits = 0


# In[ ]:

#funkcja sluzaca do badania spojnosci grafu G o N wierzcholkach. Zwraca True, jesli graf jest spojny i False, jesli graf jest niespojny.
def isConnected(G):
    N=len(G)
    visited = np.zeros(N)
    nvis=0
    stack = []
    stack.append(G[0])
    visited[0]=1
    while stack:
        vs=stack.pop()
        nvis = nvis+1
        for v in vs.neighbours:
            if not visited[v.name]:
                visited[v.name]=1
                stack.append(v)
    if nvis==N:
        return True
    else:
        return False


# In[ ]:

#funkcja zwracajaca przyblizony rozklad stacjonarny grafu G po M krokach
def approximateStationaryDistribution(G,M):
    v=G[1]
    for i in range(M):
        v.visits = v.visits+1
        v=jump(v)
    distr = graph2visits(G)
    clearGraph(G)
    return [ 1.0*x/M for x in distr ]


# In[9]:

'''Funkcja symulująca proces Markowa na grafie Graph i zwracająca czas mieszania'''
def mixingTime(graph,epsilon, resolution=1,plotRange=1000,approx=False,distr=[]):
    N = 0 #liczba wykonanych skoków
    size = len(graph)
    vertex = graph[1] #nie ma większego znaczenia, od którego wierzchołka zaczniemy
    vertex.visits = vertex.visits + 1
    distances = [] #przechowuję tu dodatkowe statystyki - TVdist po N size,size+10,... krokach
    for i in range(size): #pierwszy przebieg pętli: co najmniej tyle, ile wynosi liczba wierzchołków
        vertex = jump(vertex) #skok do nastepnego wierzcholka
        vertex.visits = vertex.visits + 1 #zaktualizowanie liczby wizyt
        N = N+1 #aktualizacja liczby skokow
    visitsvec = graph2visits(graph)
    if approx:
        tvd = TVdistance(visitsvec,distr)
    else:
        tvd = TVdistanceB(visitsvec)
    mixed = mixing(tvd,epsilon)
    while mixed == False: #dopóki łańcuch nie jest zmieszany, skacz, obliczaj i zapisuj zapisuj tvdistance
        if(N<size+plotRange): #tvdistance chcemy znac plotRange razy od N poczynajac
            distances.append(tvd)
        for i in range(resolution): #mniejsza dokładność, ale za to szybciej
            vertex = jump(vertex)
            vertex.visits = vertex.visits + 1
            N = N+1
        visitsvec = graph2visits(graph)
        if approx:
            tvd = TVdistance(visitsvec,distr)
        else:
            tvd = TVdistanceB(visitsvec)
        mixed = mixing(tvd,epsilon)
    mT = N
    while N<size+plotRange: #po osiągnięciu czasu mieszania
        distances.append(tvd)
        for i in range(resolution): #mniejsza dokładność, ale za to szybciej
            vertex = jump(vertex)
            vertex.visits = vertex.visits + 1
            N = N+1
        visitsvec = graph2visits(graph)
        if approx:
            tvd = TVdistance(visitsvec,distr)
        else:
            tvd = TVdistanceB(visitsvec)
        mixed = mixing(tvd,epsilon)   
    clearGraph(graph)
    return mT,distances


# In[ ]:

'''Zmodyfikowana funkcja symulująca proces Markowa na grafie Graph i zwracająca czas mieszania
modyfikacja polega na obliczaniu czasu mieszania dla szerszej klasy łańcuchów Markowa, tj. takich, których 
rozkład stacjonarny nie jest rozkładem jednostajnym. Rozklad stacjonarny jest szacowany poprzez wykonanie statN skokow.'''
def mixingTimeB(graph,epsilon, resolution=1,plotRange=1000, statN=500000):
    N = 0 #liczba wykonanych skoków
    size = len(graph)
    stationary = approximateStationaryDistribution(graph,statN) #aproksymacja rozkladu stacjonarnego
    vertex = graph[1] #nie ma większego znaczenia, od którego wierzchołka zaczniemy
    vertex.visits = vertex.visits + 1
    distances = [] #przechowuję tu dodatkowe statystyki - TVdist po N size,size+10,... krokach
    for i in range(size): #pierwszy przebieg pętli: co najmniej tyle, ile wynosi liczba wierzchołków
        vertex = jump(vertex) #skok do nastepnego wierzcholka
        vertex.visits = vertex.visits + 1 #zaktualizowanie liczby wizyt
        N = N+1 #aktualizacja liczby skokow
    visitsvec = graph2visits(graph)
    tvd = TVdistance(visitsvec,stationary)
    mixed = mixing(tvd,epsilon)
    while mixed == False: #dopóki łańcuch nie jest zmieszany, skacz, obliczaj i zapisuj zapisuj tvdistance
        if(N<size+plotRange): #tvdistance chcemy znac plotRange razy od N poczynajac
            distances.append(tvd)
        for i in range(resolution): #mniejsza dokładność, ale za to szybciej
            vertex = jump(vertex)
            vertex.visits = vertex.visits + 1
            N = N+1
        visitsvec = graph2visits(graph)
        tvd = TVdistance(visitsvec,stationary)
        mixed = mixing(tvd,epsilon)
    mT = N
    while N<size+plotRange: #po osiągnięciu czasu mieszania
        distances.append(tvd)
        for i in range(resolution): #mniejsza dokładność, ale za to szybciej
            vertex = jump(vertex)
            vertex.visits = vertex.visits + 1
            N = N+1
        visitsvec = graph2visits(graph)
        tvd = TVdistance(visitsvec,stationary)
        mixed = mixing(tvd,epsilon)   
    clearGraph(graph)
    return mT,distances


# In[3]:

def mixing(tvd,epsilon):
    #tvd = TVdistance(visits)
    if tvd<epsilon:
        return True
    else:
        return False


# In[10]:

'''Funkcja obsługująca przeprowadzanie steps prób Monte Carlo na grafie graph, 
w celu oszacowania czasów mieszania dla epsilon i esymacji TVdistance na 
przedziale [wielkość grafu,plotRange]
Zmienna resolution odpowiada za dokładność oszacowań - jeśli resolution=r, to
TVdistance będzie obliczana co r kroków, a nie w każdym kroku'''
def monteCarlo(graph,steps,epsilon,resolution=1,plotRange=1000,approx=False):
    mTs = []
    function = [0]*plotRange
    distr=approximateStationaryDistribution(graph,3500000)
    for i in range(steps):
        clearGraph(graph)#zerujemy wizyty w grafie
        experiment = mixingTime(graph,epsilon,resolution,approx=approx,distr=distr)
        mTs.append(float(experiment[0]))
        function = [function[i] + experiment[1][i] for i in range(plotRange)]
    function = [float(x)/float(steps) for x in function]
    clearGraph(graph)
    return mTs,function


# In[ ]:

def monteCarloB(N,steps,epsilon,resolution=1,plotRange=1000,nom=1.0,denom=1.0):
    mTs = []
    function = [0]*plotRange
    for i in range(steps):
        graph = barabasiGraph(N,nomPower=nom,denomPower=denom)
        while not isConnected(graph):
            graph = barabasiGraph(N)
        clearGraph(graph)#zerujemy wizyty w grafie
        experiment = mixingTimeB(graph,epsilon,resolution)
        mTs.append(float(experiment[0]))
        function = [function[i] + experiment[1][i] for i in range(plotRange)]
    function = [float(x)/float(steps) for x in function]
    clearGraph(graph)
    return mTs,function


# In[ ]:

def mean(numbers):
    return float(sum(numbers)) / max(len(numbers), 1)


# In[ ]:

def averageTVeps(l,eps,N):
    args = [i for i,j in enumerate(l) if j<eps]
    if args == []:
        args.append(-N)
    return min(args)+N


# In[4]:

'''Funkcja do rysowania grafów'''
def plotGraph(graph,nn):
    N=len(graph)
    plt.gcf().clear()
    coordX=[0]*N
    coordY=[0]*N
    used=[0]*N
    nodes=[]
    for i in range(N):
        if len(graph[i].neighbours)>nn:
            nodes.append(graph[i])
    numberOfNodes=len(nodes)
    step = 2.0*mt.pi/numberOfNodes
    for i in range(numberOfNodes):
        numOfNeigh =len(nodes[i].neighbours) 
        coordX[nodes[i].name]=3.0*mt.sqrt(N)*mt.cos(i*step)/mt.sqrt(numOfNeigh)
        coordY[nodes[i].name]=3.0*mt.sqrt(N)*mt.sin(i*step)/mt.sqrt(numOfNeigh)
        used[nodes[i].name]=1
        littleStep = 2.0*mt.pi/numOfNeigh
        for j in range(numOfNeigh):
            if not used[nodes[i].neighbours[j].name]:
                nnn=len(nodes[i].neighbours[j].neighbours)
                coordX[nodes[i].neighbours[j].name]=mt.sqrt(nnn)*mt.cos(j*littleStep)+coordX[nodes[i].name]
                coordY[nodes[i].neighbours[j].name]=mt.sqrt(nnn)*mt.sin(j*littleStep)+coordY[nodes[i].name]
                used[nodes[i].neighbours[j].name]=1
    for i in range(N):
        for j in range(len(graph[i].neighbours)):
            plt.plot([coordX[i],coordX[graph[i].neighbours[j].name]],[coordY[i],coordY[graph[i].neighbours[j].name]],'k-',lw=0.5)
    plt.scatter(coordX,coordY)
    plt.savefig('losowy.png',dpi=300)


# In[5]:

def graph2nx(graph):
    G = nx.Graph()
    for v in graph:
        for u in v.neighbours:
            G.add_edge(v.name,u.name,weight=1)
    pos=nx.spring_layout(G)
    nx.draw(G,pos, node_size=20,weight=None,with_labels=False)
    pylab.show()


# In[ ]:



