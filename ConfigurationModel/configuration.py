#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wednesday March 4, 2020

@author: ozan
"""

import random
#import math
import time
import sys

N = int(sys.argv[1])  # network size
alpha = float(sys.argv[2]) # powerlaw exponent

number_of_infected = int(sys.argv[3]) 
end_time = int(sys.argv[4]) 
gamma = float(sys.argv[5]) #recovery rate
beta = float(sys.argv[6]) #disease diffusion rate

position = str(sys.argv[7]) #position


k_max = int(N**(1/(alpha-1)))


# -----------------
# -----------------

def initialize_write_to_file(filename):
    f = open(filename+'.txt', "w")
    col_names = ''
    f.write(col_names)
    f.close()

def write_to_file(content,filename):
    f = open(filename+'.txt', "a")
    f.write(content)
    f.close()

# -----------------
# -----------------

def get_C(lst):
    result = 0
    for k in lst:
        result = result + k**(alpha*-1)
    return result**-1

# -----------------
# -----------------

lst = [ i+1 for i in range(k_max-1) ]
C = get_C(lst)

def prob(k):
    return C*(k**(alpha*-1))

# for k in lst:
#     print(prob(k))

cdf = []
current_sum = 0
for k in lst:
    current_sum = current_sum + prob(k)
    cdf.append(current_sum)

# -----------------
# -----------------

def sample_degree():
    r = random.uniform(0,1.0)
    result = 1
    for cur in cdf:
        if r > cur:
            result += 1
        else:
            # print(a,r,result)
            return result

# -----------------
# -----------------

def sigma(lst):
    result = 0
    for k in lst:
        result = result + k
    return result

def locate_stub(lst,limit):
    sum = 0
    length = len(lst)
    for i in range(length):
        sum = sum + lst[i]
        if sum > limit:
            return i

# -----------------
# -----------------
# -----------------
# -----------------

def generate_stubs():
    nodes = [0] * N
    for i in range(N):
        nodes[i] = sample_degree()
    return nodes

# -----------------
# -----------------
# -----------------
# -----------------
# -----------------
# -----------------
# -----------------
# -----------------


def get_edge(nodes_list):
    n1 = 0
    n2 = 0
    attempt = 0
    while n1 == n2:
        if attempt > 100:
            break
        n1 = nodes_list[random.randrange(len(nodes_list))]
        n2 = nodes_list[random.randrange(len(nodes_list))]
        attempt += 1
    # ---------------------
    # --------------------- 
    nodes_list.remove(n1)
    nodes_list.remove(n2)
    if attempt > 100:
        return nodes_list,-1,-1
    else:     
        return nodes_list,n1,n2
    
# ---------------------------------------
# ---------------------------------------
# -- THE MAIN PROCEDURE -----------------

print('The procedure will be written in the log.txt file')

sys.stdout = open("log.txt", "w")

node_stubs = generate_stubs()
number_of_stubs = sigma(node_stubs)
stubs = list(range(number_of_stubs))

# -----------------
# -----------------

if not number_of_stubs % 2 == 0:
    #number_of_stubs = sum + 1
    a = random.randrange(N)
    node_stubs[a] = node_stubs[a] + 1
    number_of_stubs = sigma(node_stubs)

# print(number_of_stubs)
# for k in range(len(node_stubs)):
#     print(k,node_stubs[k])

nodes = number_of_stubs *[0]
k = 0
for node in range(len(node_stubs)):
    number_of_stubs_on_node =  node_stubs[node]
    for i in range(number_of_stubs_on_node):
        nodes[k] = node
        k+=1

# ------------------------------------------

edges = []
count = 1
while nodes != []:
    a = get_edge(nodes)
    nodes = a[0]
    if  a[1] == -1:
        continue
    elif a[1] < a[2]:
        edges.append((a[1],a[2]))
    else:
        edges.append((a[2],a[1]))
    if count % 1000 == 0:
        print(count)
    count += 1

# ---------------------------------------

print()
print("Edges generated.")
print()
print('Writing the model to file.')
print()

# -----------
edges_difference = edges.copy()
# -----------
edges_set = edges.copy()
edges_set = list(dict.fromkeys(edges_set))
# -----------
edges.sort()
edges_set.sort()

# ---------------------------------------

print("Number of original edges:",str(len(edges)))
if edges_set == edges:
    print('No duplicate edges.')
else:
    for edge in edges_set:
        edges_difference.remove(edge)



duplicate_count = len(edges_difference)

print('Duplicate edges to be removed:',str(duplicate_count))
print('------------------------------')
# print(edges_difference)
print()

# ---------------------------------------

print("Number of network edges without duplicates:",str(len(edges_set)))

# ---------------------------------------

network_nodes = []
for edge in edges_set:
    network_nodes.append(edge[0])
    network_nodes.append(edge[1])
network_nodes = list(dict.fromkeys(network_nodes))
network_nodes.sort()

# ---------------------------------------

print()
print("Number of nodes:",str(len(network_nodes)))
start_time = time.time()

# print(edges_set)
# ---------------------------------------
# ---------------------------------------
# ---------------------------------------
# ---------------------------------------
# ---------------------------------------
# ---------------------------------------

graph = {}
for (a,b) in edges_set:
    if a in graph:
        graph[a].append(b)
    else:
        graph[a] = [b]
    if b in graph:
        graph[b].append(a)
    else:
        graph[b] = [a]

# ---------------------------------------------------
# max_node_label = max(graph.keys())
# ---------------------------------------------------

reached_list = []
def pick_smallest_node_label_not_reached(lower_bound):
    for node in graph:
        if node >= lower_bound and not node in reached_list:
            return node
    return -1

# ---------------------------------------------------

def search_from(init_node):
    current_cluster = []
    #stack_history = []
    stack = [init_node]
    while stack != []:
        stack_pop = stack.pop(0)
        if stack_pop in graph and not stack_pop in current_cluster:
            accessible = graph[stack_pop]
            stack = stack + accessible
        if not stack_pop in current_cluster:
            current_cluster.append(stack_pop)
    return current_cluster

# ---------------------------------------------------
# ---------------------------------------------------
print()
print('Searching clusters.', flush=True)

count = 1
clusters = []
current_init = 0
while current_init >=0:
# for i in range(10):
    current_init = pick_smallest_node_label_not_reached(current_init)
    # print(current_init)
    clstr = search_from(current_init)
    # print(clstr)
    if  clstr != [-1]:
        clusters.append(clstr)
        reached_list = reached_list + clstr
    if count % 100 == 0:
        print(".",end='', flush=True)
    count += 1

def sorting(lst):
    lst2 = sorted(lst, key=len, reverse=True)
    return lst2

clusters = sorting(clusters)

print()
print()

# print(clusters)
reached_list.sort()
total_number_of_clusters = len(clusters)

print()
print()
print("Number of clusters: ",str(total_number_of_clusters))
print()

print("Visited nodes: ",len(reached_list))
print()

# sum = 0
# for i in range(50):
#     clusters[i] = sorted(list(dict.fromkeys(clusters[i])))
#     l = len(clusters[i])
#     # print(i,l)
#     sum += l
# print()
# print("Sum of number of nodes in the 50 more-crowded clusters: ",sum)
# print()

# print(reached_list)
print('Search completed in')
print('--- '+str(time.time() - start_time) + 'secs. ---')
print()
# print(clusters)


# -------------------------------------------
# -------------------------------------------
# -------------------------------------------
# -------------------------------------------
# -------------------------------------------
# -------------------------------------------
# -------------------------------------------
# -------------------------------------------

final_network_nodes = clusters[0]
final_network_nodes.sort()



print('==================================')
print('Number of nodes in the model file:',len(final_network_nodes))
final_edges_set = edges_set.copy()
for edge in final_edges_set:
    (a,b) = edge
    if not a in final_network_nodes or not b in final_network_nodes:
        final_edges_set.remove(edge)

print('Number of edges in the model file:',len(final_edges_set))

output = 'reactions\n'
count = 1
for edge in final_edges_set:
    output = output + 'r' + str(count) + 'a : '
    output = output + 'I(' + str(edge[0])
    output = output + ') + S(' + str(edge[1])
    output = output + ') -> I(' + str(edge[0])
    output = output + ') + I(' + str(edge[1]) +'), '+str(beta)+';\n'
    # -------------------
    output = output + 'r' + str(count) + 'b : '
    output = output + 'I(' + str(edge[1])
    output = output + ') + S(' + str(edge[0])
    output = output + ') -> I(' + str(edge[0])
    output = output + ') + I(' + str(edge[1]) +'), '+str(beta)+';\n'
    # -------------------
    if count % 1000 == 0:
        print(count, "edges.")
    count += 1

print()
print("Enumerating the infected reactions.")
print()

count = 1
for node in network_nodes:
    output = output + 'r' + str(count) + 'c : '
    output = output + 'I(' + str(node)
    output = output + ') -> R(' + str(node)
    output = output + '), '+str(gamma)+';\n'
    if count % 1000 == 0:
        print(count, "edges.")
    count += 1


print("There are",len(final_network_nodes),"nodes.")

output = output + "\ninitial state\n"


print('Infected are in position ' + position)


susceptible_nodes = final_network_nodes.copy()
infected_nodes = []
for cn in range(number_of_infected):
    # -------------------------------
    while True:
    
        if position == 'random':
            
            a = random.randrange(len(susceptible_nodes))
            inf_node = susceptible_nodes[a]
            print(inf_node)
            break
    
        if position == 'hub':
            
            nodes_stubs_pairs = []
            for k in range(len(node_stubs)):
                nodes_stubs_pairs.append((k,node_stubs[k]))
            print()
            print("The first",number_of_infected,"nodes with the greatest degree distribution.")
            print("------------------------")
            nodes_stubs_pairs = sorted(nodes_stubs_pairs, key=lambda x: x[1], reverse=True)
            for a in nodes_stubs_pairs[:number_of_infected]:
                print(a)
            print()
            inf_node = nodes_stubs_pairs[cn][0]
            print(inf_node)
            break
        
        if position == 'peripheral':
            
            nodes_stubs_pairs = []
            for k in range(len(node_stubs)):
                nodes_stubs_pairs.append((k,node_stubs[k]))
            print()
            print("The first",number_of_infected,"nodes with the smallest degree distribution.")
            print("------------------------")
            nodes_stubs_pairs = sorted(nodes_stubs_pairs, key=lambda x: x[1]) #, reverse=True)
            for a in nodes_stubs_pairs[:number_of_infected]:
                print(a)
            print()
            filtered_nodes_stubs_pairs = nodes_stubs_pairs.copy()
            for (a,b) in nodes_stubs_pairs:
                if not a in final_network_nodes:
                    filtered_nodes_stubs_pairs.remove((a,b)) 
            print()
            inf_node = filtered_nodes_stubs_pairs[cn][0]
            print(inf_node)
            break
        
        if position == 'mean':
            
            nodes_stubs_pairs = []
            for k in range(len(node_stubs)):
               nodes_stubs_pairs.append((k,node_stubs[k]))
            print()
            print("The first",number_of_infected,"nodes with the smallest degree distribution.")
            print("------------------------")
            nodes_stubs_pairs = sorted(nodes_stubs_pairs, key=lambda x: x[1]) #, reverse=True)
            for a in nodes_stubs_pairs[:number_of_infected]:
                print(a)
            sum = 0
            for (a,b) in nodes_stubs_pairs:
                sum = sum + b
            mean_dd = sum / len(nodes_stubs_pairs) 
            print("Mean degree distribution:",mean_dd)
            mean_dd = round(mean_dd)
            print("Mean dd after rounding:",mean_dd)   
            print()
            filtered_nodes_stubs_pairs = nodes_stubs_pairs.copy()
            for (a,b) in nodes_stubs_pairs:
                if not a in final_network_nodes or b < mean_dd:
                    filtered_nodes_stubs_pairs.remove((a,b)) 
            print("First",number_of_infected,"nodes")
            for a in filtered_nodes_stubs_pairs[:number_of_infected]:
                print(a)
            print()
            inf_node = filtered_nodes_stubs_pairs[cn][0]
            print(inf_node)
            break
    
        print('Error in the choice of position')
        break
    infected_nodes.append(inf_node)
    susceptible_nodes.remove(inf_node)

print()
print(len(infected_nodes),"of these are infected.")
print()
print(len(susceptible_nodes),"of these are susceptible.")


for i in infected_nodes:
    output = output + "I("+ str(i) +") "

for i in susceptible_nodes:
    output = output + "S("+ str(i) +") "

output = output + "\nuntil\n" + str(end_time) + "\n"


initialize_write_to_file('model')
write_to_file(output,'model')
print()
print('Model is written to the file "model.txt".')
print()

edges_output = ''
count = 1
for edge in final_edges_set:
    edges_output = edges_output + str(edge) + '\n'
    if count % 1000 == 0:
        print(count)
    count += 1

initialize_write_to_file('edges')
write_to_file(edges_output,'edges')
print()
print('Edges are written to the file "edges.txt".')
print()
# -----------------
# -----------------
# -----------------
