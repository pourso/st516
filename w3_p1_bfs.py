#Uses python3

import sys
import queue

def distance(adj, s, t):
    #write your code here
    # init distances to 0, s to 0
    dist = [(len(adj)+1) for x in range(len(adj))]
    dist[s] = 0

    q = [s]
    while(q):
        x = q.pop(0)
        for y in adj[x]:
            if dist[y] == (len(adj)+1):
                q.append(y)
                dist[y] = dist[x]+1
                if(y==t): return dist[y]
    return -1

if __name__ == '__main__':
    input = sys.stdin.read()
    data = list(map(int, input.split()))
    n, m = data[0:2]
    data = data[2:]
    edges = list(zip(data[0:(2 * m):2], data[1:(2 * m):2]))
    adj = [[] for _ in range(n)]
    for (a, b) in edges:
        adj[a - 1].append(b - 1)
        adj[b - 1].append(a - 1)
    s, t = data[2 * m] - 1, data[2 * m + 1] - 1
    print(distance(adj, s, t))