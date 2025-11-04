from pyswip import Prolog

prolog = Prolog()
prolog.consult("full_pl.pl")

result = list(prolog.query("setup_world()"))

# BFS
result = list(prolog.query("find_gold_path_bfs(Path,I)"))
if result:
    path = result[0]['Path']
    print("BFS path:", path)
    i = result[0]['I']
    print("BFS I:", i)
# DFS
result = list(prolog.query("find_gold_path_dfs(Path,I)"))
if result:
    path = result[0]['Path']
    print("DFS path:", path)
    i = result[0]['I']
    print("DFS I:", i)
else:
    print("no DFS")
result = list(prolog.query("find_gold_path_ids(Path)"))
if result:
    path = result[0]['Path']
    print("IDS path:", path)

else:
    print("no IDS")
result = list(prolog.query("find_gold_path_ucs(Path,I)"))
if result:
    path = result[0]['Path']
    print("UCS path:", path)
    i = result[0]['I']
    print("UCS I:", i)

result = list(prolog.query("find_gold_path_astar_weighted(1,Path,I)"))
if result:
    path = result[0]['Path']
    print("a* 1 path:", path)
    i = result[0]['I']
    print("a* 1 I:", i)

result = list(prolog.query("find_gold_path_astar_weighted(2,Path,I)"))
if result:
    path = result[0]['Path']
    print("a* 2 path:", path)
    i = result[0]['I']
    print("a* 2 I:", i)

result = list(prolog.query("find_gold_path_astar_weighted(0.5,Path,I)"))
if result:
    path = result[0]['Path']
    print("a* 0.5 path:", path)
    i = result[0]['I']
    print("a* 0.5 I:", i)

# result = list(prolog.query("utility_search(5,3,1,Path,I)"))
# if result:
#     path = result[0]['Path'][::-1]
#     print("util 5 3 1 path:", path)
#     i = result[0]['I'][::-1]
#     print("util 5 3 1 I:", i)

# result = list(prolog.query("utility_search(8,2,3,Path,I)"))
# if result:
#     path = result[0]['Path'][::-1]
#     print("util 8 2 3 path:", path)
#     i = result[0]['I'][::-1]
#     print("util 8 2 3 I:", i)

# result = list(prolog.query("utility_search(100,1000,10000,Path,I)"))
# if result:
#     path = result[0]['Path'][::-1]
#     print("util 100,1000,10000 path:", path)
#     i = result[0]['I'][::-1]
#     print("util 100,1000,10000 I:", i)



