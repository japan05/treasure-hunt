from pyswip import Prolog

prolog = Prolog()
prolog.consult("final_w.pl")

result = list(prolog.query("run(A)"))
if result:
    path = result[0]['A']
    print("A:", path)
