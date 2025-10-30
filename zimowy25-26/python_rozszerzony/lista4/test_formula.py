from formulas import *

a = ~Var('p') - Bool(True)
d = Var('p') - ((Var('p') - Var('q')) - Var('q'))
e = Disj(~Var("x"), Conj(Var('y'), Bool(True)))

print(e)
print(e.simpl())
print(a)
b = a.simpl()
c = Formula.simplify(a)
print(b)
print(c)
print(b.free_vars())
print(a.tautology())

print()
print(d)
print(d.tautology())