from __future__ import annotations

class FreeVarError(Exception):
    def __init__(self, *args: object) -> None:
        super().__init__(*args)

class Formula:
    def __str__(self):
        return NotImplementedError("has to be implemented in the subclass")
    
    def __add__(self, other : Formula) -> Formula:
        return Disj(self,other)
    
    def __mul__(self,other : Formula) -> Formula:
        return Conj(self,other)
    
    def __sub__(self,other : Formula) -> Formula:
        return Imp(self,other)
    
    def __invert__(self) -> Formula:
        return Not(self)
    
    def tautology(self) -> bool:
        from itertools import product

        vars = sorted(list(self.free_vars()))
        for values in product([False, True], repeat=len(vars)):
            mapping = dict(zip(vars, values))
            if not self.evaluate(mapping):
                return False
        return True
    
    def evaluate(self, _ : dict[str, bool]) -> bool:
        raise NotImplementedError("has to be implemented in the subclass")
    
    def simpl(self) -> Formula:
        raise NotImplementedError("has to be implemented in the subclass")
    
    def free_vars(self) -> set:
        raise NotImplementedError("has to be implemented in the subclass")
    
    @classmethod
    def simplify(cls, formula : Formula) -> Formula:
        return formula.simpl()


class Var(Formula):
    def __init__(self, name : str) -> None:
        self.val = name

    def __str__(self) -> str:
        return self.val
    
    def evaluate(self, vars: dict[str, bool]) -> bool:
        if self.val in vars:
            return vars[self.val]
        
        raise FreeVarError(f'Variable : {self.val} not found in vars.')
    
    def simpl(self) -> Formula:
        return self
    
    def free_vars(self) -> set:
        return {self.val}
    
class Bool(Formula):
    def __init__(self, bool_type : bool) -> None:
        self.val = bool_type

    def __str__(self) -> str:
        return "⊤" if self.val else "⊥"

    def evaluate(self, _: dict[str, bool]) -> bool:
        return self.val
    
    def simpl(self) -> Formula:
        return self
    
    def free_vars(self) -> set:
        return set()
    
class Not(Formula):
    def __init__(self, f : Formula) -> None:
        self.formula = f
    
    def __str__(self) -> str:
        return f"¬({str(self.formula)})"

    def evaluate(self, vars: dict[str, bool]) -> bool:
        return not self.formula.evaluate(vars)
    
    def simpl(self) -> Formula:
        f_simpl = self.formula.simpl()
        if isinstance(f_simpl,Not):
            return f_simpl.formula
        elif isinstance(f_simpl,Bool):
            return Bool(not f_simpl.val)
        else:
            return Not(f_simpl)
        
    def free_vars(self) -> set:
        return self.formula.free_vars()
    
class Disj(Formula):
    def __init__(self, f1 : Formula, f2 : Formula) -> None:
        self.formula1 = f1
        self.formula2 = f2

    def __str__(self) -> str:
        return f"({str(self.formula1)}) ∨ ({self.formula2})"

    def evaluate(self, vars: dict[str, bool]) -> bool:
        return self.formula1.evaluate(vars) or self.formula2.evaluate(vars)
    
    def simpl(self) -> Formula:
        f1_simpl = self.formula1.simpl()
        f2_simpl = self.formula2.simpl()
        if isinstance(f1_simpl,Bool):
            if f1_simpl.val:
                return Bool(True)
            else:
                return f2_simpl
        else:
            if isinstance(f2_simpl,Bool):
                if f2_simpl.val:
                    return Bool(True)
                else:
                    return f1_simpl
            else:
                return Disj(f1_simpl, f2_simpl)
            
    def free_vars(self) -> set:
        v1 = self.formula1.free_vars()
        v2 = self.formula2.free_vars()
        return v1 | v2
    
class Conj(Formula):
    def __init__(self, f1 : Formula, f2 : Formula) -> None:
        self.formula1 = f1
        self.formula2 = f2

    def __str__(self) -> str:
        return f"({str(self.formula1)}) ∧ ({self.formula2})"
    
    def evaluate(self, vars: dict[str, bool]) -> bool:
        return self.formula1.evaluate(vars) and self.formula2.evaluate(vars)
    
    def simpl(self) -> Formula:
        f1_simpl = self.formula1.simpl()
        f2_simpl = self.formula2.simpl()
        if isinstance(f1_simpl,Bool):
            if f1_simpl.val:
                return f2_simpl
            else:
                return Bool(False)
        else:
            if isinstance(f2_simpl,Bool):
                if f2_simpl.val:
                    return f1_simpl
                else:
                    return Bool(False)
            else:
                return Conj(f1_simpl, f2_simpl)
            
    def free_vars(self) -> set:
        v1 = self.formula1.free_vars()
        v2 = self.formula2.free_vars()
        return v1 | v2
    
class Imp(Formula):
    def __init__(self, f1 : Formula, f2 : Formula) -> None:
        self.formula1 = f1
        self.formula2 = f2

    def __str__(self) -> str:
        return f"({str(self.formula1)}) → ({self.formula2})"
    
    def evaluate(self, vars: dict[str, bool]) -> bool:
        return not self.formula1.evaluate(vars) or self.formula2.evaluate(vars)
    
    def simpl(self) -> Formula:
        f1_simpl = self.formula1.simpl()
        f2_simpl = self.formula2.simpl()
        if isinstance(f1_simpl,Bool):
            if f1_simpl.val:
                return f2_simpl
            else:
                return Bool(True)
        else:
            if isinstance(f2_simpl,Bool):
                if f2_simpl.val:
                    return Bool(True)
                else:
                    return f1_simpl
            else:
                return Imp(f1_simpl, f2_simpl)
            
    def free_vars(self) -> set:
        v1 = self.formula1.free_vars()
        v2 = self.formula2.free_vars()
        return v1 | v2