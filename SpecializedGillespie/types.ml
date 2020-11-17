type expression = Val of string
                | Flt of float
                | Int of int
                | Mul of expression * expression
                | Div of expression * expression
			          | Add of expression * expression
                | Min of expression * expression

type initial = (string * int) * int

type item = string * int

type reaction = string * item list * item list * float

type observable = string 

type program = reaction list * initial list * float * int * observable list * float
