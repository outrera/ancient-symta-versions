First of all, make sure followin packages are present:
 cffi, babel, trivial-shell and trivial-garbage

Here is how I myself run Symta:
 - emacs
 - Alt-X slime
 - (require :symta)
 - ($eval "read|eval|say|loop")
 - To exit just Ctrl-C Ctrl-B

Quick Reference
------------------
  Note: functions don't change their arguments
  Case matters: variable names must start with uppercase letter
                function names must start with lowercase letter
------------------
"Hello, World" // string
say "Hello, World" // prints "Hello World" on screen
'say // unevaluated symbol
[1 2 3] // list of 1 2 3
[2+3 2-3 2*3 2/3 ~3] // usual sum, subtraction, product, quotient and negation
[A==B A<B A>B A<=B A>=B] // comparisons
[1 @[2 3] 4 @[5 6]] // list splicing
#(1 $@[] 4 $@[5 6]) // quasiquoted version
[A=123 B=456] // associative list of sorted pairs (associative map)
Map.A // returns value associated with key "A" (shorthand for Map."A")
Map.(A) // same, but A is variable.
L,N // element N-th element of list `L`. If `N` is negative, then from end
Condition |> then // if/then from
Condition |> Then :: Else // if/then/else form
[20++(rand 10)] // list of 20 times random values
A<B |> A :: B // returns smalles value of A and B
sort !List // sorts `List`
sort !List // sorts `List` and saves back value into it
[10%3 10%%3] // remainder and truncating devision
fold ?+?? [1..50] // sums numbers from 1 to 100
map ?^2 [1..50] // squares all numbers in list
fold {A B -> A+B} [1..50] // same as fold ?+?? [1..50], but with explicit body
{1->1; N->N*(r N-1)} // factorial: `r` is default name for current lambda
do A B C // evaluates A, B, and C, returns C
do `+` // treats operator `+` as normal symbol
`'` A+B // quoted expression
f A B -> A+B // definition of function `f`, that adds its arguments
f A B=3 -> A+B // same but `B` is keyword; invoked like "f 2 B=7" or "f 2"
V -> 123 // global variable definition
A:123 // variable binding
A:1 B:2 A+B // declares local variables, then adds them
[(Y:(X:2+3)+4)+5 X Y] // more advanced example of local variables
A=:123 // changes value of previously declared variable
#(&A &B &A &C) // with auto-gensyms
1 | `+` 2 | `*` 4 | say  // conveyor
[1..10] | ['start @? 'end] // another conveyor
[\a..\z] | {[_ @Middle _]->Middle} // pattern matching
all od? Xs; any odd? Xs // every, some from CL
X,f // shorthand for (f X)

/* Multi
   Line
   Comment */

// `f` calls `g` with its argument, binds value returned by
// `g` to `X` and returns `X`, if value returned by g is true
f X:!g -> X
// Example:
while stream,{x:!readLine->x,writeLine}

More Examples
------------------------
prime? N -> all N%?!=0 [2..N/2]
length [X@Xs] -> 1+Xs,length
flatten [@Xs] -> mapc flatten Xs;  X->[X]
foldr f [X@Xs] -> foldr f Xs | f X
sign neg?   ->   ~1
    ; 0     ->    0
    ;pos?   ->    1
    ; X     -> error "sign: parameter $X is unsupported"
[1..10],{[_ X @Xs]->X+Xs,r} // sum even numbers
ordered? [A B @Xs] -> A<=B && ordered [B@Xs]; [_]->y
_all P [N++P] ->
_any P [@_ P @_] ->
_keep P [@_ X:P @Xs] -> [X @(keep P Xs)]
_strip P [@S P @E]->[@S @(r P E)]; _ X->X
reverse [X@Xs] -> [@Xs,rev X]
subseq From Size Str -> drop From Str | take Size
subseq From Size [From++_ Xs:_:Size++_ @_] -> Xs
pal []; [_]; [X @Xs X]->Xs,pal // Palindrome
fib N -> fold {[a b] _->[b a+b]} [[0 1] 1..N] | ?,0 // fibonacci number
"/fs/home/user/names.txt"|flines|sort // sort lines in names.txt



