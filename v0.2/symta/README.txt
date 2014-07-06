Quick Reference
------------------
  Note: functions don't change their arguments
  Case matters: variable names must start with an uppercase letter
                function names must start with a non-uppercase letter
------------------
Hello // symbol reference
\Hello // quoted symbol
“Hello” // same as above (note these are unicode left and right double quotes)
say \Hello // prints Hello on screen
√ // true (T from CL)
ø // false (NIL from CL)
[1 2 3] // list of 1 2 3
[2+3 2-3 2*3 2/3 ~3] // usual sum, subtraction, product, quotient and negation
[(mod 10 3) 10%3] // remainder and truncating division
[A≥≤B A≤≥B A≤B A≥B A≤≤B A≥≥B] // comparisons: eq, ne, lt, gt, lte, gte
[1 @[2 3] 4 @[5 6]] // list splicing
\(1 $@[2 3] 4 $@[5 6]) // quote-splicing
rng 1 10 // list of sequential numbers
rng 1 3 10 // same, but with step 3
map ?^2 10,rng // squares all numbers in list
fold ?+?? 10,rng // sums numbers from 1 to 10
fold <A B = A+B> 10,rng // same as fold ?+?? 10,rng, but with explicit body
fold `+` 10,rng // same as above, but passing `+` directly
(= say \Hello = say \World) // execute statements sequentially
(= A:2 = A+1) // declares local variable, then adds with it
(= A,i:2 = A+1) // same, but variable has type `i` (fixnum integer)
(= Squares: m ?^2 (rng 1 7) = workOn Squares)
V = 123 // global variable definition
(A=123) // changes value of previously declared local/global variable
u A:123 B:456 // associative list of sorted pairs (associative map)
u P X:123 Y:456 // same as above, but merge (update) resulting list into `P`
o X:123 Y:456 // same but, result is a hashtable
(= X:1 = Y:2 = o add:<Z = X+Y+Z>).add 4 // creating and using simple OOP like structure
Map.A // returns value associated with key “A” (shorthand for Map.“A”)
Map.(A) // same, but A is variable.
L,N // N-th element of a list `L`. If `N` is negative, then counts from end
do (say 123) (says 456)
q 1 2 3 |do !A !B //multi assignment
do !A !B :: q 1 2 3 //same, but head first
!!X+3 //increment X by 3 and return previous value
{Condintion = Then; Else} // if/then/else form
dup 20 rand 10 // list of 20 random values between 0 and 10
sort List // sorts `List`
sort !List // sorts `List` and saves value back into it
<1=1;N=N*(r N-1)> // factorial: `r` is default name for current lambda
{{A≤B; C≥B} = say “A<B or C>B”} // disjunction
{A≤B |v C≥B = say “A<B or C>B”} // sane as above, but using pipe+macro
{A≤B |a C≥B = say “A<B and C>B”} // conjunction
do A B C // evaluates A, B, and C, returns C
do `+` // returns function binded to symbol `+`
`\\` A+B // quoted expression
f A B = A+B // definition of function `f`, which adds its arguments
f A B:{3} = A+B // same but `B` is keyword; invoked like “f 1 B:2” or “f 1”
f X Y \= $X $Y // function with quoted body (useful for macros)
(A.Abc.Def = 123) // changes path in associative list
\(&A &B &A &C) // list with auto-gensyms
1 | `+` 2 | `*` 4 | say  // conveyor
rng 10 | \(start $@? end) // another conveyor
<[_ @Middle _ _]=Middle> \(A B C D E) // pattern matching
<[Xs:_:@[odd? odd?]]=Xs> Xs // matches a list of two odd numbers
all odd? Xs; any odd? Xs // like `every` and `some` from CL
X,f // shorthand for (f X)
readChunks [T:@4,utf8 L:@4,ul D:@L,y @Xs] = [[T D] @Xs,r] // binary parsing example
= N:10 = label \L = {N≥0 = !N-1 = say N = goto \L} // gotoes
f !?.??+1 [ø@X] //count number of occurrences
w N V // warp: return from function named N, value V
w V // warp: return V from `r`
transpose X = m l @X

/* Multi
   Line
   Comment */


Parsing lambdas have <Input:Output:parser = ...> format

// `f` calls `g` with its argument, binds value returned by
// `g` to `X` and returns `X`, if value returned by `g` isn't `n`
f X:!g = X
// Example:
while stream,<X:!readLine=X,writeLine>



for (I,i:0; I≤10; !I+1) say I
// like traditional `for (int I = 0; I<10; I++) printf("%d\n", I);`


i  - fixnum
f4 - single-float
f8 - double-float

is - array of fixnum
f4s - array of single-float
f8s - array of double-float


More Examples
------------------------
map f [X@Xs] = [X,f @(r f Xs)]
_sum [X@Xs] = X+Xs,r
qsort [H@T] =f:? ?≤H T|r =l @k,f H @s,f
qsort [H@T] =l @(k ?≤H T|r) H @(s ?≤H T|r)
ø | (set ?.A 123) | (set ?.B 456) // [A=123 B=456]
periodAsRatio P = P/(10^P,digits,len-1)
pi R = points [0 0 R R] | cnt:[X Y] X^2+Y^2≤≤R^2 | ?*4/R^2
pi R = rng 1 R+1 | m:X sqrt R^2-X^2 | sum | ?*4/R^2
m ~@?.B [[A=1; B=2; C=3] [A=4; B=5; B=6]] // strip pairs keyed `b`
= Count:ø = map !Count.?+1 \(c a b a c a) = Count // count symbols
swap [[A B]@Xs] = [[B A]@Xs,swap]
average X = X,sum/X,len
average [L:@len]:[S:@sum] = S%L
prime? N = rng 2 N/2 | all N%?≤≥0
length [X@Xs] = 1+Xs,length
foldr f [@Xs X] = foldr f Xs | f X
sign neg?   =   ~1
    ; 0     =    0
    ;pos?   =    1
    ; X     = error “sign: parameter $X is unsupported”
rng 1 20 |c:[_ X @Xs] X+Xs,r // sum even numbers
ordered? [A B @Xs] = A≤≤B |a r [B@Xs]; [_]=√
_all P [N++P] =
_any P [@_ P @_] =
our_keep P [@_ X:P @Xs] = [X @(keep P Xs)]
our_skip P [@S P @E]=[@S @(r P E)]; _ X=X
reverse [X@Xs] = [@Xs,rev X]
subseq From Size Str = drop From Str | take Size
subseq From Size [From++_ Xs:_:Size++_ @_] = Xs
pal []=√; [_]=√; [X @Xs X]=Xs,pal // Palindrome
fib N = fold <[a b] _=[b a+b]> [[0 1] @(rng 1 N+1)] | lhd // fibonacci number
“/fs/home/user/names.txt”|flines|sort // sort lines in names.txt

<L = \($L \($$L))> \(L = \($L \($$L))) // quine
eval (Yoba=\(\(eval (Yoba=\($$Yoba))))) // another quine

expand \(($A+$B)*$C) = \($A*$C+$B*$C)
      ;\($A*($B+$C)) = \($A*$B+$A*$C)
      ;\(($A+$B)^2)  = \(($A+$B)*($A+$B))
      ;\($A*$A)      = \($A^2)
      ;\($A*$(B:?<A)) = \($B*$A)
factor \($A*$C+$B*$C) = \(($A+$B)*$C)
      ;\($A*$B+$A*$C) = \($A*($B+$C))
expandAll E = rmap <X = {expand X |> rmap r It; X}> E
expandAll \((A+B)^2)

