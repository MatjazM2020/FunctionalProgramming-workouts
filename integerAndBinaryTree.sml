datatype number = Zero | Succ of number | Pred of number;

(*Negates a number*)
fun neg (a : number) =
    case a of 
     Zero => Zero
     | Succ x => Pred( neg(x))
     | Pred x => Succ( neg(x));

(*Adds up two numbers*)
fun add (a : number, b : number) =
    case (a,b) of
    (Zero, b) => b
    | (a, Zero) => a
    | (Succ x, Succ y) => Succ(Succ(add(x,y)))
    | (Pred x, Succ y) => add(x,y)
    | (Succ x, Pred y) => add(x,y)
    | (Pred x, Pred y) => Pred(Pred(add(x,y)));

(*Simplifies the expression as much as possible (reduces the number of constructors if possible)*)
fun simp (a: number) = 
    case a of 
    Zero => Zero
    | Succ(Pred x) => simp(x)
    | Pred(Succ x) => simp(x)
    | Succ(Zero) => Succ(Zero)
    | Pred(Zero) => Pred(Zero)
    | Succ(Succ(x)) => (case simp(x) of 
         Zero => Succ(Succ(simp(x)))
        | Pred xx => simp(Succ(simp(x)))
        | Succ xx => Succ(Succ(simp(x))))
    | Pred(Pred(x))=> (case simp(x) of 
         Zero => Pred(Pred(simp(x)))
        | Succ xx => simp(Pred(simp(x)))
        | Pred xx => Pred(Pred(simp(x))));

(*Compare values a and b*)
fun comp (a : number, b : number) = 
        let fun tia(x: number,y: number) = 
            case (x,y) of 
                (Zero,Zero) => EQUAL
                | (Succ q, Zero) => GREATER                    
                | (Zero, Succ q) => LESS
                | (Pred q, Zero) => LESS
                | (Zero, Pred q) => GREATER
                | (Succ q, Pred r) => GREATER
                | (Pred q, Succ r) => LESS
                | (Pred q, Pred r) => tia(q,r)
                | (Succ q, Succ r) => tia(q,r)
        in tia(simp(a),simp(b))
    end;




datatype tree = Node of int * tree * tree | Leaf of int;

(*Checks if the tree contains a node with head x*)
fun contains (tree : tree, x : int) = 
    case tree of
        Leaf(a) => a = x 
        | Node(a,b,c) => a = x orelse contains(b,x) orelse contains(c,x); 

(*counts the number of leaves*)
fun countLeaves (tree : tree) = 
    case tree of 
        Leaf(a) => 1
        | Node(a,b,c) => countLeaves(b)+countLeaves(c);

(*counts the number of branches*)
fun countBranches (tree : tree) = 
    case tree of
    Leaf(a) => 0
    | Node(a,b,c) => 2 + countBranches(b) + countBranches(c);

(*Calculates the height of the tree*)
fun height (tree : tree) = 
    case tree of 
    Leaf(a) => 1
    | Node(a,b,c) => 1+ Int.max(height(b),height(c));

(*Converts the binary tree into an integer list with inorder traversal*)
fun toList (tree : tree) = 
    case tree of 
    Leaf(a) => [a]
    | Node(a,b,c) => toList(b) @ [a] @ toList(c);

(*Checks if the binary tree is balanced (i.e. if the absolute difference between the heights of left and right subtree 
is <= 1 for every node *)
fun isBalanced (tree : tree) = 
    case tree of 
        Leaf(a) => true
        | Node(a,b,c) => ((Int.abs(height(b)-height(c))) <= 1) andalso isBalanced(b) andalso isBalanced(c);

(*Checks if the binary tree is binary search tree (i.e. all values from left subtree are strictly smaller 
than the node and all the values from right subtree are strictly greater than the node, for all nodes*)
fun isBST (tree : tree) = 
    case tree of 
        Leaf(a) => true
        | Node(a,b,c) => (
            case (b,c) of 
            (Leaf(x),Leaf(y)) => x < a andalso y > a
            |(Leaf(x),Node(q,w,e)) => x < a andalso q > a andalso isBST(w) andalso isBST(e)
            |(Node(q,w,e),Leaf(x)) => q < a andalso x > a andalso isBST(w) andalso isBST(e)
            |(Node(q,w,e),Node(x,y,z)) => q < a andalso x > a andalso isBST(w) andalso isBST(e) andalso isBST(y) andalso isBST(z)
        );

