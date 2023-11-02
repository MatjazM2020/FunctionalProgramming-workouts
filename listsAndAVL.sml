datatype natural = Succ of natural | One;
exception NotNaturalNumber;


(*zip returns a list where on the i-th position, it has a pair (x_i,y_i) where x_i is the ith element of the 
list x_i, and y_i is the ith element of the list y, in the case when the lists are diff. lengths, the smaller length 
is considered*)
fun zip(x: 'a list,y: 'b list) = 
    case (x,y) of 
        ([], y) => []  
        | (x,[]) => []
        | (h1::t1, h2::t2) => (h1,h2)::zip(t1,t2); 

(*Pseudoinverse of the zip function*)
fun unzip(x: ('a * 'b) list) = 
    case x of 
    [] => ([],[])
    | (h1,h2)::t => (h1::(#1(unzip(t))), h2::(#2(unzip(t))));
        
(*Returns a natural number that corresponds to (a-b), if it is not a natural number, exception is triggered.*)
fun subtract (a: natural, b: natural) : natural= 
    case (a,b) of 
    (One, _ ) => raise NotNaturalNumber
    | (Succ(x),One) => x 
    | (Succ(x),Succ(y)) => subtract(x,y);

(*Returns true, if true is returned from f for any of the values from the list s*)
fun any(f: 'a -> bool, s: 'a list) = 
    case s of 
    [] => false
    | h::t => f(h);

(*Returns a list of mapped elements with the function f*)
fun map(f: 'a -> 'b, s:  'a list) = 
    case s of 
    [] => []
    | h::t => f(h)::map(f,t); 

(*Returns the elements from the list for which the function f returns true*)
fun filter(f: 'a -> bool, s: 'a list) = 
    case s of 
    [] => []
    | h::t => if f(h) then h::filter(f,t) else filter(f,t); 

(*Left fold function (evaluates f(...f(f(z,s_1),s_2),...s_n)*)
fun fold(f: 'a * 'b -> 'a, z: 'a, s: 'b list) = 
    case s of 
        [] => z 
        | h::t => fold(f,f(z,h),t); 


datatype 'a bstree = br of 'a bstree * 'a * 'a bstree | lf;
datatype direction = L | R;


(*Returns a rotated tree in the left or in the right, if that is possible*)
fun rotate(drevo: 'a bstree, smer: direction) = 
    case (drevo,smer) of 
    (br(l,v,br(a,b,c)),L) => br(br(l,v,a),b,c)
    | (br(l,v,lf),L) => drevo 
    |(br(br(a,b,c),v,r),R) => br(a,b,br(c,v,r))
    | (br(lf,v,r),R) => drevo 
    | (lf,_) => drevo

(*Rebalance the tree with at most 2 rotatios, the the subtrees are balanced, only in the root there might be imbalance*)
fun rebalance(drevo: 'a bstree) = 
    let 
        fun height(drevo: 'a bstree) = 
            case drevo of 
            lf => 0
            | br(l,v,r) => 1 + Int.max(height(l),height(r))
    in 
        case drevo of 
        lf => drevo 
        | br(l,v,r) => 
            if (height(l)-height(r)) = ~2 then 
                    case drevo of 
                    br(l,v,br(a,b,c)) => 
                        if (height(a)-height(c)) <= 0 then 
                            rotate(drevo,L)
                        else 
                            rotate(br(l,v,rotate(r,R)), L)
                    | _ => drevo
            else if (height(l)-height(r)) = 2 then
                    case drevo of 
                    br(br(a,b,c),v,r) => 
                        if (height(c)-height(a)) <= 0 then 
                            rotate(drevo,R)
                        else 
                            rotate(br(rotate(l,L),v,r),R)
                    | _ => drevo 
            else drevo 
    end 

(*Adds the element e in the tree, if it is not in the tree yet, for comparison, the function c is used*)
fun avl(c: ('a * 'a) -> order,drevo: 'a bstree,e: 'a) = 
    case drevo of 
    lf => br(lf,e,lf) 
    | br(l,v,r) => 
        if c(e,v) = GREATER then 
        rebalance(br(l,v,avl(c,r,e)))
        else if c(e,v) = LESS then 
        rebalance(br(avl(c,l,e),v,r))
        else 
        br(l,v,r) 
    

