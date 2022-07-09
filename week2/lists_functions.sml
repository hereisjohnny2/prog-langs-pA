(* helper funtion to operate lists *)

fun list_sum(xs: int list) = 
    if null xs
    then 0
    else hd(xs) + list_sum(tl(xs))

fun list_product(xs: int list) = 
    if null xs
    then 1
    else hd(xs) * list_product(tl(xs))

fun countdown(x: int) =
    if x = 0
    then []
    else  x::countdown(x-1)

fun append(xs: int list, ys: int list) = 
    if null xs
    then ys
    else hd(xs)::append(tl(xs), ys)

fun sum_pairs(pr1: (int*int), pr2: (int*int)) = 
    (#1 pr1 + #1 pr2, #2 pr1 + #2 pr2)

fun sum_pair_list(xs: (int*int) list) =
    if null xs
    then 0
    else #1 (hd(xs)) + #2 (hd(xs)) + sum_pair_list(tl(xs))

fun list_sum_pairs(xs: (int*int) list) = 
    if null xs
    then (0,0)
    else sum_pairs(hd(xs), list_sum_pairs(tl(xs)))

fun firsts(xs: (int*int) list) = 
    if null xs
    then []
    else (#1 (hd(xs))) :: firsts(tl(xs)) 

fun seconds(xs: (int*int) list) = 
    if null xs
    then []
    else (#2 (hd(xs))) :: seconds(tl(xs)) 

fun sum_pair_list2(xs: (int*int) list) = 
    (list_sum(firsts xs)) + (list_sum(seconds xs))

fun factorial(x: int) =
    if x = 0
    then 1
    else x * factorial(x - 1)

fun countup_from1(x: int) = 
    let
        fun count(from: int) = 
            if from = x
            then x::[]
            else from::count(from + 1)
    in
        count(1)
    end

fun list_max(xs: int list) = 
    if null xs
    then NONE
    else
        let val tl_ans = list_max(tl xs)
        in
          if isSome tl_ans andalso valOf tl_ans > hd xs
          then tl_ans
          else SOME (hd xs)
        end

fun range(from: int, to: int) = 
    let
        fun countup(from: int, to: int) = 
            if from = to
            then to::[]
            else from::countup(from + 1,to)

        fun countdown(from: int, to: int) = 
            if from = to
            then to::[]
            else from::countdown(from - 1,to) 
    in
        if from < to then countup(from, to) else countdown(from, to)
    end