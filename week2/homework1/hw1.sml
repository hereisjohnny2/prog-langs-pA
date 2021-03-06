(* Homework 1 *)

(* Date => (year, month, day) *)
(* Day of the Year => 1 ~ 365 *)

(* Ex1 *)
fun is_older(date1: (int*int*int), date2: (int*int*int)) =
    if #1 date1 < #1 date2
    then true
    else if #1 date1 > #1 date2
    then false
    else if #2 date1 < #2 date2
    then true
    else if #2 date1 > #2 date2
    then false
    else #3 date1 < #3 date2

(* Ex2 *)
fun number_in_month(dates: (int*int*int) list, month: int) =
    if null dates
    then 0
    else if #2 (hd dates) = month
    then 1 + number_in_month((tl dates), month)
    else 0 + number_in_month((tl dates), month)

(* Ex3 *)
fun number_in_months(dates: (int*int*int) list, months: int list) = 
    if null months 
    then 0
    else number_in_month(dates, (hd months)) + number_in_months(dates, (tl months))

(* Ex4 *)
fun dates_in_month(dates: (int*int*int) list, month: int) = 
    if null dates
    then []
    else if #2 (hd dates) = month
    then  (hd dates)::dates_in_month((tl dates), month)
    else dates_in_month((tl dates), month)

(* Ex5 *)
fun dates_in_months(dates: (int*int*int) list, months: int list) = 
    if null months
    then []
    else dates_in_month(dates, (hd months))@dates_in_months(dates, (tl months))

(* Ex6 *)
fun get_nth(xs: string list, n: int) = 
    if n = 1
    then hd xs
    else get_nth((tl xs), n-1)

(* Ex7 *)
fun date_to_string(date: (int*int*int)) = 
    let
      val months = [ "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"] 
      val string_year = Int.toString(#1 date)
      val string_month = get_nth(months, #2 date)
      val string_day = Int.toString(#3 date)
    in
      string_month ^ " " ^ string_day ^ ", " ^ string_year
    end

(* Ex8 *)
fun number_before_reaching_sum(sum: int, xs: int list) =
    if hd xs >= sum 
    then 0
    else 1 + number_before_reaching_sum(sum - (hd xs), (tl xs))
        
(* Ex9 *)
fun what_month(day_of_year: int) =
    let 
        val months_days = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
        number_before_reaching_sum(day_of_year, months_days) + 1
    end

(* Ex10 *)
fun month_range(day1: int, day2: int) = 
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1+1, day2)

(* Ex11 *)
fun oldest(dates: (int*int*int) list) =
    if null dates
    then NONE
    else
        let val tl_ans = oldest(tl dates)
        in
          if isSome tl_ans andalso is_older(valOf tl_ans, hd dates)
          then tl_ans
          else SOME (hd dates)
        end

(* Ex12 - Challenge Problem *)

fun number_in_months_challenge(dates: (int*int*int) list, months: int list) = 
    let
        fun remove_duplicates(xs: int list) =
            if null (tl xs)
            then xs
            else 
                let 
                    val tl_ans = remove_duplicates(tl xs)
                in
                    if (hd xs) = hd tl_ans
                    then tl_ans
                    else (hd xs) :: tl_ans
                end

        fun number_in_months(dates: (int*int*int) list, months: int list) = 
            if null months 
            then 0
            else number_in_month(dates, (hd months)) + number_in_months(dates, (tl months))
        
        val non_repeated_months = remove_duplicates(months)
    in 
        number_in_months(dates, non_repeated_months)
    end

fun reasonable_date(date: (int*int*int)) = 
    if #1 date < 1
    then false
    else if #2 date < 1 orelse #2 date > 12
    then false 
    else if #3 date < 1
    then false
    else 
        let
            fun check_for_leap_year(year: int) = year mod 100 <> 0 andalso (year mod 400 = 0 orelse year mod 4 = 0)
            
            fun get_nth(xs: int list, n: int) = 
                if n = 1
                then hd xs
                else get_nth((tl xs), n-1)

            val months_days = [31,28,31,30,31,30,31,31,30,31,30,31]
            val leap_months_days = [31,29,31,30,31,30,31,31,30,31,30,31]
        in
            if check_for_leap_year(#3 date)
            then (#3 date) <= get_nth(months_days, #2 date)
            else (#3 date) <= get_nth(leap_months_days, #2 date)
        end        