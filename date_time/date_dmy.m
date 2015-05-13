/****M* date_dmy
 * NAME
 *   utils
 * PURPOSE
 *   Library of useful tools for manipulating dates
 * AUTHOR
 *   Bert Van Nuffelen <bvn@missioncriticalit.com>
 * BUGS
 * TODO
 *   
 **** */

:- module date_dmy.

:- interface.

:- import_module io.
:- import_module int.
:- import_module string.

:- type date_dmy --->
	date_dmy(
		 day   :: int,
		 month :: int,
		 year  :: int
		).

:- pred is_valid(date_dmy::in) is semidet.

/*****P* utils.date/todays_date
 * NAME
 *   todays_date
 * SYNOPSIS
 *   returns the currrent system date
 *   
 * SOURCE
 */
:- pred todays_date(date_dmy::out, io::di, io::uo) is det.
/*****/

/*****P* utils.date/date_format
 * NAME
 *   date_format
 * SYNOPSIS
 *   formats a time given in hh,mm,ss according to a mask
 *   the mask may contain 'hh', 'mm', 'ss', specifiers which are replaced by
 *   the corresponding values
 * SOURCE
 */
:- func date_format(string, date_dmy) = string.
/*****/

/*****P* utils.date/date_parse_mask
 * NAME
 *   date_parse_mask
 * SYNOPSIS
 *   Parse a time according to an input mask, e.g. "dd/mm/yyyy".
 * SOURCE
 */
:- pred date_parse_mask(string::in, string::in, date_dmy::out) is semidet.

:- func det_date_parse_mask(string, string) = date_dmy.
/*****/
/*****/

/*****P* utils.date/date_parse
 * NAME
 *   date_parse
 * SYNOPSIS
 *   parse a date. expects the input to be dd/mm/yyyy
 * SOURCE
 */
:- pred date_parse(string::in, date_dmy::out) is semidet.

:- func det_date_parse(string) = date_dmy.
/*****/


:- func days_in_month(int, int) = int.
:- pred is_leap_year(int::in) is semidet.

	% The logic to be applied is te following : adding 1 month to the
	% current end-date and substract one day, if the date exists : Ok, if
	% not sustract one more day untill the date is correct.
:- func add_one_month_minus_one_day(date_dmy) = date_dmy.

:- func compare_date(date_dmy, date_dmy) = comparison_result.

:- implementation.

:- import_module list.
:- import_module time.
:- import_module require.


%------------------------------------------------------------------------------%
is_valid(date_dmy(Day,Month,Year)) :-
        1 =< Month, Month =< 12,
        NumDays = days_in_month(Year,Month),
        1 =< Day, Day =< NumDays.

%------------------------------------------------------------------------------%

todays_date(Date, !IO) :-
	time.time(CurrentDate, !IO),
	TM = time.localtime(CurrentDate),
    Date = date_dmy(TM ^ tm_mday, TM ^ tm_mon + 1, TM ^ tm_year + 1900).

%------------------------------------------------------------------------------%


add_one_month_minus_one_day(date_dmy(D, M, Y)) = date_dmy(NewD, NewM, NewY) :-
	( D = 1 ->
		NewD = days_in_month(Y, M),
		NewM = M,
		NewY = Y
	;
		( M = 12 ->
			NewM = 1,
			NewY = Y + 1
		;
			NewM = M + 1,
			NewY = Y
		),
		DaysInMonth = days_in_month(NewY, NewM),
		( D > DaysInMonth ->
			NewD = DaysInMonth
		;
			NewD = D - 1
		)
	).
	

days_in_month(Y, M) =
	( ( M = 1 ; M = 3 ; M = 5 ; M = 7 ; M = 8 ; M = 10 ; M = 12 ) ->
		31
	; M = 2 ->
		( is_leap_year(Y) ->
			29
		;
			28
		)
	; ( M = 4 ; M = 6 ; M = 9 ; M = 11 ) ->
		30
	;
		func_error("days_in_month: " ++ string(M))
	).


is_leap_year(Y) :-
	Y mod 4 = 0,
	( Y mod 100 = 0 ->
		Y mod 400 \= 0
	;
		fail
	).


%------------------------------------------------------------------------------%

compare_date(date_dmy(DA, MA, YA), date_dmy(DB, MB, YB)) = Result :-
        compare(YearResult, YA, YB),
        ( YearResult = (=) ->
                compare(MonthResult, MA, MB),
                ( MonthResult = (=) ->
                        compare(Result, DA, DB)
                ;
                        Result = MonthResult
                )
        ;
                Result = YearResult
        ).

%------------------------------------------------------------------------------%
% Date Formatting
%------------------------------------------------------------------------------%

date_format(!.Str, date_dmy(D,M,Y)) = !:Str :-
	!:Str = string__replace_all(!.Str,   "DD", pad_left(from_int(D),'0',2)),
	!:Str = string__replace_all(!.Str,   "MM", pad_left(from_int(M),'0',2)),
	!:Str = string__replace_all(!.Str, "YYYY", pad_left(from_int(Y),'0',4)),
	!:Str = string__replace_all(!.Str,   "YY", pad_left(from_int(Y mod 100),'0',2)).

date_parse_mask(InputStr0, Mask, date_dmy(D,M,Y)) :-
	InputStr1 = replace_all(InputStr0, "/", ""),
	InputStr = replace_all(InputStr1, " ", ""),
	( (sub_string_search(Mask, "DD", IndexD),
	   Day = substring(InputStr, IndexD, 2),
	   \+ Day = "")   ->
	    to_int(Day, D)
	;
	    D = 0
	),
	( (sub_string_search(Mask, "MM", IndexM),
	   Month = substring(InputStr, IndexM, 2),
	   \+ Month = "") ->
	    to_int(Month, M)
	;
	    M = 0
	),
	( (sub_string_search(Mask, "YYYY", IndexYY),
	   Year = substring(InputStr, IndexYY, 4),
	   \+ Year = "") ->
	    to_int(Year, Y)
	
% Don't know how to handle two-digit years yet...?
%	; sub_string_search(Mask, "YY", IndexY) ->
%	    to_int(substring(InputStr, IndexY, 2), Y)
	;
	    Y = 0
	).

det_date_parse_mask(Str, Mask) = Date :-
    ( date_parse_mask(Str, Mask, Date0) ->
	Date = Date0
    ;
	error("date_dmy.det_date_parse_mask: error parsing date " ++ 
            Str ++ " with mask " ++ Mask)
    ).
	
date_parse(InputStr, date_dmy(D,M,Y)) :-
   string.replace_all(InputStr," ","",Replaced),
	List = to_char_list(Replaced),
	( List = [D1, D2, '/', M1, M2, '/', Y1, Y2, Y3, Y4] ->
	    to_int(from_char_list([D1,D2]), D),
	    to_int(from_char_list([M1,M2]), M),
	    to_int(from_char_list([Y1,Y2,Y3,Y4]), Y)

	; List = [D1, '/', M1, M2, '/', Y1, Y2, Y3, Y4] ->
	    to_int(from_char_list([D1]), D),
	    to_int(from_char_list([M1,M2]), M),
	    to_int(from_char_list([Y1,Y2,Y3,Y4]), Y)
	
	; List = [D1, D2, '/', M1, '/', Y1, Y2, Y3, Y4] ->
	    to_int(from_char_list([D1,D2]), D),
	    to_int(from_char_list([M1]), M),
	    to_int(from_char_list([Y1,Y2,Y3,Y4]), Y)
	
	; List = [D1, '/', M1, '/', Y1, Y2, Y3, Y4] ->
	    to_int(from_char_list([D1]), D),
	    to_int(from_char_list([M1]), M),
	    to_int(from_char_list([Y1,Y2,Y3,Y4]), Y)
   
   ; Replaced = ""
         -> D=0, M=0, Y=0
   ; 
	fail
	).

det_date_parse(InputStr) = Date :-
    ( date_parse(InputStr, Date0) ->
	Date = Date0
    ;
	error("date_dmy.det_date_parse: error parsing date " ++ InputStr)
    ).

:- end_module date_dmy.
%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et tw=0 wm=0

