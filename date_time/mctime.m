%-----------------------------------------------------------------------
% Author : P.A. Massey (MC, 1999) 
% Date   : From PFI project
% Purpose: Handling of dates, etc.
%
:- module mctime.

:- interface.

:- import_module io.
:- import_module string.
:- import_module integer.

        %--------------------------------------------------------------
  
:- type date.

:- type dateoffset --->
             gmt
           ; local.


        %--------------------------------------------------------------
  
:- pred mctime__epoch(date::out) is det.

:- pred mctime__get_current(date::out,io::di,io::uo) is det.
%:- func mctime__get_current = (date::out) is det.

        %--------------------------------------------------------------
        % Force the date to be a specific time.

/*
:- pred mctime__set_datetime(date::in,date::out) is det.

:- pred mctime__reset_datetime(date::out) is det.
*/

        %--------------------------------------------------------------

:- pred mctime__date_validate(date::in) is semidet.

        %--------------------------------------------------------------
  
:- pred mctime__date_to_string(date::in,string::out) is det.

:- func mctime__date_to_string(date::in) = (string::out) is det.

:- pred mctime__date_to_day_string(date::in,string::out) is det.
	
:- pred mctime__date_to_smalldatetime_string(date::in,string::out) is det.

:- pred mctime__string_to_date(string::in,date::out) is semidet.

:- pred mctime__get_current_string(string::out,io::di,io::uo) is det.
%:- func mctime__get_current_string = (string::out) is det.

:- pred mctime__get_current_string_eng(string::out,io::di,io::uo) is det.

:- pred mctime__get_year(int::out,io::di,io::uo) is det.

:- type dayofweek --->
                mon
              ; tue
              ; wed
              ; thu
              ; fri
              ; sat
              ; sun .

:- pred mctime__get_day_of_week(date::in,dayofweek::out) is det.

:- func mctime__days_since_sunday(dayofweek) = int.

:- pred mctime__between(date::in,int::in,date::in) is semidet.

:- pred mctime__reduced(date::in, string::out, io::di, io::uo) is det.

:- pred date_reduce(string::in, string::out, io::di, io::uo) is det.

:- pred mctime__date_compare(date::in,date::in,comparison_result::out) is det.

:- pred mctime__date_to_smalldatetime(date::in,date::out) is det.

        %--------------------------------------------------------------
        % compute Julian day number (allows for computation of distances etc)
:- func julian_day(date::in)= (int::out) is det.

        %--------------------------------------------------------------
        % converts a Julian day to a date.
:- pred julian_to_date(int::in,date::out) is det.
:- pred julian_minutes_to_date(integer::in,date::out) is det.

        %--------------------------------------------------------------
        %returns day of week of any date (0 is Sunday)
:- pred day_of_week(date::in,int::out) is det.

        %--------------------------------------------------------------
        % number of days between two dates. First result is 
        % signed, second is the absolute value.
:- pred days_between(date::in,date::in,int::out,int::out) is det.

        %--------------------------------------------------------------
:- pred mctime__julian_minutes(date::in,integer::out) is det.

        %--------------------------------------------------------------
:- pred mctime__minutes_between(date::in,date::in,int::out) is det.

:- pred mctime__seconds_between(date::in,date::in,int::out) is det.

        %--------------------------------------------------------------
:- pred mctime__minutes_since(date::in,int::out,io::di,io::uo) is det.

        %--------------------------------------------------------------
        % Given a date, determine the date associated with the next
        % day of the week (ie. next tuesday or next friday). If the
        % current day is tuesday then next tuesday is 7 days away.
:- pred mctime__next_day_of_week(date::in,dayofweek::in,date::out) is det.

:- pred mctime__next_day_of_week(dayofweek::in,date::out,io::di,io::uo) is det.

        %--------------------------------------------------------------
:- pred mctime__next_month(date::in,int::in,date::out) is det.

:- pred mctime__prev_month(date::in,int::in,date::out) is det.

:- pred mctime__next_month(int::in,date::out,io::di,io::uo) is det.

        %--------------------------------------------------------------

:- pred mctime__add_days(date::in,int::in,date::out) is det.

:- pred mctime__add_months(date::in,int::in,date::out) is det.

:- pred mctime__add_years(date::in,int::in,date::out) is det.

:- pred mctime__add_minutes(date::in,int::in,date::out) is det.

        %--------------------------------------------------------------
        % Sometimes an explicite distinction is needed between a valid
        % Date as provided and a unknown NULL date.
    
:- type edate.

:- pred mctime__get_ecurrent(edate::out,io::di,io::uo) is det.
%:- func mctime__get_ecurrent = (edate::out) is det.

:- pred mctime__edate_init(edate::out) is det.
:- func mctime__edate_init = (edate::out) is det.

:- pred mctime__edate_to_string(edate::in,string::out) is det.

:- pred mctime__edate_to_smalldatetime_string(edate::in,string::out) is det.

:- pred mctime__string_to_edate(string::in,edate::out) is semidet.

:- pred mctime__date_to_edate(date::in,edate::out) is det.

:- pred mctime__edate_to_date(edate::in,date::out) is semidet.

:- func mctime__date(edate::in) = (date::out) is semidet.

:- pred mctime__det_edate_to_date(edate::in,date::out) is det.

:- pred mctime__edate_add_days(edate::in,int::in,edate::out) is det.

:- pred mctime__edate_add_months(edate::in,int::in,edate::out) is det.

:- pred mctime__edate_add_years(edate::in,int::in,edate::out) is det.

:- pred mctime__null_edate(edate::in) is semidet.

        %--------------------------------------------------------------

%:- pred mctime__is_today(date::in) is semidet.

%:- pred mctime__is_passed(date::in) is semidet.

        %--------------------------------------------------------------
:- pred day_table(string,dayofweek,int).
:- mode day_table(out,in,out) is det.
:- mode day_table(in,out,out) is semidet.
:- mode day_table(out,out,in) is semidet.

        %--------------------------------------------------------------
:- pred month_table(int,string,int).
:- mode month_table(in, out, out) is semidet.
:- mode month_table(out, in, out) is semidet.

        %--------------------------------------------------------------
:- pred get_date_date(int::out,int::out,int::out,date::in) is det.
:- pred get_date_time(int::out,int::out,int::out,date::in) is det.
:- pred get_date_offset(dateoffset::out,date::in) is det.

:- pred set_date_date(int::in,int::in,int::in,date::in,date::out) is det.
:- pred set_date_time(int::in,int::in,int::in,date::in,date::out) is det.
:- pred set_date_offset(dateoffset::in,date::in,date::out) is det.

	% init_date(Year, Month, Day, Hours, Minutes, Seconds, Offset).
	%
:- func init_date(int, int, int, int, int, int, dateoffset) = date.

%-----------------------------------------------------------------------

:- implementation.

:- import_module int.
:- import_module list.
:- import_module require.
:- import_module time.

%-----------------------------------------------------------------------

:- type date ---> 
           date(int,int,int,int,int,int,dateoffset).

:- type edate ---> 
             enull 
           ; edate(date).

init_date(Year, Month, Day, Hours, Minutes, Seconds, Offset) =
	date(Year, Month, Day, Hours, Minutes, Seconds, Offset).

get_date_date(Y,M,D,date(Y,M,D,_,_,_,_)).
get_date_time(H,M,S,date(_,_,_,H,M,S,_)).
get_date_offset(Z,  date(_,_,_,_,_,_,Z)).

set_date_date(Y,M,D,date(_,_,_,H,Min,S,Z),date(Y,M,D,H,Min,S,Z)):-
	( Y >= 0, M >= 0, M =< 12, D >= 0 ->
	     true
	;    error("set_date_date: negative values not allowed") ).
set_date_time(H,Min,S,date(Y,M,D,_,_,_,Z),date(Y,M,D,H,Min,S,Z)):-
	( H >= 0, H =< 24, Min >= 0, Min =< 60, S >= 0, S =< 60 ->
	     true
	;    error("set_date_time: negative values not allowed") ).
set_date_offset(Z,date(Y,M,D,H,Min,S,_),date(Y,M,D,H,Min,S,Z)).

mctime__julian_minutes(Date,Minutes):-
	Days = mctime__julian_day(Date),
	get_date_time(H,M,_,Date),
	Minutes = (integer(Days) * integer(24) * integer(60)) + (integer(H) * integer(60)) + integer(M).

mctime__minutes_between(Date1,Date2,Minutes) :-
	mctime__julian_minutes(Date1,M1),
	mctime__julian_minutes(Date2,M2),
	Minutes = integer__int(M1 - M2).

mctime__seconds_between(Date1,Date2,Seconds) :-
	mctime__julian_minutes(Date1,M1),
	get_date_time(_,_,Sc1,Date1),
	S1 = integer(Sc1) + (M1 * integer(60)),
	mctime__julian_minutes(Date2,M2),
	get_date_time(_,_,Sc2,Date2),
	S2 = integer(Sc2) + (M2 * integer(60)),
	Seconds = integer__int(S1 - S2).

mctime__minutes_since(Date,Minutes,!IO) :-
	mctime__get_current(Now,!IO),
	mctime__minutes_between(Now,Date,Minutes).

%%
%% Formulas from the calendar FAQ.
%%
mctime__julian_day(date(Year,Month,Day,_,_,_,_)) = JD:-
	A=(14-Month)//12,
	Y=Year+4800-A,
	M=Month+12*A -3,
	JD = Day + (153*M +2)//5 +Y*365 +Y//4-Y//100+Y//400-32045 .

:- pred mctime__julian_day_to_minutes(int::in,integer::out) is det.

mctime__julian_day_to_minutes(Days,Mins):-
	Mins = integer(Days) * integer(24*60).

%%%
%%% Julian days start at noon
%%%

mctime__julian_to_date(JD,Date):-
	Hours = 12,
	Mins = 0,
	A=JD+32045,
	B=(4*(A+36524))//146097-1,
	C= A-(B*146097)//4,
	D=(4*(C+365))//1461 - 1,
	E=C-(1461*D)//4,
	M=(5*(E-1)+2)//153,
	Day=E-(153*M+2)//5,
	Month=M+3-12*(M//10),
	Year=B*100+D-4800+M//10,
	Date = date(Year,Month,Day,Hours,Mins,0,local),
	( mctime__date_validate(Date) ->
	     true
	;    error("mctime__julian_to_date: calculation error") ).

mctime__julian_minutes_to_date(JMins,Date):-
	JD = JMins//integer(24*60),
	JR = JMins - (JD*integer(24*60)),
	JH = JR//integer(60),
	JM = JR - (JH*integer(60)),
	Hours = int(JH),
	Mins = int(JM),
	A=JD+integer(32045),
	B=(integer(4)*(A+integer(36524)))//integer(146097)-integer(1),
	C= A-(B*integer(146097))//integer(4),
	D=(integer(4)*(C+integer(365)))//integer(1461) - integer(1),
	E=C-(integer(1461)*D)//integer(4),
	M=(integer(5)*(E-integer(1))+integer(2))//integer(153),
	Day=int(E-(integer(153)*M+integer(2))//integer(5)),
	Month=int(M+integer(3)-integer(12)*(M//integer(10))),
	Year=int(B*integer(100)+D-integer(4800)+M//integer(10)),
	Date = date(Year,Month,Day,Hours,Mins,0,local),
	( mctime__date_validate(Date) ->
	     true
	;    error("mctime__julian_to_date: calculation error") ).

mctime__day_of_week(date(Year,Month,Day,_,_,_,_), DOW):-
	A=(14-Month)//12,
	Y=Year-A,
	M=Month+12*A-2,
	DOW=(Day+Y+Y//4-Y//100+Y//400+(31*M)//12) mod 7 .

mctime__epoch(Date):-
	date_init(1980,1,1,0,0,0,local,Date).   % Is a tuesday.

mctime__get_current(Date,!IO):-
	date_get(1,Year,Month,Day,Hour,Minute,Sec,!IO),
	date_init(Year,Month,Day,Hour,Minute,Sec,local,Date).

%mctime__get_current = Date :-
%        mctime__get_current(Date).

mctime__date_to_day_string(date(Year,Month,Day,_Hour,_Min,_Sec,_),DateStr):-
	string__format("%4d-%02d-%02d",[i(Year),i(Month),i(Day)],DateStr).
	
mctime__date_to_string(date(Year,Month,Day,Hour,Min,Sec,_),DateStr):-
	string__format("%4d-%02d-%02d %02d:%02d:%02d",
		       [i(Year),i(Month),i(Day),i(Hour),i(Min),i(Sec)],
		       DateStr).

mctime__date_to_string(Date) = String :-
	mctime__date_to_string(Date,String).

mctime__date_to_smalldatetime_string(date(Year,Month,Day,Hour,Min,_Sec,_),
				   DateStr):-
	string__format("%4d-%02d-%02d %d:%d",
		       [i(Year),i(Month),i(Day),i(Hour),i(Min)],DateStr).

mctime__string_to_date(DateStr0,Date):-
	DateStr = string.lstrip(string.rstrip(DateStr0)),
	( [DateS,TimeS] = string.words(DateStr),
	  [SYear,SMonth,SDay] = string.words_separator(unify('-'), DateS),
	  string__to_int(SYear,Year),
	  string__to_int(SMonth,Month),
	  string__to_int(SDay,Day),
	  List = string.words_separator(unify(':'), TimeS),
	  ( List = [SHour,SMin,SSec] ->
	       string__to_int(SHour,Hour),
	       string__to_int(SMin,Minute),
	       string__to_int(SSec,Sec)
	  ; List = [SHour,SMin] ->
	       string__to_int(SHour,Hour),
	       string__to_int(SMin,Minute),
	       Sec = 0
	  ;    Hour = 0,
	       Minute = 0,
	       Sec = 0 ) ->
	  date_init(Year,Month,Day,Hour,Minute,Sec,local,Date)
	; DateStr = "2000" ->
		date_init(2000,1,1,0,0,0,local,Date)
	; DateStr = "1999" ->
		date_init(1999,1,1,0,0,0,local,Date)
	; DateStr = "0" ->
		date_init(1997,3,1,0,0,0,local,Date)
	; string__length(DateStr,8) ->
	        string__append(DateStr," 00:00:00",NDateStr),
	        mctime__string_to_date(NDateStr,Date)
	; string__length(DateStr,9) ->
	        string__append(DateStr," 00:00:00",NDateStr),
	        mctime__string_to_date(NDateStr,Date)
	; string__length(DateStr,10) ->
	        string__append(DateStr," 00:00:00",NDateStr),
	        mctime__string_to_date(NDateStr,Date)
	; string__to_int(DateStr,YearOnly) ->
		date_init(YearOnly,1,1,1,1,1,local,Date)
	; [A, _] = string.words_separator(unify('.'), DateStr) ->
	        mctime__string_to_date(A,Date)
	; fail),
	mctime__date_validate(Date).

mctime__date_validate(date(Year,Month,Day,Hrs,Min,Secs,_)):-
	Year >= 0,
	Month >= 0, Month =< 12,
	Day >= 0, 
	Hrs >= 0, Hrs =< 24,
	Min >= 0, Min =< 60,
	Secs >= 0, Secs =< 60.

:- pred mctime__edate_valivate(edate::in) is semidet.

mctime__edate_valivate(enull).
mctime__edate_valivate(edate(Date)):-
	mctime__date_validate(Date).

mctime__get_current_string(DateStr,!IO) :-
	mctime__get_current(Date,!IO),
	mctime__date_to_string(Date,DateStr).
%mctime__get_current_string = DateStr :-
%        mctime__get_current_string(DateStr).

mctime__get_current_string_eng(DateStr,!IO) :-
        mctime__get_current(date(Year,Month,Day,Hour,Min,Sec,_),!IO),
        ( month_table(Month,MonthStr,_) ->
	  string__format("%4d %s %02d %02d:%02d:%02d",
			 [i(Year),s(MonthStr),i(Day),i(Hour),i(Min),i(Sec)],
			 DateStr)
	; error("month_table/3 called failed!!") ).
	    

        
mctime__get_year(Year,!IO) :-
	mctime__get_current(date(Year,_,_,_,_,_,_),!IO).

mctime__days_between(Date1,Date2,Days,AbsDays):-
	Days=julian_day(Date2)-julian_day(Date1),
	abs(Days,AbsDays).

mctime__between(Date1,Days,Date2):-
	days_between(Date1,Date2,_,Between),
	Between > Days.

/*
mctime__is_today(Date) :-
	mctime__get_current(Current,!IO),
	get_date_date(Year,Month,Day,Date),
	get_date_date(Year,Month,Day,Current).

mctime__is_passed(Date) :-
	mctime__get_current(Current),
	mctime__days_between(Current,Date,Days,_AbsDays),
	Days < 0 .

*/

:- pred date_init(int,int,int,int,int,int,dateoffset,date).
:- mode date_init(in,in,in,in,in,in,in,out) is det.

date_init(Year,Month,Day,Hour,Min,Sec,Zone,
	  date(Year,Month,Day,Hour,Min,Sec,Zone)).

:- pragma foreign_decl("C", "
#include <stdio.h>
#include <time.h>
").

:- pragma foreign_decl("C", local, "
static double mctime_offset = 0;
").

/*
:- pred mctime__set_datetime(date::in,date::out) is det.

:- pragma promise_pure(mctime__set_datetime/2).
:- pragma promise_pure(mctime__reset_datetime/1).

mctime__set_datetime(date(Year,Month,Day,Hour,Min,Sec,_Zone),CurrentDate,!IO):-
	mctime__get_current(CurrentDate,!IO),
	impure datetime_setC(Year,Month,Day,Hour,Min,Sec).

:- impure pred datetime_setC(int,int,int,int,int,int).
:- mode datetime_setC(in,in,in,in,in,in) is det.

   % Note: we simply calculate the difference between
   % the current date and the requested date and then add
   % the diff to the current date when requesting the date. 
   % Problem is portability of this solution ?
  
:- pragma c_code(datetime_setC(Year::in,Month::in,Day::in,Hour::in,
			  Minute::in,Sec::in),"{
   struct tm timestamp;
   timestamp.tm_year = (Year - 1900);
   timestamp.tm_mon = (Month - 1);
   timestamp.tm_mday = Day;
   timestamp.tm_hour = Hour;
   timestamp.tm_min = Minute;
   timestamp.tm_sec = Sec;
   timestamp.tm_isdst = -1;
   mctime_offset = difftime(time((time_t*)0),mktime(&timestamp));
   };").

mctime__reset_datetime(Date):-
	impure reset_datetimeC,
	mctime__get_current(Date).

:- impure pred reset_datetimeC is det.
:- pragma c_code(reset_datetimeC,"{ mctime_offset = 0; };").
*/

:- pred date_get(int,int,int,int,int,int,int,io,io).
:- mode date_get(in,out,out,out,out,out,out,di,uo) is det.

:- pragma promise_pure(date_get/9).
:- pragma foreign_proc("C",
    date_get(Zone::in,Year::out,Month::out,Day::out,Hour::out,
        Minute::out,Sec::out, IO0::di, IO::uo),
    [will_not_call_mercury, thread_safe, tabled_for_io, may_not_duplicate],
"
	struct tm *timestamp;
        time_t When;

        When = (time((time_t*)0) - mctime_offset);

        if (Zone == 1) {
	   timestamp = localtime(&When);
        } else {
           timestamp = gmtime(&When);
        };

        Year = (timestamp->tm_year + 1900);     /* -1900 */
        Month = (timestamp->tm_mon + 1) ;	/* 0..11 */
	Day = (timestamp->tm_mday);		/* 1..31 */
        Hour = (timestamp->tm_hour);	        /* 1..24 */ /* XXX 0-23? */
	Minute = (timestamp->tm_min);		/* 0..59 */
	Sec = (timestamp->tm_sec);		/* 0..59 */ /* XXX 0-61? */
	IO = IO0;
").

date_get(Zone, Year, Month, Day, Hour, Minute, Sec, !IO) :-
    time.time(Time, !IO),
    % XXX the C code subtracts offset, but offset always zero
    ( Zone = 1 ->
        TM = time.localtime(Time)
    ;
        TM = time.gmtime(Time)
    ),
    TM = time.tm(Year0, Month, Day, Hour, Minute, Sec, _Yday, _Wday, _IsDst),
    Year = Year0 + 1900.

%------------------------------------------------------------------------
%
date_reduce(Date, RDate, !IO) :-
	mctime__get_year(CurrYear,!IO),
	string__left(Date,16,RDate0),
	( [RDate1,Time] = string.words(RDate0),
	  [Year,Month,Day] = string.words_separator(unify('-'), RDate1),
	  string__to_int(Month, MonthInt),
	  month_table(MonthInt,MonthStr,_) ->
		( string__format("%d",[i(CurrYear)],Year) ->
			string__append_list([Day,"-",MonthStr," ",Time],
					    RDate)
		;	string__append_list([Day,"-",MonthStr,"-",Year],RDate))
	;	RDate = RDate0).

mctime__reduced(date(Year,Month,Day,Hour,Min,_Sec,_), String, !IO):-
	mctime__get_year(CurrYear,!IO),
	( CurrYear = Year ->
		YearStr = ""
	;	string__format("-%d",[i(Year)],YearStr)),
	( month_table(Month,MonthStr0,_) ->
		MonthStr = MonthStr0
	;	error("time__reduced: fatal error")),
	( Hour \= 0, Min \= 0 ->
		string__format(" %d:%d",[i(Hour),i(Min)],TimeStr)
	;	TimeStr = "" ),
	string__format("%d-%s%s%s",[i(Day),s(MonthStr),s(YearStr),s(TimeStr)],
		       String).

month_table(1, "Jan", 31).
month_table(2, "Feb", 29). % WRONG 75 % of the time!
month_table(3, "Mar", 31).
month_table(4, "Apr", 30).
month_table(5, "May", 31).
month_table(6, "Jun", 30).
month_table(7, "Jul", 31).
month_table(8, "Aug", 31).
month_table(9, "Sep", 30).
month_table(10, "Oct",31).
month_table(11, "Nov",30).
month_table(12, "Dec",31).

day_table("sun",sun,0).
day_table("mon",mon,1).
day_table("tue",tue,2).
day_table("wed",wed,3).
day_table("thu",thu,4).
day_table("fri",fri,5).
day_table("sat",sat,6).

mctime__date_compare(Date1,Date2,Res):-
	mctime__seconds_between(Date1,Date2,Between),
	( Between = 0  ->
	     Res = (=)
	; Between > 0 ->
	     Res = (>)
	;    Res = (<) ).

mctime__date_to_smalldatetime(DateIn,DateOut):-
	get_date_time(HH,MM,_SS,DateIn),
	set_date_time(HH,MM,0,DateIn,DateOut).

mctime__get_day_of_week(DateIn,DayOfWeek):-
	day_of_week(DateIn,DayNumber),
	( day_table(_,DayOfWeek0,DayNumber) ->
	     DayOfWeek = DayOfWeek0
	;    error("mctime__get_day_of_week/2: internal error")).

mctime__days_since_sunday(mon) = 1.
mctime__days_since_sunday(tue) = 2.
mctime__days_since_sunday(wed) = 3.
mctime__days_since_sunday(thu) = 4.
mctime__days_since_sunday(fri) = 5.
mctime__days_since_sunday(sat) = 6.
mctime__days_since_sunday(sun) = 0.

mctime__next_day_of_week(DateIn,DayOfWeek,DateOut):-
	mctime__get_day_of_week(DateIn,DayOfWeek1),
	( DayOfWeek1 = DayOfWeek ->
	        julian_day_to_minutes(julian_day(DateIn) + 7,NewMins),
	        julian_minutes_to_date(NewMins,DateOut)
	;       day_table(_,DayOfWeek1,Current),
	        day_table(_,DayOfWeek,Requested),
	        StartWeek = julian_day(DateIn) - Current,
  	        ( Requested < Current ->
		      R = StartWeek + 7 + Requested
	        ;     R = StartWeek + Requested ),
	        julian_day_to_minutes(R,Mins),
	        julian_minutes_to_date(Mins,DateOut)).
	       
mctime__next_day_of_week(DayOfWeek,Date,!IO):-
	mctime__get_current(Today,!IO),
	mctime__next_day_of_week(Today,DayOfWeek,Date).

mctime__next_month(DateIn,DayNumber,DateOut):-
	get_date_date(Year,Month,_Day,DateIn),
	( Month = 12 ->
	     set_date_date(Year+1,1,DayNumber,DateIn,DateOut)
	;    set_date_date(Year,Month+1,DayNumber,DateIn,DateOut)).

mctime__prev_month(DateIn,DayNumber,DateOut):-
	get_date_date(Year,Month,_Day,DateIn),
	( Month = 1 ->
	     set_date_date(Year-1,12,DayNumber,DateIn,DateOut)
	;    set_date_date(Year,Month-1,DayNumber,DateIn,DateOut)).

mctime__next_month(DayNumber,DateOut,!IO):-
	mctime__get_current(Today,!IO),
	mctime__next_month(Today,DayNumber,DateOut).

mctime__add_days(DateIn,Days,DateOut):-
	get_date_time(H,M,S,DateIn),
	julian_to_date(Days+julian_day(DateIn),DateOut0),
	set_date_time(H,M,S,DateOut0,DateOut).

mctime__add_months(DateIn,MonthsToAdd,DateOut):-
	get_date_date(Year,Month,Day,DateIn),
	( MonthsToAdd = 0 ->
	          DateOut = DateIn
	; Month + MonthsToAdd =< 0 ->
	          mctime__add_years(DateIn,-1,NDate),
	          get_date_date(NYear,_Month,NDay,NDate),
	          set_date_date(NYear,12,NDay,NDate,NDate1),
	          mctime__add_months(NDate1,(MonthsToAdd+Month),DateOut)
	; Month + MonthsToAdd > 12 ->
	          mctime__add_years(DateIn,1,NDate),
	          get_date_date(NYear,_Month,NDay,NDate),
	          set_date_date(NYear,1,NDay,NDate,NDate1),
	          mctime__add_months(NDate1,(Month+MonthsToAdd-12-1),DateOut)
	;         set_date_date(Year,Month+MonthsToAdd,Day,DateIn,DateOut) ).

mctime__add_years(DateIn,Years,DateOut):-
	get_date_date(Year,Month,Day,DateIn),
	set_date_date(Year+Years,Month,Day,DateIn,DateOut).

mctime__add_minutes(DateIn,Minutes,DateOut):-
	julian_minutes(DateIn,DInMins),
	julian_minutes_to_date(DInMins+integer(Minutes),DateOut).

        %---------------------------------------------------------------
  
mctime__edate_init(enull).

mctime__edate_init = EDate :-
        mctime__edate_init(EDate).

mctime__edate_to_string(EDate,String) :-
	( EDate = enull,
	      String = ""
	; EDate = edate(Date),
	      mctime__date_to_string(Date,String) ).

mctime__string_to_edate(String,EDate) :-
	( String = "" ->
	      EDate = enull
	;     mctime__string_to_date(String,Date),
	      EDate = edate(Date)).

mctime__edate_to_smalldatetime_string(EDate,String):-
	( EDate = enull ,
	      String = ""
	; EDate = edate(Date),
	      mctime__date_to_smalldatetime_string(Date,String)).

mctime__date_to_edate(Date,edate(Date)).

mctime__edate_to_date(edate(Date),Date).

mctime__date(Edate) = Date :-
	mctime__edate_to_date(Edate,Date).

mctime__det_edate_to_date(EDate,Date):-
	( mctime__edate_to_date(EDate,Date0) ->
	      Date = Date0
	;     error("mctime__det_edate_to_date: given null date!") ).

mctime__edate_add_days(EDate,Int,edate(NDate)):-
	( mctime__edate_to_date(EDate,Date) ->
	      mctime__add_days(Date,Int,NDate)
	;     error("edate_add_days/3: callled with null edate")).

mctime__edate_add_months(EDate,Int,edate(NDate)):-
	( mctime__edate_to_date(EDate,Date) ->
	      mctime__add_months(Date,Int,NDate)
	;     error("edate_add_months/3: callled with null edate")).

mctime__edate_add_years(EDate,Int,edate(NDate)):-
	( mctime__edate_to_date(EDate,Date) ->
	      mctime__add_years(Date,Int,NDate)
	;     error("edate_add_years/3: callled with null edate")).

mctime__null_edate(enull).

mctime__get_ecurrent(edate(Date),!IO):-
	mctime__get_current(Date,!IO).

%mctime__get_ecurrent = Edate :-
%        mctime__get_ecurrent(Edate).

:- end_module mctime.
