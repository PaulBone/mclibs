:- module calendar.

:- interface.

:- import_module io.

%------------------------------------------------------------------------------%
% Public declaration of the data type calendar

:- type calendar.

% Public declaration of the typeclass calendar

:- typeclass calendar(T) where [
	func calendar(io, io) = T,
	mode calendar(di, uo) = out is det,
	func calendar(int, int, int) = T,
	
	func get_year(T) = int,
	func get_month(T) = int,
	func get_day(T) = int,
	func get_hour(T) = int,
	func get_minute(T) = int,
	func get_second(T) = int,
	
	func add_year(int, T) = T,
	func add_month(int, T) = T,
	func add_day(int, T) = T
].

:- pred is_leap(int::in) is semidet.

% Default calendar instance
:- instance calendar(calendar).

%------------------------------------------------------------------------------%

:- implementation.

%------------------------------------------------------------------------------%

:- import_module int, string, maybe.
:- import_module require.
:- import_module time.

% Default calendar type
:- type calendar ---> calendar(
	year	:: int,
	month	:: int,
	day		:: int,
	hour	:: int,
	minute	:: int,
	second	:: int
).

:- type month ---> (
	januari;
	februari;
	march;
	april;
	may;
	june;
	juli;
	august;
	september;
	october;
	november;
	december
).

:- func month_enum(month) = int.
:- mode month_enum(in) = out is det.
:- mode month_enum(out) = in is semidet.

month_enum(januari) = 1.
month_enum(februari) = 2.
month_enum(march) = 3.
month_enum(april) = 4.
month_enum(may) = 5.
month_enum(june) = 6.
month_enum(juli) = 7.
month_enum(august) = 8.
month_enum(september) = 9.
month_enum(october) = 10.
month_enum(november) = 11.
month_enum(december) = 12.

:- func month_to_int(month) = int.
month_to_int(Month) = month_enum(Month).

:- func int_to_month(int) = month is det.
int_to_month(Integer) = Month :- (
	( Integer = month_enum(Month0) ->
		Month = Month0
	;
		error("calendar.int_to_month: Argument = '" ++ string(Integer) ++"' should be within range 0-11")
	)
).

:- func number_of_months = int.
number_of_months = 12.

:- func days_in_month(month, int) = int.
days_in_month(januari, _) = 31.
days_in_month(februari, Year) = Days :-( is_leap(Year) -> Days = 29; Days = 28).
days_in_month(march, _) = 31.
days_in_month(april, _) = 30.
days_in_month(may, _) = 31.
days_in_month(june, _) = 30.
days_in_month(juli, _) = 31.
days_in_month(august, _) = 31.
days_in_month(september, _) = 30.
days_in_month(october, _) = 31.
days_in_month(november, _) = 30.
days_in_month(december, _) = 31.

is_leap(Year) :- (
		( Year rem 4 = 0, Year rem 100 \= 0 ); Year rem 400 = 0
	).

% Default implementation of the calendar instance
% Not the smartest implementation, but it works :-)

:- instance calendar(calendar) where [
	calendar(!IO) = Calendar :- (
		time.time(Timestamp, !IO),
		TM = Timestamp ^ time.localtime,
		Calendar = calendar(TM ^ tm_year + 1900, TM ^ tm_mon + 1, TM ^ tm_mday,
							TM ^ tm_hour, TM ^ tm_min, TM ^ tm_sec)
	),
	
	calendar(Year, Month, Day) = Calendar :- (
		require(Year >= 1900, "calendar.calendar: supported years start at 1900"),
		require(Month >= 1, "calendar.calendar: supported months start at 1"),
		require(Month =< 12, "calendar.calendar: supported months end at 12"),
		require(Day >= 1, "calendar.calendar: supported days start at 1"),
		require(Day =< 31, "calendar.calendar: supported days end at 31"),

		% TM used for validation, if any...
		TM = tm(Year - 1900 , Month - 1, Day, 0, 0, 0, 0, 0, no),
		Calendar = calendar(TM ^ tm_year + 1900, TM ^ tm_mon + 1, TM ^ tm_mday,
							TM ^ tm_hour, TM ^ tm_min, TM ^ tm_sec)
	),
	
	Calendar ^ get_year = Calendar ^ year,
	Calendar ^ get_month = Calendar ^ month,
	Calendar ^ get_day = Calendar ^ day,
	Calendar ^ get_hour = Calendar ^ hour,
	Calendar ^ get_minute = Calendar ^ minute,
	Calendar ^ get_second = Calendar ^ second,
	
	Calendar ^ add_year(Number) = UpdatedCalendar :- (
		Year = Calendar ^ year + Number,
		Month = Calendar ^ month,
		Day = Calendar ^ day,
		LastDay = days_in_month(int_to_month(Month), Year),
		( Day > LastDay ->
			UpdatedCalendar = (Calendar ^ year := Year) ^ day := LastDay
		;
			UpdatedCalendar = Calendar ^ year := Year 
		)
	),

	Calendar ^ add_month(Number) = UpdatedCalendar :- (
		Year0 = Calendar ^ year + (Number // number_of_months),
		Month0 = Calendar ^ month + (Number rem number_of_months),
		( Month0 < month_to_int(januari) ->
			Year = Year0 - 1,
			Month = Month0 + number_of_months
		; Month0 > month_to_int(december) ->
			Year = Year0 + 1,
			Month = Month0 - number_of_months
		;
			Year = Year0,
			Month = Month0
		),
		Day = Calendar ^ day,
		LastDay = days_in_month(int_to_month(Month), Year),
		( Day > LastDay ->
			UpdatedCalendar = ((Calendar ^ year := Year) ^ month := Month) ^ day := LastDay
		;
			UpdatedCalendar = (Calendar ^ year := Year) ^ month := Month
		)
	),

	Calendar ^ add_day(Number) = UpdatedCalendar :- (
		require(Number >= 0, "calendar.calendar: negative number of days are currently not supported"),
		DaysInMonth = days_in_month(int_to_month(Calendar ^ month), Calendar ^ year),
		Day0 = Calendar ^ day + Number,
		( Day0 =< DaysInMonth ->
			UpdatedCalendar = Calendar ^ day := Day0
		;
			Day = 1,
			Month0 = Calendar ^ month + 1,
			Year0 = Calendar ^ year,
			( Month0 > month_to_int(december) ->
				Year = Year0 + 1,
				Month = Month0 - number_of_months
			;
				Year = Year0,
				Month = Month0
			),
			UpdatedCalendar0 = ((Calendar ^ year := Year) ^ month := Month) ^ day := Day,
			UpdatedCalendar = UpdatedCalendar0 ^ add_day(Day0 - DaysInMonth - 1)
		)
	)
].

%------------------------------------------------------------------------------%

:- end_module calendar.
