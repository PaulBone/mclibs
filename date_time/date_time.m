%----------------------------------------------------------------------------%
:- module date_time.
% Copyright (c) Mission Critical IT
% MIT License
% A module for working with date-times and durations conforming to
% http://www.w3.org/TR/xmlschema-2/.
%----------------------------------------------------------------------------%

:- interface.

:- import_module io.
:- import_module maybe.
:- import_module xsd.
:- import_module xsd.mercury.

%----------------------------------------------------------------------------%

    % A date-time conforming to http://www.w3.org/TR/xmlschema-2/#dateTime.
    % Dates are in the Gregorian calendar.
    %
:- type date_time.

    % A date conforming to http://www.w3.org/TR/xmlschema-2/#date.
    % Dates are in the Gregorian calendar.
    %
:- type date.

    % A year month conforming to http://www.w3.org/TR/xmlschema-2/#gYearMonth.
    % Dates are in the Gregorian calendar.
    %
:- type year_month.

    % A time conforming to http://www.w3.org/TR/xmlschema-2/#time.
    %
:- type time.

    % A duration conforming to http://www.w3.org/TR/xmlschema-2/#duration.
    % A duration is a six dimensional vector representing the time elapsed
    % between two date-times.  The dimension are years, months, days, hours,
    % minutes and seconds.
    %
:- type duration.

    % A time zone is a duration with only hours and minutes.
    %
:- type time_zone.

    % An optional time zone.
    %
:- type maybe_zoned == maybe(time_zone).

:- type month == int.  % 1..12

    % Any year except 0 is allowed. Negative years are BC.
    % XXX The XML Schema spec says this may change in the future.
    %
:- type year == int.          

:- type day_of_month == int. % 1..31 depending on the month and year.
:- type hour == int.         % 0..23
:- type minute == int.       % 0..59
:- type second == float.     % [0.0 .. 60.0) (i.e. 0.0 allowed, but 60.0 not)

:- type day_of_week
    --->    sunday
    ;       monday
    ;       tuesday
    ;       wednesday
    ;       thursday
    ;       friday
    ;       saturday.

    % Duration dimensions.
    %
:- type years == int.
:- type months == int.
:- type days == int.
:- type hours == int.
:- type minutes == int.
:- type seconds == float.

    % Parse a date-time string conforming to the lexical representation
    % described at http://www.w3.org/TR/xmlschema-2/#dateTime.
    %
:- pred date_time_from_xsd_string(string::in, date_time::out) is semidet.

    % Same as above, but aborts if the lexical representation is invalid.
    %
:- func det_date_time_from_xsd_string(string) = date_time.

    % Convert a date-time to the normalized lexical representation
    % described at http://www.w3.org/TR/xmlschema-2/#dateTime.
    %
:- func date_time_to_xsd_string(date_time) = string.

    % serialize the dateTime to the standard BE format date.
:- func date_time_to_date_BEformat(date_time) = string.


    % Parse a duration string conforming to the lexical representation
    % described at http://www.w3.org/TR/xmlschema-2/#duration.
    %
:- pred duration_from_xsd_string(string::in, duration::out) is semidet.

    % Same as above, but aborts if the lexical representation is invalid.
    %
:- func det_duration_from_xsd_string(string) = duration.

    % Convert a duration to the lexical representation
    % described at http://www.w3.org/TR/xmlschema-2/#duration.
    %
:- func duration_to_xsd_string(duration) = string.

    % Normalize duration so that seconds, minutes, hours and
    % months are within their ranges. Only days are not normalized.
    %
:- func normalize_duration(duration) = duration.

    % Add two durations. The result is a normalized duration.
    %
:- func add_durations(duration, duration) = duration.

    % Succeeds if the duration is an xsd:dayTimeDuration,
    % i.e. the months and years components are zero.
    %
:- pred is_day_time_duration(duration::in) is semidet.

    % Succeeds if the duration is an xsd:yearMonthDuration,
    % i.e. the days, hours, minutes and seconds components are zero.
    %
:- pred is_year_month_duration(duration::in) is semidet.

    % This predicate implements the partial order relation on date-times
    % described at http://www.w3.org/TR/xmlschema-2/#dateTime.
    % Note that if date_time_leq(X, Y) fails, then this does NOT imply
    % that X > Y.
    %
:- pred date_time_leq(date_time::in, date_time::in) is semidet.

:- pred is_zero_duration(duration::in) is semidet.

    % This predicate implements the partial order relation on durations
    % described at http://www.w3.org/TR/xmlschema-2/#duration.
    % Note that if duration_leq(X, Y) fails, then this does NOT imply
    % that X > Y.
    %
:- pred duration_leq(duration::in, duration::in) is semidet.

:- pred date_or_time_equal(T::in, T::in) is semidet <= date_or_time(T).

    % A total order exists on the set of all zoned date-times.
    % This predicate first converts its arguments to zoned date-times
    % (using the supplied time zone for unzoned date-times).  It then
    % compares the zoned date-times.
    %
:- pred compare_date_times_using_zone(date_time::in, date_time::in,
    time_zone::in, builtin.comparison_result::out) is det.

%----------------------------------------------------------------------------%
% Typeclasses for dates and times.


:- typeclass date_or_time(T) <= 
    (xsd.mercury.builtin_primitive_type(T), xsd.mercury.partially_ordered(T)) where [

    func maybe_zoned(T) = maybe_zoned,
    pred set_maybe_zoned(maybe_zoned::in, T::in, T::out) is det,
    pred add_duration(duration::in, T::in, T::out) is det,

        % Find the duration between two dates that has only days, hours,
        % minutes and seconds and where the number of hours in the duration will be
        % less than 24 and the minutes and seconds will both be less than 60.
        % Fails if one date-time is zoned and the other isn't.
        %
    pred day_time_between(T::in, T::in, duration::out) is semidet,

        % Return the current/date time. Preferably, the predicate
        % should return local time together with the time zone.
    pred current_time(T::out, io::di, io::uo) is det
].

:- type some_date_or_time ---> some [D]
        some_date_or_time(D) => date_or_time(D).


:- instance any_type(time).
:- instance atomic_type(time).
:- instance builtin_type(time).
:- instance builtin_primitive_type(time).
:- instance partially_ordered(time).
:- instance date_or_time(time).

:- instance any_type(date).
:- instance atomic_type(date).
:- instance builtin_type(date).
:- instance builtin_primitive_type(date).
:- instance partially_ordered(date).
:- instance date_or_time(date).

:- instance any_type(year_month).
:- instance atomic_type(year_month).
:- instance builtin_type(year_month).
:- instance builtin_primitive_type(year_month).
:- instance partially_ordered(year_month).
:- instance date_or_time(year_month).

:- instance any_type(date_time).
:- instance atomic_type(date_time).
:- instance builtin_type(date_time).
:- instance builtin_primitive_type(date_time).
:- instance partially_ordered(date_time.date_time).

:- instance date_or_time(date_time).

:- typeclass date(T) <= date_or_time(T) where [
    func year(T) = year,
    func month(T) = month,
    func day_of_month(T) = day_of_month,
    pred init_date(year::in, month::in, day_of_month::in, 
	maybe_zoned::in, T::out) is semidet
].

:- instance date(date).
:- instance date(date_time).
:- instance date(year_month).

:- typeclass time(T) <= date_or_time(T) where [
    func hour(T) = hour,
    func minute(T) = minute,
    func second(T) = second,
    pred init_time(hour::in, minute::in, second::in, maybe_zoned::in, T::out) is semidet
].

:- func det_init_time(hour, minute, second, maybe_zoned) = T <= time(T).

:- func det_init_date(year, month, day_of_month, maybe_zoned) = T <= date(T).

    %
    % Converts from one date type to another
    %
:- pred date_to_date(D1::in, D2::out) is semidet <= (date(D1), date(D2)).

    %
    % Converts from one time type to another
    %
:- pred time_to_time(T1::in, T2::out) is semidet <= (time(T1), time(T2)).

:- instance time(time).
:- instance time(date_time).

:- typeclass date_time(T) <= (date(T), time(T)) where [
].

:- instance date_time(date_time).

%----------------------------------------------------------------------------%

    % These functions return the various dimensions of a duration.
    % For example: years(det_duration_from_xsd_string("P1Y400D")) = 1,
    % months(det_duration_from_xsd_string("P10Y18M300DT20M")) = 18,
    % seconds(det_duration_from_xsd_string("PT13M")) = 0.0,
    % hours(det_duration_from_xsd_string("-P1Y2DT3H")) = -3.
    %
:- func years(duration) = int.
:- func months(duration) = int.
:- func days(duration) = int.
:- func hours(duration) = int.
:- func minutes(duration) = int.
:- func seconds(duration) = float.

:- func day_of_week(T) = day_of_week <= date(T).

    % init_date_time(Year, Month, Day, Hour, Minute, Second, MaybeZoned) = DT.
    % Initialize a new date_time.
    % Aborts if the given date-time is invalid.
    %
:- func det_init_date_time(year, month, day_of_month, hour, minute, second,
    maybe_zoned) = date_time.

    % As above, but fails if the date-time is invalid.
    %
:- pred init_date_time(year::in, month::in, day_of_month::in, hour::in,
    minute::in, second::in, maybe_zoned::in, date_time::out) is semidet.

    % Get the current local time as an unzoned date-time.
    %
:- pred current_unzoned_local_time(date_time::out, io::di, io::uo) is det.

    % Get the current time as a UTC date-time.
    %
:- pred current_utc_time(date_time::out, io::di, io::uo) is det.

    % Get the current time as a zoned date-time.
    %
:- pred current_zoned_time(date_time::out, io::di, io::uo) is det.

    % Get the local time zone.
    %
:- pred local_time_zone(time_zone::out, io::di, io::uo) is det.

    % init_{positive|negative}_duration(Years, Months, Days, Hours, Minutes,
    %   Seconds)
    % Create a new positive or negative duration.  All the supplied dimensions
    % should be non-negative. If they are not the function aborts.
    %
:- func init_positive_duration(years, months, days, hours, minutes, seconds) =
    duration.
:- func init_negative_duration(years, months, days, hours, minutes, seconds) =
    duration.

    % init_{positive|negative}_time_zone(Hours, Minutes)
    % Create a new positive or negative time zone.  Hours and Minutes should
    % both be positive.  Hours should be less than or equal to 14 and if it is
    % 14, minutes should be 0.  If this is not the case, the predicates fail.
    %
:- pred init_positive_time_zone(hours::in, minutes::in, time_zone::out) is semidet.
:- pred init_negative_time_zone(hours::in, minutes::in, time_zone::out) is semidet.

    % Minutes should be between -840 and 840 inclusive.
    %
:- pred init_time_zone_from_minutes(minutes::in, time_zone::out) is semidet.

:- func det_init_positive_time_zone(hours, minutes) = time_zone.
:- func det_init_negative_time_zone(hours, minutes) = time_zone.

    % Converts a time zone to duration. The duration has the same sign
    % as the time zone.
    %
:- func time_zone_to_duration(time_zone) = duration.

    % Converts a duration to a time_zone. The years, months, days and
    % seconds components must be empty and the hours and minutes must be
    % withen the range -14:00 and 14:00 inclusive.
    % The time zone has the same sign as the duration.
    %
:- pred duration_to_time_zone(duration::in, time_zone::out) is semidet.

:- func utc_time_zone = time_zone.


    % Negate a duration.
    %
:- func negate(duration) = duration.

:- func absolute(duration) = duration.

    % Returns the zero length duration.
    %
:- func zero_duration = duration.

    % to_utc_zone(LocalDate) = UtcDate.
    % If LocalDate is already zoned, then convert it to utc zone,
    % otherwise LocalDate remains unchanges.
    %
:- func to_utc_zone(T) = T <= date_or_time(T).

    % If the input date/time is unzoned, convert it to a zoned date/time using
    % the supplied zone, otherwise leave it unchanges.
    %
:- func zone_if_unzoned(T, time_zone) = T <= date_or_time(T).

    % Update a date/time type using the given time zone, i.e. the time zone is
    % converted to a duration and subtracted from the date/time.
    % The original time zone of the date/time remains unchanged.
    %
:- pred apply_time_zone(time_zone::in, T::in, T::out) is det <= date_or_time(T).

    % Add the given duration to the given date-time using the algorithm
    % described at
    % http://www.w3.org/TR/xmlschema-2/#adding-durations-to-dateTimes.
    %
:- pred add_duration_to_date_time(duration::in, date_time::in, date_time::out)
    is det.

    % Same as day_time_between, but first converts both date-times to
    % zoned date-times using the supplied time zone for unzoned date-times.
    %
:- pred day_time_between_using_zone(date_time::in, date_time::in,
    time_zone::in, duration::out) is det.

    % Find the duration between two dates using a "greedy" algorithm.  The
    % algorithm is greedy in the sense that it will try to maximise each
    % dimension in the returned duration in the following order: years, months,
    % days, hours, minutes, seconds.
    % Fails if one date-time is zoned and the other isn't.
    %
    % If greedy_duration_between(Date1, Date2, Duration) succeeds, then
    % add_duration_to_date_time(Date1, Duration, Date2) will hold
    % (modulo any precision lost via floating point computations), but
    % add_duration_to_date_time(Date2, negate(Duration), Date1) may not
    % hold.  For example if:
    %   Date1 = det_date_time_from_xsd_string("2001-01-31T00:00:00Z"),
    %   Date2 = det_date_time_from_xsd_string("2001-02-28T00:00:00Z"),
    %   Duration = det_duration_from_xsd_string("P1M")
    % then the following holds:
    %   greedy_duration_between(Date1, Date2, Duration),
    %   add_duration_to_date_time(Duration, Date1, Date2)
    % but the following does not:
    %   add_duration_to_date_time(negate(Duration), Date2, Date1).
    % (Adding "-P1M" to Date2 will yield "2001-01-28T00:00:00Z".)
    %
:- pred greedy_duration_between(date_time::in, date_time::in, duration::out)
    is semidet.

    % Same as above, but first converts both date-times to zoned date-times
    % using the supplied time zone for unzoned date-times.
    %
:- pred greedy_duration_between_using_zone(date_time::in, date_time::in,
    time_zone::in, duration::out) is det.

%----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.
:- import_module time.

%----------------------------------------------------------------------------%

:- type date_time
    --->    date_time(
                dt_year         :: int,
                dt_month        :: int,
                dt_day          :: int,
                dt_hour         :: int,
                dt_minute       :: int,
                dt_second       :: float,
                dt_maybe_zoned  :: maybe_zoned
            ).

:- type date
    --->    date(
                d_year         :: int,
                d_month        :: int,
                d_day          :: int,
                d_maybe_zoned  :: maybe_zoned
            ).

:- type year_month
    --->    year_month(
                ym_year         :: int,
                ym_month        :: int,
                ym_maybe_zoned  :: maybe_zoned
            ).

:- type time
    --->    time(
                t_hour         :: int,
                t_minute       :: int,
                t_second       :: float,
                t_maybe_zoned  :: maybe_zoned
            ).

:- type duration
    --->    duration(
                dur_years   :: int,
                dur_months  :: int,
                dur_days    :: int,
                dur_hours   :: int,
                dur_minutes :: int,
                dur_seconds :: float
            ).

    %
    % Number of minutes in the range −840 and 840 inclusive.
    %
:- type time_zone == int.


%-----------------------------------------------------------------------------%
% Typeclasses

:- instance any_type(time) where [
    pred(equal/2) is date_or_time_equal,

    (is_valid(_) :- semidet_succeed), % It's impossible to construct an invalid time.

    default_value = det_init_time(0, 0, 0.0, no),

    (from_string(XSDStr, Time) :-
        some [!Chars] (
            !:Chars = string.to_char_list(XSDStr),
            read_int_and_num_chars(Hour, 2, !Chars),
            Hour >= 0,
            Hour =< 24, % 24 is allowed in certain cases, see below.
            read_char((:), !Chars),
            read_int_and_num_chars(Minute, 2, !Chars),
            Minute >= 0,
            Minute =< 59,
            read_char((:), !Chars),
            % Make sure there are two digits for the whole number part of the
            % second field.
            read_int_and_num_chars(_, 2, !.Chars, _),
            read_float(Second, !Chars),
            Second < 60.0,
            ( read_time_zone(TZ, !.Chars, Rest2) ->
                MaybeTZ = yes(TZ),
                !:Chars = Rest2
            ;
                MaybeTZ = no
            ),
            !.Chars = [],
            ( Hour = 24 ->
                %
                % If the hour is 24, then the minutes and seconds must be zero
                % and the date represented is the start of the next day.
                %
                Minute = 0,
                Second = 0.0,
                Time = time(0, 0, 0.0, MaybeTZ)
            ;
                Time = time(Hour, Minute, Second, MaybeTZ)
            )
        )
    ),

    (to_string(time(Hour, Minute, Second, MaybeZoned)) = XSDStr :-
        WholeSecond = truncate_to_int(Second),
        SecondFraction = Second - float(WholeSecond),
        ( SecondFraction = 0.0 ->
            SecondFractionStr = ""
        ;
            SecondFractionStr = lstrip_pred(unify('0'),
                float_to_string(SecondFraction))
        ),
	TZStr = maybe_zoned_str(MaybeZoned),
        XSDStr = string.format("%02d:%02d:%02d%s%s",
            [i(Hour), i(Minute), i(WholeSecond), s(SecondFractionStr), s(TZStr)])
    ),

    mercury_name(_) = "date_time.time",
    mercury_modules(_) = ["date_time"],
    mercury_term(D) = string.format("date_time.init_time(%i,%i,%f,%s)",
	[i(D ^ hour), i(D ^ minute), f(D ^ second), s(string(D ^ maybe_zoned))])
].
:- instance atomic_type(time) where [
    atomic_type(T) = builtin_type(builtin_type(T))
].
:- instance builtin_type(time) where [
    builtin_type(T) = builtin_primitive_type(primitive_type(T))
].
:- instance builtin_primitive_type(time) where [
    primitive_type(_) = xsd_time
].
:- instance partially_ordered(time) where [
    (leq(Time1, Time2) :-
	time_to_time(Time1, DateTime1),
	time_to_time(Time2, DateTime2),
	date_time_leq(DateTime1, DateTime2)
    )
].

:- instance date_or_time(time) where [
    (maybe_zoned(T) = T ^ t_maybe_zoned),

    (set_maybe_zoned(MaybeZoned, !T) :- !T ^ t_maybe_zoned := MaybeZoned),

    (add_duration(!.Duration, !Time) :-
	% Reset the year-month-day part of !.Duration
        !Duration ^ dur_years := 0,
	!Duration ^ dur_months := 0,
	!Duration ^ dur_days := 0,

	% Initialize a date_time from time
        DateTime0 = det_init_time(!.Time ^ hour, !.Time ^ minute, !.Time ^ second,
	                          !.Time ^ maybe_zoned),
	% Add the duration
        add_duration_to_date_time(!.Duration, DateTime0, DateTime),
	
	% Ignore the date part
	!:Time = time(DateTime ^ hour, DateTime ^ minute, DateTime ^ second,
	    DateTime ^ maybe_zoned)
    ),

    (day_time_between(Time1, Time2, Duration) :-
	time_to_time(Time1, DateTime1),
	time_to_time(Time2, DateTime2),
	day_time_between_date_times(DateTime1, DateTime2, Duration)
    ),

    (current_time(Time, !IO) :-
        time.time(TimeT, !IO),
        TM = time.localtime(TimeT),
	TMSecond = TM ^ tm_sec,
	Second = float(int.min(TMSecond, 59)),
	local_time_zone(TimeZone, !IO),
	Time = time(TM ^ tm_hour, TM ^ tm_min, Second, yes(TimeZone))
    )
].

:- instance any_type(date) where [
    pred(equal/2) is date_or_time_equal,

    (is_valid(_) :- semidet_succeed), % It's impossible to construct an invalid date.

    default_value = det_init_date(1970, 1, 1, no),

    ( from_string(XSDStr, Date) :-
        some [!Chars] (
            !:Chars = string.to_char_list(XSDStr),
            ( read_char((-), !.Chars, Rest1) ->
                !:Chars = Rest1,
                read_int_and_num_chars(Year0, YearChars, !Chars),
                Year = -Year0
            ;
                read_int_and_num_chars(Year, YearChars, !Chars)
            ),
            YearChars >= 4,
            Year \= 0,
            read_char((-), !Chars),
            read_int_and_num_chars(Month, 2, !Chars),
            Month >= 1,
            Month =< 12,
            read_char((-), !Chars),
            read_int_and_num_chars(Day, 2, !Chars),
            Day >= 1,
            Day =< max_day_in_month_for(Year, Month),
            ( read_time_zone(TZ, !.Chars, Rest2) ->
                MaybeTZ = yes(TZ),
                !:Chars = Rest2
            ;
                MaybeTZ = no
            ),
            !.Chars = [],
            Date = date(Year, Month, Day, MaybeTZ)
        )
    ),

    ( to_string(date(Year0, Month, Day, MaybeZoned)) = XSDStr :-
        ( Year0 < 0 ->
            SignStr = "-",
            Year = -Year0
        ;
            SignStr = "",
            Year = Year0
        ),
	TZStr = maybe_zoned_str(MaybeZoned),
        XSDStr = string.format("%s%04d-%02d-%02d%s",
            [s(SignStr), i(Year), i(Month), i(Day), s(TZStr)])
    ),

    mercury_name(_) = "date_time.date",
    mercury_modules(_) = ["date_time"],
    mercury_term(D) = string.format("date_time.init_date(%i,%s,%i,%s)",
	[i(D ^ year),s(string(D ^ month)),i(D ^ day_of_month),s(string(D ^ maybe_zoned))])
].
:- instance atomic_type(date) where [
    atomic_type(T) = builtin_type(builtin_type(T))
].
:- instance builtin_type(date) where [
    builtin_type(T) = builtin_primitive_type(primitive_type(T))
].
:- instance builtin_primitive_type(date) where [
    primitive_type(_) = xsd_date
].
:- instance partially_ordered(date) where [
    (leq(Date1, Date2) :-
	date_to_date(Date1, DateTime1),
	date_to_date(Date2, DateTime2),
	date_time_leq(DateTime1, DateTime2)
    )
].

:- instance date_or_time(date) where [
    (maybe_zoned(T) = T ^ d_maybe_zoned),
    (set_maybe_zoned(MaybeZoned, !T) :- !T ^ d_maybe_zoned := MaybeZoned),
    (add_duration(Duration, !Date) :-
	% Initialize a date_time from date (setting hours, minutes and seconds to zero)
        DateTime0 = det_init_date(!.Date ^ year, !.Date ^ month, !.Date ^ day_of_month,
	                      !.Date ^ maybe_zoned),
	% Add the duration
        add_duration_to_date_time(Duration, DateTime0, DateTime),

	% Ignore the time part
	!:Date = date(DateTime ^ year, DateTime ^ month, DateTime ^ day_of_month,
	    DateTime ^ maybe_zoned)
    ),

    (day_time_between(Date1, Date2, Duration) :- 
	date_to_date(Date1, DateTime1),
	date_to_date(Date2, DateTime2),
	day_time_between_date_times(DateTime1, DateTime2, Duration)
    ),

    (current_time(Date, !IO) :-
        time.time(TimeT, !IO),
        TM = time.localtime(TimeT),
	local_time_zone(TimeZone, !IO),
	Date = date(1900 + TM ^ tm_year, 1 + TM ^ tm_mon, TM ^ tm_mday, yes(TimeZone))
    )
].

:- instance any_type(date_time) where [
    pred(equal/2) is date_or_time_equal,
    ( is_valid(_) :- semidet_succeed ), % It's impossible to construct an invalid date_time.
    ( default_value = det_init_date_time(1970, 1, 1, 0, 0, 0.0, no) ),
    pred(from_string/2) is date_time_from_xsd_string,
    func(to_string/1) is date_time_to_xsd_string,
    ( mercury_name(_) = "date_time.date_time" ),
    ( mercury_modules(_) = ["date_time"] ),
    ( mercury_term(D) = "det_date_time_from_xsd_string(""" ++ 
        date_time_to_xsd_string(D) ++ """)" )
].
:- instance atomic_type(date_time) where [
    atomic_type(T) = builtin_type(builtin_type(T))
].
:- instance builtin_type(date_time) where [
    builtin_type(T) = builtin_primitive_type(primitive_type(T))
].
:- instance builtin_primitive_type(date_time) where [
    primitive_type(_) = xsd_dateTime
].
:- instance partially_ordered(date_time) where [
    pred(leq/2) is date_time_leq
].
:- instance date_or_time(date_time) where [
    (maybe_zoned(T) = T ^ dt_maybe_zoned),
    (set_maybe_zoned(MaybeZoned, !T) :- !T ^ dt_maybe_zoned := MaybeZoned),
    pred(add_duration/3) is add_duration_to_date_time,
    pred(day_time_between/3) is day_time_between_date_times,
    pred(current_time/3) is current_zoned_time
].

:- instance date(date) where [
    (year(T) = T ^ d_year),
    (month(T) = T ^ d_month),
    (day_of_month(T) = T ^ d_day),
    (init_date(Year, Month, Day, MaybeZoned, Date) :-
        Year \= 0,
        Day >= 1,
	Month >= 1,
	Month =< 12,
        Day =< max_day_in_month_for(Year, Month),
        Date = date(Year,Month,Day,MaybeZoned)
    )
].

:- instance date(date_time) where [
    (year(T) = T ^ dt_year),
    (month(T) = T ^ dt_month),
    (day_of_month(T) = T ^ dt_day),
    (init_date(Year, Month, Day, MaybeZoned, Date) :-
        init_date_time(Year,Month,Day,0,0,0.0,MaybeZoned,Date)
    )
].

:- instance time(time) where [
    (hour(T) = T ^ t_hour),
    (minute(T) = T ^ t_minute),
    (second(T) = T ^ t_second),
    (init_time(Hour, Minute, Second, MaybeZoned, time(Hour, Minute, Second, MaybeZoned)) :-
        Hour < 24,
        Minute < 60,
        Second < 60.0
    )
].

:- instance time(date_time) where [
    (hour(T) = T ^ dt_hour),
    (minute(T) = T ^ dt_minute),
    (second(T) = T ^ dt_second),
    (init_time(Hour, Minute, Second, MaybeZoned, DateTime) :-
        init_date_time(1970, 1, 1, Hour, Minute, Second, MaybeZoned, DateTime)
    )
].

:- instance date_time(date_time) where [].

%-----------------------------------------------------------------------------%

:- instance any_type(year_month) where [
    pred(equal/2) is date_or_time_equal,

    (is_valid(_) :- semidet_succeed), % It's impossible to construct an invalid year_month.

    default_value = year_month(1970, 1, no),

    ( from_string(XSDStr, YM) :-
        some [!Chars] (
            !:Chars = string.to_char_list(XSDStr),
            ( read_char((-), !.Chars, Rest1) ->
                !:Chars = Rest1,
                read_int_and_num_chars(Year0, YearChars, !Chars),
                Year = -Year0
            ;
                read_int_and_num_chars(Year, YearChars, !Chars)
            ),
            YearChars >= 4,
            Year \= 0,
            read_char((-), !Chars),
            read_int_and_num_chars(Month, 2, !Chars),
            Month >= 1,
            Month =< 12,
            ( read_time_zone(TZ, !.Chars, Rest2) ->
                MaybeTZ = yes(TZ),
                !:Chars = Rest2
            ;
                MaybeTZ = no
            ),
            !.Chars = [],
            YM = year_month(Year, Month, MaybeTZ)
        )
    ),

    ( to_string(year_month(Year0, Month, MaybeZoned)) = XSDStr :-
        ( Year0 < 0 ->
            SignStr = "-",
            Year = -Year0
        ;
            SignStr = "",
            Year = Year0
        ),
	TZStr = maybe_zoned_str(MaybeZoned),
        XSDStr = string.format("%s%04d-%02d%s",
            [s(SignStr), i(Year), i(Month), s(TZStr)])
    ),

    mercury_name(_) = "date_time.year_month",
    mercury_modules(_) = ["date_time"],
    mercury_term(D) = string.format("date_time.year_month(%i,%s,%s)",
	[i(D ^ year),s(string(D ^ month)),s(string(D ^ maybe_zoned))])
].
:- instance atomic_type(year_month) where [
    atomic_type(T) = builtin_type(builtin_type(T))
].
:- instance builtin_type(year_month) where [
    builtin_type(T) = builtin_primitive_type(primitive_type(T))
].
:- instance builtin_primitive_type(year_month) where [
    primitive_type(_) = xsd_gYearMonth
].
:- instance partially_ordered(year_month) where [
    (leq(year_month(Year1, Month1, _), year_month(Year2, Month2, _)) :-
        % XXX we ignore the zones!
        ( Year1 < Year2
        ; Year1 = Year2,
            Month1 < Month2
        )
    )
].

:- instance date_or_time(year_month) where [
    (maybe_zoned(T) = T ^ ym_maybe_zoned),
    (set_maybe_zoned(MaybeZoned, !T) :- !T ^ ym_maybe_zoned := MaybeZoned),
    (add_duration(Duration, !Date) :-
	% Initialize a date_time from year_month (setting hours, minutes and seconds to zero)
        DateTime0 = det_init_date(!.Date ^ ym_year, !.Date ^ ym_month, 1,
	                      !.Date ^ ym_maybe_zoned),
	% Add the duration
        add_duration_to_date_time(Duration, DateTime0, DateTime),

	% Ignore the time part and day.
	!:Date = year_month(DateTime ^ year, DateTime ^ month, DateTime ^ maybe_zoned)
    ),

    (day_time_between(Date1, Date2, Duration) :-
         Duration = duration( Date2 ^ ym_year - Date1 ^ ym_year,
                Date2 ^ ym_month - Date1 ^ ym_month, 0, 0, 0, 0.0)
         % we ignore the time zone.
    ),

    (current_time(_Date, !IO) :-
        error("current_time year_month NYI")
    )
].

:- instance date(year_month) where [
    (year(T) = T ^ ym_year),
    (month(T) = T ^ ym_month),
    (day_of_month(_) = 1),
    (init_date(Year, Month, _, MaybeZoned, year_month(Year, Month, MaybeZoned)))
].

%-----------------------------------------------------------------------------%
% Parsing.
%

date_time_from_xsd_string(XSDStr, Date) :-
    some [!Chars] (
        !:Chars = string.to_char_list(XSDStr),
        ( read_char((-), !.Chars, Rest1) ->
            !:Chars = Rest1,
            read_int_and_num_chars(Year0, YearChars, !Chars),
            Year = -Year0
        ;
            read_int_and_num_chars(Year, YearChars, !Chars)
        ),
        YearChars >= 4,
        Year \= 0,
        read_char((-), !Chars),
        read_int_and_num_chars(Month, 2, !Chars),
        Month >= 1,
        Month =< 12,
        read_char((-), !Chars),
        read_int_and_num_chars(Day, 2, !Chars),
        Day >= 1,
        Day =< max_day_in_month_for(Year, Month),
        read_char('T', !Chars),
        read_int_and_num_chars(Hour, 2, !Chars),
        Hour >= 0,
        Hour =< 24, % 24 is allowed in certain cases, see below.
        read_char((:), !Chars),
        read_int_and_num_chars(Minute, 2, !Chars),
        Minute >= 0,
        Minute =< 59,
        read_char((:), !Chars),
        % Make sure there are two digits for the whole number part of the
        % second field.
        read_int_and_num_chars(_, 2, !.Chars, _),
        read_float(Second, !Chars),
        Second < 60.0,
        ( read_time_zone(TZ, !.Chars, Rest2) ->
            MaybeTZ = yes(TZ),
            !:Chars = Rest2
        ;
            MaybeTZ = no
        ),
        !.Chars = [],
        ( Hour = 24 ->
            %
            % If the hour is 24, then the minutes and seconds must be zero
            % and the date represented is the start of the next day.
            %
            Minute = 0,
            Second = 0.0,
            OneDay = init_positive_duration(0, 0, 1, 0, 0, 0.0),
            LocalDate0 = date_time(Year, Month, Day, 0, 0, 0.0, MaybeTZ),
            add_duration_to_date_time(OneDay, LocalDate0, Date)
        ;
            Date = date_time(Year, Month, Day, Hour, Minute, Second, MaybeTZ)
        )
    ).
 
:- pred read_time_zone(time_zone::out, list(char)::in, list(char)::out)
    is semidet.

read_time_zone(TZ, !Chars) :-
    !.Chars = [First | !:Chars],
    (
        First = 'Z',
        TZ = 0
    ;
        ( First = (-),
            TZSign = negative
        ; First = (+),
            TZSign = positive
        ),
        read_int_and_num_chars(TZHours, 2, !Chars),
        read_char((:), !Chars),
        read_int_and_num_chars(TZMinutes, 2, !Chars),
        ( TZSign = negative,
            init_negative_time_zone(TZHours, TZMinutes, TZ)
        ; TZSign = positive,
            init_positive_time_zone(TZHours, TZMinutes, TZ)
        )
    ).

:- pred read_int_and_num_chars(int::out, int::out,
    list(char)::in, list(char)::out) is det.

read_int_and_num_chars(Val, N, !Chars) :-
    read_int_and_num_chars_2(0, Val, 0, N, !Chars).

:- pred read_int_and_num_chars_2(int::in, int::out, int::in, int::out,
    list(char)::in, list(char)::out) is det.

read_int_and_num_chars_2(!Val, !N, !Chars) :-
    (
        !.Chars = [Char | Rest],
        is_digit(Char, Digit)
    ->
        !:Val = !.Val * 10 + Digit,
        read_int_and_num_chars_2(!Val, !.N + 1, !:N, Rest, !:Chars)
    ;
        true
    ).

duration_from_xsd_string(XSDStr, Duration) :-
    some [!Chars] (
        !:Chars = string.to_char_list(XSDStr),
        read_sign(Sign, !Chars),
        read_char('P', !Chars),
        read_years(Years, !Chars),
        read_months(Months, !Chars),
        read_days(Days, !Chars),
        ( read_char('T', !.Chars, TimePart) ->
            TimePart = [_ | _],
            read_hours(Hours, TimePart, !:Chars),
            read_minutes(Minutes, !Chars),
            read_seconds(Seconds, !Chars),
            !.Chars = [],
            Duration = make_duration_with_sign(Sign, Years, Months, Days,
                Hours, Minutes, Seconds)
        ;
            !.Chars = [],
            Duration = make_duration_with_sign(Sign, Years, Months, Days,
                0, 0, 0.0)
        )
    ).


:- type sign
    --->    positive
    ;       negative.

:- pred read_sign(sign::out, list(char)::in, list(char)::out) is det.

read_sign(Sign, !Chars) :-
    ( !.Chars = [(-) | Rest] ->
        !:Chars = Rest,
        Sign = negative
    ;
        Sign = positive
    ).

:- pred read_char(char::out, list(char)::in, list(char)::out) is semidet.

read_char(Char, [Char | Rest], Rest).

:- pred read_years(int::out, list(char)::in, list(char)::out) is det.

read_years(Years, !Chars) :-
    read_int_and_char_or_zero(Years, 'Y', !Chars).

:- pred read_months(int::out, list(char)::in, list(char)::out) is det.

read_months(Months, !Chars) :-
    read_int_and_char_or_zero(Months, 'M', !Chars).

:- pred read_days(int::out, list(char)::in, list(char)::out) is det.

read_days(Days, !Chars) :-
    read_int_and_char_or_zero(Days, 'D', !Chars).

:- pred read_hours(int::out, list(char)::in, list(char)::out) is det.

read_hours(Hours, !Chars) :-
    read_int_and_char_or_zero(Hours, 'H', !Chars).

:- pred read_minutes(int::out, list(char)::in, list(char)::out) is det.

read_minutes(Minutes, !Chars) :-
    read_int_and_char_or_zero(Minutes, 'M', !Chars).

:- pred read_seconds(float::out, list(char)::in, list(char)::out) is det.

read_seconds(Seconds, !Chars) :-
    read_float_and_char_or_zero(Seconds, 'S', !Chars).

:- pred read_int_and_char_or_zero(int::out, char::in,
    list(char)::in, list(char)::out) is det.

read_int_and_char_or_zero(Int, Char, !Chars) :-
    (
        read_int(Int0, !.Chars, Chars1),
        Chars1 = [Char | Rest]
    ->
        !:Chars = Rest,
        Int = Int0
    ;
        Int = 0
    ).

:- pred read_float_and_char_or_zero(float::out, char::in,
    list(char)::in, list(char)::out) is det.

read_float_and_char_or_zero(Float, Char, !Chars) :-
    (
        read_float(Float0, !.Chars, Chars1),
        Chars1 = [Char | Rest]
    ->
        !:Chars = Rest,
        Float = Float0
    ;
        Float = 0.0
    ).

:- pred read_int(int::out, list(char)::in, list(char)::out) is det.

read_int(Val, !Chars) :-
    read_int_2(0, Val, !Chars).

:- pred read_int_2(int::in, int::out, list(char)::in, list(char)::out) is det.

read_int_2(!Val, !Chars) :-
    (
        !.Chars = [Char | Rest],
        is_digit(Char, Digit)
    ->
        !:Val = !.Val * 10 + Digit,
        read_int_2(!Val, Rest, !:Chars)
    ;
        true
    ).

:- pred read_float(float::out, list(char)::in, list(char)::out) is semidet.

read_float(Val, !Chars) :-
    list.head(!.Chars) \= (.),
    read_int(WholePart, !Chars),
    ( !.Chars = [(.) | Rest] ->
        is_digit(list.head(Rest), _),
        read_fraction(Fraction, Rest, !:Chars),
        Val = float(WholePart) + Fraction
    ;
        Val = float(WholePart)
    ).

:- pred read_fraction(float::out, list(char)::in, list(char)::out) is det.

read_fraction(Frac, !Chars) :-
    read_fraction_2(0.1, 0.0, Frac, !Chars).

:- pred read_fraction_2(float::in, float::in, float::out,
    list(char)::in, list(char)::out) is det.

read_fraction_2(DigitVal, !Val, !Chars) :-
    (
        !.Chars = [Char | Rest],
        is_digit(Char, Digit)
    ->
        !:Val = !.Val + DigitVal * float(Digit),
        read_fraction_2(DigitVal / 10.0, !Val, Rest, !:Chars)
    ;
        true
    ).

:- pred is_digit(char::in, int::out) is semidet.

is_digit('0', 0).
is_digit('1', 1).
is_digit('2', 2).
is_digit('3', 3).
is_digit('4', 4).
is_digit('5', 5).
is_digit('6', 6).
is_digit('7', 7).
is_digit('8', 8).
is_digit('9', 9).

:- func make_duration_with_sign(sign, int, int, int, int, int, float)
    = duration.

make_duration_with_sign(positive, Years, Months, Days, Hours, Minutes,
        Seconds) =
    init_positive_duration(Years, Months, Days, Hours, Minutes, Seconds).
make_duration_with_sign(negative, Years, Months, Days, Hours, Minutes,
        Seconds) =
    init_negative_duration(Years, Months, Days, Hours, Minutes, Seconds).

%-----------------------------------------------------------------------------%
% Serialization.
%

date_time_to_xsd_string(Date) = XSDStr :-
    Date = date_time(Year0, Month, Day, Hour, Minute, Second, MaybeZoned),
    ( Year0 < 0 ->
        SignStr = "-",
        Year = -Year0
    ;
        SignStr = "",
        Year = Year0
    ),
    WholeSecond = truncate_to_int(Second),
    SecondFraction = Second - float(WholeSecond),
    ( SecondFraction = 0.0 ->
        SecondFractionStr = ""
    ;
        SecondFractionStr = lstrip_pred(unify('0'),
            float_to_string(SecondFraction))
    ),
    TZStr = maybe_zoned_str(MaybeZoned),
    XSDStr = string.format("%s%04d-%02d-%02dT%02d:%02d:%02d%s%s",
        [s(SignStr), i(Year), i(Month), i(Day), i(Hour), i(Minute),
         i(WholeSecond), s(SecondFractionStr), s(TZStr)]).


date_time_to_date_BEformat(Date) = XSDStr :-
    Date = date_time(Year0, Month, Day, _Hour, _Minute, _Second, _MaybeZoned),
    ( Year0 < 0 ->
        SignStr = "-",
        Year = -Year0
    ;
        SignStr = "",
        Year = Year0
    ),
    XSDStr = string.format("%02d/%02d/%s%04d",
        [i(Day), i(Month), s(SignStr), i(Year)]).


:- func maybe_zoned_str(maybe_zoned) = string.
maybe_zoned_str(yes(TimeZone)) = Str :-
    (TimeZone = 0 ->
	Str = "Z"
    ;
	Str = format("%+03d:%02d", [i(Hours), i(Minutes)]),
        Minutes = abs(TimeZone rem 60),
        Hours = TimeZone / 60  % Rounds towards zero
    ).

maybe_zoned_str(no) = "".

duration_to_xsd_string(duration(Years, Months, Days, Hours, Minutes, Seconds) @ Dur)
        = Str :-
    (
        Years = 0,
        Months = 0,
        Days = 0,
        Hours = 0,
        Minutes = 0,
        Seconds = 0.0
    ->
        % At least one dimension must appear in the string.  The choice
        % of days is arbitrary.
        Str = "P0D"
    ;
        (
            Years >= 0,
            Months >= 0,
            Days >= 0,
            Hours >= 0,
            Minutes >= 0,
            Seconds >= 0.0
        ->
            Sign = 1,
            SignStr = ""
        ;
            Years =< 0,
            Months =< 0,
            Days =< 0,
            Hours =< 0,
            Minutes =< 0,
            Seconds =< 0.0
        ->
            Sign = -1,
            SignStr = "-"
        ;
            error("duration_to_xsd_string: " ++
                "duration components have mixed signs " ++ string(Dur))
        ),
        (
            Hours = 0,
            Minutes = 0,
            Seconds = 0.0
        ->
            TimePart = []
        ;
            TimePart = ["T",
                string_if_nonzero(Sign * Hours, "H"),
                string_if_nonzero(Sign * Minutes, "M"),
                float_string_if_nonzero(float(Sign) * Seconds, "S")
            ]
        ),
        Str = string.append_list([
            SignStr, "P",
            string_if_nonzero(Sign * Years, "Y"),
            string_if_nonzero(Sign * Months, "M"),
            string_if_nonzero(Sign * Days, "D")] ++ TimePart)
    ).

:- func string_if_nonzero(int, string) = string.

string_if_nonzero(X, Suffix) =
    ( X = 0 ->
        ""
    ;
        int_to_string(X) ++ Suffix
    ).

:- func float_string_if_nonzero(float, string) = string.

float_string_if_nonzero(X, Suffix) = Str :-
    ( X = 0.0 ->
        Str = ""
    ;
        Str0 = float_to_string(X),
        ( string.remove_suffix(Str0, ".0", Str1) ->
            Str = Str1 ++ Suffix
        ;
            Str = Str0 ++ Suffix
        )
    ).

%-----------------------------------------------------------------------------%

normalize_duration(!.Duration) = !:Duration :-
    Sec = !.Duration ^ dur_seconds,
    ( abs(Sec) >= 60.0 ->
        SecTrunc = truncate_to_int(Sec / 60.0),
	Min = !.Duration ^ dur_minutes + SecTrunc,
	!Duration ^ dur_seconds := Sec - 60.0 * float(SecTrunc),
	!Duration ^ dur_minutes := Min
    ;
	Min = !.Duration ^ dur_minutes
    ),
    ( abs(Min) >= 60 ->
        !Duration ^ dur_minutes := int.rem(Min, 60),
	Hours = !.Duration ^ dur_hours + (Min // 60),
	!Duration ^ dur_hours := Hours
    ;
	Hours = !.Duration ^ dur_hours
    ),
    ( abs(Hours) >= 24 ->
	!Duration ^ dur_hours := int.rem(Hours, 24),
	Days = !.Duration ^ dur_days + (Hours // 24),
	!Duration ^ dur_days := Days
    ;
	true
    ),
    Months = !.Duration ^ dur_months,
    ( abs(Months) >= 12 ->
	!Duration ^ dur_months := int.rem(Months, 12),
	Years = !.Duration ^ dur_years + (Months // 12),
	!Duration ^ dur_years := Years
    ;
	true
    ).

%-----------------------------------------------------------------------------%

add_durations(D, !.Duration) = !:Duration :-
    !Duration ^ dur_seconds := !.Duration ^ dur_seconds + D ^ dur_seconds,
    !Duration ^ dur_minutes := !.Duration ^ dur_minutes + D ^ dur_minutes,
    !Duration ^ dur_hours   := !.Duration ^ dur_hours +   D ^ dur_hours,
    !Duration ^ dur_hours   := !.Duration ^ dur_hours +   D ^ dur_hours,
    !Duration ^ dur_days    := !.Duration ^ dur_days +    D ^ dur_days,
    !Duration ^ dur_months  := !.Duration ^ dur_months +  D ^ dur_months,
    !Duration ^ dur_years   := !.Duration ^ dur_years +   D ^ dur_years,
    !:Duration = normalize_duration(!.Duration).

%-----------------------------------------------------------------------------%

is_day_time_duration(Duration) :-
    Duration ^ dur_years = 0,
    Duration ^ dur_months = 0.

is_year_month_duration(Duration) :-
    Duration ^ dur_days = 0,
    Duration ^ dur_hours = 0,
    Duration ^ dur_minutes = 0,
    Duration ^ dur_seconds = 0.0.

is_zero_duration(Duration) :-
    Duration ^ dur_years = 0,
    Duration ^ dur_months = 0,
    Duration ^ dur_days = 0,
    Duration ^ dur_hours = 0,
    Duration ^ dur_minutes = 0,
    Duration ^ dur_seconds = 0.0.

%-----------------------------------------------------------------------------%
% Partial order.
%

date_time_leq(!.Date1, !.Date2) :-
    MaybeZoned1 = maybe_zoned(!.Date1),
    MaybeZoned2 = maybe_zoned(!.Date2),

    ( MaybeZoned1 = yes(Zone1),
	apply_time_zone(Zone1, !Date1),
        ( MaybeZoned2 = yes(Zone2),
	    % Both dates are zoned
	    apply_time_zone(Zone2, !Date2),
	    date_time_compare_ignore_zonedness(!.Date1, !.Date2, Result),
	    ( Result = (<) ; Result = (=) )

        ; MaybeZoned2 = no,
	    % Date1 is zoned but Date2 is not -
	    % assume Date2 is in the latest time zone (+14:00) which
	    % will produce the earliest date in UTC.
	    apply_time_zone(plus_forteen_hours, !Date2),
	    date_time_compare_ignore_zonedness(!.Date1, !.Date2, (<))
        )

    ; MaybeZoned1 = no,
        ( MaybeZoned2 = yes(Zone2),
	    % Date2 is zoned but Date1 is not -
	    % assume Date1 is in the earlist time zone (-14:00) which
            % will produce the latest date in UTC.
	    apply_time_zone(minus_forteen_hours, !Date1),
	    apply_time_zone(Zone2, !Date2),
	    date_time_compare_ignore_zonedness(!.Date1, !.Date2, (<))

        ; MaybeZoned2 = no,
	    % Both dates are unzoned
	    date_time_compare_ignore_zonedness(!.Date1, !.Date2, Result),
	    ( Result = (<) ; Result = (=) )
        )
    ).



:- func plus_forteen_hours = time_zone.
plus_forteen_hours = 14*60.

:- func minus_forteen_hours = time_zone.
minus_forteen_hours = -14*60.

:- pred date_time_compare_ignore_zonedness(date_time::in, date_time::in,
    builtin.comparison_result::out) is det.

date_time_compare_ignore_zonedness(Date1, Date2, Result) :-
    Date1 = date_time(Year1, Month1, Day1, Hour1, Minute1, Second1, _),
    Date2 = date_time(Year2, Month2, Day2, Hour2, Minute2, Second2, _),
    ( Year1 < Year2 ->
        Result = (<)
    ; Year1 > Year2 ->
        Result = (>)
    ; Month1 < Month2 ->
        Result = (<)
    ; Month1 > Month2 ->
        Result = (>)
    ; Day1 < Day2 ->
        Result = (<)
    ; Day1 > Day2 ->
        Result = (>)
    ; Hour1 < Hour2 ->
        Result = (<)
    ; Hour1 > Hour2 ->
        Result = (>)
    ; Minute1 < Minute2 ->
        Result = (<)
    ; Minute1 > Minute2 ->
        Result = (>)
    ; Second1 < Second2 ->
        Result = (<)
    ; Second1 > Second2 ->
        Result = (>)
    ;
        Result = (=)
    ).

duration_leq(Dur1, Dur2) :-
    (
        is_normalized_day_time_duration(Dur1),
        is_normalized_day_time_duration(Dur2)
    ->
        % Optimise for the case where both durations are
        % normalized day-time durations.
        ( days(Dur1) < days(Dur2) ->
            true
        ; days(Dur1) > days(Dur2) ->
            fail
        ; hours(Dur1) < hours(Dur2) ->
            true
        ; hours(Dur1) > hours(Dur2) ->
            fail
        ; minutes(Dur1) < minutes(Dur2) ->
            true
        ; minutes(Dur1) > minutes(Dur2) ->
            fail
        ;
            seconds(Dur1) < seconds(Dur2)
        )
    ;
        list.all_true(
            ( pred(TestDate::in) is semidet :-
                add_duration_to_date_time(Dur1, TestDate, Date1),
                add_duration_to_date_time(Dur2, TestDate, Date2),
                date_time_leq(Date1, Date2)
            ), test_dates)
    ).

:- pred is_normalized_day_time_duration(duration::in) is semidet.

is_normalized_day_time_duration(Duration) :-
    years(Duration) = 0,
    months(Duration) = 0,
    Hours = hours(Duration),
    Hours < 24, Hours > -24,
    Minutes = minutes(Duration),
    Minutes < 60, Minutes > -60,
    Seconds = seconds(Duration),
    Seconds < 60.0, Seconds > -60.0.

date_or_time_equal(D1, D2) :-
    day_time_between(D1, D2, Dur),
    is_zero_duration(Dur).

    % Returns dates used to compare durations.
    %
:- func test_dates = list(date_time).

    % We memo test_dates to avoid reallocating cons cells everytime it is
    % called.
    %
:- pragma memo(test_dates/0).

test_dates = [
    date_time(1696, 9, 1, 0, 0, 0.0, yes(utc_time_zone)),
    date_time(1697, 2, 1, 0, 0, 0.0, yes(utc_time_zone)),
    date_time(1903, 3, 1, 0, 0, 0.0, yes(utc_time_zone)),
    date_time(1903, 7, 1, 0, 0, 0.0, yes(utc_time_zone))
].

utc_time_zone = 0.

%-----------------------------------------------------------------------------%
% Total order.
%

compare_date_times_using_zone(Date1, Date2, LocalTZ, Result) :-
    MaybeZoned1 = maybe_zoned(Date1),
    ( MaybeZoned1 = yes(Zone1)
    ; MaybeZoned1 = no,
	Zone1 = LocalTZ
    ),
    MaybeZoned2 = maybe_zoned(Date2),
    ( MaybeZoned2 = yes(Zone2)
    ; MaybeZoned2 = no,
	Zone2 = LocalTZ
    ),
    apply_time_zone(Zone1, Date1, ZonedDate1),
    apply_time_zone(Zone2, Date2, ZonedDate2),
    date_time_compare_ignore_zonedness(ZonedDate1, ZonedDate2, Result).

%-----------------------------------------------------------------------------%
% Adding durations to date times.
%
% The following is a fairly direct translation of the algorithm at 
% http://www.w3.org/TR/xmlschema-2/#adding-durations-to-dateTimes.
% 

:- func fquotient(float, float) = int.

fquotient(A, B) = float.floor_to_int(A / B).

:- func fquotient(float, float, float) = int.

fquotient(A, Low, High) = fquotient(A - Low, High - Low).

:- func ifquotient(int, int, int) = int.

ifquotient(A, Low, High) = int.div(A - Low, High - Low).

:- func modulo(float, float) = float.

modulo(A, B) = A - float(fquotient(A, B)) * B.

:- func modulo(float, float, float) = float.

modulo(A, Low, High) = modulo(A - Low, High - Low) + Low.

:- func imodulo(int, int, int) = int.

imodulo(A, Low, High) = int.mod(A - Low, High - Low) + Low.

:- func max_day_in_month_for(int, int) = int.

max_day_in_month_for(YearValue, MonthValue) = Max :-
    M = int.mod(MonthValue - 1, 12) + 1,
    Y = YearValue + int.div(MonthValue - 1, 12),
    ( 
        ( ( M = 1 ; M = 3 ; M = 5 ; M = 7 ; M = 8 ; M = 10 ; M = 12 ),
            Max0 = 31
        ; ( M = 4 ; M = 6 ; M = 9 ; M = 11 ),
            Max0 = 30
        ; M = 2,
            ( ( Y mod 400 = 0 ; ( Y mod 100 \= 0, Y mod 4 = 0 ) ) ->
                Max0 = 29
            ;
                Max0 = 28
            )
        )
    ->
        Max = Max0
    ;
        % This should never happen.
        error("max_day_in_month_for: unexpected value for M: " ++
            string(M))
    ).

add_duration_to_date_time(D, S, !:E) :-
    some [!Temp, !Carry] (
        !:E = date_time(0, 0, 0, 0, 0, 0.0, no),
        % Months
        !:Temp = S ^ dt_month + D ^ dur_months,
        !E ^ dt_month := imodulo(!.Temp, 1, 13),
        !:Carry = ifquotient(!.Temp, 1, 13),
        % Years
        !E ^ dt_year := S ^ dt_year + D ^ dur_years + !.Carry,
        % Zone
        !E ^ dt_maybe_zoned := S ^ dt_maybe_zoned,
        % Seconds
        !:Temp = S ^ dt_second + D ^ dur_seconds,
        !E ^ dt_second := modulo(!.Temp, 60.0),
        !:Carry = fquotient(!.Temp, 60.0),
        % Minutes
        !:Temp = S ^ dt_minute + D ^ dur_minutes + !.Carry,
        !E ^ dt_minute := int.mod(!.Temp, 60),
        !:Carry = int.div(!.Temp, 60),
        % Hours
        !:Temp = S ^ dt_hour + D ^ dur_hours + !.Carry,
        !E ^ dt_hour := int.mod(!.Temp, 24),
        !:Carry = int.div(!.Temp, 24),
        % Days
        MaxDaysInMonth = max_day_in_month_for(!.E ^ dt_year, !.E ^ dt_month),
        ( S ^ dt_day > MaxDaysInMonth ->
            TempDays = MaxDaysInMonth
        ; S ^ dt_day < 1 ->
            TempDays = 1
        ;
            TempDays = S ^ dt_day
        ),
        !E ^ dt_day := TempDays + D ^ dur_days + !.Carry,
        add_duration_loop(D, S, !E)
    ).

:- pred add_duration_loop(duration::in, date_time::in,
    date_time::in, date_time::out) is det.

add_duration_loop(D, S, !E) :-
    ( !.E ^ dt_day < 1 ->
        !E ^ dt_day := !.E ^ dt_day +
            max_day_in_month_for(!.E ^ dt_year, !.E ^ dt_month - 1),
        Carry = -1,
        Temp = !.E ^ dt_month + Carry,
        !E ^ dt_month := imodulo(Temp, 1, 13),
        !E ^ dt_year := !.E ^ dt_year + ifquotient(Temp, 1, 13),
        add_duration_loop(D, S, !E)
    ; 
        MaxDaysInMonth = max_day_in_month_for(!.E ^ dt_year, !.E ^ dt_month),
        !.E ^ dt_day > MaxDaysInMonth
    ->
        !E ^ dt_day := !.E ^ dt_day - MaxDaysInMonth,
        Carry = 1,
        Temp = !.E ^ dt_month + Carry,
        !E ^ dt_month := imodulo(Temp, 1, 13),
        !E ^ dt_year := !.E ^ dt_year + ifquotient(Temp, 1, 13),
        add_duration_loop(D, S, !E)
    ;
        true
    ).

%-----------------------------------------------------------------------------%
% Computing duration between dates.
%

:- pred day_time_between_date_times(date_time::in, date_time::in, duration::out) is semidet.
day_time_between_date_times(Date1, Date2, Duration) :-
    ( maybe_zoned(Date1) = no, maybe_zoned(Date2) = no ->
        day_time_subtract_ignoring_zonedness(Date2, Date1, Duration)

    ; maybe_zoned(Date1) = yes(_Zone1), maybe_zoned(Date2) = yes(_Zone2) ->
        day_time_subtract_ignoring_zonedness(to_utc_zone(Date2), to_utc_zone(Date1), Duration)
    ;
	    fail
    ).

:- pred day_time_subtract_ignoring_zonedness(date_time::in, date_time::in,
    duration::out) is det.

day_time_subtract_ignoring_zonedness(Date1, Date2, Duration) :-
    compare_date_times_using_zone(Date1, Date2, utc_time_zone, CompResult),
    ( CompResult = (<),
        day_time_subtract_ignoring_zonedness(Date2, Date1, Duration0),
        Duration = negate(Duration0)
    ; CompResult = (=),
        Duration = zero_duration
    ; CompResult = (>),
        some [!Borrow] (
            Second1 = Date1 ^ dt_second,
            Second2 = Date2 ^ dt_second,
            subtract_floats_with_borrow(60.0, Second1, Second2, Seconds,
                !:Borrow),
            Minute1 = Date1 ^ dt_minute - !.Borrow,
            Minute2 = Date2 ^ dt_minute,
            subtract_ints_with_borrow(60, Minute1, Minute2, Minutes,
                !:Borrow),
            Hour1 = Date1 ^ dt_hour - !.Borrow,
            Hour2 = Date2 ^ dt_hour,
            subtract_ints_with_borrow(24, Hour1, Hour2, Hours, !:Borrow),
            JDN1 = julian_day(Date1 ^ dt_year, Date1 ^ dt_month,
                Date1 ^ dt_day),
            JDN2 = julian_day(Date2 ^ dt_year, Date2 ^ dt_month,
                Date2 ^ dt_day),
            Days = JDN1 - !.Borrow - JDN2,
            Duration = duration(0, 0, Days, Hours, Minutes, Seconds)
        )
    ).

day_time_between_using_zone(Date1, Date2, LocalTZ, Duration) :-
    ZonedDate1 = zone_if_unzoned(Date1, LocalTZ),
    ZonedDate2 = zone_if_unzoned(Date2, LocalTZ),
    day_time_subtract_ignoring_zonedness(ZonedDate2, ZonedDate1, Duration).

greedy_duration_between(Date1, Date2, Duration) :-
    ( maybe_zoned(Date1) = no, maybe_zoned(Date2) = no ->
	greedy_subtract_ignoring_zonedness(Date2, Date1, Duration)

    ; maybe_zoned(Date1) = yes(Zone1), maybe_zoned(Date2) = yes(Zone2) ->
	apply_time_zone(Zone1, Date1, Date1a),
	apply_time_zone(Zone2, Date2, Date2a),
	greedy_subtract_ignoring_zonedness(Date2a, Date1a, Duration)
    ;
	fail
    ).

greedy_duration_between_using_zone(Date1, Date2, LocalTZ, Duration) :-
    ZonedDate1 = zone_if_unzoned(Date1, LocalTZ),
    ZonedDate2 = zone_if_unzoned(Date2, LocalTZ),
    greedy_subtract_ignoring_zonedness(ZonedDate2, ZonedDate1, Duration).

:- pred greedy_subtract_ignoring_zonedness(date_time::in, date_time::in,
    duration::out) is det.

greedy_subtract_ignoring_zonedness(Date1, Date2, Duration) :-
    compare_date_times_using_zone(Date1, Date2, utc_time_zone, CompResult),
    ( CompResult = (<),
        greedy_subtract_descending_ignoring_zonedness(ascending, Date2, Date1, Duration0),
        Duration = negate(Duration0)
    ; CompResult = (=),
        Duration = zero_duration
    ; CompResult = (>),
        greedy_subtract_descending_ignoring_zonedness(descending, Date1, Date2, Duration)
    ).

:- type order
    --->    ascending
    ;       descending.

:- pred greedy_subtract_descending_ignoring_zonedness(order::in, date_time::in, date_time::in,
    duration::out) is det.

    % This predicate has the precondition that Date1 < Date2.  OriginalOrder is the
    % original order of the date arguments (descending means that in the original call
    % Date1 < Date2, while ascending means that in the original call Date1 > Date2).
    % This is needed to correctly compute the days dimension of the resulting duration.
    % The calculation is different depending on the original order, because we want the
    % invarient:
    %   greedy_duration_between(Date1, Date2, Duration)
    %   add_duration_to_date_time(Duration, Date1, Date2)
    % to hold, and in the case where Date1 > Date2, Duration will be negative.
    %
greedy_subtract_descending_ignoring_zonedness(OriginalOrder, Date1, Date2, Duration) :-
    some [!Borrow] (
        Second1 = Date1 ^ dt_second,
        Second2 = Date2 ^ dt_second,
        subtract_floats_with_borrow(60.0, Second1, Second2, Seconds,
            !:Borrow),
        Minute1 = Date1 ^ dt_minute - !.Borrow,
        Minute2 = Date2 ^ dt_minute,
        subtract_ints_with_borrow(60, Minute1, Minute2, Minutes, !:Borrow),
        Hour1 = Date1 ^ dt_hour - !.Borrow,
        Hour2 = Date2 ^ dt_hour,
        subtract_ints_with_borrow(24, Hour1, Hour2, Hours, !:Borrow),
        ( OriginalOrder = descending,
            add_duration_to_date_time(duration(0, -1, 0, 0, 0, 0.0), Date1, Date1Minus1Month),
            DaysToBorrow = max_day_in_month_for(Date1Minus1Month ^ dt_year,
                Date1Minus1Month ^ dt_month),
            Date1EndOfMonth = max_day_in_month_for(Date1 ^ dt_year, Date1 ^ dt_month),
            Day1 = Date1 ^ dt_day - !.Borrow,
            Day2 = int.min(Date2 ^ dt_day, Date1EndOfMonth)
        ; OriginalOrder = ascending,
            DaysToBorrow = max_day_in_month_for(Date2 ^ dt_year, Date2 ^ dt_month),
            Date2EndOfMonth = max_day_in_month_for(Date2 ^ dt_year, Date2 ^ dt_month),
            Day1 = int.min(Date1 ^ dt_day - !.Borrow, Date2EndOfMonth),
            Day2 = Date2 ^ dt_day
        ),
        subtract_ints_with_borrow(DaysToBorrow, Day1, Day2, Days, !:Borrow),
        Month1 = Date1 ^ dt_month - !.Borrow,
        Month2 = Date2 ^ dt_month,
        subtract_ints_with_borrow(12, Month1, Month2, Months, !:Borrow),
        Year1 = Date1 ^ dt_year - !.Borrow,
        Year2 = Date2 ^ dt_year,
        ( Year1 >= Year2 ->
            Years = Year1 - Year2
        ;
            % If this happens then Date1 < Date2 which violates a precondition
            % of this predicate.
            error("greedy_subtract_descending_ignoring_zonedness: " ++
                "left over years")
        ),
        Duration = duration(Years, Months, Days, Hours, Minutes, Seconds)
    ).

    % subtract_ints_with_borrow(BorrowAmount, Val1, Val2, Val, Borrow)
    % Subtract Val2 from Val1, possibly borrowing BorrowAmount if Val1 < Val2.
    % If an amount is borrowed, then Borrow is set to 1, otherwise it is set
    % to 0.
    %
:- pred subtract_ints_with_borrow(int::in, int::in, int::in, int::out,
    int::out) is det.

subtract_ints_with_borrow(BorrowVal, Val1, Val2, Diff, Borrow) :-
    ( Val1 >= Val2 ->
        Borrow = 0,
        Diff = Val1 - Val2
    ;
        Borrow = 1,
        Diff = BorrowVal + Val1 - Val2
    ).

    % Same as subtract_ints_with_borrow, but for floats.
    %
:- pred subtract_floats_with_borrow(float::in, float::in, float::in,
    float::out, int::out) is det.

subtract_floats_with_borrow(BorrowVal, Val1, Val2, Diff, Borrow) :-
    ( Val1 >= Val2 ->
        Borrow = 0,
        Diff = Val1 - Val2
    ;
        Borrow = 1,
        Diff = BorrowVal + Val1 - Val2
    ).

%-----------------------------------------------------------------------------%
% Misc
%

:- func julian_day(int, int, int) = int.

julian_day(Year, Month, Day) = JDN :-
    %
    % The following calculation comes from
    % http://en.wikipedia.org/wiki/Julian_day.
    %
    A = (14 - Month) // 12,
    Y = Year + 4800 - A,
    M = Month + 12 * A - 3,
    JDN = Day + ( 153 * M + 2 ) // 5 + 365 * Y + Y // 4 - Y // 100 + Y // 400
        - 32045.

years(Dur) = Dur ^ dur_years.
months(Dur) = Dur ^ dur_months.
days(Dur) = Dur ^ dur_days.
hours(Dur) = Dur ^ dur_hours.
minutes(Dur) = Dur ^ dur_minutes.
seconds(Dur) = Dur ^ dur_seconds.

to_utc_zone(!.Date) = !:Date :-
    MaybeZoned = !.Date ^ maybe_zoned,
    ( MaybeZoned = yes(Zone),
	apply_time_zone(Zone, !Date),
	set_maybe_zoned(yes(utc_time_zone), !Date)
    ; MaybeZoned = no
    ).

zone_if_unzoned(!.Date, LocalZone) = !:Date :-
    MaybeZoned = !.Date ^ maybe_zoned,
    ( MaybeZoned = yes(_)
    ; MaybeZoned = no,
	set_maybe_zoned(yes(LocalZone), !Date)
    ).

apply_time_zone(TimeZone, !Date) :-
    add_duration(negate(time_zone_to_duration(TimeZone)), !Date).

det_date_time_from_xsd_string(XSDStr) = Date :-
    ( date_time_from_xsd_string(XSDStr, Date0) ->
        Date = Date0
    ;
        error("det_date_time_from_xsd_string: invalid date-time: " ++
            XSDStr)
    ).

det_duration_from_xsd_string(XSDStr) = Duration :-
    ( duration_from_xsd_string(XSDStr, Duration0) ->
        Duration = Duration0
    ;
        error("det_duration_from_xsd_string: invalid duration: " ++
            XSDStr)
    ).

:- pred is_valid_date_time(date_time::in) is semidet.
is_valid_date_time(date_time(Year, Month, Day, Hour, Minute, Second, _MaybeZoned)) :-
    Year \= 0,
    Month >= 1,
    Month =< 12,
    Day >= 1,
    Day =< max_day_in_month_for(Year, Month),
    Hour < 24,
    Minute < 60,
    Second < 60.0.

init_date_time(Year, Month, Day, Hour, Minute, Second, MaybeZoned, Date) :-
    Date = date_time(Year, Month, Day, Hour, Minute, Second, MaybeZoned),
    is_valid_date_time(Date).

det_init_date_time(Year, Month, Day, Hour, Minute, Second, MaybeZoned) = Date
        :-
    (
        init_date_time(Year, Month, Day, Hour, Minute, Second, MaybeZoned,
            Date0)
    ->
        Date = Date0
    ;
        error(string.format("date_time.det_init_date_time: invalid date: " ++
            "%i-%i-%iT%i:%i:%f", [i(Year), i(Month), i(Day), i(Hour),
            i(Minute), f(Second)]))
    ).

det_init_date(Year, Month, Day, MaybeZoned) = Date :-
    (
        init_date(Year, Month, Day, MaybeZoned, Date0)
    ->
        Date = Date0
    ;
        error(string.format("date_time.det_init_date: invalid date: " ++
              "%i-%i-%i", [i(Year), i(Month), i(Day)]))
    ).

det_init_time(Hour, Minute, Second, MaybeZoned) = Time :-
    (
        init_time(Hour, Minute, Second, MaybeZoned, Time0)
    ->
        Time = Time0
    ;
        error(string.format("date_time.det_init_time: invalid time: " ++
              "%i:%i:%f", [i(Hour), i(Minute), f(Second)]))
    ).

date_to_date(D1, D2) :-
    init_date(D1 ^ year, D1 ^ month, D1 ^ day_of_month, D1 ^ maybe_zoned, D2).

time_to_time(T1, T2) :-
    init_time(T1 ^ hour, T1 ^ minute, T1 ^ second, T1 ^ maybe_zoned, T2).

current_unzoned_local_time(Now, !IO) :-
    time.time(TimeT, !IO),
    TM = time.localtime(TimeT),
    Now = tm_to_date_time(TM, no).

current_zoned_time(Now, !IO) :-
    time.time(TimeT, !IO),
    TM = time.localtime(TimeT),
    local_time_zone(TimeZone, !IO),
    Now = tm_to_date_time(TM, yes(TimeZone)).

current_utc_time(Now, !IO) :-
    time.time(TimeT, !IO),
    TM = time.gmtime(TimeT),
    Now = tm_to_date_time(TM, yes(utc_time_zone)).

:- func tm_to_date_time(time.tm, maybe_zoned) = date_time.

tm_to_date_time(TM, MaybeZoned) = Date :-
    TM = tm(TMYear, TMMonth, TMDay, TMHour, TMMinute, TMSecond, _, _, _),
    Year = 1900 + TMYear,
    Month = TMMonth + 1,
    Day = TMDay,
    Hour = TMHour,
    Minute = TMMinute,
    Second = float(int.min(TMSecond, 59)),
    Date = date_time(Year, Month, Day, Hour, Minute, Second, MaybeZoned).

local_time_zone(TZ, !IO) :-
    time.time(TimeT, !IO),
    LocalTM = time.localtime(TimeT),
    GMTM = time.gmtime(TimeT),
    LocalTime = tm_to_date_time(LocalTM, no),
    GMTime = tm_to_date_time(GMTM, no),
    day_time_subtract_ignoring_zonedness(LocalTime, GMTime, Dur),
    %
    % XXX, we ignore the years, months and seconds components but
    % they should be zero!
    %
    TZ = (Dur ^ dur_days * 24 + Dur ^ dur_hours) * 60 + Dur ^ dur_minutes.

init_positive_duration(Years, Months, Days, Hours, Minutes, Seconds) =
    ( all_non_negative(Years, Months, Days, Hours, Minutes, Seconds) ->
        duration(Years, Months, Days, Hours, Minutes, Seconds)
    ;
        func_error(string.format("init_positive_duration: some dimensions " ++
            "are negative: %iY%iM%iDT%iH%iM%fS", [i(Years), i(Months), i(Days),
            i(Hours), i(Minutes), f(Seconds)]))
    ).

init_negative_duration(Years, Months, Days, Hours, Minutes, Seconds) =
    ( all_non_negative(Years, Months, Days, Hours, Minutes, Seconds) ->
        duration(-Years, -Months, -Days, -Hours, -Minutes, -Seconds)
    ;
        func_error(string.format("init_negative_duration: some dimensions " ++
            "are negative: %iY%iM%iDT%iH%iM%fS", [i(Years), i(Months), i(Days),
            i(Hours), i(Minutes), f(Seconds)]))
    ).

:- pred all_non_negative(years::in, months::in, days::in, hours::in,
    minutes::in, seconds::in) is semidet.

all_non_negative(Years, Months, Days, Hours, Minutes, Seconds) :-
    Years >= 0,
    Months >= 0,
    Days >= 0,
    Hours >= 0,
    Minutes >= 0,
    Seconds >= 0.0.

init_positive_time_zone(Hours, Minutes, TimeZone) :-
    Hours >= 0,
    Minutes >= 0,
    RealMinutes = Hours*60 + Minutes,
    init_time_zone_from_minutes(RealMinutes, TimeZone).

init_negative_time_zone(Hours, Minutes, TimeZone) :-
    Hours >= 0,
    Minutes >= 0,
    RealMinutes = -Hours*60 + Minutes,
    init_time_zone_from_minutes(RealMinutes, TimeZone).

init_time_zone_from_minutes(Minutes, Minutes) :-
    -840 =< Minutes,
    Minutes =< 840.

det_init_positive_time_zone(Hours, Minutes) =
    ( init_positive_time_zone(Hours, Minutes, TimeZone0) ->
        TimeZone0
    ;
        func_error(string.format(
            "init_positive_time_zone: invalid time zone:+%i:%i", [i(Hours),
            i(Minutes)]))
    ).

det_init_negative_time_zone(Hours, Minutes) =
    ( init_negative_time_zone(Hours, Minutes, TimeZone0) ->
        TimeZone0
    ;
        func_error(string.format(
            "init_negative_time_zone: invalid time zone:-%i:%i", [i(Hours),
            i(Minutes)]))
    ).

time_zone_to_duration(Minutes) = !:Duration :-
    !:Duration = zero_duration,

    !Duration ^ dur_minutes := Minutes rem 60,
    Hours = Minutes / 60,

    !Duration ^ dur_hours := Hours rem 24,
    Days = Hours / 24,

    % XXX should not happend but better not to loose information
    !Duration ^ dur_days := Days.

duration_to_time_zone(Duration, TimeZone) :-
    Duration ^ dur_years = 0,
    Duration ^ dur_months = 0,
    Duration ^ dur_days = 0,
    Duration ^ dur_seconds = 0.0,
    Minutes = Duration ^ dur_hours * 60 + Duration ^ dur_minutes,
    init_time_zone_from_minutes(Minutes, TimeZone).

negate(duration(Years, Months, Days, Hours, Minutes, Seconds)) =
    duration(-Years, -Months, -Days, -Hours, -Minutes, -Seconds).


absolute(Duration) = AbsoluteDuration :-
    ( duration_leq(Duration, zero_duration) ->
        AbsoluteDuration = negate(Duration)
    ;
        AbsoluteDuration = Duration
    ).

zero_duration = duration(0, 0, 0, 0, 0, 0.0).

day_of_week(Date) = DayOfWeek :-
    JDN = julian_day(Date ^ year, Date ^ month, Date ^ day_of_month),
    Mod = JDN mod 7,
    DayOfWeek = det_day_of_week_from_mod(Mod).

:- func det_day_of_week_from_mod(int) = day_of_week.

det_day_of_week_from_mod(Mod) = DayOfWeek :-
    ( day_of_week_num(DayOfWeek0, Mod) ->
        DayOfWeek = DayOfWeek0
    ;
        error("det_day_of_week_from_mod: invalid mod: " ++
            int_to_string(Mod))
    ).

:- pred day_of_week_num(day_of_week, int).
:- mode day_of_week_num(in, out) is det.
:- mode day_of_week_num(out, in) is semidet.

day_of_week_num(monday, 0).
day_of_week_num(tuesday, 1).
day_of_week_num(wednesday, 2).
day_of_week_num(thursday, 3).
day_of_week_num(friday, 4).
day_of_week_num(saturday, 5).
day_of_week_num(sunday, 6).

%-----------------------------------------------------------------------------%
:- end_module date_time.
%-----------------------------------------------------------------------------%
% vim: ft=mercury sw=4 et fileencoding=utf8
% -*- coding:utf8; -*-
