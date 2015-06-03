%------------------------------------------------------------------------------%
/*
 * NAME
 *   log4m
 * COPYING
 *   Copyright (c) Mission Critical Australia Pty Ltd
 *   MIT License
 * PURPOSE
 *   A module for providing logging services.
 * AUTHOR
 *   Peter Ross <pro@missioncriticalit.com>
 *   Peter Wang <pwa@missioncriticalit.com>
 * BUGS
 *   This module isn't thread-safe, as one updates to the logger state can
 *   be lost.
 * TODO
 *   Make the update of the internal log state thread-safe.
 *   Nicer messages when LOG.ini contains errors.
 *   Split the appender from its layout: separate the where/how from the what
 */

:- module log4m.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module list.

:- import_module email.

%-----------------------------------------------------------------------------%

    % Activate the logs mechanism.
    %
    % This will use the ini configuration file given.
    % the default level will be set to error, overriding any previous
    % levels defined.
    %
:- pred activate_logs_from_ini(string::in, io::di, io::uo) is det.

    % Activate the logs mechanism.
    %
    % This will use the LOG.ini configuration file if present, otherwise
    % the older LOG definition. 
    %
:- pred activate_logs(io::di, io::uo) is det.

    % Activate log4m so that it forwards all logging to log4j.
    %
:- pred use_log4j(io::di, io::uo) is det.

/*
 *    activate the logs mechanism
 *      - log definition in LOG
 *      - output in a specific file (by default, LOG_OUTPUT)
 *
 *    The predicates and types below allow to write a similar activation to be written.
 %
 %    activate_logs/2 should be preferred.
 */
:- pred activate_logs(string::in, io::di, io::uo) is det.

/*
 *    An id for identifying a logger.  The id's are hierachical.
 *    The root logger is [].
 *    A child of the root logger is ["Performance"]
 *    A child of the previous logger is ["Child", "Performance"]
 *    Note that the lists are in reverse order to the naming of loggers
 *    in log4j.
 */
:- type id == list(string).

/*
 *    A log message is given a level.  debug is the lowest level, fatal
 *    the highest.
 */
:- type level
    --->    debug
    ;       info
    ;       warn
    ;       error
    ;       fatal.

/*
 *    The additivity type is used to determine the whether to continue
 *    or stop searching for appenders.
 */
:- type additivity
    --->    stop
    ;       continue.

    % for backwards compatibility
:- type addivity == additivity.

:- type lazy_string == ((func) = string).

% The typeclass appender describes types which can be used to write
% a log message somewhere.
%
:- typeclass appender(T) where [
    (pred write_message(T::in, id::in, level::in, lazy_string::in,
        io::di, io::uo) is det),
    (pred shutdown(T::in, io::di, io::uo) is det)
].  


/*
 *    Resets the logger state to be empty, then sets the level of the
 *    root logger to be debug.
 *
 *    Note that all the appenders are lost.
 */
:- pred debug(io::di, io::uo) is det.

/*
 *    Resets the logger state to be empty, then sets the level of the
 *    root logger to be info.
 *
 *    Note that all the appenders are lost.
 */
:- pred info(io::di, io::uo) is det.

/*
 *    Resets the logger state to be empty, then sets the level of the
 *    root logger to be warn.
 *
 *    Note that all the appenders are lost.
 */
:- pred warn(io::di, io::uo) is det.

/*
 *    Resets the logger state to be empty, then sets the level of the
 *    root logger to be error.
 *
 *    Note that all the appenders are lost.
 */
:- pred error(io::di, io::uo) is det.

/*
 *    Resets the logger state to be empty, then sets the level of the
 *    root logger to be fatal.
 *
 *    Note that all the appenders are lost.
 */
:- pred fatal(io::di, io::uo) is det.

/*
 *    Update the logging level of an id.
 
 *    id - The id of the logger whose level we are updating.
 *
 *    level - The level to set the logger to.
 * TODO
 *    Make this function thread safe.
 */
:- pred update_level(id::in, level::in, io::di, io::uo) is det.

/*
 *    Add an appender to be called when logging at a specified id,
 *    and specify via the additivity whether to continue searching for
 *    appenders to call in the parent loggers.
 *    Two appenders added with the same name are considered to be the same,
 *    if one add different appenders with the same name, only the last one
 *    will be kept.
 *
 * FUNCTION
 *    The sequence of calls:
 *      add_appender(["L1"], "A1", stop, A1, !IO),
 *      add_appender(["L1"], "A2", continue, A2, !IO),
 *      add_appender(["L2", "L1"], "B1", stop, B1, !IO),
 *      add_appender(["L3, "L2", "L1"], "C1", continue, C1, !IO)
 *    will have the following behaviour.
 *
 *    When logging for ["L1"] appenders A1 and A2 will be called.
 *    The appenders for [] will not be called because we specified that
 *    the additivity stop, when setting the appender A1.  Setting the
 *    additivity to continue for A2 does not override the original stop.
 *
 *    When logging for ["L2", "L1"] only appender B1 will be called.
 *    The additivity stop prevents calling the appenders for ["L1"] and [].
 *
 *    When logging for ["L3, "L2", "L1"] the appenders C1 and B1.
 *    B1 is called because the additivity is set to continue and B1
 *    is implied by the level ["L2", "L1"].
 *
 *    appenders names do not influence the additivity overriding, e.g.
 *      add_appender(["L1"], "A1", stop, A1, !IO),
 *      add_appender(["L1"], "A1", continue, A1, !IO)
 *    the continue will not override the stop of the first call.
 *    
 *
 * TODO
 *    Determine if the setting of additivity continue after the setting
 *    of the additivity stop should override the stop.
 */
:- pred add_appender(id::in, string::in, additivity::in, T::in,
                io::di, io::uo) is det <= appender(T).

/*
 *    For a given logger id at the specified level return an indicator
 *    of whether or not logging would occur.
 */
:- pred will_log(id::in, level::in, bool::out, io::di, io::uo) is det.

/*
 *    If will_log < log4m/log4m.m/will_log > indicates that logging
 *    should occur then call the appenders implied by the logger id
 *    with the specified string.
 */
:- pred log(id::in, level::in, string::in, io::di, io::uo) is det.

/*
 *    If will_log < log4m/log4m.m/will_log > indicates that logging
 *    should occur then call the appenders implied by the logger id
 *    with the result of evaluating the function which generates a string.
 *
 *    The idea behind this predicate is to avoid the possibly expensive
 *    creation of log messages, only to not have the message logged.
 *    This is done by passing a closure which creates the log message
 *    only when it is needed.  Note one still has to be careful that
 *    all the expensive calculations are done inside the closure.
 *
 *    *Don't* be stupid and pass a closure which returns a constant string.
 *    Use log4m.log for that.
 */
:- pred log_f(id::in, level::in, lazy_string::in, io::di, io::uo) is det.

%
% Embed a call to log inside a trace goal
% The logging is included in the executable when compiled in a debug
% grade or with "--trace-flag do_logging".
% Note that, depending on the semantic options given to mmc, any calls to
% trace_log could still be optimized away, even in a debug grade or with
% "--trace-flag do_logging".  See the "Semantics" section of the Mercury
% reference manual.
%
:- pred trace_log(id::in, level::in, string::in) is det.

%
% Embed a call to log_f inside a trace goal
% The logging is included in the executable when compiled in a debug
% grade or with "--trace-flag do_logging".
% Note that, depending on the semantic options given to mmc, any calls to
% trace_log_f could still be optimized away, even in a debug grade or with
% "--trace-flag do_logging".  See the "Semantics" section of the Mercury
% reference manual.
%
% NOTE: *Don't* be stupid and pass a closure which returns a constant string.
% Use log4m.trace_log for that (or log4m.log).
%
:- pred trace_log_f(id::in, level::in, lazy_string::in) is det.

/*
 *    If will_log indicates that loggind should occur _and_ predicate
 *    check_condition succeeds then call the appenders implied by the logger id
 *    with the result of evaluating the predicate which generates a string.
 */
:- type check_condition == pred(string).
:- inst check == (pred(out) is semidet).

:- pred log_p(id::in, level::in, check_condition::in(check), io::di, io::uo) is det.


    % Update the logger state.
    % does not throw any exception, but may return an error.
    % in case of an error, the log level is kept as it was previously.
    %
    % This will use the ini configuration file given as first argument.
    %
:- pred update_log_from_ini(string::in, io.res::out, io::di, io::uo) is det.

    % Update the logger state.
    % does not throw any exception, but may return an error.
    % in case of an error, the log level is kept as it was previously.
    %
    % This will use the LOG.ini configuration file if present, otherwise
    % the older LOG definition. 
    %
:- pred update_log(io.res::out, io::di, io::uo) is det.

/*
 *    Update the logger state with levels read from a file.
 *    Returns an error if the file couldn't be opened.
 *
 *    Expects a file where each line is in the following format.
 *
 *      set_level(["L1"], debug).
 *
 *    This predicate will throw an exception if the lines are of the
 *    incorrect format.
 %    update_log/2 should be preferred.
 */
:- pred update_log(string::in, io.res::out, io::di, io::uo) is det.

%------------------------------------------------------------------------------%

  % reset_all(!IO) will remove all appenders,
  % reset all levels to error.
:- pred reset_all(io::di, io::uo) is det.

%------------------------------------------------------------------------------%
/*
 *    A formatted appender takes a description of how a message is to be
 *    formatted plus the underlying appender that is to be used to write
 *    the actual log message.
 *
 *    The message is formatted by simply applying in order each component
 *    in the list(spec) to the underlying formatter (T).
 */
:- type formatted_appender(T)
    --->    formatted_appender(list(spec), T).

/*
 *    A component to be output
 *      - date
 *          The current date in the format "YYYY-MM-DD HH:MM:SS"
 *      - id
 *          The id of the current message to be logged in the format
 *          ["A", "B"]
 *      - level
 *          The level of the log message as a string. eg "debug"
 *      - message
 *          The actual message to be logged
 *      - str(S)
 *          The string, S, is written to the underlying appender.
 *      - thread_id
 *          The id of the thread which output the log message.
 */
:- type spec
    --->    date
    ;       id
    ;       level
    ;       message
    ;       str(string)
    ;       thread_id
    .

:- instance appender(formatted_appender(T)) <= appender(T).

:- instance appender(io.output_stream).

% to be used with formatted_appender
:- type daily_rolling_file_appender.

% init_daily_rolling_file_appender(FilePrefix)
:- pred init_daily_rolling_file_appender(string::in, daily_rolling_file_appender::out, 
                    io::di, io::uo) is det.

:- instance appender(daily_rolling_file_appender).

:- type email_appender
    ---> email_appender(smtphost, email).

:- instance appender(email_appender).

:- type min_level_filter_appender(Appender)
    ---> min_level_filter_appender(Appender, level).

:- instance appender(min_level_filter_appender(Appender)) <= appender(Appender).

    % This appender forwards logging to log4j.
    %
:- type log4j_appender ---> log4j_appender.

:- instance appender(log4j_appender).


:- pred shutdown(io::di, io::uo) is det.
%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module exception.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module time.

:- import_module mc_ini.
:- import_module syslog.

%-----------------------------------------------------------------------------%

:- type log
    --->    log(
                log_timestamp       :: log_timestamp, 
                log_levels          :: level_map, 
                log_appenders       :: log_appenders,
                log_backend         :: log_backend
            ).

:- type log_timestamp == maybe(time_t).

:- type level_map == map(id, level).

:- type appender_name == string.
:- type named_appenders == map(appender_name, appender).
:- type appenders_map == map(id, appenders).
:- type log_appenders 
    --->    log_appenders(appenders_map, named_appenders).

:- type appenders
    --->    stop(list(appender_name))
    ;       continue(list(appender_name))
    .

:- type appender
    --->    some [T] appender(T) => appender(T).

:- type log_backend
    --->    mercury
    ;       log4j
    .
       

%------------------------------------------------------------------------------%

:- func log_file = string.
log_file = "LOG".

:- func log_ini_file = string.
log_ini_file = "LOG.ini".

%------------------------------------------------------------------------------%

reset_all(!IO) :-
    set_log(log(no, map.set(map.init, [], error), log_appenders(map.init, map.init), mercury), !IO).

%------------------------------------------------------------------------------%
activate_logs_from_ini(IniFile, !IO) :-
    error(!IO),
    load_ini_file(IniFile, LoadRes, !IO),
    ( LoadRes = io.ok(Ini),
        activate_logs_by_ini(Ini, !IO)
    ; LoadRes = io.error(Error),
        error($pred ++ " cannot open " ++ IniFile ++ " " ++ io.error_message(Error))
    ). 

activate_logs(!IO):-
    % Helpful to know if an application enabled logging. 
    trace [runtime(env("LOG4M")), io(!TIO)] (
        io.stderr_stream(StdErr, !TIO),
        io.write_string(StdErr, $pred, !TIO),
        io.write_string(StdErr, " called\n", !TIO)
    ),
    error(!IO),

    % Use the new style LOG.ini if possible.
    io.check_file_accessibility(log_ini_file, [read], IniExists, !IO),
    ( IniExists = ok,
        update_log(Result, !IO),
        ( Result = ok
        ; Result = error(Error),
            throw(Error)
        )
    ; IniExists = error(_Error),
        % Probably the config file doesn't exist; fall back to old behaviour.
        activate_logs("LOG_OUTPUT", !IO)
    ).


    % activate_logs/2 is preferred.
    %
activate_logs(LogFileName, !IO):-
    error(!IO),
    io.stderr_stream(StdErr, !IO),

    % Helpful to know if an application enabled logging. 
    trace [runtime(env("LOG4M")), io(!TIO)] (
        io.write_string(StdErr, $pred, !TIO),
        io.write_string(StdErr, " called\n", !TIO)
    ),

    add_formatted_stream_appender("std_err", StdErr, !IO),
    add_output_file_appender(LogFileName, LogFileName, !IO),

     %
     % If you want to turn logging on just add lines such a
     %   set_level(["service"], debug).
     % to the file LOG
     %
    update_log(log_file, _LogResult, !IO).


:- pred add_output_file_appender(string::in, string::in, io::di, io::uo) is det.

add_output_file_appender(AppenderName, LogFileName, !IO) :-
    io.open_append(LogFileName, LogOutRes, !IO),
    (
        LogOutRes = ok(LogOutput),
        add_formatted_stream_appender(AppenderName, LogOutput, !IO)
    ;
        LogOutRes = error(_),
        error("couldn't open " ++ LogFileName)
    ).

:- pred add_formatted_stream_appender(appender_name::in, io.output_stream::in, io::di, io::uo)
    is det.

add_formatted_stream_appender(AppenderName, Stream, !IO) :-
    FormatAppender = formatted_appender(default_format, Stream),
    add_appender([], AppenderName, stop, FormatAppender, !IO).

:- func default_format = list(spec).

default_format = Format :-
    Sep = str(" - "),
    Format = [date, Sep, thread_id, Sep, id, Sep, level, Sep, message].

:- func default_syslog_format = list(spec).

default_syslog_format = Format :-
    Sep = str(" - "),
    % syslog already provides the timestamp.
    Format = [id, Sep, level, Sep, message].

%------------------------------------------------------------------------------%

use_log4j(!IO) :-
    set_log(log(no, map.set(map.init, [], debug),
        log_appenders(map.set(map.init, [], stop(["log4j"])),
        map.set(map.init, "log4j", 'new appender'(log4j_appender))), log4j), !IO).

%-----------------------------------------------------------------------------%
%
% LOG.ini config file parsing
%

:- pred activate_logs_by_ini(mc_ini::in, io::di, io::uo) is det.

activate_logs_by_ini(Ini, !IO) :-
    mc_ini.get_section_names(Ini, SectionNames),
    list.filter(probably_id, SectionNames, LoggerNames, AppenderNames),
    list.foldl2(set_up_appender_by_ini(Ini), AppenderNames,
        map.init, AppendersByName, !IO),
    list.foldl(set_up_logger_by_ini(Ini, AppendersByName), LoggerNames, !IO).

:- pred set_up_appender_by_ini(mc_ini::in, string::in,
    named_appenders::in, named_appenders::out,
    io::di, io::uo) is det.

set_up_appender_by_ini(Ini, AppenderName, !AppendersByName, !IO) :-
    ( mc_ini.search(Ini, AppenderName, "file", FileName) ->
        set_up_file_appender(Ini, AppenderName, FileName,
            !AppendersByName, !IO)

    ; mc_ini.search(Ini, AppenderName, "stderr", _) ->
        set_up_stderr_appender(Ini, AppenderName,
            !AppendersByName, !IO)

    ; mc_ini.search(Ini, AppenderName, "syslog", FacilityString) ->
        set_up_syslog_appender(Ini, AppenderName, rstrip(FacilityString),
            !AppendersByName, !IO)

    ; mc_ini.search(Ini, AppenderName, "daily_file", FilePrefix) ->
        set_up_daily_rolling_file_appender(Ini, AppenderName, FilePrefix,
            !AppendersByName, !IO)

    ; mc_ini.search(Ini, AppenderName, "email", Email) ->
        set_up_email_appender(Ini, AppenderName, Email,
            !AppendersByName, !IO)

    ; mc_ini.search(Ini, AppenderName, "log4j", _) ->
        set_up_log4j_appender(Ini, AppenderName,
            !AppendersByName, !IO)

    ;
        io.stderr_stream(Stderr, !IO),
        io.write_string(Stderr, "WARNING: ignoring appender ", !IO),
        io.write_string(Stderr, AppenderName, !IO),
        io.write_string(Stderr, " (unknown appender type)\n", !IO)
    ).

:- pred set_up_file_appender(mc_ini::in, string::in, string::in,
    named_appenders::in, named_appenders::out,
    io::di, io::uo) is det.

set_up_file_appender(Ini, AppenderName, FileName, !AppendersByName, !IO) :-
    io.open_append(FileName, OpenRes, !IO),
    (
        OpenRes = ok(Stream),
        set_up_other_appenders(Ini, AppenderName, Stream, default_format,
            !AppendersByName)
    ;
        OpenRes = error(Error),
        error("couldn't open " ++ FileName ++ ": " ++ io.error_message(Error))
    ).

:- pred set_up_stderr_appender(mc_ini::in, string::in,
    named_appenders::in, named_appenders::out,
    io::di, io::uo) is det.

set_up_stderr_appender(Ini, AppenderName, !AppendersByName, !IO) :-
    io.stderr_stream(StdErr, !IO),
    set_up_other_appenders(Ini, AppenderName, StdErr, default_format,
        !AppendersByName).

:- pred set_up_syslog_appender(mc_ini::in, string::in, string::in,
    named_appenders::in, named_appenders::out,
    io::di, io::uo) is det.

set_up_syslog_appender(Ini, AppenderName, FacilityString,
        !AppendersByName, !IO) :-
    ( facility_to_string(Facilty, FacilityString) ->
        mc_ini.search_default(Ini, global_section, "syslog_ident", "log4m",
            Ident),
        init_syslog_appender(Ident, Facilty, Appender, !IO),
        set_up_other_appenders(Ini, AppenderName, Appender,
            default_syslog_format, !AppendersByName)
    ;
        error("unknown syslog facility: " ++ FacilityString)
    ).

:- pred set_up_other_appenders(mc_ini::in, string::in,
    Appender::in, list(spec)::in,
    named_appenders::in, named_appenders::out)
    is det <= appender(Appender).

set_up_other_appenders(Ini, AppenderName, Appender,
        DefaultFormat, !AppendersByName) :-
    ( mc_ini.search(Ini, AppenderName, "format", FormatString) ->
        Format = parse_format(FormatString)
    ;
        Format = DefaultFormat
    ),
    FormatAppender = formatted_appender(Format, Appender),
    ( mc_ini.search(Ini, AppenderName, "min_level", MinLevelString) ->
        ( level(MinLevel) = MinLevelString ->
            MinLevelAppender = min_level_filter_appender(FormatAppender, MinLevel),
            FinalAppender = 'new appender'(MinLevelAppender)
        ;
            error("unrecognised level: " ++ MinLevelString)
        )
    ;
        FinalAppender = 'new appender'(FormatAppender)
    ),
    map.det_insert(AppenderName, FinalAppender, !AppendersByName).


:- pred set_up_logger_by_ini(mc_ini::in, named_appenders::in,
    string::in, io::di, io::uo) is det.

set_up_logger_by_ini(Ini, AppendersByName, LoggerName, !IO) :-
    Id = parse_id(LoggerName),

    ( mc_ini.search(Ini, LoggerName, "level", LevelString) ->
        ( level(Level) = LevelString ->
            update_level(Id, Level, !IO)
        ;
            error("unrecognised level: " ++ LevelString)
        )
    ;
        true
    ),

    ( mc_ini.search(Ini, LoggerName, "appenders", AppendersString) ->
        split_appenders_list(AppendersString, Appenders),
        mc_ini.search_default(Ini, LoggerName, "inherit_appenders", "yes",
            InheritString),
        inherit_string_to_additivity(InheritString, Additivity),
        list.foldl(add_appender_by_name(AppendersByName, Id, Additivity),
            Appenders, !IO)
    ;
        true
    ).

:- pred split_appenders_list(string::in, list(string)::out) is det.

split_appenders_list(AppendersString, Appenders) :-
    Appenders = string.words_separator(appender_list_delim, AppendersString).

:- pred appender_list_delim(char::in) is semidet.

appender_list_delim(Char) :-
    ( char.is_whitespace(Char)
    ; Char = (',')
    ).

:- pred inherit_string_to_additivity(string::in, additivity::out) is det.

inherit_string_to_additivity(String, Additivity) :-
    (
        ( String = "yes"
        ; String = "y"
        ; String = "true"
        ; String = "1"
        )
    ->
        Additivity = continue
    ;
        ( String = "no"
        ; String = "n"
        ; String = "false"
        ; String = "0"
        )
    ->
        Additivity = stop
    ;
        error("unrecognised inherit_appenders value: \"" ++ String ++ "\"")
    ).

:- pred add_appender_by_name(named_appenders::in, id::in, additivity::in,
    string::in, io::di, io::uo) is det.

add_appender_by_name(AppendersByName, Id, Addivity, AppenderName, !IO) :-
    map.lookup(AppendersByName, AppenderName, appender(Appender)),
    add_appender(Id, AppenderName, Addivity, Appender, !IO).

:- pred probably_id(string::in) is semidet.

probably_id(String) :-
    ( String = ""
    ; string.contains_char(String, '"')
    ).

:- func parse_id(string) = id.

parse_id(String) = Id :-
    ToParse = "[" ++ String ++ "].",
    io.read_from_string("", ToParse, string.length(ToParse), ReadRes,
        posn(0, 0, 0), _Posn),
    (
        ReadRes = ok(Id : id)
    ;
        ( ReadRes = eof
        ; ReadRes = error(_, _)
        ),
        error("couldn't parse logger id: " ++ String ++ "")
    ).

:- func parse_format(string) = list(spec).

parse_format(String) = Format :-
    ToParse = String ++ ".",
    io.read_from_string("", ToParse, string.length(ToParse), ReadRes,
        posn(0, 0, 0), _Posn),
    (
        ReadRes = ok(Format)
    ;
        ( ReadRes = eof
        ; ReadRes = error(_, _)
        ),
        error("couldn't parse format: " ++ String ++ "")
    ).

%-----------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

debug(!IO) :- set_log(debug, !IO).
info(!IO) :- set_log(info, !IO).
warn(!IO) :- set_log(warn, !IO).
error(!IO) :- set_log(error, !IO).
fatal(!IO) :- set_log(fatal, !IO).

:- func debug = log.
:- func info = log.
:- func warn = log.
:- func error = log.
:- func fatal = log.

debug = log(no, map.set(map.init, [], debug), log_appenders(map.init, map.init), mercury).
info = log(no, map.set(map.init, [], info), log_appenders(map.init, map.init), mercury).
warn = log(no, map.set(map.init, [], warn), log_appenders(map.init, map.init), mercury).
error = log(no, map.set(map.init, [], error), log_appenders(map.init, map.init), mercury).
fatal = log(no, map.set(map.init, [], fatal), log_appenders(map.init, map.init), mercury).

will_log(Id, Level, WillLog, !IO) :-
    get_log(Log, !IO),
    Backend = Log ^ log_backend,
    ( Backend = mercury,
        LevelsMap = Log ^ log_levels,
        LoggerLevel = find_logger_level(Id, LevelsMap),
        compare(Res, Level, LoggerLevel),
        (
            ( Res = (=)
            ; Res = (>)
            ),
            WillLog = yes
        ;
            Res = (<),
            WillLog = no
        )
    ; Backend = log4j,
        will_log_log4j(to_log4j_name(Id), to_log4j_level(Level), WillLog, !IO)
    ).

:- pred will_log_log4j(string::in, log4j_level::in, bool::out, io::di, io::uo) is det.

will_log_log4j(_, _, no, !IO).

:- pragma foreign_proc("Java", will_log_log4j(Name::in, Level::in, WillLog::out,
    _IO0::di, _IO::uo),
    [promise_pure, thread_safe, terminates],
"
    org.apache.log4j.Logger logger = org.apache.log4j.Logger.getLogger(Name);
    if (logger.isEnabledFor(Level)) {
        WillLog = jmercury.bool.YES;
    } else {
        WillLog = jmercury.bool.NO;
    }
").


log(Id, Level, Msg, !IO) :-
    will_log(Id, Level, WillLog, !IO),
    (
        WillLog = yes,
        get_log(Log, !IO),
        Appenders = Log ^ log_appenders,
        write_levels(Id, Level, (func) = Msg, Appenders, !IO)
    ;
        WillLog = no
    ).

log_f(Id, Level, Func, !IO) :-
    will_log(Id, Level, WillLog, !IO),
    (
        WillLog = yes,
        get_log(Log, !IO),
        Appenders = Log ^ log_appenders,
        write_levels(Id, Level, Func, Appenders, !IO)
    ;
        WillLog = no
    ).

trace_log(Id, Level, Msg) :-
    trace [
        compile_time(flag("do_logging") or grade(debug)),
        io(!TraceIO)
    ] (
        log(Id, Level, Msg, !TraceIO)
    ).

trace_log_f(Id, Level,
            Func) :-
    trace [
        compile_time(flag("do_logging") or grade(debug)),
        io(!TraceIO)
    ] (
        log_f(Id, Level, Func, !TraceIO)
    ).

:- func find_logger_level(id, level_map) = level.

find_logger_level([], LevelMap) = map.lookup(LevelMap, []).
find_logger_level([H|T], LevelMap) =
    ( map.search(LevelMap, [H|T], Level) ->
        Level
    ;
        find_logger_level(T, LevelMap)
    ).

%------------------------------------------------------------------------------%

log_p(Id, Level, P, !IO) :-
    will_log(Id, Level, WillLog, !IO),
    (
        WillLog = yes,
        P(ToLog)
    ->
        get_log(Log, !IO),
        Appenders = Log ^ log_appenders,
        write_levels(Id, Level, (func) = ToLog, Appenders, !IO)
    ;
        true
    ).

%------------------------------------------------------------------------------%

:- pred write_levels(id::in, level::in, lazy_string::in,
                log_appenders::in, io::di, io::uo) is det.

write_levels(Id, Level, String, LogAppenders, !IO) :-
    gather_appenders(Id, Level, LogAppenders, Appenders),
    write_levels(Appenders, Id, Level, String, LogAppenders, !IO).

:- pred gather_appenders(id::in, level::in, log_appenders::in, list(appender_name)::out) is det.

gather_appenders([], _Level, log_appenders(LevelAppenders, _AppenderNames), Appenders) :-
    ( map.search(LevelAppenders, [], Data) ->
        ( Data = stop(Appenders)
        ; Data = continue(Appenders)
        )
    ;
        Appenders = []
    ).

gather_appenders([H|T], Level, log_appenders(LevelAppenders, AppenderNames), Appenders) :-
    ( map.search(LevelAppenders, [H|T], Data) ->
        ( Data = stop(Appenders)
        ; Data = continue(Appenders0),
            gather_appenders(T, Level, log_appenders(LevelAppenders, AppenderNames), Appenders1),
            Appenders = remove_dups(Appenders0 ++ Appenders1)
        )
    ;
        gather_appenders(T, Level, log_appenders(LevelAppenders, AppenderNames), Appenders)
    ).

:- pred write_levels(list(appender_name)::in, id::in, level::in, lazy_string::in,
                log_appenders::in, io::di, io::uo) is det.

write_levels(Appenders, Id, Level, String, log_appenders(_LevelAppenders, AppenderNames), !IO) :-
    list.foldl(write_appender(Id, Level, String, AppenderNames), Appenders, !IO).

:- pred write_appender(id::in, level::in, lazy_string::in, named_appenders::in, 
                    appender_name::in, io::di, io::uo) is det.

write_appender(Id, Level, Msg, AppenderNames, AppenderName, !IO) :-
    map.lookup(AppenderNames, AppenderName, appender(A)),
    write_message(A, Id, Level, Msg, !IO).

%------------------------------------------------------------------------------%

update_log_from_ini(IniFile, Result, !IO) :-
    promise_equivalent_solutions [Result, !:IO]
    (try [io(!IO)] (

        io.check_file_accessibility(IniFile, [read], IniExists, !IO),
        ( IniExists = io.ok,
            update_timestamp_and_check_file_has_changed(IniFile, UpdateNeeded, !IO),
            ( UpdateNeeded = yes,
                % Use the new style LOG.ini if possible.
                load_ini_file(IniFile, LoadRes, !IO),
                (
                    LoadRes = io.ok(Ini),
                    activate_logs_by_ini(Ini, !IO),
                    Result = io.ok
                ;
                    LoadRes = io.error(_Error),
                    % Probably the config file doesn't exist; fall back to old behaviour.
                    update_log(log_file, Result, !IO)
                )
            ; UpdateNeeded = bool.no,
                Result = io.ok
            )
        ; IniExists = io.error(_Error),
            % Probably the config file doesn't exist; fall back to old behaviour.
            update_log(log_file, Result, !IO)
        )
    )
    then
        true
    catch_any Other ->
        Result = error(io.make_io_error(string(Other)))
    ).
    

update_log(Result, !IO) :-
    update_log_from_ini(log_ini_file, Result, !IO).

%------------------------------------------------------------------------------%
:- type logfile_directive
    --->    set_level(id, level).

update_log(FileName, Result, !IO) :-
    update_timestamp_and_check_file_has_changed(FileName, UpdateNeeded, !IO),
    ( UpdateNeeded = yes,
        io.open_input(FileName, OpenRes, !IO),
        ( OpenRes = ok(Stream),
            get_log(Log0, !IO),
            read_file(Stream, Log0, Log, !IO),
            set_log(Log, !IO),
            io.close_input(Stream, !IO),
            Result = ok
        ; OpenRes = error(E),
            Result = error(E)
        )
    ; UpdateNeeded = no,
        Result = ok
    ).

:- pred read_file(io.input_stream::in,
                log::in, log::out, io::di, io::uo) is det.

read_file(Input, !Log, !IO) :-
    io.read(Input, Result, !IO),
    ( Result = ok(set_level(Id, Level)),
        !:Log = update_level(Id, Level, !.Log),
        read_file(Input, !Log, !IO)
    ; Result = eof,
        true
    ; Result = error(Msg, Line),
        input_stream_name(Input, Name, !IO),
        error(format("log4m.read_file: %s line %d has error %s.",
                [s(Name), i(Line), s(Msg)]))
    ).
    
:- pred update_timestamp_and_check_file_has_changed(string::in, bool::out, io::di, io::uo) is det.

update_timestamp_and_check_file_has_changed(FileName, Result, !IO) :-
    get_log(Log0, !IO),
    io.file_modification_time(FileName, ModTimeRes, !IO),
    ( Log0 ^ log_timestamp = yes(TS0),
        ( 
            ( ModTimeRes = ok(TS),
              compare((<), TS0, TS))
        ->
            Result = yes,
            MTS = yes(TS)
        ;
            Result = no,
            MTS = yes(TS0)
        )
    ; Log0 ^ log_timestamp = no,
        Result = yes,
        ( ModTimeRes = ok(TS) ->
            MTS = yes(TS)
        ;
            MTS = no
        )
    ),
    Log = Log0 ^ log_timestamp := MTS,
    set_log(Log, !IO).

%------------------------------------------------------------------------------%

update_level(Id, Level, !IO) :-
    get_log(Log, !IO),
    set_log(update_level(Id, Level, Log), !IO).

:- func update_level(id, level, log) = log.

update_level(Id, Level, log(TS, M, A, B)) = log(TS, map.set(M, Id, Level), A, B).

%------------------------------------------------------------------------------%

add_appender(Id, AppenderName, Addivity, Appender, !IO) :-
    get_log(Log0, !IO),
    Appenders0 = Log0 ^ log_appenders,
    add_appender_2(Id, AppenderName, Addivity, Appender, Appenders0, Appenders),
    Log = Log0 ^ log_appenders := Appenders,
    set_log(Log, !IO).

:- pred add_appender_2(id::in, appender_name::in, additivity::in, T::in,
                log_appenders::in, log_appenders::out) is det <= appender(T).

add_appender_2(Id, AppenderName, Addivity, T, 
                    log_appenders(!.LevelAppenders, !.AppenderNames),
                    log_appenders(!:LevelAppenders, !:AppenderNames)) :-
    App = 'new appender'(T),
    ( map.search(!.LevelAppenders, Id, Data0) ->
        ( Data0 = stop(Appenders),
            Data = stop(remove_dups([AppenderName | Appenders]))
        ; Data0 = continue(Appenders),
            ( Addivity = stop,
                Data = stop(remove_dups([AppenderName | Appenders]))
            ; Addivity = continue,
                Data = continue(remove_dups([AppenderName | Appenders]))
            )
        )
    ;
        ( Addivity = stop,
            Data = stop([AppenderName])
        ; Addivity = continue,
            Data = continue([AppenderName])
        )
    ),
    !:LevelAppenders = map.set(!.LevelAppenders, Id, Data),
    !:AppenderNames = map.set(!.AppenderNames, AppenderName, App).

%------------------------------------------------------------------------------%
%
% Instance of appender that writes to standard output/error
%

:- instance appender(io.output_stream) where [
    ( write_message(S, _Id, _Level, Msg, !IO) :-
        io.write_string(S, apply(Msg), !IO),
        io.flush_output(S, !IO)
    ),
    (shutdown(S, !IO) :-
        io.close_output(S, !IO)
    )
].

%------------------------------------------------------------------------------%
%
% Instance of append that writes to syslog
%

:- type syslog_appender
    --->    syslog_appender.

:- pred init_syslog_appender(string::in, syslog.facility::in,
    syslog_appender::out, io::di, io::uo) is det.

init_syslog_appender(Ident, Facility, syslog_appender, !IO) :-
    openlog(Ident, Facility, !IO).

:- instance log4m.appender(syslog_appender) where [
    ( write_message(syslog_appender, _Id, Level, Msg, !IO) :-
        Priority = level_to_priority(Level),
        % XXX use more efficient way that doesn't generate garbage?
        String = apply(Msg),
        syslog(Priority, String, !IO)
    ),
    (shutdown(syslog_appender, !IO) :-
        closelog(!IO)
    )
].

:- func level_to_priority(level) = priority.

level_to_priority(Level) = Priority :-
    (
        Level = debug,
        Priority = debug
    ;
        Level = info,
        Priority = info
    ;
        Level = warn,
        Priority = warning
    ;
        Level = error,
        Priority = err
    ;
        Level = fatal,
        Priority = crit     % seems excessive
    ).

%------------------------------------------------------------------------------%

:- mutable(log, log, fatal, ground, [untrailed, attach_to_io_state]).

%------------------------------------------------------------------------------%

:- initialize initialize_log4m/2.

:- pred initialize_log4m(io::di, io::uo) is det.

initialize_log4m(!IO).

:- pragma foreign_proc(erlang, initialize_log4m(_IO0::di, _IO::uo),
        [will_not_call_mercury, thread_safe, promise_pure], "
    register(log4m_log, spawn(fun() -> log4m_log() end))
").

:- pragma foreign_code(erlang, "
log4m_log() ->
    log4m_log_loop(unknown).

log4m_log_loop(Log) ->
    receive
        {get, PID} ->
            NewLog = Log,
            PID ! Log;
        {set, SetLog} ->
            NewLog = SetLog
    end,
    log4m_log_loop(NewLog).
").

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- instance appender(formatted_appender(T)) <= appender(T) where [
    ( write_message(formatted_appender(Specs, A), Id, Level, Msg, !IO) :-
        interpret_specs(Id, Level, apply(Msg), Specs, [], Strings, !IO),
        write_message(A, Id, Level, (func) = string.append_list(Strings), !IO)
    ),
    (shutdown(formatted_appender(_, A), !IO) :-
        shutdown(A, !IO)
    )
].

:- pred interpret_specs(id::in, level::in, string::in, list(spec)::in,
    list(string)::in, list(string)::out, io::di, io::uo) is det.

interpret_specs(Id, Level, Message, Specs, !Strings, !IO) :-
    (
        Specs = []
    ;
        Specs = [Spec | SpecsRest],
        interpret_specs(Id, Level, Message, SpecsRest, !Strings, !IO),
        interpret_spec(Id, Level, Message, Spec, !Strings, !IO)
    ).

:- pred interpret_spec(id::in, level::in, string::in, spec::in,
    list(string)::in, list(string)::out, io::di, io::uo) is det.

interpret_spec(Id, Level, Message, Spec, !Strings, !IO) :-
    (
        Spec = date,
        time(Time, !IO),
        TM = localtime(Time),
        TM = tm(Yr, Mnt, MD, Hrs, Min, Sec, _YD, _WD, _DST),
        Date = string.format("%4d-%02d-%02d %02d:%02d:%02d",
                [i(Yr+1900), i(Mnt+1), i(MD), i(Hrs), i(Min), i(Sec)]),
        !:Strings = [Date | !.Strings]
    ;
        Spec = id,
        !:Strings = ["]" | !.Strings],
        prepend_id(Id, !Strings),
        !:Strings = ["[" | !.Strings]
    ;
        Spec = level,
        !:Strings = [level(Level) | !.Strings]
    ;
        Spec = message,
        !:Strings = [Message] ++ !.Strings
    ;
        Spec = str(S),
        !:Strings = [S | !.Strings]
    ;
        Spec = thread_id,
        thread_id(TId, !IO),
        S = string.format("%06d", [i(TId)]),
        !:Strings = [S | !.Strings]
    ).

:- pred prepend_id(id::in, list(string)::in, list(string)::out) is det.

prepend_id([], S, S).
prepend_id([Id | Ids], S0, S) :-
    (
        Ids = [],
        S = ["\"", Id, "\"" | S0]
    ;
        Ids = [_ | _],
        prepend_id(Ids, S0, S1),
        S = ["\"", Id, "\", " | S1]
    ).

:- func level(level) = string.
:- mode level(in) = out is det.
:- mode level(out) = in is semidet.

level(debug) = "debug".
level(info) = "info".
level(warn) = "warn".
level(error) = "error".
level(fatal) = "fatal".

:- pred thread_id(int::out, io::di, io::uo) is det.  

:- pragma foreign_proc("C",
    thread_id(Id::out, IO0::di, IO::uo),
    [will_not_call_mercury, thread_safe, promise_pure, tabled_for_io],
"
#ifdef MR_THREAD_SAFE
    pthread_t thread;
    thread = pthread_self();
    Id = (int) thread;
#else
    Id = getpid();
#endif
    IO = IO0;
").

thread_id(-1, !IO).

%------------------------------------------------------------------------------%

:- import_module store.

:- type daily_rolling_file_appender 
        ---> daily_rolling_file_appender(string, io_mutvar(daily_rolling_file_handle)).

:- type daily_rolling_file_handle ---> daily_rolling_file_handle(string, io.output_stream).

:- instance appender(daily_rolling_file_appender) where [
    ( write_message(daily_rolling_file_appender(FilePrefix, CurrentHandle),
                        _Id, _Level, Msg, !IO) :-

        store.get_mutvar(CurrentHandle, daily_rolling_file_handle(LastDate, LastFileHandle), !IO),
        time.time(Time, !IO),
        LocalTime = time.localtime(Time),
        TodayDate = file_date_format(LocalTime),
        ( TodayDate = LastDate ->
            Stream = LastFileHandle
        ;
            io.close_output(LastFileHandle, !IO),
            LogFileName = FilePrefix ++ TodayDate,
            io.open_append(LogFileName, LogOutRes, !IO),
            ( LogOutRes = ok(Stream),
                store.set_mutvar(CurrentHandle, daily_rolling_file_handle(TodayDate, Stream), !IO)
            ; LogOutRes = error(E),
                error("couldn't open " ++ LogFileName ++ " " ++ io.error_message(E))
            )
        ),
        io.write_string(Stream, apply(Msg), !IO),
        io.flush_output(Stream, !IO)
    ),
    (shutdown(daily_rolling_file_appender(_, CurrentHandle), !IO) :-
        store.get_mutvar(CurrentHandle, daily_rolling_file_handle(_, LastFileHandle), !IO),
        io.close_output(LastFileHandle, !IO)
    )
].

:- func file_date_format(time.tm) = string.

file_date_format(TM) =
    string.format("%04d-%02d-%02d", [
        i(TM ^ tm_year + 1900),
        i(TM ^ tm_mon + 1),
        i(TM ^ tm_mday)
    ]).


init_daily_rolling_file_appender(FilePrefix, daily_rolling_file_appender(FilePrefix, Store), !IO) :-
    time.time(Time, !IO),
    LocalTime = time.localtime(Time),
    TodayDate = file_date_format(LocalTime),
    LogFileName = FilePrefix ++ TodayDate,
    io.open_append(LogFileName, LogOutRes, !IO),
    ( LogOutRes = ok(Stream),
        store.new_mutvar(daily_rolling_file_handle(TodayDate, Stream), Store, !IO)
    ; LogOutRes = error(E),
        error("couldn't open " ++ LogFileName ++ " " ++ io.error_message(E))
    ).


:- pred set_up_daily_rolling_file_appender(mc_ini::in, string::in, string::in,
    named_appenders::in, named_appenders::out,
    io::di, io::uo) is det.

set_up_daily_rolling_file_appender(Ini, AppenderName, FilePrefix, !AppendersByName, !IO) :-
    init_daily_rolling_file_appender(FilePrefix, DailyAppender, !IO),
    set_up_other_appenders(Ini, AppenderName, DailyAppender, default_format, !AppendersByName).

%------------------------------------------------------------------------------%

:- instance appender(email_appender) where [
    ( write_message(email_appender(SmtpHost, Email),
                        _Id, Level, Msg, !IO) :-
        send_email(SmtpHost, "log4m@missioncriticalit.com", [Email],
            string(Level), apply(Msg), _Sent, !IO)
    ),
    (shutdown(_, !IO))
].


:- pred set_up_email_appender(mc_ini::in, string::in, string::in,
    named_appenders::in, named_appenders::out,
    io::di, io::uo) is det.

set_up_email_appender(Ini, AppenderName, EmailString, !AppendersByName, !IO) :-
    ( 
        ( string.words(EmailString) = [Host, PortString, Email],
          string.to_int(PortString, Port)
        )
    ->
        Appender = email_appender(smtphost(Host, Port), Email),
        set_up_other_appenders(Ini, AppenderName, Appender, default_format, !AppendersByName)
    ;
        error("Invalid email configuration " ++ EmailString ++ " expected host port email")
    ).

%------------------------------------------------------------------------------%

:- instance appender(log4j_appender) where [
    pred(write_message/6) is log4j_write_message,
    pred(shutdown/3) is log4j_shutdown
].

:- pred set_up_log4j_appender(mc_ini::in, string::in,
    named_appenders::in, named_appenders::out,
    io::di, io::uo) is det.

set_up_log4j_appender(Ini, AppenderName, !AppendersByName, !IO) :-
    Appender = log4j_appender,
    set_up_other_appenders(Ini, AppenderName, Appender, default_format,
        !AppendersByName).

:- pred log4j_write_message(log4j_appender::in, id::in, level::in, lazy_string::in,
    io::di, io::uo) is det.

log4j_write_message(_, Id, Log4mLevel, Msg, !IO) :-
    Log4jName = to_log4j_name(Id),
    Log4jLevel = to_log4j_level(Log4mLevel),
    log4j_write_strings_impl(Log4jName, Log4jLevel, lazy_string_wrap(Msg), !IO).

:- pred log4j_shutdown(log4j_appender::in, io::di, io::uo) is det.

:- pragma foreign_proc("Java", log4j_shutdown(_L::in, _IO0::di, _IO::uo),
    [promise_pure, thread_safe, terminates, will_not_call_mercury],
"
    org.apache.log4j.Logger.shutdown();
").

log4j_shutdown(_, !IO).


:- type lazy_string_wrap ---> lazy_string_wrap(lazy_string).

:- func eval_lazy_string(lazy_string_wrap) = string.

:- pragma foreign_export("Java", eval_lazy_string(in) = (out), "LOG4M_eval_lazy_string").

eval_lazy_string(lazy_string_wrap(LazyString)) = apply(LazyString).

:- func to_log4j_name(id) = string.

to_log4j_name(Id) = string.join_list(".", list.reverse(Id)).

:- type log4j_level ---> log4j_level.

:- pragma foreign_type("Java", log4j_level, "org.apache.log4j.Level").

:- func to_log4j_level(level) = log4j_level.

to_log4j_level(debug) = log4j_debug.
to_log4j_level(info) = log4j_info.
to_log4j_level(warn) = log4j_warn.
to_log4j_level(error) = log4j_error.
to_log4j_level(fatal) = log4j_fatal.

:- func log4j_debug = log4j_level.

:- pragma foreign_proc("Java", log4j_debug = (Level::out),
    [promise_pure, thread_safe, terminates, will_not_call_mercury],
"
    Level = org.apache.log4j.Level.DEBUG;
").

log4j_debug = log4j_level.

:- func log4j_info = log4j_level.

:- pragma foreign_proc("Java", log4j_info = (Level::out),
    [promise_pure, thread_safe, terminates, will_not_call_mercury],
"
    Level = org.apache.log4j.Level.INFO;
").

log4j_info = log4j_level.

:- func log4j_warn = log4j_level.

:- pragma foreign_proc("Java", log4j_warn = (Level::out),
    [promise_pure, thread_safe, terminates, will_not_call_mercury],
"
    Level = org.apache.log4j.Level.WARN;
").

log4j_warn = log4j_level.

:- func log4j_error = log4j_level.

:- pragma foreign_proc("Java", log4j_error = (Level::out),
    [promise_pure, thread_safe, terminates, will_not_call_mercury],
"
    Level = org.apache.log4j.Level.ERROR;
").

log4j_error = log4j_level.

:- func log4j_fatal = log4j_level.

:- pragma foreign_proc("Java", log4j_fatal = (Level::out),
    [promise_pure, thread_safe, terminates, will_not_call_mercury],
"
    Level = org.apache.log4j.Level.FATAL;
").

log4j_fatal = log4j_level.

:- pred log4j_write_strings_impl(string::in, log4j_level::in, lazy_string_wrap::in,
    io::di, io::uo) is det.

:- pragma foreign_proc("Java", log4j_write_strings_impl(Name::in, Level::in, Msg::in,
    _IO0::di, _IO::uo),
    [promise_pure, thread_safe, terminates],
"
    org.apache.log4j.Logger logger = org.apache.log4j.Logger.getLogger(Name);
    final Lazy_string_wrap_0 fmsg = (Lazy_string_wrap_0)Msg;
    logger.log(Level, new Object() {
        public String toString() {
            return LOG4M_eval_lazy_string(fmsg);
        }
    });
").

log4j_write_strings_impl(_, _, _, !IO).

%------------------------------------------------------------------------------%

:- instance appender(min_level_filter_appender(Appender)) <= appender(Appender) where [
    ( write_message(min_level_filter_appender(Appender, MinLevel), Id, Level, Msg, !IO) :-
        compare(Res, Level, MinLevel),
        (
            ( Res = (=)
            ; Res = (>)
            ),
            write_message(Appender, Id, Level, Msg, !IO)
        ;
            Res = (<)
        )
    ),
    ( shutdown(min_level_filter_appender(Appender, _), !IO) :-
        shutdown(Appender, !IO)
    )
    
].

shutdown(!IO) :-
    get_log(Log, !IO),
    Log ^ log_appenders = log_appenders(_, Map),
    list.foldl(shutdown_some_appender, map.values(Map), !IO).

:- pred shutdown_some_appender(appender::in, io::di, io::uo) is det.

shutdown_some_appender(appender(A), !IO) :-
    shutdown(A, !IO).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et tw=0 wm=0
