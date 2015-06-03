%-----------------------------------------------------------------------------%
%
% Low-level syslog interface.
% Author: Peter Wang <pwa@missioncriticalit.com>
% Copyright (c) Mission Critical Australia Pty Ltd
% MIT License
%
% You can use this directly or through log4m.
%
% Note you must set the C processor symbol `HAVE_SYSLOG_H'
% when compiling this module, otherwise nothing will work.
%
%-----------------------------------------------------------------------------%

:- module syslog.
:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

% See syslog(3) for documentation.

    % openlog(Ident, Facility, !IO)
    %
:- pred openlog(string::in, facility::in, io::di, io::uo) is det.

:- pred syslog(priority::in, string::in, io::di, io::uo) is det.

:- pred closelog(io::di, io::uo) is det.

:- type facility
    --->    daemon
    ;       local0
    ;       local1
    ;       local2
    ;       local3
    ;       local4
    ;       local5
    ;       local6
    ;       local7
    ;       user.

:- type priority
    --->    emerg
    ;       alert
    ;       crit
    ;       err
    ;       warning
    ;       notice
    ;       info
    ;       debug.

:- pred facility_to_string(facility, string).
:- mode facility_to_string(in, out) is det.
:- mode facility_to_string(out, in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C",
"
/* Get HAVE_SYSLOG_H */
#include ""posix_conf.h""

#ifdef HAVE_SYSLOG_H
    #include <syslog.h>
#else

    #define LOG_DAEMON      (3<<3)  /* system daemons */
    #define LOG_LOCAL0      (16<<3) /* reserved for local use */
    #define LOG_LOCAL1      (17<<3) /* reserved for local use */
    #define LOG_LOCAL2      (18<<3) /* reserved for local use */
    #define LOG_LOCAL3      (19<<3) /* reserved for local use */
    #define LOG_LOCAL4      (20<<3) /* reserved for local use */
    #define LOG_LOCAL5      (21<<3) /* reserved for local use */
    #define LOG_LOCAL6      (22<<3) /* reserved for local use */
    #define LOG_LOCAL7      (23<<3) /* reserved for local use */
    #define LOG_USER        (1<<3)  /* random user-level messages */

    #define LOG_EMERG   0
    #define LOG_ALERT   0
    #define LOG_CRIT    0
    #define LOG_ERR     0
    #define LOG_WARNING 0
    #define LOG_NOTICE  0
    #define LOG_INFO    0
    #define LOG_DEBUG   0
#endif
").

:- pragma foreign_enum("C", facility/0, [
    daemon  - "LOG_DAEMON",
    local0  - "LOG_LOCAL0",
    local1  - "LOG_LOCAL1",
    local2  - "LOG_LOCAL2",
    local3  - "LOG_LOCAL3",
    local4  - "LOG_LOCAL4",
    local5  - "LOG_LOCAL5",
    local6  - "LOG_LOCAL6",
    local7  - "LOG_LOCAL7",
    user    - "LOG_USER"
]).

:- pragma foreign_enum("C", priority/0, [
    emerg   - "LOG_EMERG",
    alert   - "LOG_ALERT",
    crit    - "LOG_CRIT",
    err     - "LOG_ERR",
    warning - "LOG_WARNING",
    notice  - "LOG_NOTICE",
    info    - "LOG_INFO",
    debug   - "LOG_DEBUG"
]).

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", local,
"
    static MR_ConstString SYSLOG_keep_ident;
").

:- pragma foreign_proc("C",
    openlog(Ident::in, Facility::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
#ifdef HAVE_SYSLOG_H

    /* We don't support any options (yet). */
    int Options = 0;

    /* The man page says that the identifier might not be copied.
     * Therefore we retain a pointer to it to prevent it from being
     * garbage collected.
     */
    SYSLOG_keep_ident = Ident;

    openlog(Ident, Options, Facility);

#else

    fprintf(stderr,
        ""WARNING: syslog support not compiled in, ignoring.\\n"");

#endif

    IO = IO0;
").
:- pragma foreign_proc("Java",
    openlog(_Ident::in, _Facility::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    if (1 == 1) throw new Error(\"not supported in Java grade\");
").

:- pragma foreign_proc("C#",
    openlog(_Ident::in, _Facility::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    if (1 == 1) throw new System.Exception(\"not supported in C# grade\");
").


:- pragma foreign_proc("C",
    syslog(Priority::in, Message::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
#ifdef HAVE_SYSLOG_H
    syslog(Priority, ""%s"", Message);
#endif
    IO = IO0;
").
:- pragma foreign_proc("Java",
    syslog(_Priority::in, _Message::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    if (1 == 1) throw new Error(\"not supported in Java grade\");
").
:- pragma foreign_proc("C#",
    syslog(_Priority::in, _Message::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    if (1 == 1) throw new System.Exception(\"not supported in C# grade\");
").

:- pragma foreign_proc("C",
    closelog(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
#ifdef HAVE_SYSLOG_H
    closelog();
#endif
    SYSLOG_keep_ident = NULL;
    IO = IO0;
").
:- pragma foreign_proc("Java",
    closelog(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    if (1 == 1) throw new Error(\"not supported in Java grade\");
").
:- pragma foreign_proc("C#",
    closelog(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    if (1 == 1) throw new System.Exception(\"not supported in C# grade\");
").

%-----------------------------------------------------------------------------%

facility_to_string(daemon, "daemon").
facility_to_string(local0, "local0").
facility_to_string(local1, "local1").
facility_to_string(local2, "local2").
facility_to_string(local3, "local3").
facility_to_string(local4, "local4").
facility_to_string(local5, "local5").
facility_to_string(local6, "local6").
facility_to_string(local7, "local7").
facility_to_string(user, "user").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=8 sts=4 sw=4 et
