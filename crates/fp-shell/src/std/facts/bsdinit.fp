pub const fn status() -> str {
    std::shell::process::output(
        "for SERVICE in `find /etc/rc.d /usr/local/etc/rc.d -type f 2>/dev/null`; do $SERVICE status 2> /dev/null || $SERVICE check 2> /dev/null; echo \"`basename $SERVICE`=$?\"; done"
    )
}
