pub const fn status() -> str {
    std::shell::process::output(
        "for SERVICE in `ls /etc/init.d/ 2>/dev/null`; do _=`cat /etc/init.d/$SERVICE | grep '### BEGIN INIT INFO'`; if [ \"$?\" = \"0\" ]; then /etc/init.d/$SERVICE status >/dev/null 2>&1; echo \"$SERVICE=$?\"; fi; done"
    )
}
