fp-native dump: format=MachO arch=Aarch64 entry=0x0

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
global __const_data_0 ty=Array(I8, 11) constant=true initializer=Some(Bytes([70, 101, 114, 114, 111, 80, 104, 97, 115, 101, 0]))
global NAME ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0]))
global __const_data_1 ty=Array(I8, 6) constant=true initializer=Some(Bytes([48, 46, 49, 46, 48, 0]))
global VERSION ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0]))
global NAME_LEN ty=I64 constant=true initializer=Some(Bytes([10, 0, 0, 0, 0, 0, 0, 0]))
global VERSION_LEN ty=I64 constant=true initializer=Some(Bytes([5, 0, 0, 0, 0, 0, 0, 0]))
global PREFIX_OK ty=I1 constant=true initializer=Some(Bytes([1]))
global SUFFIX_OK ty=I1 constant=true initializer=Some(Bytes([1]))
global HAS_PHASE ty=I1 constant=true initializer=Some(Bytes([1]))
global __const_data_2 ty=Array(I8, 6) constant=true initializer=Some(Bytes([70, 101, 114, 114, 111, 0]))
global SHORT ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0]))
global __const_data_3 ty=Array(I8, 6) constant=true initializer=Some(Bytes([80, 104, 97, 115, 101, 0]))
global TAIL ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0]))
global __const_data_4 ty=Array(I8, 6) constant=true initializer=Some(Bytes([97, 108, 112, 104, 97, 0]))
global __const_data_5 ty=Array(I8, 5) constant=true initializer=Some(Bytes([98, 101, 116, 97, 0]))
global __const_data_6 ty=Array(I8, 6) constant=true initializer=Some(Bytes([103, 97, 109, 109, 97, 0]))
global __const_data_7 ty=Array(I8, 6) constant=true initializer=Some(Bytes([100, 101, 108, 116, 97, 0]))
global WORDS ty=Array(Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") }, 4) constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0]))
global WORD_LENGTHS ty=Array(I64, 4) constant=true initializer=Some(Bytes([5, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0]))
global TOTAL_WORD_LEN ty=I64 constant=true initializer=Some(Bytes([19, 0, 0, 0, 0, 0, 0, 0]))
global __const_data_8 ty=Array(I8, 18) constant=true initializer=Some(Bytes([70, 101, 114, 114, 111, 80, 104, 97, 115, 101, 32, 118, 48, 46, 49, 46, 48, 0]))
global BANNER ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 17, 0, 0, 0, 0, 0, 0, 0]))
fn fputc
fn flockfile
fn funlockfile
fn open_memstream
fn strtoll
fn posix_spawn_file_actions_addclose
fn posix_spawnattr_setpgroup
fn task_resume
fn thread_set_state
fn atoll
fn vm_allocate
fn mach_port_rename
fn pid_for_task
fn NSCreateObjectFileImageFromFile
fn shmctl
fn mlock
fn ctermid
fn wcscasecmp
fn thread_set_mach_voucher
fn wcsspn
fn basename
fn mach_port_move_member
fn dup2
fn fputwc
fn __istype
fn getrlimit
fn sigdelset
fn asctime
fn dlerror
fn getlogin
fn symlink
fn processor_set_default
fn insque
fn _exit
fn getegid
fn host_register_well_known_mach_voucher_attr_manager
fn memchr
fn mach_port_kobject_description
fn getuid
fn realloc
fn mig_get_reply_port
fn semaphore_signal
fn wcsftime
fn task_purgable_info
fn thread_terminate
fn uselocale
fn task_set_emulation
fn __wcwidth
fn feclearexcept
fn posix_spawn_file_actions_destroy
fn linkat
fn _kernelrpc_mach_port_mod_refs_trap
fn mknod
fn NXSwapDouble
fn execvp
fn task_self_trap
fn iswxdigit
fn isspace
fn towlower
fn putwc
fn raise
fn mkdirat
fn mach_voucher_deallocate
fn processor_set_tasks
fn task_set_ras_pc
fn task_get_state
fn strrchr
fn iswalpha
fn openlog
fn unlink
fn fchmodat
fn setrlimit
fn task_test_sync_upcall
fn mach_port_insert_right
fn getdate
fn mach_msg_send
fn NXSwapLittleIntToHost
fn _dyld_lookup_and_bind_fully
fn posix_spawnattr_getsigmask
fn __srget
fn strcpy
fn fstatat
fn mach_port_get_context
fn NXSwapHostLongToBig
fn task_generate_corpse
fn wcstoumax
fn strerror_r
fn wmemset
fn iswhexnumber
fn mach_msg
fn _kernelrpc_mach_vm_allocate_trap
fn ftrylockfile
fn wmemcpy
fn posix_spawnattr_setflags
fn thread_set_policy
fn mach_port_set_context
fn mach_zone_info
fn mbrtowc
fn endgrent
fn kill
fn ffs
fn task_get_exception_ports
fn mach_port_destruct
fn host_create_mach_voucher
fn swab
fn task_set_port_space
fn pclose
fn tcgetattr
fn _kernelrpc_mach_port_request_notification_trap
fn NSNameOfModule
fn thread_get_exception_ports
fn task_swap_exception_ports
fn memccpy
fn atol
fn setkey
fn task_suspend
fn wcsncpy
fn task_create_identity_token
fn _dyld_all_twolevel_modules_prebound
fn wcsnrtombs
fn getsid
fn pthread_testcancel
fn setlogmask
fn faccessat
fn feholdexcept
fn lockf
fn iconv_open
fn processor_set_policy_disable
fn thread_swap_exception_ports
fn getservbyport
fn vm_read_overwrite
fn mach_port_assert_attributes
fn thread_resume
fn vm_region
fn _kernelrpc_mach_port_insert_member_trap
fn wcspbrk
fn OSHostByteOrder
fn thread_set_special_port
fn confstr
fn remove
fn thread_assign
fn host_kernel_version
fn renameat
fn voucher_mach_msg_clear
fn NXSwapLittleLongToHost
fn host_set_special_port
fn exit
fn __error
fn _dyld_get_image_header_containing_address
fn mach_port_kernel_object
fn toascii
fn uname
fn task_register_dyld_get_process_state
fn tcdrain
fn tcgetsid
fn thread_policy_get
fn task_for_pid
fn send
fn _Exit
fn tcflow
fn getpgrp
fn semaphore_wait_signal
fn _kernelrpc_mach_vm_purgable_control_trap
fn NSUnLinkModule
fn __darwin_fd_clr
fn endnetent
fn task_set_policy
fn mach_msg_destroy
fn __sputc
fn system
fn abort
fn __isctype
fn localeconv
fn tmpfile
fn setstate
fn wcstol
fn iswideogram
fn sem_destroy
fn __vsprintf_chk
fn fopen
fn thread_abort_safely
fn lock_set_create
fn sockatmark
fn NXSwapFloat
fn getxattr
fn vdprintf
fn iswrune
fn atomic_flag_test_and_set
fn unlockpt
fn iconv_close
fn div
fn getsockname
fn if_nameindex
fn closelog
fn posix_spawnattr_getpgroup
fn iswprint
fn encrypt
fn act_set_state
fn isdigit
fn _kernelrpc_mach_port_move_member_trap
fn strsignal
fn _dyld_bind_fully_image_containing_address
fn processor_assign
fn clock_settime
fn _OSReadInt32
fn __maskrune
fn asctime_r
fn imaxdiv
fn inet_ntoa
fn ungetwc
fn _dyld_present
fn pathconf
fn atomic_flag_test_and_set_explicit
fn wcscspn
fn lio_listio
fn setprotoent
fn globfree
fn shmdt
fn processor_exit
fn vm_write
fn mach_task_is_self
fn thread_policy_set
fn fegetround
fn strcoll
fn geteuid
fn pipe
fn usleep
fn gethostbyaddr
fn feof
fn nrand48
fn posix_madvise
fn kext_request
fn task_set_emulation_vector
fn task_register_dyld_set_dyld_state
fn processor_set_info
fn aio_return
fn fsetpos
fn wcwidth
fn processor_info
fn getprotoent
fn task_set_corpse_forking_behavior
fn thread_assign_default
fn vsscanf
fn strpbrk
fn aio_fsync
fn memmove
fn sem_getvalue
fn posix_spawnp
fn pread
fn getgrnam
fn posix_spawn_file_actions_init
fn semget
fn mach_make_memory_entry
fn wcstoul
fn ualarm
fn pselect
fn mach_msg_overwrite
fn vm_mapped_pages_info
fn mach_host_self
fn fileno
fn open_wmemstream
fn __toupper
fn pthread_kill
fn ferror
fn strftime
fn wcswidth
fn aio_suspend
fn connect
fn rewind
fn recvmsg
fn alarm
fn getcwd
fn listen
fn regfree
fn cfgetospeed
fn fstat
fn mktime
fn gmtime_r
fn sem_post
fn gethostid
fn getlogin_r
fn __vsnprintf_chk
fn putc
fn lseek
fn _OSWriteInt16
fn task_dyld_process_info_notify_register
fn strtoimax
fn voucher_mach_msg_adopt
fn _dyld_get_image_header
fn gethostname
fn kmod_destroy
fn fmemopen
fn isascii
fn localtime
fn endhostent
fn act_get_state
fn thread_info
fn wmemchr
fn getitimer
fn hcreate
fn NSSymbolDefinitionNameInObjectFileImage
fn NSModuleForSymbol
fn task_get_exception_ports_info
fn processor_get_assignment
fn execv
fn thread_policy
fn endservent
fn wcpcpy
fn iswblank
fn freeaddrinfo
fn getservent
fn ttyname_r
fn _OSWriteSwapInt64
fn host_get_exception_ports
fn semaphore_signal_all
fn mach_generate_activity_id
fn debug_control_port_for_pid
fn mach_vm_reclaim_update_kernel_accounting_trap
fn posix_openpt
fn fread
fn strtok_r
fn btowc
fn NSVersionOfLinkTimeLibrary
fn clonefileat
fn gai_strerror
fn processor_set_destroy
fn endprotoent
fn strncpy
fn task_get_assignment
fn thread_swap_mach_voucher
fn labs
fn host_set_UNDServer
fn duplocale
fn getchar
fn fsync
fn __darwin_check_fd_set_overflow
fn fputws
fn getwc
fn vwprintf
fn sigsetjmp
fn recvfrom
fn regexec
fn setuid
fn shmget
fn sync
fn host_get_clock_service
fn newlocale
fn atomic_signal_fence
fn ptsname
fn ispunct
fn getgrgid_r
fn host_create_mach_voucher_trap
fn vm_region_64
fn freopen
fn wcsxfrm
fn __darwin_fd_set
fn mach_port_space_info
fn fremovexattr
fn vsprintf
fn setgrfile
fn ftruncate
fn getprotobynumber
fn mkfifoat
fn vm_behavior_set
fn iswupper
fn host_page_size
fn vprintf
fn fchmod
fn task_create
fn vfork
fn task_set_exception_ports
fn task_identity_token_get_task_port
fn perror
fn task_unregister_dyld_image_infos
fn mkfifo
fn tmpnam
fn getrusage
fn setregid
fn host_get_UNDServer
fn NSAddLibrary
fn getpwnam
fn clock_getres
fn setxattr
fn mach_port_guard
fn NXSwapLittleShortToHost
fn getdelim
fn processor_set_threads
fn fwrite
fn mach_port_get_srights
fn gethostbyname
fn ldiv
fn towctrans
fn lock_set_destroy
fn NXSwapHostIntToLittle
fn fgetwc
fn getgrnam_r
fn NXSwapShort
fn thread_convert_thread_state
fn mmap
fn NSAddLibraryWithSearching
fn flistxattr
fn _dyld_lookup_and_bind
fn _OSReadSwapInt16
fn psignal
fn task_threads
fn mach_port_destroy
fn clock_set_res
fn __darwin_fd_isset
fn sigignore
fn free
fn mach_ports_register
fn NXSwapBigShortToHost
fn etap_trace_thread
fn lldiv
fn mkstemp
fn socketpair
fn sem_close
fn host_get_special_port
fn waitid
fn thread_get_exception_ports_info
fn mach_port_dnrequest_info
fn _dyld_get_image_name
fn vm_inherit
fn mrand48
fn mach_make_memory_entry_64
fn _longjmp
fn srand48
fn strndup
fn NSDestroyObjectFileImage
fn getentropy
fn dlopen
fn iscntrl
fn host_get_boot_info
fn vm_remap
fn msgrcv
fn if_freenameindex
fn NXSwapBigIntToHost
fn sendmsg
fn _OSSwapInt64
fn macx_triggers
fn host_virtual_physical_table_info
fn calloc
fn _kernelrpc_mach_vm_deallocate_trap
fn ungetc
fn fsetxattr
fn sigemptyset
fn wmemmove
fn popen
fn tzset
fn msgsnd
fn __darwin_check_fd_set
fn processor_set_tasks_with_flavor
fn thread_suspend
fn task_policy_set
fn setvbuf
fn host_check_multiuser_mode
fn realpath
fn abs
fn opendir
fn wcstoull
fn __swbuf
fn strcspn
fn tcgetpgrp
fn sysconf
fn atomic_thread_fence
fn getprotobyname
fn strlen
fn nanosleep
fn isxdigit
fn access
fn task_suspend2
fn shm_unlink
fn setgid
fn task_map_kcdata_object_64
fn thread_depress_abort
fn msgget
fn dirfd
fn task_resume2
fn sethostent
fn sighold
fn hdestroy
fn task_terminate
fn seteuid
fn task_set_state
fn fegetexceptflag
fn wcstoimax
fn _setjmp
fn task_map_corpse_info_64
fn mach_port_type
fn mbsinit
fn wcstok
fn setenv
fn isgraph
fn iswnumber
fn mach_port_is_connection_for_service
fn mach_thread_self
fn malloc
fn strtoumax
fn vm_read
fn __svfscanf
fn NSLookupSymbolInModule
fn getnetbyname
fn NSAddImage
fn mach_port_set_mscount
fn aio_error
fn sem_init
fn NSIsSymbolDefinedInObjectFileImage
fn bind
fn fgetc
fn task_get_exc_guard_behavior
fn vm_deallocate
fn recv
fn gettimeofday
fn _dyld_shared_cache_contains_path
fn vscanf
fn strchr
fn host_statistics64
fn NXSwapHostLongLongToBig
fn NSLinkEditError
fn pthread_key_delete
fn wcslen
fn voucher_mach_msg_set
fn toupper
fn kqueue
fn wcscat
fn wcsrchr
fn host_swap_exception_ports
fn mach_vm_region_info_64
fn host_statistics
fn fseek
fn iswlower
fn mbsnrtowcs
fn rewinddir
fn kmod_get_info
fn task_register_dyld_image_infos
fn fclose
fn mach_error_string
fn read
fn putchar_unlocked
fn sigaltstack
fn NSInstallLinkEditErrorHandlers
fn thread_get_special_port
fn rename
fn host_info
fn processor_set_create
fn memset
fn posix_spawn_file_actions_addfchdir
fn semaphore_destroy
fn task_dyld_process_info_notify_deregister
fn fdopen
fn thread_create
fn processor_set_stack_usage
fn host_set_multiuser_config_flags
fn task_get_special_port
fn waitpid
fn semaphore_create
fn mach_memory_object_memory_entry_64
fn slot_name
fn fesetenv
fn strtok
fn fseeko
fn localtime_r
fn chdir
fn clock_set_time
fn wcschr
fn mach_voucher_extract_attr_recipe_trap
fn NSCreateObjectFileImageFromMemory
fn poll
fn thread_wire
fn strcat
fn mach_port_allocate_qos
fn _dyld_image_containing_address
fn task_get_mach_voucher
fn posix_spawn
fn wmemcmp
fn task_assign
fn listxattr
fn wcscmp
fn inet_addr
fn getpgid
fn munlock
fn NXSwapLittleLongLongToHost
fn _OSWriteSwapInt32
fn mig_strncpy
fn vm_copy
fn clock
fn _dyld_image_count
fn rand
fn unsetenv
fn wcscoll
fn gmtime
fn __math_errhandling
fn isalpha
fn fwide
fn _host_page_size
fn siglongjmp
fn mach_port_names
fn memcpy
fn memcmp
fn tcsetpgrp
fn msync
fn posix_spawnattr_setsigmask
fn setpgrp
fn truncate
fn host_default_memory_manager
fn processor_control
fn vm_msync
fn mach_port_space_basic_info
fn ctime_r
fn readdir_r
fn wcsncat
fn tcsetattr
fn mach_port_get_set_status
fn NSLookupAndBindSymbolWithHint
fn setjmp
fn sigwait
fn putc_unlocked
fn mbsrtowcs
fn fnmatch
fn sched_get_priority_min
fn isprint
fn fchown
fn cfsetospeed
fn task_zone_info
fn task_swap_mach_voucher
fn task_map_corpse_info
fn host_lockgroup_info
fn NSAddressOfSymbol
fn NSLinkModule
fn task_get_dyld_image_infos
fn task_inspect
fn futimens
fn srandom
fn fesetexceptflag
fn fgetws
fn posix_spawnattr_getsigdefault
fn setitimer
fn utimes
fn iswphonogram
fn vm_stats
fn mblen
fn fdopendir
fn task_get_emulation_vector
fn aligned_alloc
fn wctob
fn utimensat
fn sched_yield
fn sem_wait
fn thread_set_exception_ports
fn host_security_set_task_token
fn wcpncpy
fn host_get_multiuser_config_flags
fn NXSwapBigLongLongToHost
fn NSSymbolReferenceCountInObjectFileImage
fn vswprintf
fn kevent
fn mbrlen
fn getpriority
fn link
fn sigaction
fn puts
fn getline
fn feupdateenv
fn llabs
fn if_nametoindex
fn vfprintf
fn posix_spawnattr_init
fn readlink
fn task_set_info
fn fflush
fn thread_get_state
fn mach_port_allocate
fn mach_port_allocate_full
fn vm_region_recurse
fn macx_backing_store_recovery
fn lstat
fn mach_port_get_service_port_info
fn sigpause
fn dup
fn strncat
fn posix_spawn_file_actions_addchdir
fn _OSReadInt64
fn creat
fn NXSwapHostShortToLittle
fn _OSWriteInt32
fn _OSReadSwapInt64
fn _kernelrpc_mach_port_extract_member_trap
fn longjmp
fn processor_start
fn sigfillset
fn tolower
fn task_set_mach_voucher
fn wcstombs
fn rand_r
fn vwscanf
fn fchownat
fn thread_sample
fn mach_port_set_attributes
fn mach_port_construct
fn isblank
fn pthread_getconcurrency
fn host_request_notification
fn select
fn clock_sleep_trap
fn inet_pton
fn macx_backing_store_suspend
fn NXSwapHostLongToLittle
fn NSVersionOfRunTimeLibrary
fn mach_zone_info_for_zone
fn strcmp
fn mach_port_deallocate
fn thread_abort
fn write
fn execve
fn quick_exit
fn wcsdup
fn setegid
fn processor_set_statistics
fn putchar
fn host_set_exception_ports
fn pause
fn mach_port_get_refs
fn lrand48
fn getaddrinfo
fn _OSWriteInt64
fn thread_get_mach_voucher
fn mach_port_set_seqno
fn mach_port_extract_member
fn task_name_for_pid
fn getnetent
fn mach_error_type
fn mach_msg_receive
fn putwchar
fn cfsetispeed
fn posix_memalign
fn getservbyname
fn clearerr
fn close
fn vm_remap_new
fn thread_get_assignment
fn clock_gettime
fn seekdir
fn towupper
fn wcsstr
fn NXSwapLongLong
fn strspn
fn ___runetype
fn stat
fn getnetbyaddr
fn task_test_async_upcall_propagation
fn getpid
fn strcasecmp
fn mach_vm_region_info
fn jrand48
fn setreuid
fn strstr
fn sigrelse
fn ___toupper
fn lcong48
fn setgroupent
fn getsubopt
fn closedir
fn alphasort
fn task_sample
fn setsid
fn task_policy
fn strtoull
fn _kernelrpc_mach_port_get_attributes_trap
fn sigprocmask
fn time
fn setnetent
fn getc_unlocked
fn umask
fn shutdown
fn cfgetispeed
fn getpwent
fn getgroups
fn tcflush
fn mbstowcs
fn clock_get_res
fn NXSwapHostIntToBig
fn kevent64
fn ftello
fn NXSwapLong
fn wctrans
fn vm_wire
fn getgrent
fn fstatvfs
fn regcomp
fn msgctl
fn host_get_io_main
fn unlinkat
fn iconv
fn inet_ntop
fn _dyld_get_image_vmaddr_slide
fn NSIsSymbolNameDefined
fn dirname
fn lchown
fn munmap
fn removexattr
fn getchar_unlocked
fn getgrgid
fn _kernelrpc_mach_port_unguard_trap
fn aio_read
fn pthread_sigmask
fn macx_swapoff
fn posix_spawnattr_destroy
fn mig_put_reply_port
fn vm_machine_attribute
fn mach_memory_info
fn socket
fn aio_cancel
fn getgid
fn host_reboot
fn processor_set_max_priority
fn thread_adopt_exception_handler
fn __sigbits
fn iswgraph
fn fclonefileat
fn feraiseexcept
fn task_register_hardened_exception_handler
fn statvfs
fn semaphore_signal_thread
fn setgrent
fn imaxabs
fn ftell
fn shmat
fn isupper
fn NXSwapBigLongToHost
fn iswspace
fn semaphore_timedwait
fn putenv
fn mig_dealloc_reply_port
fn NSGetSectionDataInObjectFileImage
fn clock_sleep
fn mprotect
fn _tlv_bootstrap
fn fputs
fn stpncpy
fn vfscanf
fn atoi
fn vm_allocate_cpm
fn strerror
fn posix_spawn_file_actions_addopen
fn clock_set_attributes
fn host_get_clock_control
fn dlsym
fn processor_set_policy_enable
fn iswalnum
fn telldir
fn getopt
fn l64a
fn __NDR_convert__mig_reply_error_t
fn freelocale
fn vsnprintf
fn __tolower
fn atomic_flag_clear
fn strtol
fn sendto
fn vm_protect
fn mach_port_unguard
fn mach_memory_object_memory_entry
fn ___tolower
fn fegetenv
fn wctomb
fn host_security_create_task_token
fn task_policy_get
fn mach_port_extract_right
fn NSSymbolReferenceNameInObjectFileImage
fn fgetxattr
fn mktemp
fn strncmp
fn strxfrm
fn symlinkat
fn ctime
fn munlockall
fn tempnam
fn mach_vm_wire
fn task_info
fn task_set_special_port
fn panic_init
fn mig_reply_setup
fn macx_swapon
fn NSLibraryNameForModule
fn sigpending
fn iswcntrl
fn _OSSwapInt16
fn task_register_dyld_shared_cache_image_info
fn _kernelrpc_mach_vm_map_trap
fn _OSWriteSwapInt16
fn chown
fn fesetround
fn fetestexcept
fn mbtowc
fn getnameinfo
fn pthread_setconcurrency
fn getpwnam_r
fn fchdir
fn task_wire
fn stpcpy
fn times
fn utime
fn thread_create_running
fn getpwuid
fn setlocale
fn host_priv_statistics
fn wcsncasecmp
fn sigsuspend
fn setbuf
fn wcrtomb
fn wcsncmp
fn pwrite
fn chmod
fn siginterrupt
fn ftok
fn isalnum
fn vm_map
fn dlclose
fn if_indextoname
fn mach_port_allocate_name
fn _OSSwapInt32
fn tcsendbreak
fn task_dyld_process_info_notify_get
fn _kernelrpc_mach_port_deallocate_trap
fn _kernelrpc_mach_port_destruct_trap
fn strtoul
fn thread_switch
fn NXSwapHostShortToBig
fn iswctype
fn mig_deallocate
fn vm_region_recurse_64
fn getwchar
fn getsockopt
fn vm_map_64
fn mach_port_guard_with_flags
fn host_processors
fn mach_port_peek
fn vfwscanf
fn sched_get_priority_max
fn NSIsSymbolNameDefinedWithHint
fn semaphore_wait
fn _kernelrpc_mach_port_allocate_trap
fn mach_port_swap_guard
fn getpwuid_r
fn _kernelrpc_mach_port_guard_trap
fn swtch_pri
fn killpg
fn sem_trywait
fn NSLookupSymbolInImage
fn setsockopt
fn __assert_rtn
fn NSIsSymbolNameDefinedInImage
fn mach_error
fn _dyld_lookup_and_bind_with_hint
fn _NSGetExecutablePath
fn _OSReadSwapInt32
fn strdup
fn endpwent
fn rmdir
fn nice
fn semaphore_timedwait_signal
fn _kernelrpc_mach_port_construct_trap
fn sigismember
fn mig_allocate
fn sem_unlink
fn ttyname
fn aio_write
fn _kernelrpc_mach_port_type_trap
fn NSSymbolDefinitionCountInObjectFileImage
fn posix_spawn_file_actions_adddup2
fn mknodat
fn setpgid
fn vm_map_page_query
fn NXSwapInt
fn strptime
fn getc
fn wait
fn a64l
fn initstate
fn random
fn wcsrtombs
fn islower
fn strncasecmp
fn getppid
fn wctype
fn vfwprintf
fn readlinkat
fn nl_langinfo
fn processor_set_policy_control
fn mach_port_get_attributes
fn host_set_atm_diagnostic_flag
fn setpriority
fn kmod_control
fn NSNameOfSymbol
fn mach_port_insert_member
fn fgetpos
fn posix_spawnattr_getflags
fn crypt
fn task_assign_default
fn setservent
fn _kernelrpc_mach_vm_protect_trap
fn host_processor_info
fn voucher_mach_msg_revert
fn getpeername
fn timespec_get
fn sleep
fn mach_port_mod_refs
fn NSLookupAndBindSymbol
fn host_processor_set_priv
fn sigaddset
fn seed48
fn grantpt
fn host_register_mach_voucher_attr_manager
fn iswdigit
fn wcsnlen
fn accept
fn vm_map_exec_lockdown
fn host_get_atm_diagnostic_flag
fn iswascii
fn atomic_flag_clear_explicit
fn readdir
fn hsearch
fn mach_ports_lookup
fn _kernelrpc_mach_port_insert_right_trap
fn strnlen
fn vswscanf
fn NXSwapHostLongLongToLittle
fn vm_read_list
fn gets
fn mig_strncpy_zerofill
fn mach_port_request_notification
fn _OSReadInt16
fn _dyld_launched_prebound
fn clonefile
fn kmod_create
fn fork
fn semop
fn posix_spawnattr_setsigdefault
fn mach_port_kobject
fn fgets
fn task_set_exc_guard_behavior
fn getenv
fn wcscpy
fn regerror
fn remque
fn wcstoll
fn gethostent
fn isatty
fn mlockall
fn mkdir
fn task_set_phys_footprint_limit
fn iswpunct
fn vm_purgable_control
fn host_processor_sets
fn swtch
fn srand
fn NXHostByteOrder
fn fpathconf
fn setpwent
fn iswspecial
fn main
  bb0 bb0
    alloca Virtual { id: 34, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 35, bank: General, size_bits: 64 }, 1
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println), symbol(__const_data_0), 10
    intrinsic.call symbol(intrinsic.println), symbol(__const_data_1), 5
    intrinsic.call symbol(intrinsic.println), 1, 1, 1
    intrinsic.call symbol(intrinsic.println), symbol(__const_data_2), symbol(__const_data_3)
    intrinsic.call symbol(intrinsic.println)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 35, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 0
    br
  bb1 bb1
    alloca Virtual { id: 47, bank: General, size_bits: 64 }, 1
    load Virtual { id: 48, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 35, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    lt Virtual { id: 49, bank: General, size_bits: 8 }, Virtual { id: 48, bank: General, size_bits: 64 }, 4
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 47, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 49, bank: General, size_bits: 8 }
    load Virtual { id: 51, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 47, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 52, bank: General, size_bits: 8 }, Virtual { id: 51, bank: General, size_bits: 8 }, 1
    condbr
  bb2 bb2
    alloca Virtual { id: 53, bank: General, size_bits: 64 }, 1
    load Virtual { id: 54, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 35, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 53, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 54, bank: General, size_bits: 64 }
    alloca Virtual { id: 56, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 56, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.array)
    alloca Virtual { id: 58, bank: General, size_bits: 64 }, 1
    load Virtual { id: 59, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 35, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 58, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 59, bank: General, size_bits: 64 }
    alloca Virtual { id: 61, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 61, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.array)
    load Virtual { id: 63, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 53, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    mul Virtual { id: 64, bank: General, size_bits: 64 }, Virtual { id: 63, bank: General, size_bits: 64 }, 16
    bitcast Virtual { id: 65, bank: General, size_bits: 64 }, Virtual { id: 56, bank: General, size_bits: 64 }
    gep Virtual { id: 66, bank: General, size_bits: 64 }, Virtual { id: 65, bank: General, size_bits: 64 }, Virtual { id: 64, bank: General, size_bits: 64 }
    bitcast Virtual { id: 67, bank: General, size_bits: 64 }, Virtual { id: 66, bank: General, size_bits: 64 }
    bitcast Virtual { id: 68, bank: General, size_bits: 64 }, Virtual { id: 67, bank: General, size_bits: 64 }
    load Virtual { id: 69, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 68, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 70, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 58, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    mul Virtual { id: 71, bank: General, size_bits: 64 }, Virtual { id: 70, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 72, bank: General, size_bits: 64 }, Virtual { id: 61, bank: General, size_bits: 64 }
    gep Virtual { id: 73, bank: General, size_bits: 64 }, Virtual { id: 72, bank: General, size_bits: 64 }, Virtual { id: 71, bank: General, size_bits: 64 }
    bitcast Virtual { id: 74, bank: General, size_bits: 64 }, Virtual { id: 73, bank: General, size_bits: 64 }
    load Virtual { id: 75, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 74, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 69, bank: General, size_bits: 64 }, Virtual { id: 75, bank: General, size_bits: 64 }
    load Virtual { id: 77, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 35, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 78, bank: General, size_bits: 64 }, Virtual { id: 77, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 35, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 78, bank: General, size_bits: 64 }
    br
  bb3 bb3
    intrinsic.call symbol(intrinsic.println), 19
    alloca Virtual { id: 81, bank: General, size_bits: 64 }, 1
    eq Virtual { id: 82, bank: General, size_bits: 8 }, 10, 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 81, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 82, bank: General, size_bits: 8 }
    alloca Virtual { id: 84, bank: General, size_bits: 64 }, 1
    gt Virtual { id: 85, bank: General, size_bits: 8 }, 10, 5
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 84, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 85, bank: General, size_bits: 8 }
    load Virtual { id: 87, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 81, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 88, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 84, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 87, bank: General, size_bits: 8 }, Virtual { id: 88, bank: General, size_bits: 8 }
    intrinsic.call symbol(intrinsic.println), symbol(__const_data_8)
    alloca Virtual { id: 91, bank: General, size_bits: 64 }, 1
    gt Virtual { id: 92, bank: General, size_bits: 8 }, 10, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 91, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 92, bank: General, size_bits: 8 }
    load Virtual { id: 94, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 91, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 95, bank: General, size_bits: 8 }, Virtual { id: 94, bank: General, size_bits: 8 }, 1
    condbr
  bb4 bb4
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 34, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 256
    br
  bb5 bb5
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 34, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 128
    br
  bb6 bb6
    load Virtual { id: 98, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 34, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 98, bank: General, size_bits: 64 }
    ret


Symbols:
  main                             0x00000000

Text relocations:
  offset=0x00000030 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000003c kind=CallRel32 symbol=printf addend=0
  offset=0x00000040 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000004c kind=CallRel32 symbol=printf addend=0
  offset=0x00000050 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000005c kind=CallRel32 symbol=printf addend=0
  offset=0x00000060 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000006c kind=CallRel32 symbol=printf addend=0
  offset=0x00000070 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000007c kind=CallRel32 symbol=printf addend=0
  offset=0x00000080 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000008c kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x00000094 kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x000000ac kind=CallRel32 symbol=printf addend=0
  offset=0x000000b0 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000000bc kind=Aarch64AdrpAdd symbol=__const_data_1 addend=0
  offset=0x000000c4 kind=Aarch64AdrpAdd symbol=__const_data_1 addend=0
  offset=0x000000dc kind=CallRel32 symbol=printf addend=0
  offset=0x000000e0 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000110 kind=CallRel32 symbol=printf addend=0
  offset=0x00000114 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000120 kind=Aarch64AdrpAdd symbol=__const_data_2 addend=0
  offset=0x00000128 kind=Aarch64AdrpAdd symbol=__const_data_2 addend=0
  offset=0x00000134 kind=Aarch64AdrpAdd symbol=__const_data_3 addend=0
  offset=0x0000013c kind=Aarch64AdrpAdd symbol=__const_data_3 addend=0
  offset=0x00000148 kind=CallRel32 symbol=printf addend=0
  offset=0x0000014c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000158 kind=CallRel32 symbol=printf addend=0
  offset=0x00000204 kind=Aarch64AdrpAdd symbol=__const_data_4 addend=0
  offset=0x00000230 kind=Aarch64AdrpAdd symbol=__const_data_5 addend=0
  offset=0x0000025c kind=Aarch64AdrpAdd symbol=__const_data_6 addend=0
  offset=0x00000288 kind=Aarch64AdrpAdd symbol=__const_data_7 addend=0
  offset=0x000003e4 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000408 kind=CallRel32 symbol=printf addend=0
  offset=0x00000434 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000044c kind=CallRel32 symbol=printf addend=0
  offset=0x000004b8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000004dc kind=CallRel32 symbol=printf addend=0
  offset=0x000004e0 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000004ec kind=Aarch64AdrpAdd symbol=__const_data_8 addend=0
  offset=0x000004f4 kind=Aarch64AdrpAdd symbol=__const_data_8 addend=0
  offset=0x00000500 kind=CallRel32 symbol=printf addend=0
  offset=0x00000584 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000059c kind=CallRel32 symbol=printf addend=0

Section relocations:
  section=Data offset=0x00000000 kind=Abs64 symbol=__const_data_0 addend=0
  section=Data offset=0x00000010 kind=Abs64 symbol=__const_data_1 addend=0
  section=Data offset=0x00000020 kind=Abs64 symbol=__const_data_2 addend=0
  section=Data offset=0x00000030 kind=Abs64 symbol=__const_data_3 addend=0
  section=Data offset=0x00000040 kind=Abs64 symbol=__const_data_4 addend=0
  section=Data offset=0x00000050 kind=Abs64 symbol=__const_data_5 addend=0
  section=Data offset=0x00000060 kind=Abs64 symbol=__const_data_6 addend=0
  section=Data offset=0x00000070 kind=Abs64 symbol=__const_data_7 addend=0
  section=Data offset=0x00000080 kind=Abs64 symbol=__const_data_8 addend=0

.text (1468 bytes):
  00000000  ff 03 10 d1 f0 03 00 91  10 c2 0f 91 1d 7a 00 a9 
  00000010  fd 03 00 91 1f 20 03 d5  f0 03 00 91 10 42 0d 91 
  00000020  f0 13 00 f9 f0 03 00 91  10 62 0d 91 f0 17 00 f9 
  00000030  00 00 00 90 00 00 00 91  00 40 02 91 00 00 00 94 
  00000040  00 00 00 90 00 00 00 91  00 e0 02 91 00 00 00 94 
  00000050  00 00 00 90 00 00 00 91  00 e0 03 91 00 00 00 94 
  00000060  00 00 00 90 00 00 00 91  00 a0 04 91 00 00 00 94 
  00000070  00 00 00 90 00 00 00 91  00 40 05 91 00 00 00 94 
  00000080  00 00 00 90 00 00 00 91  00 60 05 91 01 00 00 90 
  00000090  21 00 00 91 10 00 00 90  10 02 00 91 f0 03 00 f9 
  000000a0  42 01 80 d2 50 01 80 d2  f0 07 00 f9 00 00 00 94 
  000000b0  00 00 00 90 00 00 00 91  00 c0 05 91 01 00 00 90 
  000000c0  21 00 00 91 10 00 00 90  10 02 00 91 f0 03 00 f9 
  000000d0  a2 00 80 d2 b0 00 80 d2  f0 07 00 f9 00 00 00 94 
  000000e0  00 00 00 90 00 00 00 91  00 20 06 91 21 00 80 d2 
  000000f0  30 00 80 d2 f0 03 00 f9  22 00 80 d2 30 00 80 d2 
  00000100  f0 07 00 f9 23 00 80 d2  30 00 80 d2 f0 0b 00 f9 
  00000110  00 00 00 94 00 00 00 90  00 00 00 91 00 e0 06 91 
  00000120  01 00 00 90 21 00 00 91  10 00 00 90 10 02 00 91 
  00000130  f0 03 00 f9 02 00 00 90  42 00 00 91 10 00 00 90 
  00000140  10 02 00 91 f0 07 00 f9  00 00 00 94 00 00 00 90 
  00000150  00 00 00 91 00 60 07 91  00 00 00 94 f1 17 40 f9 
  00000160  10 00 80 d2 30 02 00 f9  01 00 00 14 f0 03 00 91 
  00000170  10 82 0d 91 f0 47 00 f9  f0 17 40 f9 11 02 40 f9 
  00000180  f1 4b 00 f9 f0 4b 40 f9  1f 12 00 f1 f0 a7 9f 9a 
  00000190  f0 4f 00 f9 f1 47 40 f9  f0 63 42 39 30 02 00 39 
  000001a0  f0 47 40 f9 11 02 40 39  f1 57 00 f9 f0 a3 42 39 
  000001b0  1f 06 00 f1 f0 17 9f 9a  f0 5b 00 f9 f0 5b 40 f9 
  000001c0  1f 02 00 f1 41 00 00 54  9b 00 00 14 f0 03 00 91 
  000001d0  10 a2 0d 91 f0 5f 00 f9  f0 17 40 f9 11 02 40 f9 
  000001e0  f1 63 00 f9 f1 5f 40 f9  f0 63 40 f9 30 02 00 f9 
  000001f0  f0 03 00 91 10 c2 0d 91  f0 6b 00 f9 f1 6b 40 f9 
  00000200  e9 03 11 aa 10 00 00 90  10 02 00 91 30 01 00 f9 
  00000210  b0 00 80 d2 10 00 a0 f2  10 00 c0 f2 10 00 e0 f2 
  00000220  29 21 00 91 30 01 00 f9  e9 03 11 aa 29 41 00 91 
  00000230  10 00 00 90 10 02 00 91  30 01 00 f9 90 00 80 d2 
  00000240  10 00 a0 f2 10 00 c0 f2  10 00 e0 f2 29 21 00 91 
  00000250  30 01 00 f9 e9 03 11 aa  29 81 00 91 10 00 00 90 
  00000260  10 02 00 91 30 01 00 f9  b0 00 80 d2 10 00 a0 f2 
  00000270  10 00 c0 f2 10 00 e0 f2  29 21 00 91 30 01 00 f9 
  00000280  e9 03 11 aa 29 c1 00 91  10 00 00 90 10 02 00 91 
  00000290  30 01 00 f9 b0 00 80 d2  10 00 a0 f2 10 00 c0 f2 
  000002a0  10 00 e0 f2 29 21 00 91  30 01 00 f9 f0 03 00 91 
  000002b0  10 c2 0e 91 f0 73 00 f9  f0 17 40 f9 11 02 40 f9 
  000002c0  f1 77 00 f9 f1 73 40 f9  f0 77 40 f9 30 02 00 f9 
  000002d0  f0 03 00 91 10 e2 0e 91  f0 7f 00 f9 f1 7f 40 f9 
  000002e0  b0 00 80 d2 10 00 a0 f2  10 00 c0 f2 10 00 e0 f2 
  000002f0  e9 03 11 aa 30 01 00 f9  90 00 80 d2 10 00 a0 f2 
  00000300  10 00 c0 f2 10 00 e0 f2  e9 03 11 aa 29 21 00 91 
  00000310  30 01 00 f9 b0 00 80 d2  10 00 a0 f2 10 00 c0 f2 
  00000320  10 00 e0 f2 e9 03 11 aa  29 41 00 91 30 01 00 f9 
  00000330  b0 00 80 d2 10 00 a0 f2  10 00 c0 f2 10 00 e0 f2 
  00000340  e9 03 11 aa 29 61 00 91  30 01 00 f9 f0 5f 40 f9 
  00000350  11 02 40 f9 f1 87 00 f9  f0 87 40 f9 11 02 80 d2 
  00000360  10 7e 11 9b f0 8b 00 f9  f0 6b 40 f9 f0 8f 00 f9 
  00000370  f0 8f 40 f9 f1 8b 40 f9  10 02 11 8b f0 93 00 f9 
  00000380  f0 93 40 f9 f0 97 00 f9  f0 97 40 f9 f0 9b 00 f9 
  00000390  f0 9b 40 f9 11 02 40 f9  f1 9f 00 f9 f0 73 40 f9 
  000003a0  11 02 40 f9 f1 a3 00 f9  f0 a3 40 f9 11 01 80 d2 
  000003b0  10 7e 11 9b f0 a7 00 f9  f0 7f 40 f9 f0 ab 00 f9 
  000003c0  f0 ab 40 f9 f1 a7 40 f9  10 02 11 8b f0 af 00 f9 
  000003d0  f0 af 40 f9 f0 b3 00 f9  f0 b3 40 f9 11 02 40 f9 
  000003e0  f1 b7 00 f9 00 00 00 90  00 00 00 91 00 80 07 91 
  000003f0  e1 9f 40 f9 f0 9f 40 f9  f0 03 00 f9 e2 b7 40 f9 
  00000400  f0 b7 40 f9 f0 07 00 f9  00 00 00 94 f0 17 40 f9 
  00000410  11 02 40 f9 f1 bf 00 f9  f0 bf 40 f9 10 06 00 91 
  00000420  f0 c3 00 f9 f1 17 40 f9  f0 c3 40 f9 30 02 00 f9 
  00000430  4f ff ff 17 00 00 00 90  00 00 00 91 00 e0 07 91 
  00000440  61 02 80 d2 70 02 80 d2  f0 03 00 f9 00 00 00 94 
  00000450  f0 03 00 91 10 62 0f 91  f0 cf 00 f9 50 01 80 d2 
  00000460  1f 02 00 f1 f0 17 9f 9a  f0 d3 00 f9 f1 cf 40 f9 
  00000470  f0 83 46 39 30 02 00 39  f0 03 00 91 10 82 0f 91 
  00000480  f0 db 00 f9 50 01 80 d2  1f 16 00 f1 f0 d7 9f 9a 
  00000490  f0 df 00 f9 f1 db 40 f9  f0 e3 46 39 30 02 00 39 
  000004a0  f0 cf 40 f9 11 02 40 39  f1 e7 00 f9 f0 db 40 f9 
  000004b0  11 02 40 39 f1 eb 00 f9  00 00 00 90 00 00 00 91 
  000004c0  00 40 08 91 e1 23 47 39  f0 23 47 39 f0 03 00 f9 
  000004d0  e2 43 47 39 f0 43 47 39  f0 07 00 f9 00 00 00 94 
  000004e0  00 00 00 90 00 00 00 91  00 a0 08 91 01 00 00 90 
  000004f0  21 00 00 91 10 00 00 90  10 02 00 91 f0 03 00 f9 
  00000500  00 00 00 94 f0 03 00 91  10 a2 0f 91 f0 f7 00 f9 
  00000510  50 01 80 d2 1f 22 00 f1  f0 d7 9f 9a f0 fb 00 f9 
  00000520  f1 f7 40 f9 f0 c3 47 39  30 02 00 39 f0 f7 40 f9 
  00000530  11 02 40 39 f1 03 01 f9  f0 03 48 39 1f 06 00 f1 
  00000540  f0 17 9f 9a f0 07 01 f9  f0 07 41 f9 1f 02 00 f1 
  00000550  41 00 00 54 05 00 00 14  f1 13 40 f9 10 20 80 d2 
  00000560  30 02 00 f9 05 00 00 14  f1 13 40 f9 10 10 80 d2 
  00000570  30 02 00 f9 01 00 00 14  f0 13 40 f9 11 02 40 f9 
  00000580  f1 13 01 f9 00 00 00 90  00 00 00 91 00 e0 08 91 
  00000590  e1 13 41 f9 f0 13 41 f9  f0 03 00 f9 00 00 00 94 
  000005a0  bf 03 00 91 f0 03 00 91  10 c2 0f 91 1d 7a 40 a9 
  000005b0  ff 03 10 91 00 00 80 d2  c0 03 5f d6 

.rodata (586 bytes):
  00000000  46 65 72 72 6f 50 68 61  73 65 00 30 2e 31 2e 30 
  00000010  00 00 00 00 00 00 00 00  0a 00 00 00 00 00 00 00 
  00000020  05 00 00 00 00 00 00 00  01 01 01 46 65 72 72 6f 
  00000030  00 50 68 61 73 65 00 61  6c 70 68 61 00 62 65 74 
  00000040  61 00 67 61 6d 6d 61 00  64 65 6c 74 61 00 00 00 
  00000050  05 00 00 00 00 00 00 00  04 00 00 00 00 00 00 00 
  00000060  05 00 00 00 00 00 00 00  05 00 00 00 00 00 00 00 
  00000070  13 00 00 00 00 00 00 00  46 65 72 72 6f 50 68 61 
  00000080  73 65 20 76 30 2e 31 2e  30 00 00 00 00 00 00 00 
  00000090  f0 9f 93 98 20 54 75 74  6f 72 69 61 6c 3a 20 30 
  000000a0  32 5f 73 74 72 69 6e 67  5f 70 72 6f 63 65 73 73 
  000000b0  69 6e 67 2e 66 70 0a 00  f0 9f a7 ad 20 46 6f 63 
  000000c0  75 73 3a 20 43 6f 6d 70  69 6c 65 2d 74 69 6d 65 
  000000d0  20 73 74 72 69 6e 67 20  6f 70 65 72 61 74 69 6f 
  000000e0  6e 73 20 61 6e 64 20 69  6e 74 72 69 6e 73 69 63 
  000000f0  73 0a 00 00 00 00 00 00  f0 9f a7 aa 20 57 68 61 
  00000100  74 20 74 6f 20 6c 6f 6f  6b 20 66 6f 72 3a 20 6c 
  00000110  61 62 65 6c 65 64 20 6f  75 74 70 75 74 73 20 62 
  00000120  65 6c 6f 77 0a 00 00 00  e2 9c 85 20 45 78 70 65 
  00000130  63 74 61 74 69 6f 6e 3a  20 6f 75 74 70 75 74 73 
  00000140  20 6d 61 74 63 68 20 6c  61 62 65 6c 73 0a 00 00 
  00000150  0a 00 00 00 00 00 00 00  6e 61 6d 65 3d 27 25 73 
  00000160  27 20 6c 65 6e 3d 25 6c  6c 75 0a 00 00 00 00 00 
  00000170  76 65 72 73 69 6f 6e 3d  27 25 73 27 20 6c 65 6e 
  00000180  3d 25 6c 6c 75 0a 00 00  70 72 65 66 69 78 5f 6f 
  00000190  6b 3d 25 64 2c 20 73 75  66 66 69 78 5f 6f 6b 3d 
  000001a0  25 64 2c 20 63 6f 6e 74  61 69 6e 73 5f 70 68 61 
  000001b0  73 65 3d 25 64 0a 00 00  73 6c 69 63 65 73 3a 20 
  000001c0  73 68 6f 72 74 3d 27 25  73 27 20 74 61 69 6c 3d 
  000001d0  27 25 73 27 0a 00 00 00  77 6f 72 64 73 3a 0a 00 
  000001e0  20 20 25 73 20 2d 3e 20  6c 65 6e 3d 25 6c 6c 75 
  000001f0  0a 00 00 00 00 00 00 00  74 6f 74 61 6c 20 77 6f 
  00000200  72 64 20 6c 65 6e 67 74  68 3d 25 6c 6c 75 0a 00 
  00000210  65 6d 70 74 79 3d 25 64  2c 20 6c 6f 6e 67 3d 25 
  00000220  64 0a 00 00 00 00 00 00  62 61 6e 6e 65 72 3d 27 
  00000230  25 73 27 0a 00 00 00 00  62 75 66 66 65 72 5f 73 
  00000240  69 7a 65 3d 25 6c 6c 75  0a 00 
