fp-native dump: format=MachO arch=Aarch64 entry=0x750

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
global __const_data_0 ty=Array(I8, 5) constant=true initializer=Some(Bytes([68, 97, 116, 97, 0]))
global DATA_TYPE_NAME ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0]))
global DATA_TYPE_NAME ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0]))
global __const_data_1 ty=Array(I8, 4) constant=true initializer=Some(Bytes([105, 54, 52, 0]))
global DATA_FIELD_A_TYPE ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0]))
global DATA_FIELD_A_TYPE ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0]))
global __const_data_2 ty=Array(I8, 3) constant=true initializer=Some(Bytes([117, 56, 0]))
global HEADER_FIELD_VERSION_TYPE ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0]))
global HEADER_FIELD_VERSION_TYPE ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0]))
global MAX_SIZE ty=I64 constant=true initializer=Some(Bytes([64, 0, 0, 0, 0, 0, 0, 0]))
global MAX_SIZE ty=I64 constant=true initializer=Some(Bytes([64, 0, 0, 0, 0, 0, 0, 0]))
fn execvp
fn strncmp
fn wcwidth
fn fputws
fn mach_port_set_attributes
fn timespec_get
fn gethostent
fn strnlen
fn wcrtomb
fn uselocale
fn task_register_dyld_set_dyld_state
fn getentropy
fn bind
fn vm_remap_new
fn isxdigit
fn recv
fn tcgetpgrp
fn task_register_dyld_image_infos
fn thread_assign_default
fn iswascii
fn aio_write
fn dlsym
fn pselect
fn setlocale
fn grantpt
fn fpathconf
fn semaphore_destroy
fn putc
fn towctrans
fn endprotoent
fn iscntrl
fn posix_spawn_file_actions_adddup2
fn cfgetispeed
fn localtime
fn shmget
fn _OSReadInt16
fn inet_pton
fn setgrent
fn getnetent
fn mig_get_reply_port
fn mig_strncpy_zerofill
fn raise
fn wcsxfrm
fn host_set_UNDServer
fn task_assign_default
fn task_set_mach_voucher
fn host_page_size
fn fsetxattr
fn sem_destroy
fn processor_set_tasks
fn msgsnd
fn vm_allocate_cpm
fn processor_set_policy_disable
fn iswspecial
fn popen
fn poll
fn iswgraph
fn vm_map_64
fn mach_port_allocate_full
fn _kernelrpc_mach_vm_purgable_control_trap
fn ___runetype
fn getprotobyname
fn sem_init
fn thread_set_policy
fn vm_map_exec_lockdown
fn __vsnprintf_chk
fn host_request_notification
fn NXSwapLittleLongToHost
fn _dyld_get_image_vmaddr_slide
fn strpbrk
fn gmtime_r
fn _OSSwapInt64
fn mblen
fn atomic_thread_fence
fn feof
fn l64a
fn getgrnam_r
fn endgrent
fn close
fn dup
fn ftruncate
fn ptsname
fn host_processor_info
fn wcstoimax
fn times
fn iswpunct
fn newlocale
fn vm_region
fn vfwscanf
fn getgrnam
fn host_reboot
fn processor_info
fn vm_read_overwrite
fn getsockname
fn getc_unlocked
fn labs
fn getpeername
fn sem_trywait
fn posix_spawn_file_actions_destroy
fn getcwd
fn processor_control
fn semaphore_wait
fn NSSymbolReferenceCountInObjectFileImage
fn setgrfile
fn iconv_open
fn mach_port_rename
fn psignal
fn mach_voucher_deallocate
fn feclearexcept
fn _kernelrpc_mach_port_mod_refs_trap
fn if_nameindex
fn mach_memory_object_memory_entry
fn _OSReadInt32
fn setstate
fn pwrite
fn thread_policy
fn vfork
fn task_generate_corpse
fn fegetenv
fn mbtowc
fn thread_policy_get
fn getsubopt
fn thread_get_exception_ports
fn alarm
fn _dyld_get_image_header
fn gethostid
fn _dyld_get_image_header_containing_address
fn _setjmp
fn llabs
fn __vsprintf_chk
fn dlerror
fn lrand48
fn wcspbrk
fn dlopen
fn posix_spawn
fn msgget
fn semop
fn mig_put_reply_port
fn unlockpt
fn unsetenv
fn globfree
fn strcasecmp
fn closelog
fn quick_exit
fn wcsstr
fn _longjmp
fn setpwent
fn host_get_exception_ports
fn host_get_atm_diagnostic_flag
fn NXSwapHostLongToLittle
fn nanosleep
fn fdopendir
fn pipe
fn mach_msg_overwrite
fn vm_stats
fn task_terminate
fn wcsncpy
fn task_set_state
fn mach_vm_region_info_64
fn fesetround
fn task_inspect
fn task_resume
fn host_set_exception_ports
fn host_statistics64
fn openlog
fn _kernelrpc_mach_vm_deallocate_trap
fn setpgrp
fn task_swap_exception_ports
fn NXSwapHostLongToBig
fn host_get_special_port
fn task_info
fn tmpfile
fn mbsnrtowcs
fn feholdexcept
fn fileno
fn host_set_special_port
fn mach_port_destruct
fn wcsdup
fn atomic_signal_fence
fn __math_errhandling
fn mach_voucher_extract_attr_recipe_trap
fn getpgrp
fn insque
fn random
fn fflush
fn tzset
fn towlower
fn endnetent
fn posix_spawn_file_actions_init
fn thread_sample
fn atoll
fn mach_port_extract_member
fn mach_zone_info_for_zone
fn gettimeofday
fn slot_name
fn clonefile
fn _OSReadSwapInt64
fn NXSwapLittleLongLongToHost
fn ___tolower
fn processor_set_destroy
fn mach_vm_region_info
fn removexattr
fn strcoll
fn opendir
fn __error
fn memcmp
fn posix_spawnattr_getsigdefault
fn faccessat
fn fopen
fn linkat
fn getlogin
fn feupdateenv
fn pthread_sigmask
fn uname
fn mig_deallocate
fn thread_wire
fn symlinkat
fn socketpair
fn setuid
fn statvfs
fn strtoimax
fn iswalnum
fn processor_set_policy_control
fn clock
fn munlock
fn task_identity_token_get_task_port
fn gmtime
fn vm_region_recurse
fn mach_port_request_notification
fn __isctype
fn setjmp
fn fputwc
fn symlink
fn mig_allocate
fn ualarm
fn thread_swap_exception_ports
fn wctob
fn posix_spawnattr_destroy
fn mach_port_deallocate
fn _kernelrpc_mach_port_move_member_trap
fn dirfd
fn getrusage
fn getaddrinfo
fn dlclose
fn usleep
fn vsprintf
fn _OSWriteSwapInt32
fn vm_read
fn vm_region_64
fn mach_port_extract_right
fn processor_set_max_priority
fn wcscasecmp
fn getgrgid_r
fn host_get_clock_service
fn fseeko
fn mlockall
fn regcomp
fn wcsnlen
fn __darwin_fd_clr
fn thread_info
fn wcsncasecmp
fn OSHostByteOrder
fn strcpy
fn vm_machine_attribute
fn sigaltstack
fn mach_port_mod_refs
fn kill
fn regfree
fn ttyname
fn link
fn _dyld_bind_fully_image_containing_address
fn fstatvfs
fn strncpy
fn task_policy_get
fn pause
fn task_self_trap
fn _tlv_bootstrap
fn mach_msg_destroy
fn _dyld_present
fn sigprocmask
fn getsid
fn mktemp
fn _kernelrpc_mach_port_insert_right_trap
fn hdestroy
fn vm_write
fn _kernelrpc_mach_port_destruct_trap
fn host_check_multiuser_mode
fn isalpha
fn _kernelrpc_mach_port_allocate_trap
fn strcspn
fn thread_resume
fn remove
fn wmemset
fn mach_host_self
fn processor_set_create
fn fchownat
fn endservent
fn tcsendbreak
fn host_get_boot_info
fn vswprintf
fn nl_langinfo
fn NXSwapHostShortToBig
fn thread_get_mach_voucher
fn chdir
fn ungetc
fn sethostent
fn mach_msg_send
fn div
fn calloc
fn strdup
fn waitpid
fn towupper
fn accept
fn getgrent
fn fwide
fn setgroupent
fn tcgetattr
fn host_swap_exception_ports
fn task_register_dyld_get_process_state
fn NXSwapShort
fn NXSwapLittleIntToHost
fn _dyld_get_image_name
fn pid_for_task
fn getpriority
fn mach_port_move_member
fn atol
fn gethostname
fn atomic_flag_test_and_set_explicit
fn fread
fn readlinkat
fn task_policy_set
fn mach_port_get_set_status
fn sem_getvalue
fn fchmodat
fn NXSwapLong
fn freelocale
fn macx_swapoff
fn posix_spawnattr_setpgroup
fn task_name_for_pid
fn stpncpy
fn pathconf
fn host_create_mach_voucher
fn host_get_UNDServer
fn wcstoumax
fn getenv
fn wcsrtombs
fn wmemchr
fn pthread_key_delete
fn vm_remap
fn host_set_multiuser_config_flags
fn NXSwapLongLong
fn _dyld_image_containing_address
fn mprotect
fn mach_ports_lookup
fn iswblank
fn panic_init
fn sigismember
fn setenv
fn getprotobynumber
fn hcreate
fn strftime
fn __maskrune
fn sleep
fn strndup
fn getpwuid
fn __darwin_check_fd_set
fn thread_set_special_port
fn mach_port_allocate
fn getgid
fn vprintf
fn act_get_state
fn mach_port_get_attributes
fn isblank
fn sighold
fn iswxdigit
fn posix_spawn_file_actions_addclose
fn aio_cancel
fn posix_spawnattr_setflags
fn geteuid
fn task_dyld_process_info_notify_deregister
fn pclose
fn ftello
fn sigemptyset
fn malloc
fn iswalpha
fn getpid
fn chmod
fn getdelim
fn thread_set_state
fn msgctl
fn _kernelrpc_mach_port_insert_member_trap
fn inet_ntop
fn mach_port_names
fn _kernelrpc_mach_port_deallocate_trap
fn mach_memory_object_memory_entry_64
fn mach_port_allocate_name
fn host_default_memory_manager
fn fegetround
fn vfwprintf
fn futimens
fn host_security_create_task_token
fn semaphore_signal_thread
fn mbrtowc
fn _kernelrpc_mach_port_type_trap
fn unlinkat
fn atomic_flag_clear
fn sigwait
fn wcpncpy
fn memset
fn select
fn setnetent
fn host_processor_sets
fn mach_port_dnrequest_info
fn fgets
fn getnetbyname
fn swtch_pri
fn btowc
fn posix_spawnattr_setsigmask
fn thread_set_mach_voucher
fn sigpending
fn ftell
fn socket
fn __assert_rtn
fn fnmatch
fn regexec
fn fchmod
fn semaphore_signal
fn voucher_mach_msg_set
fn fclose
fn _kernelrpc_mach_port_request_notification_trap
fn host_info
fn processor_start
fn vm_read_list
fn wcsnrtombs
fn sigaction
fn getpwent
fn vm_purgable_control
fn __toupper
fn __darwin_fd_set
fn getchar_unlocked
fn task_register_hardened_exception_handler
fn macx_backing_store_recovery
fn killpg
fn fgetc
fn mbsrtowcs
fn execve
fn host_processor_set_priv
fn isspace
fn aligned_alloc
fn memchr
fn munmap
fn task_zone_info
fn etap_trace_thread
fn NSIsSymbolDefinedInObjectFileImage
fn rand_r
fn encrypt
fn strspn
fn feraiseexcept
fn lio_listio
fn wcstok
fn wcscoll
fn NSSymbolReferenceNameInObjectFileImage
fn strtoul
fn strerror_r
fn time
fn gets
fn lcong48
fn msgrcv
fn __darwin_check_fd_set_overflow
fn fwrite
fn setitimer
fn wcscpy
fn mach_vm_wire
fn task_set_special_port
fn lock_set_create
fn thread_abort
fn task_resume2
fn ffs
fn read
fn voucher_mach_msg_adopt
fn setvbuf
fn fremovexattr
fn wcsspn
fn _kernelrpc_mach_port_get_attributes_trap
fn sigsuspend
fn vwscanf
fn __sigbits
fn task_get_dyld_image_infos
fn cfsetospeed
fn mach_port_construct
fn NXHostByteOrder
fn __svfscanf
fn atomic_flag_clear_explicit
fn processor_assign
fn rename
fn _dyld_launched_prebound
fn clock_sleep
fn shutdown
fn tcsetpgrp
fn setsockopt
fn memccpy
fn pthread_testcancel
fn sigdelset
fn task_threads
fn task_get_exc_guard_behavior
fn iswphonogram
fn mach_thread_self
fn vdprintf
fn fsync
fn toupper
fn thread_get_exception_ports_info
fn NSCreateObjectFileImageFromMemory
fn islower
fn setegid
fn swab
fn getitimer
fn tcflow
fn mig_dealloc_reply_port
fn setpriority
fn wait
fn tcdrain
fn task_get_mach_voucher
fn iswideogram
fn task_map_kcdata_object_64
fn NSGetSectionDataInObjectFileImage
fn _OSWriteInt32
fn pread
fn mach_error
fn dirname
fn atomic_flag_test_and_set
fn lldiv
fn access
fn NSIsSymbolNameDefined
fn toascii
fn renameat
fn _OSSwapInt32
fn semget
fn listxattr
fn waitid
fn iswlower
fn vsnprintf
fn __wcwidth
fn seekdir
fn getpwnam
fn posix_spawnattr_init
fn mlock
fn thread_assign
fn processor_get_assignment
fn NSLookupSymbolInModule
fn getservbyport
fn host_create_mach_voucher_trap
fn isalnum
fn host_get_multiuser_config_flags
fn wcscmp
fn NSLookupAndBindSymbolWithHint
fn task_set_port_space
fn getuid
fn shmat
fn fmemopen
fn nice
fn iswupper
fn wcstoull
fn aio_return
fn rewinddir
fn voucher_mach_msg_revert
fn posix_openpt
fn seed48
fn isascii
fn sysconf
fn putwc
fn _OSWriteInt16
fn posix_madvise
fn kmod_control
fn __tolower
fn getgroups
fn rand
fn putwchar
fn NSAddLibraryWithSearching
fn wcstoll
fn realloc
fn setrlimit
fn semaphore_signal_all
fn unlink
fn thread_set_exception_ports
fn NSVersionOfLinkTimeLibrary
fn tmpnam
fn lock_set_destroy
fn sigignore
fn setprotoent
fn task_dyld_process_info_notify_get
fn NXSwapHostIntToLittle
fn task_purgable_info
fn strtoumax
fn ___toupper
fn munlockall
fn __istype
fn imaxdiv
fn aio_fsync
fn _OSReadSwapInt16
fn perror
fn getsockopt
fn shmdt
fn host_priv_statistics
fn semaphore_create
fn task_get_assignment
fn vm_map
fn vm_inherit
fn clock_getres
fn processor_set_tasks_with_flavor
fn thread_create_running
fn task_set_ras_pc
fn siglongjmp
fn getnetbyaddr
fn tcsetattr
fn __NDR_convert__mig_reply_error_t
fn clock_set_attributes
fn thread_get_state
fn _kernelrpc_mach_port_unguard_trap
fn mach_port_is_connection_for_service
fn task_get_emulation_vector
fn vm_behavior_set
fn macx_swapon
fn getc
fn iswctype
fn wcsrchr
fn mkfifoat
fn mach_port_type
fn fclonefileat
fn stpcpy
fn system
fn thread_suspend
fn mach_generate_activity_id
fn task_get_state
fn getservent
fn clock_get_res
fn NXSwapLittleShortToHost
fn NSAddImage
fn host_kernel_version
fn strcat
fn ftrylockfile
fn host_lockgroup_info
fn strlen
fn lockf
fn semaphore_wait_signal
fn ferror
fn __sputc
fn putc_unlocked
fn strtol
fn nrand48
fn dup2
fn thread_terminate
fn thread_abort_safely
fn sigpause
fn vfscanf
fn strncasecmp
fn task_suspend
fn mach_port_set_mscount
fn mach_port_unguard
fn NSAddressOfSymbol
fn mbsinit
fn creat
fn iswprint
fn confstr
fn gethostbyaddr
fn strchr
fn isdigit
fn strncat
fn basename
fn getline
fn posix_spawn_file_actions_addopen
fn semaphore_timedwait_signal
fn NXSwapBigShortToHost
fn iswdigit
fn putenv
fn fesetexceptflag
fn posix_spawn_file_actions_addchdir
fn mknod
fn processor_set_threads
fn _kernelrpc_mach_vm_protect_trap
fn _kernelrpc_mach_vm_map_trap
fn sigaddset
fn endpwent
fn NSCreateObjectFileImageFromFile
fn flockfile
fn strtok
fn wctomb
fn mbrlen
fn sendmsg
fn setservent
fn _OSReadSwapInt32
fn macx_triggers
fn processor_set_default
fn vfprintf
fn srand
fn ungetwc
fn getpwuid_r
fn a64l
fn wcstoul
fn _exit
fn mach_port_allocate_qos
fn utimensat
fn getegid
fn setpgid
fn iswspace
fn task_map_corpse_info_64
fn clearerr
fn posix_spawnattr_setsigdefault
fn vm_allocate
fn NXSwapDouble
fn wcsncmp
fn task_swap_mach_voucher
fn longjmp
fn setgid
fn vm_wire
fn if_freenameindex
fn mkdir
fn thread_switch
fn NXSwapFloat
fn host_register_mach_voucher_attr_manager
fn getrlimit
fn semaphore_timedwait
fn send
fn mach_memory_info
fn _dyld_lookup_and_bind_fully
fn mach_port_get_context
fn NSSymbolDefinitionNameInObjectFileImage
fn memcpy
fn fchdir
fn fetestexcept
fn NSSymbolDefinitionCountInObjectFileImage
fn thread_create
fn task_set_exception_ports
fn sched_get_priority_max
fn wctrans
fn sync
fn _OSWriteSwapInt16
fn task_policy
fn task_set_policy
fn vm_deallocate
fn mach_port_insert_right
fn NSLibraryNameForModule
fn tcflush
fn task_sample
fn getxattr
fn inet_ntoa
fn fchown
fn initstate
fn processor_set_policy_enable
fn task_create
fn mach_port_insert_member
fn debug_control_port_for_pid
fn _host_page_size
fn voucher_mach_msg_clear
fn asctime_r
fn inet_addr
fn vsscanf
fn wctype
fn wcsncat
fn mach_error_string
fn processor_set_stack_usage
fn duplocale
fn putchar
fn NXSwapInt
fn mach_task_is_self
fn _NSGetExecutablePath
fn _dyld_lookup_and_bind_with_hint
fn getprotoent
fn setxattr
fn freeaddrinfo
fn atoi
fn isgraph
fn sockatmark
fn host_get_io_main
fn getdate
fn gai_strerror
fn lchown
fn mach_msg
fn sigrelse
fn task_set_corpse_forking_behavior
fn NSLookupSymbolInImage
fn thread_get_special_port
fn recvmsg
fn NSDestroyObjectFileImage
fn ldiv
fn NSInstallLinkEditErrorHandlers
fn isprint
fn getservbyname
fn host_get_clock_control
fn strcmp
fn wcstol
fn getopt
fn _OSWriteInt64
fn task_get_exception_ports
fn mach_port_peek
fn ispunct
fn ctime_r
fn task_dyld_process_info_notify_register
fn _dyld_image_count
fn __srget
fn isupper
fn strerror
fn iconv_close
fn vm_protect
fn NSUnLinkModule
fn clock_gettime
fn task_test_async_upcall_propagation
fn NSIsSymbolNameDefinedInImage
fn NSNameOfModule
fn mach_port_get_srights
fn rewind
fn _OSSwapInt16
fn regerror
fn mach_port_destroy
fn hsearch
fn fputs
fn localeconv
fn act_set_state
fn _dyld_shared_cache_contains_path
fn posix_spawn_file_actions_addfchdir
fn NSVersionOfRunTimeLibrary
fn thread_swap_mach_voucher
fn getgrgid
fn vscanf
fn readlink
fn wmemcmp
fn cfsetispeed
fn shmctl
fn sem_post
fn task_map_corpse_info
fn vm_msync
fn mach_make_memory_entry_64
fn isatty
fn fork
fn task_set_exc_guard_behavior
fn imaxabs
fn kext_request
fn strsignal
fn alphasort
fn wcstombs
fn strtok_r
fn write
fn task_set_emulation
fn mach_port_get_service_port_info
fn host_set_atm_diagnostic_flag
fn vm_mapped_pages_info
fn kqueue
fn vm_map_page_query
fn _dyld_all_twolevel_modules_prebound
fn wcswidth
fn wmemmove
fn NSLookupAndBindSymbol
fn wcscat
fn fdopen
fn aio_error
fn readdir
fn iconv
fn ttyname_r
fn memmove
fn if_nametoindex
fn remque
fn seteuid
fn processor_set_statistics
fn open_memstream
fn crypt
fn ctime
fn open_wmemstream
fn sem_wait
fn posix_spawnp
fn localtime_r
fn srandom
fn mmap
fn lstat
fn processor_set_info
fn task_set_emulation_vector
fn thread_depress_abort
fn mach_port_space_info
fn setbuf
fn _OSReadInt64
fn mach_port_kobject_description
fn wcslen
fn mach_make_memory_entry
fn mach_zone_info
fn task_set_info
fn abort
fn task_test_sync_upcall
fn ftok
fn mach_port_guard_with_flags
fn getwchar
fn macx_backing_store_suspend
fn free
fn chown
fn mach_ports_register
fn lseek
fn recvfrom
fn mach_port_guard
fn host_virtual_physical_table_info
fn iswhexnumber
fn utimes
fn mig_strncpy
fn fstatat
fn posix_memalign
fn NXSwapHostLongLongToLittle
fn task_get_exception_ports_info
fn getppid
fn mach_port_assert_attributes
fn wcschr
fn endhostent
fn pthread_getconcurrency
fn NXSwapBigLongLongToHost
fn clock_set_res
fn utime
fn mkdirat
fn _OSWriteSwapInt64
fn mach_error_type
fn mach_msg_receive
fn posix_spawnattr_getpgroup
fn vm_region_recurse_64
fn pthread_kill
fn jrand48
fn setkey
fn posix_spawnattr_getsigmask
fn kmod_create
fn _kernelrpc_mach_port_extract_member_trap
fn mach_vm_reclaim_update_kernel_accounting_trap
fn NXSwapHostShortToLittle
fn puts
fn getlogin_r
fn tcgetsid
fn task_register_dyld_shared_cache_image_info
fn host_register_well_known_mach_voucher_attr_manager
fn NXSwapBigLongToHost
fn tolower
fn strptime
fn sigfillset
fn rmdir
fn NXSwapHostIntToBig
fn clonefileat
fn if_indextoname
fn tempnam
fn freopen
fn getwc
fn iswrune
fn sched_get_priority_min
fn setlogmask
fn host_security_set_task_token
fn kevent
fn telldir
fn fseek
fn mktime
fn listen
fn thread_get_assignment
fn mach_port_set_context
fn wcsftime
fn host_statistics
fn NSNameOfSymbol
fn vswscanf
fn fgetxattr
fn strtoull
fn flistxattr
fn getpgid
fn NSLinkModule
fn closedir
fn stat
fn umask
fn ctermid
fn host_processors
fn mach_port_get_refs
fn wcscspn
fn fgetpos
fn setsid
fn clock_set_time
fn sched_yield
fn _Exit
fn realpath
fn getpwnam_r
fn posix_spawnattr_getflags
fn truncate
fn strrchr
fn mkfifo
fn kevent64
fn getchar
fn sendto
fn fsetpos
fn mkstemp
fn wcpcpy
fn getnameinfo
fn siginterrupt
fn task_assign
fn thread_adopt_exception_handler
fn mach_port_kobject
fn putchar_unlocked
fn fgetwc
fn fgetws
fn pthread_setconcurrency
fn mach_port_set_seqno
fn kmod_destroy
fn abs
fn __swbuf
fn iswcntrl
fn sem_close
fn funlockfile
fn strtoll
fn fputc
fn clock_settime
fn readdir_r
fn execv
fn processor_exit
fn task_get_special_port
fn vwprintf
fn sigsetjmp
fn mbstowcs
fn asctime
fn iswnumber
fn aio_suspend
fn thread_convert_thread_state
fn vm_copy
fn strxfrm
fn task_wire
fn task_for_pid
fn mach_port_space_basic_info
fn wmemcpy
fn mach_port_swap_guard
fn task_suspend2
fn mach_port_kernel_object
fn _kernelrpc_mach_port_construct_trap
fn NXSwapBigIntToHost
fn __darwin_fd_isset
fn aio_read
fn task_create_identity_token
fn NXSwapHostLongLongToBig
fn msync
fn sem_unlink
fn _kernelrpc_mach_vm_allocate_trap
fn _kernelrpc_mach_port_guard_trap
fn swtch
fn setregid
fn _dyld_lookup_and_bind
fn kmod_get_info
fn fstat
fn gethostbyname
fn fesetenv
fn task_unregister_dyld_image_infos
fn strstr
fn NSIsSymbolNameDefinedWithHint
fn NSLinkEditError
fn fegetexceptflag
fn clock_sleep_trap
fn NSModuleForSymbol
fn thread_policy_set
fn shm_unlink
fn mrand48
fn cfgetospeed
fn exit
fn srand48
fn task_set_phys_footprint_limit
fn mknodat
fn mig_reply_setup
fn NSAddLibrary
fn connect
fn setreuid
fn __fp_comptime_const_DATA_SIZE_12496315179962112615
  bb0 bb0
    alloca Virtual { id: 0, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 32
    load Virtual { id: 2, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn __fp_comptime_const_DATA_SIZE_12496315179962112615
  bb0 bb0
    alloca Virtual { id: 3, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 3, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 32
    load Virtual { id: 5, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 3, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn __fp_comptime_const_DATA_FIELDS_2148887068295082777
  bb0 bb0
    alloca Virtual { id: 6, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 6, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 3
    load Virtual { id: 8, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 6, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn __fp_comptime_const_DATA_FIELDS_2148887068295082777
  bb0 bb0
    alloca Virtual { id: 9, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 9, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 3
    load Virtual { id: 11, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 9, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn __fp_comptime_const_DATA_HAS_A_11358022796829243658
  bb0 bb0
    alloca Virtual { id: 12, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 12, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 1
    load Virtual { id: 14, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 12, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn __fp_comptime_const_DATA_HAS_A_11358022796829243658
  bb0 bb0
    alloca Virtual { id: 15, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 15, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 1
    load Virtual { id: 17, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 15, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn __fp_comptime_const_DATA_HAS_X_17511507201974146095
  bb0 bb0
    alloca Virtual { id: 18, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 18, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 0
    load Virtual { id: 20, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 18, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn __fp_comptime_const_DATA_HAS_X_17511507201974146095
  bb0 bb0
    alloca Virtual { id: 21, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 21, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 0
    load Virtual { id: 23, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 21, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn __fp_comptime_const_HEADER_SIZE_8777257750399411709
  bb0 bb0
    alloca Virtual { id: 24, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 24, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 16
    load Virtual { id: 26, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 24, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn __fp_comptime_const_HEADER_SIZE_8777257750399411709
  bb0 bb0
    alloca Virtual { id: 27, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 27, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 16
    load Virtual { id: 29, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 27, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn __fp_comptime_const_HEADER_FIELDS_13621153736526922648
  bb0 bb0
    alloca Virtual { id: 30, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 30, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 4
    load Virtual { id: 32, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 30, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn __fp_comptime_const_HEADER_FIELDS_13621153736526922648
  bb0 bb0
    alloca Virtual { id: 33, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 33, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 4
    load Virtual { id: 35, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 33, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn __fp_comptime_const_HEADER_HAS_VERSION_14485270751929506700
  bb0 bb0
    alloca Virtual { id: 36, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 36, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 1
    load Virtual { id: 38, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 36, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn __fp_comptime_const_HEADER_HAS_VERSION_14485270751929506700
  bb0 bb0
    alloca Virtual { id: 39, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 39, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 1
    load Virtual { id: 41, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 39, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn __fp_comptime_const_HAS_TO_STRING_3418668455111666345
  bb0 bb0
    alloca Virtual { id: 42, bank: General, size_bits: 64 }, 1
    load Virtual { id: 43, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 42, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn __fp_comptime_const_HAS_TO_STRING_3418668455111666345
  bb0 bb0
    alloca Virtual { id: 44, bank: General, size_bits: 64 }, 1
    load Virtual { id: 45, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 44, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn __fp_comptime_const_DATA_OK_2676155988063261106
  bb0 bb0
    alloca Virtual { id: 46, bank: General, size_bits: 64 }, 1
    le Virtual { id: 47, bank: General, size_bits: 8 }, symbol(__fp_const_07_compile_time_validation_4), 64
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 46, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 47, bank: General, size_bits: 8 }
    load Virtual { id: 49, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 46, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn __fp_comptime_const_DATA_OK_2676155988063261106
  bb0 bb0
    alloca Virtual { id: 50, bank: General, size_bits: 64 }, 1
    le Virtual { id: 51, bank: General, size_bits: 8 }, symbol(__fp_const_07_compile_time_validation_4), 64
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 50, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 51, bank: General, size_bits: 8 }
    load Virtual { id: 53, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 50, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn __fp_comptime_const_HEADER_OK_8245564061542629924
  bb0 bb0
    alloca Virtual { id: 54, bank: General, size_bits: 64 }, 1
    le Virtual { id: 55, bank: General, size_bits: 8 }, symbol(__fp_const_07_compile_time_validation_8), 64
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 54, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 55, bank: General, size_bits: 8 }
    load Virtual { id: 57, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 54, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn __fp_comptime_const_HEADER_OK_8245564061542629924
  bb0 bb0
    alloca Virtual { id: 58, bank: General, size_bits: 64 }, 1
    le Virtual { id: 59, bank: General, size_bits: 8 }, symbol(__fp_const_07_compile_time_validation_8), 64
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 58, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 59, bank: General, size_bits: 8 }
    load Virtual { id: 61, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 58, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn __fp_comptime_const_TOTAL_SIZE_3445646378656047617
  bb0 bb0
    alloca Virtual { id: 62, bank: General, size_bits: 64 }, 1
    add Virtual { id: 63, bank: General, size_bits: 64 }, symbol(__fp_const_07_compile_time_validation_4), symbol(__fp_const_07_compile_time_validation_8)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 62, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 63, bank: General, size_bits: 64 }
    load Virtual { id: 65, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 62, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn __fp_comptime_const_TOTAL_SIZE_3445646378656047617
  bb0 bb0
    alloca Virtual { id: 66, bank: General, size_bits: 64 }, 1
    add Virtual { id: 67, bank: General, size_bits: 64 }, symbol(__fp_const_07_compile_time_validation_4), symbol(__fp_const_07_compile_time_validation_8)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 66, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 67, bank: General, size_bits: 64 }
    load Virtual { id: 69, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 66, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn __fp_comptime_const_TOTAL_OK_2863895862815852204
  bb0 bb0
    alloca Virtual { id: 70, bank: General, size_bits: 64 }, 1
    le Virtual { id: 71, bank: General, size_bits: 8 }, symbol(__fp_const_07_compile_time_validation_18), 96
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 70, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 71, bank: General, size_bits: 8 }
    load Virtual { id: 73, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 70, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn __fp_comptime_const_TOTAL_OK_2863895862815852204
  bb0 bb0
    alloca Virtual { id: 74, bank: General, size_bits: 64 }, 1
    le Virtual { id: 75, bank: General, size_bits: 8 }, symbol(__fp_const_07_compile_time_validation_18), 96
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 74, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 75, bank: General, size_bits: 8 }
    load Virtual { id: 77, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 74, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn main
  bb0 bb0
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println), symbol(__fp_const_07_compile_time_validation_4), symbol(__fp_const_07_compile_time_validation_5)
    intrinsic.call symbol(intrinsic.println), symbol(__fp_const_07_compile_time_validation_6), symbol(__fp_const_07_compile_time_validation_7)
    intrinsic.call symbol(intrinsic.println), symbol(__fp_const_07_compile_time_validation_8), symbol(__fp_const_07_compile_time_validation_9), symbol(__fp_const_07_compile_time_validation_10)
    intrinsic.call symbol(intrinsic.println), symbol(__const_data_0), symbol(__const_data_1), symbol(__const_data_2)
    intrinsic.call symbol(intrinsic.println), symbol(__fp_const_07_compile_time_validation_14)
    intrinsic.call symbol(intrinsic.println), symbol(__fp_const_07_compile_time_validation_16), symbol(__fp_const_07_compile_time_validation_17), symbol(__fp_const_07_compile_time_validation_19), symbol(__fp_const_07_compile_time_validation_18)
    ret


Symbols:
  __fp_comptime_const_DATA_SIZE_12496315179962112615 0x00000048
  __fp_comptime_const_DATA_FIELDS_2148887068295082777 0x000000d8
  __fp_comptime_const_DATA_HAS_A_11358022796829243658 0x00000168
  __fp_comptime_const_DATA_HAS_X_17511507201974146095 0x000001f8
  __fp_comptime_const_HEADER_SIZE_8777257750399411709 0x00000288
  __fp_comptime_const_HEADER_FIELDS_13621153736526922648 0x00000318
  __fp_comptime_const_HEADER_HAS_VERSION_14485270751929506700 0x000003a8
  __fp_comptime_const_HAS_TO_STRING_3418668455111666345 0x0000042c
  __fp_comptime_const_DATA_OK_2676155988063261106 0x000004c4
  __fp_comptime_const_HEADER_OK_8245564061542629924 0x0000057c
  __fp_comptime_const_TOTAL_SIZE_3445646378656047617 0x00000638
  __fp_comptime_const_TOTAL_OK_2863895862815852204 0x000006f4
  main                             0x00000750

Text relocations:
  offset=0x00000484 kind=Aarch64GotLoad symbol=__fp_const_07_compile_time_validation_4 addend=0
  offset=0x000004e0 kind=Aarch64GotLoad symbol=__fp_const_07_compile_time_validation_4 addend=0
  offset=0x0000053c kind=Aarch64GotLoad symbol=__fp_const_07_compile_time_validation_8 addend=0
  offset=0x00000598 kind=Aarch64GotLoad symbol=__fp_const_07_compile_time_validation_8 addend=0
  offset=0x000005f4 kind=Aarch64GotLoad symbol=__fp_const_07_compile_time_validation_4 addend=0
  offset=0x000005fc kind=Aarch64GotLoad symbol=__fp_const_07_compile_time_validation_8 addend=0
  offset=0x00000654 kind=Aarch64GotLoad symbol=__fp_const_07_compile_time_validation_4 addend=0
  offset=0x0000065c kind=Aarch64GotLoad symbol=__fp_const_07_compile_time_validation_8 addend=0
  offset=0x000006b4 kind=Aarch64GotLoad symbol=__fp_const_07_compile_time_validation_18 addend=0
  offset=0x00000710 kind=Aarch64GotLoad symbol=__fp_const_07_compile_time_validation_18 addend=0
  offset=0x00000760 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000076c kind=CallRel32 symbol=printf addend=0
  offset=0x00000770 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000077c kind=CallRel32 symbol=printf addend=0
  offset=0x00000780 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000078c kind=CallRel32 symbol=printf addend=0
  offset=0x00000790 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000079c kind=CallRel32 symbol=printf addend=0
  offset=0x000007a0 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000007ac kind=CallRel32 symbol=printf addend=0
  offset=0x000007b0 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000007bc kind=Aarch64GotLoad symbol=__fp_const_07_compile_time_validation_4 addend=0
  offset=0x000007c4 kind=Aarch64GotLoad symbol=__fp_const_07_compile_time_validation_4 addend=0
  offset=0x000007d0 kind=Aarch64GotLoad symbol=__fp_const_07_compile_time_validation_5 addend=0
  offset=0x000007d8 kind=Aarch64GotLoad symbol=__fp_const_07_compile_time_validation_5 addend=0
  offset=0x000007e4 kind=CallRel32 symbol=printf addend=0
  offset=0x000007e8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000007f4 kind=Aarch64GotLoad symbol=__fp_const_07_compile_time_validation_6 addend=0
  offset=0x000007fc kind=Aarch64GotLoad symbol=__fp_const_07_compile_time_validation_6 addend=0
  offset=0x00000808 kind=Aarch64GotLoad symbol=__fp_const_07_compile_time_validation_7 addend=0
  offset=0x00000810 kind=Aarch64GotLoad symbol=__fp_const_07_compile_time_validation_7 addend=0
  offset=0x0000081c kind=CallRel32 symbol=printf addend=0
  offset=0x00000820 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000082c kind=Aarch64GotLoad symbol=__fp_const_07_compile_time_validation_8 addend=0
  offset=0x00000834 kind=Aarch64GotLoad symbol=__fp_const_07_compile_time_validation_8 addend=0
  offset=0x00000840 kind=Aarch64GotLoad symbol=__fp_const_07_compile_time_validation_9 addend=0
  offset=0x00000848 kind=Aarch64GotLoad symbol=__fp_const_07_compile_time_validation_9 addend=0
  offset=0x00000854 kind=Aarch64GotLoad symbol=__fp_const_07_compile_time_validation_10 addend=0
  offset=0x0000085c kind=Aarch64GotLoad symbol=__fp_const_07_compile_time_validation_10 addend=0
  offset=0x00000868 kind=CallRel32 symbol=printf addend=0
  offset=0x0000086c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000878 kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x00000880 kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x0000088c kind=Aarch64AdrpAdd symbol=__const_data_1 addend=0
  offset=0x00000894 kind=Aarch64AdrpAdd symbol=__const_data_1 addend=0
  offset=0x000008a0 kind=Aarch64AdrpAdd symbol=__const_data_2 addend=0
  offset=0x000008a8 kind=Aarch64AdrpAdd symbol=__const_data_2 addend=0
  offset=0x000008b4 kind=CallRel32 symbol=printf addend=0
  offset=0x000008b8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000008c4 kind=Aarch64GotLoad symbol=__fp_const_07_compile_time_validation_14 addend=0
  offset=0x000008cc kind=Aarch64GotLoad symbol=__fp_const_07_compile_time_validation_14 addend=0
  offset=0x000008d8 kind=CallRel32 symbol=printf addend=0
  offset=0x000008dc kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000008e8 kind=Aarch64GotLoad symbol=__fp_const_07_compile_time_validation_16 addend=0
  offset=0x000008f0 kind=Aarch64GotLoad symbol=__fp_const_07_compile_time_validation_16 addend=0
  offset=0x000008fc kind=Aarch64GotLoad symbol=__fp_const_07_compile_time_validation_17 addend=0
  offset=0x00000904 kind=Aarch64GotLoad symbol=__fp_const_07_compile_time_validation_17 addend=0
  offset=0x00000910 kind=Aarch64GotLoad symbol=__fp_const_07_compile_time_validation_19 addend=0
  offset=0x00000918 kind=Aarch64GotLoad symbol=__fp_const_07_compile_time_validation_19 addend=0
  offset=0x00000924 kind=Aarch64GotLoad symbol=__fp_const_07_compile_time_validation_18 addend=0
  offset=0x0000092c kind=Aarch64GotLoad symbol=__fp_const_07_compile_time_validation_18 addend=0
  offset=0x00000938 kind=CallRel32 symbol=printf addend=0

Section relocations:
  section=Data offset=0x00000000 kind=Abs64 symbol=__const_data_0 addend=0
  section=Data offset=0x00000010 kind=Abs64 symbol=__const_data_0 addend=0
  section=Data offset=0x00000020 kind=Abs64 symbol=__const_data_1 addend=0
  section=Data offset=0x00000030 kind=Abs64 symbol=__const_data_1 addend=0
  section=Data offset=0x00000040 kind=Abs64 symbol=__const_data_2 addend=0
  section=Data offset=0x00000050 kind=Abs64 symbol=__const_data_2 addend=0

.text (2384 bytes):
  00000000  ff 03 01 d1 fd 7b 03 a9  fd 03 00 91 1f 20 03 d5 
  00000010  f0 03 00 91 10 82 00 91  f0 03 00 f9 f1 03 40 f9 
  00000020  10 04 80 d2 30 02 00 f9  f0 03 40 f9 11 02 40 f9 
  00000030  f1 0b 00 f9 e0 0b 40 f9  bf 03 00 91 fd 7b 43 a9 
  00000040  ff 03 01 91 c0 03 5f d6  ff 03 01 d1 fd 7b 03 a9 
  00000050  fd 03 00 91 1f 20 03 d5  f0 03 00 91 10 82 00 91 
  00000060  f0 03 00 f9 f1 03 40 f9  10 04 80 d2 30 02 00 f9 
  00000070  f0 03 40 f9 11 02 40 f9  f1 0b 00 f9 e0 0b 40 f9 
  00000080  bf 03 00 91 fd 7b 43 a9  ff 03 01 91 c0 03 5f d6 
  00000090  ff 03 01 d1 fd 7b 03 a9  fd 03 00 91 1f 20 03 d5 
  000000a0  f0 03 00 91 10 82 00 91  f0 03 00 f9 f1 03 40 f9 
  000000b0  70 00 80 d2 30 02 00 f9  f0 03 40 f9 11 02 40 f9 
  000000c0  f1 0b 00 f9 e0 0b 40 f9  bf 03 00 91 fd 7b 43 a9 
  000000d0  ff 03 01 91 c0 03 5f d6  ff 03 01 d1 fd 7b 03 a9 
  000000e0  fd 03 00 91 1f 20 03 d5  f0 03 00 91 10 82 00 91 
  000000f0  f0 03 00 f9 f1 03 40 f9  70 00 80 d2 30 02 00 f9 
  00000100  f0 03 40 f9 11 02 40 f9  f1 0b 00 f9 e0 0b 40 f9 
  00000110  bf 03 00 91 fd 7b 43 a9  ff 03 01 91 c0 03 5f d6 
  00000120  ff 03 01 d1 fd 7b 03 a9  fd 03 00 91 1f 20 03 d5 
  00000130  f0 03 00 91 10 82 00 91  f0 03 00 f9 f1 03 40 f9 
  00000140  30 00 80 d2 30 02 00 39  f0 03 40 f9 11 02 40 39 
  00000150  f1 0b 00 f9 e0 43 40 39  bf 03 00 91 fd 7b 43 a9 
  00000160  ff 03 01 91 c0 03 5f d6  ff 03 01 d1 fd 7b 03 a9 
  00000170  fd 03 00 91 1f 20 03 d5  f0 03 00 91 10 82 00 91 
  00000180  f0 03 00 f9 f1 03 40 f9  30 00 80 d2 30 02 00 39 
  00000190  f0 03 40 f9 11 02 40 39  f1 0b 00 f9 e0 43 40 39 
  000001a0  bf 03 00 91 fd 7b 43 a9  ff 03 01 91 c0 03 5f d6 
  000001b0  ff 03 01 d1 fd 7b 03 a9  fd 03 00 91 1f 20 03 d5 
  000001c0  f0 03 00 91 10 82 00 91  f0 03 00 f9 f1 03 40 f9 
  000001d0  10 00 80 d2 30 02 00 39  f0 03 40 f9 11 02 40 39 
  000001e0  f1 0b 00 f9 e0 43 40 39  bf 03 00 91 fd 7b 43 a9 
  000001f0  ff 03 01 91 c0 03 5f d6  ff 03 01 d1 fd 7b 03 a9 
  00000200  fd 03 00 91 1f 20 03 d5  f0 03 00 91 10 82 00 91 
  00000210  f0 03 00 f9 f1 03 40 f9  10 00 80 d2 30 02 00 39 
  00000220  f0 03 40 f9 11 02 40 39  f1 0b 00 f9 e0 43 40 39 
  00000230  bf 03 00 91 fd 7b 43 a9  ff 03 01 91 c0 03 5f d6 
  00000240  ff 03 01 d1 fd 7b 03 a9  fd 03 00 91 1f 20 03 d5 
  00000250  f0 03 00 91 10 82 00 91  f0 03 00 f9 f1 03 40 f9 
  00000260  10 02 80 d2 30 02 00 f9  f0 03 40 f9 11 02 40 f9 
  00000270  f1 0b 00 f9 e0 0b 40 f9  bf 03 00 91 fd 7b 43 a9 
  00000280  ff 03 01 91 c0 03 5f d6  ff 03 01 d1 fd 7b 03 a9 
  00000290  fd 03 00 91 1f 20 03 d5  f0 03 00 91 10 82 00 91 
  000002a0  f0 03 00 f9 f1 03 40 f9  10 02 80 d2 30 02 00 f9 
  000002b0  f0 03 40 f9 11 02 40 f9  f1 0b 00 f9 e0 0b 40 f9 
  000002c0  bf 03 00 91 fd 7b 43 a9  ff 03 01 91 c0 03 5f d6 
  000002d0  ff 03 01 d1 fd 7b 03 a9  fd 03 00 91 1f 20 03 d5 
  000002e0  f0 03 00 91 10 82 00 91  f0 03 00 f9 f1 03 40 f9 
  000002f0  90 00 80 d2 30 02 00 f9  f0 03 40 f9 11 02 40 f9 
  00000300  f1 0b 00 f9 e0 0b 40 f9  bf 03 00 91 fd 7b 43 a9 
  00000310  ff 03 01 91 c0 03 5f d6  ff 03 01 d1 fd 7b 03 a9 
  00000320  fd 03 00 91 1f 20 03 d5  f0 03 00 91 10 82 00 91 
  00000330  f0 03 00 f9 f1 03 40 f9  90 00 80 d2 30 02 00 f9 
  00000340  f0 03 40 f9 11 02 40 f9  f1 0b 00 f9 e0 0b 40 f9 
  00000350  bf 03 00 91 fd 7b 43 a9  ff 03 01 91 c0 03 5f d6 
  00000360  ff 03 01 d1 fd 7b 03 a9  fd 03 00 91 1f 20 03 d5 
  00000370  f0 03 00 91 10 82 00 91  f0 03 00 f9 f1 03 40 f9 
  00000380  30 00 80 d2 30 02 00 39  f0 03 40 f9 11 02 40 39 
  00000390  f1 0b 00 f9 e0 43 40 39  bf 03 00 91 fd 7b 43 a9 
  000003a0  ff 03 01 91 c0 03 5f d6  ff 03 01 d1 fd 7b 03 a9 
  000003b0  fd 03 00 91 1f 20 03 d5  f0 03 00 91 10 82 00 91 
  000003c0  f0 03 00 f9 f1 03 40 f9  30 00 80 d2 30 02 00 39 
  000003d0  f0 03 40 f9 11 02 40 39  f1 0b 00 f9 e0 43 40 39 
  000003e0  bf 03 00 91 fd 7b 43 a9  ff 03 01 91 c0 03 5f d6 
  000003f0  ff c3 00 d1 fd 7b 02 a9  fd 03 00 91 1f 20 03 d5 
  00000400  f0 03 00 91 10 62 00 91  f0 03 00 f9 f0 03 40 f9 
  00000410  11 02 40 39 f1 07 00 f9  e0 23 40 39 bf 03 00 91 
  00000420  fd 7b 42 a9 ff c3 00 91  c0 03 5f d6 ff c3 00 d1 
  00000430  fd 7b 02 a9 fd 03 00 91  1f 20 03 d5 f0 03 00 91 
  00000440  10 62 00 91 f0 03 00 f9  f0 03 40 f9 11 02 40 39 
  00000450  f1 07 00 f9 e0 23 40 39  bf 03 00 91 fd 7b 42 a9 
  00000460  ff c3 00 91 c0 03 5f d6  ff 03 01 d1 fd 7b 03 a9 
  00000470  fd 03 00 91 1f 20 03 d5  f0 03 00 91 10 a2 00 91 
  00000480  f0 03 00 f9 10 00 00 90  10 02 40 f9 1f 02 01 f1 
  00000490  f0 c7 9f 9a f0 07 00 f9  f1 03 40 f9 f0 23 40 39 
  000004a0  30 02 00 39 f0 03 40 f9  11 02 40 39 f1 0f 00 f9 
  000004b0  e0 63 40 39 bf 03 00 91  fd 7b 43 a9 ff 03 01 91 
  000004c0  c0 03 5f d6 ff 03 01 d1  fd 7b 03 a9 fd 03 00 91 
  000004d0  1f 20 03 d5 f0 03 00 91  10 a2 00 91 f0 03 00 f9 
  000004e0  10 00 00 90 10 02 40 f9  1f 02 01 f1 f0 c7 9f 9a 
  000004f0  f0 07 00 f9 f1 03 40 f9  f0 23 40 39 30 02 00 39 
  00000500  f0 03 40 f9 11 02 40 39  f1 0f 00 f9 e0 63 40 39 
  00000510  bf 03 00 91 fd 7b 43 a9  ff 03 01 91 c0 03 5f d6 
  00000520  ff 03 01 d1 fd 7b 03 a9  fd 03 00 91 1f 20 03 d5 
  00000530  f0 03 00 91 10 a2 00 91  f0 03 00 f9 10 00 00 90 
  00000540  10 02 40 f9 1f 02 01 f1  f0 c7 9f 9a f0 07 00 f9 
  00000550  f1 03 40 f9 f0 23 40 39  30 02 00 39 f0 03 40 f9 
  00000560  11 02 40 39 f1 0f 00 f9  e0 63 40 39 bf 03 00 91 
  00000570  fd 7b 43 a9 ff 03 01 91  c0 03 5f d6 ff 03 01 d1 
  00000580  fd 7b 03 a9 fd 03 00 91  1f 20 03 d5 f0 03 00 91 
  00000590  10 a2 00 91 f0 03 00 f9  10 00 00 90 10 02 40 f9 
  000005a0  1f 02 01 f1 f0 c7 9f 9a  f0 07 00 f9 f1 03 40 f9 
  000005b0  f0 23 40 39 30 02 00 39  f0 03 40 f9 11 02 40 39 
  000005c0  f1 0f 00 f9 e0 63 40 39  bf 03 00 91 fd 7b 43 a9 
  000005d0  ff 03 01 91 c0 03 5f d6  ff 03 01 d1 fd 7b 03 a9 
  000005e0  fd 03 00 91 1f 20 03 d5  f0 03 00 91 10 a2 00 91 
  000005f0  f0 03 00 f9 10 00 00 90  10 02 40 f9 11 00 00 90 
  00000600  31 02 40 f9 10 02 11 8b  f0 07 00 f9 f1 03 40 f9 
  00000610  f0 07 40 f9 30 02 00 f9  f0 03 40 f9 11 02 40 f9 
  00000620  f1 0f 00 f9 e0 0f 40 f9  bf 03 00 91 fd 7b 43 a9 
  00000630  ff 03 01 91 c0 03 5f d6  ff 03 01 d1 fd 7b 03 a9 
  00000640  fd 03 00 91 1f 20 03 d5  f0 03 00 91 10 a2 00 91 
  00000650  f0 03 00 f9 10 00 00 90  10 02 40 f9 11 00 00 90 
  00000660  31 02 40 f9 10 02 11 8b  f0 07 00 f9 f1 03 40 f9 
  00000670  f0 07 40 f9 30 02 00 f9  f0 03 40 f9 11 02 40 f9 
  00000680  f1 0f 00 f9 e0 0f 40 f9  bf 03 00 91 fd 7b 43 a9 
  00000690  ff 03 01 91 c0 03 5f d6  ff 03 01 d1 fd 7b 03 a9 
  000006a0  fd 03 00 91 1f 20 03 d5  f0 03 00 91 10 a2 00 91 
  000006b0  f0 03 00 f9 10 00 00 90  10 02 40 f9 1f 82 01 f1 
  000006c0  f0 c7 9f 9a f0 07 00 f9  f1 03 40 f9 f0 23 40 39 
  000006d0  30 02 00 39 f0 03 40 f9  11 02 40 39 f1 0f 00 f9 
  000006e0  e0 63 40 39 bf 03 00 91  fd 7b 43 a9 ff 03 01 91 
  000006f0  c0 03 5f d6 ff 03 01 d1  fd 7b 03 a9 fd 03 00 91 
  00000700  1f 20 03 d5 f0 03 00 91  10 a2 00 91 f0 03 00 f9 
  00000710  10 00 00 90 10 02 40 f9  1f 82 01 f1 f0 c7 9f 9a 
  00000720  f0 07 00 f9 f1 03 40 f9  f0 23 40 39 30 02 00 39 
  00000730  f0 03 40 f9 11 02 40 39  f1 0f 00 f9 e0 63 40 39 
  00000740  bf 03 00 91 fd 7b 43 a9  ff 03 01 91 c0 03 5f d6 
  00000750  ff c3 03 d1 fd 7b 0e a9  fd 03 00 91 1f 20 03 d5 
  00000760  00 00 00 90 00 00 00 91  00 80 00 91 00 00 00 94 
  00000770  00 00 00 90 00 00 00 91  00 40 01 91 00 00 00 94 
  00000780  00 00 00 90 00 00 00 91  00 80 02 91 00 00 00 94 
  00000790  00 00 00 90 00 00 00 91  00 40 03 91 00 00 00 94 
  000007a0  00 00 00 90 00 00 00 91  00 e0 03 91 00 00 00 94 
  000007b0  00 00 00 90 00 00 00 91  00 00 04 91 01 00 00 90 
  000007c0  21 00 40 f9 10 00 00 90  10 02 40 f9 f0 03 00 f9 
  000007d0  02 00 00 90 42 00 40 f9  10 00 00 90 10 02 40 f9 
  000007e0  f0 07 00 f9 00 00 00 94  00 00 00 90 00 00 00 91 
  000007f0  00 80 04 91 01 00 00 90  21 00 40 f9 10 00 00 90 
  00000800  10 02 40 f9 f0 03 00 f9  02 00 00 90 42 00 40 f9 
  00000810  10 00 00 90 10 02 40 f9  f0 07 00 f9 00 00 00 94 
  00000820  00 00 00 90 00 00 00 91  00 00 05 91 01 00 00 90 
  00000830  21 00 40 f9 10 00 00 90  10 02 40 f9 f0 03 00 f9 
  00000840  02 00 00 90 42 00 40 f9  10 00 00 90 10 02 40 f9 
  00000850  f0 07 00 f9 03 00 00 90  63 00 40 f9 10 00 00 90 
  00000860  10 02 40 f9 f0 0b 00 f9  00 00 00 94 00 00 00 90 
  00000870  00 00 00 91 00 e0 05 91  01 00 00 90 21 00 00 91 
  00000880  10 00 00 90 10 02 00 91  f0 03 00 f9 02 00 00 90 
  00000890  42 00 00 91 10 00 00 90  10 02 00 91 f0 07 00 f9 
  000008a0  03 00 00 90 63 00 00 91  10 00 00 90 10 02 00 91 
  000008b0  f0 0b 00 f9 00 00 00 94  00 00 00 90 00 00 00 91 
  000008c0  00 80 06 91 01 00 00 90  21 00 40 f9 10 00 00 90 
  000008d0  10 02 40 f9 f0 03 00 f9  00 00 00 94 00 00 00 90 
  000008e0  00 00 00 91 00 e0 06 91  01 00 00 90 21 00 40 f9 
  000008f0  10 00 00 90 10 02 40 f9  f0 03 00 f9 02 00 00 90 
  00000900  42 00 40 f9 10 00 00 90  10 02 40 f9 f0 07 00 f9 
  00000910  03 00 00 90 63 00 40 f9  10 00 00 90 10 02 40 f9 
  00000920  f0 0b 00 f9 04 00 00 90  84 00 40 f9 10 00 00 90 
  00000930  10 02 40 f9 f0 0f 00 f9  00 00 00 94 bf 03 00 91 
  00000940  fd 7b 4e a9 ff c3 03 91  00 00 80 d2 c0 03 5f d6 

.rodata (504 bytes):
  00000000  44 61 74 61 00 69 36 34  00 75 38 00 00 00 00 00 
  00000010  40 00 00 00 00 00 00 00  40 00 00 00 00 00 00 00 
  00000020  f0 9f 93 98 20 54 75 74  6f 72 69 61 6c 3a 20 30 
  00000030  37 5f 63 6f 6d 70 69 6c  65 5f 74 69 6d 65 5f 76 
  00000040  61 6c 69 64 61 74 69 6f  6e 2e 66 70 0a 00 00 00 
  00000050  f0 9f a7 ad 20 46 6f 63  75 73 3a 20 43 6f 6d 70 
  00000060  69 6c 65 2d 74 69 6d 65  20 76 61 6c 69 64 61 74 
  00000070  69 6f 6e 20 75 73 69 6e  67 20 63 6f 6e 73 74 20 
  00000080  65 78 70 72 65 73 73 69  6f 6e 73 20 61 6e 64 20 
  00000090  69 6e 74 72 6f 73 70 65  63 74 69 6f 6e 0a 00 00 
  000000a0  f0 9f a7 aa 20 57 68 61  74 20 74 6f 20 6c 6f 6f 
  000000b0  6b 20 66 6f 72 3a 20 6c  61 62 65 6c 65 64 20 6f 
  000000c0  75 74 70 75 74 73 20 62  65 6c 6f 77 0a 00 00 00 
  000000d0  e2 9c 85 20 45 78 70 65  63 74 61 74 69 6f 6e 3a 
  000000e0  20 6f 75 74 70 75 74 73  20 6d 61 74 63 68 20 6c 
  000000f0  61 62 65 6c 73 0a 00 00  0a 00 00 00 00 00 00 00 
  00000100  64 61 74 61 3a 20 73 69  7a 65 6f 66 3d 25 6c 6c 
  00000110  75 2c 20 66 69 65 6c 64  73 3d 25 6c 6c 64 0a 00 
  00000120  64 61 74 61 3a 20 68 61  73 5f 61 3d 25 64 2c 20 
  00000130  68 61 73 5f 78 3d 25 64  0a 00 00 00 00 00 00 00 
  00000140  68 65 61 64 65 72 3a 20  73 69 7a 65 6f 66 3d 25 
  00000150  6c 6c 75 2c 20 66 69 65  6c 64 73 3d 25 6c 6c 64 
  00000160  2c 20 68 61 73 5f 76 65  72 73 69 6f 6e 3d 25 64 
  00000170  0a 00 00 00 00 00 00 00  74 79 70 65 73 3a 20 64 
  00000180  61 74 61 3d 27 25 73 27  20 61 3d 27 25 73 27 20 
  00000190  76 65 72 73 69 6f 6e 3d  27 25 73 27 0a 00 00 00 
  000001a0  64 61 74 61 20 68 61 73  20 74 6f 5f 73 74 72 69 
  000001b0  6e 67 3a 20 25 64 0a 00  6c 61 79 6f 75 74 3a 20 
  000001c0  64 61 74 61 5f 6f 6b 3d  25 64 2c 20 68 65 61 64 
  000001d0  65 72 5f 6f 6b 3d 25 64  2c 20 74 6f 74 61 6c 5f 
  000001e0  6f 6b 3d 25 64 2c 20 74  6f 74 61 6c 5f 73 69 7a 
  000001f0  65 3d 25 6c 6c 75 0a 00 
