fp-native dump: format=MachO arch=Aarch64 entry=0x2c0

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
global __const_data_0 ty=Array(I8, 11) constant=true initializer=Some(Bytes([70, 101, 114, 114, 111, 80, 104, 97, 115, 101, 0]))
global NAME ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0]))
global NAME ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0]))
global __const_data_1 ty=Array(I8, 6) constant=true initializer=Some(Bytes([48, 46, 49, 46, 48, 0]))
global VERSION ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0]))
global VERSION ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0]))
global NAME_LEN ty=I64 constant=true initializer=Some(Bytes([10, 0, 0, 0, 0, 0, 0, 0]))
global NAME_LEN ty=I64 constant=true initializer=Some(Bytes([10, 0, 0, 0, 0, 0, 0, 0]))
global VERSION_LEN ty=I64 constant=true initializer=Some(Bytes([5, 0, 0, 0, 0, 0, 0, 0]))
global VERSION_LEN ty=I64 constant=true initializer=Some(Bytes([5, 0, 0, 0, 0, 0, 0, 0]))
global PREFIX_OK ty=I1 constant=true initializer=Some(Bytes([1]))
global PREFIX_OK ty=I1 constant=true initializer=Some(Bytes([1]))
global SUFFIX_OK ty=I1 constant=true initializer=Some(Bytes([1]))
global SUFFIX_OK ty=I1 constant=true initializer=Some(Bytes([1]))
global HAS_PHASE ty=I1 constant=true initializer=Some(Bytes([1]))
global HAS_PHASE ty=I1 constant=true initializer=Some(Bytes([1]))
global __const_data_2 ty=Array(I8, 6) constant=true initializer=Some(Bytes([70, 101, 114, 114, 111, 0]))
global SHORT ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0]))
global SHORT ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0]))
global __const_data_3 ty=Array(I8, 6) constant=true initializer=Some(Bytes([80, 104, 97, 115, 101, 0]))
global TAIL ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0]))
global TAIL ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0]))
global __const_data_4 ty=Array(I8, 6) constant=true initializer=Some(Bytes([97, 108, 112, 104, 97, 0]))
global __const_data_5 ty=Array(I8, 5) constant=true initializer=Some(Bytes([98, 101, 116, 97, 0]))
global __const_data_6 ty=Array(I8, 6) constant=true initializer=Some(Bytes([103, 97, 109, 109, 97, 0]))
global __const_data_7 ty=Array(I8, 6) constant=true initializer=Some(Bytes([100, 101, 108, 116, 97, 0]))
global WORDS ty=Array(Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") }, 4) constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0]))
global WORDS ty=Array(Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") }, 4) constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0]))
global WORD_LENGTHS ty=Array(I64, 4) constant=true initializer=Some(Bytes([5, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0]))
global WORD_LENGTHS ty=Array(I64, 4) constant=true initializer=Some(Bytes([5, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0]))
global TOTAL_WORD_LEN ty=I64 constant=true initializer=Some(Bytes([19, 0, 0, 0, 0, 0, 0, 0]))
global TOTAL_WORD_LEN ty=I64 constant=true initializer=Some(Bytes([19, 0, 0, 0, 0, 0, 0, 0]))
global __const_data_8 ty=Array(I8, 18) constant=true initializer=Some(Bytes([70, 101, 114, 114, 111, 80, 104, 97, 115, 101, 32, 118, 48, 46, 49, 46, 48, 0]))
global BANNER ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 17, 0, 0, 0, 0, 0, 0, 0]))
global BANNER ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 17, 0, 0, 0, 0, 0, 0, 0]))
fn _kernelrpc_mach_vm_map_trap
fn __swbuf
fn task_get_special_port
fn waitid
fn _kernelrpc_mach_port_move_member_trap
fn macx_backing_store_suspend
fn swtch_pri
fn fgetwc
fn NSInstallLinkEditErrorHandlers
fn wcscpy
fn wcscmp
fn NSGetSectionDataInObjectFileImage
fn localtime
fn task_identity_token_get_task_port
fn iswrune
fn mach_msg
fn posix_spawnattr_setsigmask
fn insque
fn task_get_exception_ports_info
fn task_register_hardened_exception_handler
fn thread_adopt_exception_handler
fn NSIsSymbolNameDefinedInImage
fn setrlimit
fn iswpunct
fn getservbyport
fn pthread_testcancel
fn posix_spawnattr_getsigmask
fn perror
fn vm_stats
fn _exit
fn task_generate_corpse
fn vm_allocate
fn vm_read
fn quick_exit
fn host_set_exception_ports
fn mach_port_rename
fn thread_switch
fn _dyld_image_containing_address
fn inet_pton
fn readdir
fn semaphore_signal_thread
fn ungetc
fn __srget
fn posix_spawn_file_actions_addclose
fn getgrgid_r
fn task_register_dyld_shared_cache_image_info
fn putenv
fn gethostent
fn pthread_kill
fn time
fn getpwnam_r
fn fputwc
fn fchown
fn pselect
fn processor_start
fn NSNameOfSymbol
fn stpcpy
fn msgctl
fn ualarm
fn memchr
fn vfwscanf
fn semaphore_signal_all
fn task_policy_set
fn mach_msg_destroy
fn gethostbyaddr
fn endgrent
fn shmctl
fn OSHostByteOrder
fn thread_assign
fn getnetbyname
fn getlogin
fn freopen
fn setkey
fn _kernelrpc_mach_port_insert_right_trap
fn mkfifoat
fn mig_reply_setup
fn fgetc
fn wcrtomb
fn execve
fn thread_terminate
fn ferror
fn getc
fn vprintf
fn wcstoul
fn getitimer
fn mach_thread_self
fn isspace
fn debug_control_port_for_pid
fn wcscat
fn _OSWriteInt32
fn memccpy
fn isalpha
fn killpg
fn memset
fn _kernelrpc_mach_port_deallocate_trap
fn task_sample
fn a64l
fn _OSSwapInt32
fn setgrent
fn processor_set_threads
fn thread_set_state
fn realpath
fn towlower
fn endhostent
fn getrlimit
fn endprotoent
fn ftok
fn stpncpy
fn wcsncasecmp
fn task_suspend
fn setjmp
fn mach_port_insert_right
fn NSDestroyObjectFileImage
fn getpid
fn _dyld_lookup_and_bind_fully
fn processor_set_info
fn sigprocmask
fn sem_getvalue
fn thread_create
fn lock_set_create
fn strtoumax
fn fgets
fn atoi
fn fwide
fn gethostbyname
fn clock_getres
fn if_freenameindex
fn pread
fn vsprintf
fn wcsrtombs
fn wmemmove
fn statvfs
fn task_set_info
fn feraiseexcept
fn task_assign
fn task_dyld_process_info_notify_deregister
fn setegid
fn wcstoumax
fn vsnprintf
fn srandom
fn posix_openpt
fn __maskrune
fn strncat
fn ftell
fn mrand48
fn vwprintf
fn fflush
fn ctermid
fn ___toupper
fn lldiv
fn memmove
fn strcoll
fn mktime
fn aio_return
fn getgrgid
fn setpriority
fn posix_spawnattr_getpgroup
fn semget
fn strerror
fn mbstowcs
fn setsockopt
fn fstatvfs
fn isgraph
fn aio_write
fn unlink
fn setstate
fn fwrite
fn kmod_create
fn waitpid
fn setlogmask
fn mknod
fn processor_set_policy_control
fn sync
fn wcstombs
fn strpbrk
fn _OSReadSwapInt64
fn ungetwc
fn atoll
fn wcslen
fn wcpcpy
fn posix_spawnattr_setflags
fn _OSWriteInt16
fn task_suspend2
fn thread_set_exception_ports
fn tzset
fn sigpause
fn isatty
fn vm_region
fn sigaddset
fn thread_abort_safely
fn task_policy_get
fn mach_port_destruct
fn fseeko
fn sethostent
fn posix_spawn
fn NXHostByteOrder
fn mach_port_names
fn mach_port_space_info
fn host_page_size
fn task_set_port_space
fn mach_port_request_notification
fn _kernelrpc_mach_port_construct_trap
fn etap_trace_thread
fn mach_voucher_extract_attr_recipe_trap
fn pipe
fn ffs
fn ctime
fn host_processor_sets
fn thread_get_exception_ports_info
fn aio_error
fn mach_port_get_service_port_info
fn mach_memory_object_memory_entry
fn _dyld_bind_fully_image_containing_address
fn wcwidth
fn endpwent
fn sendto
fn tolower
fn mach_port_get_set_status
fn NXSwapLong
fn getwc
fn _dyld_get_image_header
fn islower
fn sigdelset
fn task_unregister_dyld_image_infos
fn aio_read
fn host_set_atm_diagnostic_flag
fn _longjmp
fn vsscanf
fn getwchar
fn link
fn fdopen
fn strsignal
fn getprotobynumber
fn pthread_getconcurrency
fn regerror
fn stat
fn slot_name
fn __darwin_check_fd_set
fn iswcntrl
fn isprint
fn task_policy
fn getgid
fn feof
fn abort
fn asctime
fn cfsetospeed
fn task_set_policy
fn clock_set_res
fn freeaddrinfo
fn getgrnam_r
fn getprotobyname
fn iconv_open
fn strtok
fn wcstok
fn mach_port_unguard
fn NSLookupSymbolInImage
fn mlockall
fn utimes
fn llabs
fn strptime
fn fegetenv
fn strchr
fn strncmp
fn fdopendir
fn _kernelrpc_mach_port_extract_member_trap
fn getgrnam
fn fstatat
fn vm_machine_attribute
fn host_get_atm_diagnostic_flag
fn NXSwapHostLongToLittle
fn kevent
fn NXSwapLittleIntToHost
fn strcat
fn towupper
fn getnetbyaddr
fn NSLinkModule
fn posix_spawn_file_actions_adddup2
fn fremovexattr
fn fgetpos
fn write
fn dup2
fn vfork
fn mach_ports_register
fn __darwin_fd_isset
fn task_get_state
fn clock_gettime
fn lio_listio
fn mach_port_kobject_description
fn host_get_boot_info
fn thread_create_running
fn _kernelrpc_mach_port_guard_trap
fn kqueue
fn fgetxattr
fn strncpy
fn mach_port_get_refs
fn mprotect
fn sysconf
fn mach_make_memory_entry
fn _OSReadSwapInt16
fn vm_read_overwrite
fn mach_task_is_self
fn strdup
fn NXSwapHostShortToBig
fn mach_memory_info
fn fopen
fn localeconv
fn dlsym
fn wctob
fn setprotoent
fn mach_error_string
fn mbsrtowcs
fn clock_set_time
fn gmtime
fn wmemcpy
fn getuid
fn host_security_create_task_token
fn setgroupent
fn NSVersionOfRunTimeLibrary
fn rand
fn processor_set_create
fn posix_spawnp
fn kevent64
fn funlockfile
fn mbsinit
fn wcspbrk
fn hcreate
fn setregid
fn wcsnlen
fn sem_trywait
fn mig_allocate
fn thread_get_state
fn _Exit
fn mach_memory_object_memory_entry_64
fn mach_error
fn accept
fn isblank
fn atomic_thread_fence
fn wcsnrtombs
fn mach_port_set_attributes
fn mach_port_guard_with_flags
fn macx_swapon
fn macx_triggers
fn iswxdigit
fn sighold
fn wcstoimax
fn processor_set_max_priority
fn fclonefileat
fn socket
fn tmpfile
fn _dyld_shared_cache_contains_path
fn iswalpha
fn wcsspn
fn task_swap_exception_ports
fn mlock
fn macx_swapoff
fn clonefile
fn host_kernel_version
fn dup
fn toascii
fn localtime_r
fn tcsetpgrp
fn _OSWriteSwapInt16
fn task_map_kcdata_object_64
fn mach_port_set_context
fn shutdown
fn voucher_mach_msg_clear
fn task_for_pid
fn strtoull
fn task_swap_mach_voucher
fn _host_page_size
fn iswprint
fn getpwent
fn task_terminate
fn aligned_alloc
fn sem_destroy
fn times
fn mig_dealloc_reply_port
fn clonefileat
fn sched_get_priority_min
fn task_set_special_port
fn __darwin_fd_set
fn host_processor_set_priv
fn getsockopt
fn __vsnprintf_chk
fn wcswidth
fn wcsncmp
fn vm_map_64
fn mach_port_kobject
fn tcdrain
fn hdestroy
fn semaphore_timedwait
fn mach_msg_receive
fn _kernelrpc_mach_port_destruct_trap
fn popen
fn shm_unlink
fn task_set_exception_ports
fn mblen
fn nanosleep
fn wcstoull
fn duplocale
fn poll
fn toupper
fn setpwent
fn sem_post
fn setsid
fn rand_r
fn setpgrp
fn vm_protect
fn connect
fn readdir_r
fn thread_info
fn _kernelrpc_mach_port_allocate_trap
fn longjmp
fn msgget
fn getpwuid
fn timespec_get
fn wcsncpy
fn recv
fn crypt
fn remove
fn mach_ports_lookup
fn mach_port_deallocate
fn NSSymbolDefinitionNameInObjectFileImage
fn task_set_ras_pc
fn NSLookupAndBindSymbolWithHint
fn NXSwapLittleLongToHost
fn host_create_mach_voucher
fn getppid
fn mach_port_construct
fn NSIsSymbolNameDefined
fn setbuf
fn processor_set_destroy
fn host_info
fn closelog
fn tcgetpgrp
fn ftrylockfile
fn _dyld_lookup_and_bind
fn getchar
fn sigwait
fn wctomb
fn strlen
fn send
fn posix_spawn_file_actions_addchdir
fn lcong48
fn cfgetispeed
fn ttyname_r
fn wcsncat
fn strcmp
fn task_register_dyld_image_infos
fn task_register_dyld_set_dyld_state
fn NXSwapFloat
fn NXSwapDouble
fn chmod
fn vm_map_exec_lockdown
fn fchdir
fn sigfillset
fn vfwprintf
fn fetestexcept
fn posix_memalign
fn listen
fn mach_error_type
fn macx_backing_store_recovery
fn strtoimax
fn munmap
fn getdate
fn ldiv
fn mach_port_guard
fn mach_vm_reclaim_update_kernel_accounting_trap
fn vm_copy
fn vscanf
fn dlclose
fn fork
fn task_set_phys_footprint_limit
fn _kernelrpc_mach_port_request_notification_trap
fn sigsuspend
fn unlockpt
fn iswascii
fn getgroups
fn host_set_multiuser_config_flags
fn feholdexcept
fn openlog
fn lrand48
fn flockfile
fn endnetent
fn l64a
fn truncate
fn read
fn fgetws
fn encrypt
fn getdelim
fn __math_errhandling
fn imaxabs
fn putc_unlocked
fn getgrent
fn fsetpos
fn if_nametoindex
fn msgrcv
fn host_get_UNDServer
fn vm_write
fn cfsetispeed
fn futimens
fn mbsnrtowcs
fn getservent
fn iswupper
fn thread_set_policy
fn _tlv_bootstrap
fn NSAddLibraryWithSearching
fn strcspn
fn fnmatch
fn task_zone_info
fn mach_port_insert_member
fn clock_sleep
fn NXSwapBigLongToHost
fn getsubopt
fn strtoll
fn iswlower
fn socketpair
fn __darwin_fd_clr
fn mig_strncpy
fn task_map_corpse_info
fn memcmp
fn _dyld_present
fn strcpy
fn sigignore
fn setlocale
fn clock
fn bind
fn setnetent
fn task_get_dyld_image_infos
fn sigismember
fn clock_settime
fn if_nameindex
fn shmdt
fn strnlen
fn wmemchr
fn srand48
fn nrand48
fn iswdigit
fn iswspace
fn div
fn hsearch
fn isxdigit
fn readlink
fn open_wmemstream
fn gai_strerror
fn regfree
fn sigrelse
fn setgid
fn processor_set_stack_usage
fn recvfrom
fn globfree
fn vswprintf
fn setitimer
fn symlinkat
fn __NDR_convert__mig_reply_error_t
fn processor_set_statistics
fn sem_init
fn task_dyld_process_info_notify_register
fn host_get_special_port
fn nice
fn host_processors
fn vm_map
fn close
fn utimensat
fn mach_port_get_attributes
fn task_wire
fn getpwnam
fn task_info
fn fstat
fn getc_unlocked
fn symlink
fn thread_get_exception_ports
fn vm_map_page_query
fn pthread_setconcurrency
fn isalnum
fn vm_remap_new
fn basename
fn _OSWriteInt64
fn abs
fn sigaction
fn iswideogram
fn aio_suspend
fn strspn
fn setservent
fn iswctype
fn dirfd
fn sem_close
fn umask
fn posix_spawn_file_actions_addfchdir
fn task_inspect
fn act_get_state
fn host_processor_info
fn mach_port_dnrequest_info
fn thread_get_assignment
fn mach_port_set_mscount
fn mach_zone_info
fn task_dyld_process_info_notify_get
fn NSLookupAndBindSymbol
fn fclose
fn mmap
fn task_set_corpse_forking_behavior
fn thread_depress_abort
fn closedir
fn strtok_r
fn linkat
fn wcscspn
fn thread_set_mach_voucher
fn swtch
fn getpwuid_r
fn NSAddLibrary
fn thread_set_special_port
fn NSLinkEditError
fn __toupper
fn thread_resume
fn sched_yield
fn iconv_close
fn _kernelrpc_mach_port_type_trap
fn processor_set_default
fn sigaltstack
fn mach_msg_overwrite
fn task_create
fn iswblank
fn wcsxfrm
fn srand
fn vm_region_recurse_64
fn host_lockgroup_info
fn host_check_multiuser_mode
fn vm_remap
fn _dyld_image_count
fn flistxattr
fn mach_port_allocate
fn sem_wait
fn regexec
fn strftime
fn posix_madvise
fn processor_get_assignment
fn telldir
fn sleep
fn __tolower
fn mach_zone_info_for_zone
fn iswphonogram
fn posix_spawn_file_actions_destroy
fn isdigit
fn vfscanf
fn iswspecial
fn gethostname
fn getlogin_r
fn newlocale
fn voucher_mach_msg_set
fn creat
fn setgrfile
fn ftruncate
fn task_map_corpse_info_64
fn fchownat
fn getservbyname
fn mach_voucher_deallocate
fn _OSReadInt16
fn clock_get_res
fn host_set_special_port
fn siginterrupt
fn setvbuf
fn processor_set_tasks_with_flavor
fn realloc
fn mach_port_peek
fn NXSwapLongLong
fn _dyld_all_twolevel_modules_prebound
fn putwc
fn pthread_key_delete
fn _OSWriteSwapInt32
fn thread_policy_get
fn NSNameOfModule
fn mbrtowc
fn ctime_r
fn mkdirat
fn strcasecmp
fn exit
fn ptsname
fn semaphore_wait_signal
fn _OSReadInt64
fn atomic_flag_test_and_set
fn getrusage
fn pause
fn utime
fn host_get_exception_ports
fn ttyname
fn task_get_exc_guard_behavior
fn mach_generate_activity_id
fn processor_assign
fn mach_vm_region_info
fn pwrite
fn host_request_notification
fn host_statistics64
fn NXSwapHostIntToBig
fn iswalnum
fn vm_msync
fn semaphore_create
fn _dyld_get_image_name
fn processor_set_policy_disable
fn raise
fn posix_spawnattr_getflags
fn setxattr
fn _dyld_get_image_header_containing_address
fn fileno
fn clock_sleep_trap
fn NXSwapHostShortToLittle
fn atol
fn putchar
fn thread_swap_mach_voucher
fn imaxdiv
fn getegid
fn atomic_signal_fence
fn regcomp
fn lseek
fn thread_get_mach_voucher
fn mach_port_move_member
fn NXSwapHostLongToBig
fn listxattr
fn open_memstream
fn strstr
fn free
fn strerror_r
fn inet_ntop
fn mig_get_reply_port
fn gmtime_r
fn remque
fn mig_put_reply_port
fn _kernelrpc_mach_port_unguard_trap
fn tcflush
fn task_create_identity_token
fn vm_read_list
fn vm_region_recurse
fn host_swap_exception_ports
fn __sputc
fn iscntrl
fn sched_get_priority_max
fn mach_msg_send
fn NSLookupSymbolInModule
fn system
fn mach_port_extract_member
fn NXSwapHostLongLongToBig
fn mig_deallocate
fn mach_port_is_connection_for_service
fn dirname
fn pid_for_task
fn mach_port_mod_refs
fn tempnam
fn wcsftime
fn kmod_destroy
fn task_resume2
fn setpgid
fn thread_policy
fn vm_purgable_control
fn NSCreateObjectFileImageFromMemory
fn getline
fn __vsprintf_chk
fn tcflow
fn wmemcmp
fn mach_vm_wire
fn clock_set_attributes
fn task_get_mach_voucher
fn getnetent
fn fesetenv
fn geteuid
fn fmemopen
fn host_set_UNDServer
fn recvmsg
fn fputs
fn host_create_mach_voucher_trap
fn host_statistics
fn panic_init
fn host_virtual_physical_table_info
fn NXSwapBigIntToHost
fn feclearexcept
fn NSModuleForSymbol
fn atomic_flag_clear
fn task_set_mach_voucher
fn NSLibraryNameForModule
fn grantpt
fn if_indextoname
fn wcschr
fn getpeername
fn mbtowc
fn rewinddir
fn memcpy
fn execv
fn renameat
fn puts
fn __svfscanf
fn vfprintf
fn vdprintf
fn setenv
fn putwchar
fn unsetenv
fn dlopen
fn tcgetattr
fn posix_spawnattr_setsigdefault
fn isascii
fn _OSReadSwapInt32
fn task_assign_default
fn vm_mapped_pages_info
fn mach_port_kernel_object
fn pclose
fn getpriority
fn task_set_emulation_vector
fn unlinkat
fn host_reboot
fn __assert_rtn
fn strxfrm
fn getsid
fn posix_spawn_file_actions_addopen
fn mknodat
fn processor_info
fn mach_port_destroy
fn mach_port_allocate_full
fn NXSwapShort
fn swab
fn NSAddressOfSymbol
fn getxattr
fn act_set_state
fn _kernelrpc_mach_port_mod_refs_trap
fn kmod_control
fn _kernelrpc_mach_port_get_attributes_trap
fn seekdir
fn fputc
fn btowc
fn vm_behavior_set
fn _kernelrpc_mach_vm_protect_trap
fn _kernelrpc_mach_vm_deallocate_trap
fn host_register_well_known_mach_voucher_attr_manager
fn ispunct
fn feupdateenv
fn psignal
fn _setjmp
fn __isctype
fn clearerr
fn malloc
fn labs
fn alarm
fn random
fn seed48
fn wmemset
fn processor_exit
fn task_set_emulation
fn task_test_async_upcall_propagation
fn __istype
fn kill
fn rmdir
fn thread_convert_thread_state
fn mach_port_get_context
fn fesetexceptflag
fn inet_addr
fn sigsetjmp
fn wcsdup
fn lchown
fn semaphore_timedwait_signal
fn pthread_sigmask
fn __wcwidth
fn wcstol
fn execvp
fn lock_set_destroy
fn task_name_for_pid
fn host_get_multiuser_config_flags
fn NSAddImage
fn vswscanf
fn fchmod
fn processor_control
fn NSIsSymbolNameDefinedWithHint
fn _dyld_lookup_and_bind_with_hint
fn NXSwapHostLongLongToLittle
fn calloc
fn _NSGetExecutablePath
fn mkstemp
fn gethostid
fn ___runetype
fn strrchr
fn opendir
fn fegetround
fn seteuid
fn msgsnd
fn semaphore_wait
fn thread_suspend
fn aio_fsync
fn mach_port_swap_guard
fn wctype
fn isupper
fn mach_port_space_basic_info
fn siglongjmp
fn _OSReadInt32
fn NXSwapInt
fn tmpnam
fn NXSwapBigShortToHost
fn NXSwapBigLongLongToHost
fn NSCreateObjectFileImageFromFile
fn task_set_state
fn NSSymbolReferenceNameInObjectFileImage
fn _dyld_launched_prebound
fn fread
fn fesetround
fn putc
fn wcscasecmp
fn fpathconf
fn __error
fn select
fn task_purgable_info
fn lstat
fn lockf
fn shmget
fn thread_abort
fn usleep
fn mbrlen
fn iconv
fn _kernelrpc_mach_vm_purgable_control_trap
fn host_get_clock_service
fn NSIsSymbolDefinedInObjectFileImage
fn _OSWriteSwapInt64
fn posix_spawnattr_setpgroup
fn shmat
fn task_get_exception_ports
fn mach_port_type
fn rename
fn mach_port_allocate_qos
fn _kernelrpc_mach_vm_allocate_trap
fn getentropy
fn uselocale
fn mktemp
fn getcwd
fn munlock
fn thread_get_special_port
fn NXSwapLittleLongLongToHost
fn getaddrinfo
fn _kernelrpc_mach_port_insert_member_trap
fn getopt
fn getnameinfo
fn getsockname
fn munlockall
fn task_threads
fn task_self_trap
fn host_register_mach_voucher_attr_manager
fn mach_vm_region_info_64
fn voucher_mach_msg_revert
fn removexattr
fn sigpending
fn alphasort
fn thread_wire
fn _OSSwapInt64
fn mach_port_assert_attributes
fn wait
fn wcsrchr
fn NSSymbolReferenceCountInObjectFileImage
fn sockatmark
fn ftello
fn dlerror
fn host_get_io_main
fn NSUnLinkModule
fn fputws
fn posix_spawnattr_init
fn getenv
fn __sigbits
fn initstate
fn processor_set_policy_enable
fn atomic_flag_test_and_set_explicit
fn sendmsg
fn tcsendbreak
fn semaphore_signal
fn mach_port_allocate_name
fn inet_ntoa
fn fchmodat
fn task_get_emulation_vector
fn task_test_sync_upcall
fn posix_spawn_file_actions_init
fn iswhexnumber
fn faccessat
fn mach_port_set_seqno
fn iswnumber
fn voucher_mach_msg_adopt
fn _dyld_get_image_vmaddr_slide
fn jrand48
fn strndup
fn asctime_r
fn fegetexceptflag
fn iswgraph
fn chdir
fn setuid
fn setreuid
fn host_priv_statistics
fn thread_sample
fn __darwin_check_fd_set_overflow
fn freelocale
fn posix_spawnattr_destroy
fn uname
fn mach_port_get_srights
fn gettimeofday
fn atomic_flag_clear_explicit
fn rewind
fn wcscoll
fn access
fn host_default_memory_manager
fn kext_request
fn task_set_exc_guard_behavior
fn vwscanf
fn wcstoll
fn task_register_dyld_get_process_state
fn NXSwapHostIntToLittle
fn tcsetattr
fn NSVersionOfLinkTimeLibrary
fn _OSSwapInt16
fn nl_langinfo
fn chown
fn task_get_assignment
fn strncasecmp
fn host_security_set_task_token
fn kmod_get_info
fn NXSwapLittleShortToHost
fn cfgetospeed
fn strtol
fn getpgid
fn getpgrp
fn mkdir
fn fsetxattr
fn confstr
fn processor_set_tasks
fn getchar_unlocked
fn thread_policy_set
fn mach_host_self
fn fseek
fn ___tolower
fn aio_cancel
fn putchar_unlocked
fn vm_region_64
fn sigemptyset
fn NSSymbolDefinitionCountInObjectFileImage
fn fsync
fn endservent
fn posix_spawnattr_getsigdefault
fn vm_allocate_cpm
fn semaphore_destroy
fn gets
fn readlinkat
fn wcpncpy
fn pathconf
fn vm_wire
fn msync
fn mach_port_extract_right
fn mkfifo
fn vm_inherit
fn host_get_clock_control
fn sem_unlink
fn semop
fn strtoul
fn tcgetsid
fn task_resume
fn thread_swap_exception_ports
fn thread_assign_default
fn wcsstr
fn towctrans
fn vm_deallocate
fn wctrans
fn mig_strncpy_zerofill
fn mach_make_memory_entry_64
fn getprotoent
fn __fp_comptime_const_IS_EMPTY_13350332860524102640
  bb0 bb0
    alloca Virtual { id: 0, bank: General, size_bits: 64 }, 1
    eq Virtual { id: 1, bank: General, size_bits: 8 }, 10, 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 1, bank: General, size_bits: 8 }
    load Virtual { id: 3, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn __fp_comptime_const_IS_EMPTY_13350332860524102640
  bb0 bb0
    alloca Virtual { id: 4, bank: General, size_bits: 64 }, 1
    eq Virtual { id: 5, bank: General, size_bits: 8 }, 10, 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 4, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 5, bank: General, size_bits: 8 }
    load Virtual { id: 7, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 4, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn __fp_comptime_const_IS_LONG_17080943128318633337
  bb0 bb0
    alloca Virtual { id: 8, bank: General, size_bits: 64 }, 1
    gt Virtual { id: 9, bank: General, size_bits: 8 }, 10, 5
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 8, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 9, bank: General, size_bits: 8 }
    load Virtual { id: 11, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 8, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn __fp_comptime_const_IS_LONG_17080943128318633337
  bb0 bb0
    alloca Virtual { id: 12, bank: General, size_bits: 64 }, 1
    gt Virtual { id: 13, bank: General, size_bits: 8 }, 10, 5
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 12, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 13, bank: General, size_bits: 8 }
    load Virtual { id: 15, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 12, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn __fp_comptime_const_BUFFER_SIZE_6515539559299490477
  bb0 bb0
    alloca Virtual { id: 16, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 17, bank: General, size_bits: 64 }, 1
    gt Virtual { id: 18, bank: General, size_bits: 8 }, 10, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 17, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 18, bank: General, size_bits: 8 }
    load Virtual { id: 20, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 17, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 21, bank: General, size_bits: 8 }, Virtual { id: 20, bank: General, size_bits: 8 }, 1
    condbr
  bb1 bb1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 16, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 256
    br
  bb2 bb2
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 16, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 128
    br
  bb3 bb3
    load Virtual { id: 24, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 16, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn __fp_comptime_const_BUFFER_SIZE_6515539559299490477
  bb0 bb0
    alloca Virtual { id: 25, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 26, bank: General, size_bits: 64 }, 1
    gt Virtual { id: 27, bank: General, size_bits: 8 }, 10, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 26, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 27, bank: General, size_bits: 8 }
    load Virtual { id: 29, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 26, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 30, bank: General, size_bits: 8 }, Virtual { id: 29, bank: General, size_bits: 8 }, 1
    condbr
  bb1 bb1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 25, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 256
    br
  bb2 bb2
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 25, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 128
    br
  bb3 bb3
    load Virtual { id: 33, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 25, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn main
  bb0 bb0
    alloca Virtual { id: 34, bank: General, size_bits: 64 }, 1
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
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 34, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 0
    br
  bb1 bb1
    alloca Virtual { id: 46, bank: General, size_bits: 64 }, 1
    load Virtual { id: 47, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 34, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    lt Virtual { id: 48, bank: General, size_bits: 8 }, Virtual { id: 47, bank: General, size_bits: 64 }, 4
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 46, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 48, bank: General, size_bits: 8 }
    load Virtual { id: 50, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 46, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 51, bank: General, size_bits: 8 }, Virtual { id: 50, bank: General, size_bits: 8 }, 1
    condbr
  bb2 bb2
    alloca Virtual { id: 52, bank: General, size_bits: 64 }, 1
    load Virtual { id: 53, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 34, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 52, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 53, bank: General, size_bits: 64 }
    alloca Virtual { id: 55, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 55, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.array)
    alloca Virtual { id: 57, bank: General, size_bits: 64 }, 1
    load Virtual { id: 58, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 34, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 57, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 58, bank: General, size_bits: 64 }
    alloca Virtual { id: 60, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 60, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.array)
    load Virtual { id: 62, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 52, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    mul Virtual { id: 63, bank: General, size_bits: 64 }, Virtual { id: 62, bank: General, size_bits: 64 }, 16
    bitcast Virtual { id: 64, bank: General, size_bits: 64 }, Virtual { id: 55, bank: General, size_bits: 64 }
    gep Virtual { id: 65, bank: General, size_bits: 64 }, Virtual { id: 64, bank: General, size_bits: 64 }, Virtual { id: 63, bank: General, size_bits: 64 }
    bitcast Virtual { id: 66, bank: General, size_bits: 64 }, Virtual { id: 65, bank: General, size_bits: 64 }
    bitcast Virtual { id: 67, bank: General, size_bits: 64 }, Virtual { id: 66, bank: General, size_bits: 64 }
    load Virtual { id: 68, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 67, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 69, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 57, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    mul Virtual { id: 70, bank: General, size_bits: 64 }, Virtual { id: 69, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 71, bank: General, size_bits: 64 }, Virtual { id: 60, bank: General, size_bits: 64 }
    gep Virtual { id: 72, bank: General, size_bits: 64 }, Virtual { id: 71, bank: General, size_bits: 64 }, Virtual { id: 70, bank: General, size_bits: 64 }
    bitcast Virtual { id: 73, bank: General, size_bits: 64 }, Virtual { id: 72, bank: General, size_bits: 64 }
    load Virtual { id: 74, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 73, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 68, bank: General, size_bits: 64 }, Virtual { id: 74, bank: General, size_bits: 64 }
    load Virtual { id: 76, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 34, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 77, bank: General, size_bits: 64 }, Virtual { id: 76, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 34, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 77, bank: General, size_bits: 64 }
    br
  bb3 bb3
    intrinsic.call symbol(intrinsic.println), 19
    intrinsic.call symbol(intrinsic.println), symbol(__fp_const_02_string_processing_14), symbol(__fp_const_02_string_processing_15)
    intrinsic.call symbol(intrinsic.println), symbol(__const_data_8)
    intrinsic.call symbol(intrinsic.println), symbol(__fp_const_02_string_processing_17)
    ret


Symbols:
  __fp_comptime_const_IS_EMPTY_13350332860524102640 0x00000058
  __fp_comptime_const_IS_LONG_17080943128318633337 0x00000108
  __fp_comptime_const_BUFFER_SIZE_6515539559299490477 0x00000210
  main                             0x000002c0

Text relocations:
  offset=0x000002e4 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000002f0 kind=CallRel32 symbol=printf addend=0
  offset=0x000002f4 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000300 kind=CallRel32 symbol=printf addend=0
  offset=0x00000304 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000310 kind=CallRel32 symbol=printf addend=0
  offset=0x00000314 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000320 kind=CallRel32 symbol=printf addend=0
  offset=0x00000324 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000330 kind=CallRel32 symbol=printf addend=0
  offset=0x00000334 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000340 kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x00000348 kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x00000360 kind=CallRel32 symbol=printf addend=0
  offset=0x00000364 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000370 kind=Aarch64AdrpAdd symbol=__const_data_1 addend=0
  offset=0x00000378 kind=Aarch64AdrpAdd symbol=__const_data_1 addend=0
  offset=0x00000390 kind=CallRel32 symbol=printf addend=0
  offset=0x00000394 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000003c4 kind=CallRel32 symbol=printf addend=0
  offset=0x000003c8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000003d4 kind=Aarch64AdrpAdd symbol=__const_data_2 addend=0
  offset=0x000003dc kind=Aarch64AdrpAdd symbol=__const_data_2 addend=0
  offset=0x000003e8 kind=Aarch64AdrpAdd symbol=__const_data_3 addend=0
  offset=0x000003f0 kind=Aarch64AdrpAdd symbol=__const_data_3 addend=0
  offset=0x000003fc kind=CallRel32 symbol=printf addend=0
  offset=0x00000400 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000040c kind=CallRel32 symbol=printf addend=0
  offset=0x000004b8 kind=Aarch64AdrpAdd symbol=__const_data_4 addend=0
  offset=0x000004e4 kind=Aarch64AdrpAdd symbol=__const_data_5 addend=0
  offset=0x00000510 kind=Aarch64AdrpAdd symbol=__const_data_6 addend=0
  offset=0x0000053c kind=Aarch64AdrpAdd symbol=__const_data_7 addend=0
  offset=0x00000698 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000006bc kind=CallRel32 symbol=printf addend=0
  offset=0x000006e8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000700 kind=CallRel32 symbol=printf addend=0
  offset=0x00000704 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000710 kind=Aarch64GotLoad symbol=__fp_const_02_string_processing_14 addend=0
  offset=0x00000718 kind=Aarch64GotLoad symbol=__fp_const_02_string_processing_14 addend=0
  offset=0x00000724 kind=Aarch64GotLoad symbol=__fp_const_02_string_processing_15 addend=0
  offset=0x0000072c kind=Aarch64GotLoad symbol=__fp_const_02_string_processing_15 addend=0
  offset=0x00000738 kind=CallRel32 symbol=printf addend=0
  offset=0x0000073c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000748 kind=Aarch64AdrpAdd symbol=__const_data_8 addend=0
  offset=0x00000750 kind=Aarch64AdrpAdd symbol=__const_data_8 addend=0
  offset=0x0000075c kind=CallRel32 symbol=printf addend=0
  offset=0x00000760 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000076c kind=Aarch64GotLoad symbol=__fp_const_02_string_processing_17 addend=0
  offset=0x00000774 kind=Aarch64GotLoad symbol=__fp_const_02_string_processing_17 addend=0
  offset=0x00000780 kind=CallRel32 symbol=printf addend=0

Section relocations:
  section=Data offset=0x00000000 kind=Abs64 symbol=__const_data_0 addend=0
  section=Data offset=0x00000010 kind=Abs64 symbol=__const_data_0 addend=0
  section=Data offset=0x00000020 kind=Abs64 symbol=__const_data_1 addend=0
  section=Data offset=0x00000030 kind=Abs64 symbol=__const_data_1 addend=0
  section=Data offset=0x00000040 kind=Abs64 symbol=__const_data_2 addend=0
  section=Data offset=0x00000050 kind=Abs64 symbol=__const_data_2 addend=0
  section=Data offset=0x00000060 kind=Abs64 symbol=__const_data_3 addend=0
  section=Data offset=0x00000070 kind=Abs64 symbol=__const_data_3 addend=0
  section=Data offset=0x00000080 kind=Abs64 symbol=__const_data_4 addend=0
  section=Data offset=0x00000090 kind=Abs64 symbol=__const_data_5 addend=0
  section=Data offset=0x000000a0 kind=Abs64 symbol=__const_data_6 addend=0
  section=Data offset=0x000000b0 kind=Abs64 symbol=__const_data_7 addend=0
  section=Data offset=0x000000c0 kind=Abs64 symbol=__const_data_4 addend=0
  section=Data offset=0x000000d0 kind=Abs64 symbol=__const_data_5 addend=0
  section=Data offset=0x000000e0 kind=Abs64 symbol=__const_data_6 addend=0
  section=Data offset=0x000000f0 kind=Abs64 symbol=__const_data_7 addend=0
  section=Data offset=0x00000100 kind=Abs64 symbol=__const_data_8 addend=0
  section=Data offset=0x00000110 kind=Abs64 symbol=__const_data_8 addend=0

.text (1952 bytes):
  00000000  ff 03 01 d1 fd 7b 03 a9  fd 03 00 91 1f 20 03 d5 
  00000010  f0 03 00 91 10 a2 00 91  f0 03 00 f9 50 01 80 d2 
  00000020  1f 02 00 f1 f0 17 9f 9a  f0 07 00 f9 f1 03 40 f9 
  00000030  f0 23 40 39 30 02 00 39  f0 03 40 f9 11 02 40 39 
  00000040  f1 0f 00 f9 e0 63 40 39  bf 03 00 91 fd 7b 43 a9 
  00000050  ff 03 01 91 c0 03 5f d6  ff 03 01 d1 fd 7b 03 a9 
  00000060  fd 03 00 91 1f 20 03 d5  f0 03 00 91 10 a2 00 91 
  00000070  f0 03 00 f9 50 01 80 d2  1f 02 00 f1 f0 17 9f 9a 
  00000080  f0 07 00 f9 f1 03 40 f9  f0 23 40 39 30 02 00 39 
  00000090  f0 03 40 f9 11 02 40 39  f1 0f 00 f9 e0 63 40 39 
  000000a0  bf 03 00 91 fd 7b 43 a9  ff 03 01 91 c0 03 5f d6 
  000000b0  ff 03 01 d1 fd 7b 03 a9  fd 03 00 91 1f 20 03 d5 
  000000c0  f0 03 00 91 10 a2 00 91  f0 03 00 f9 50 01 80 d2 
  000000d0  1f 16 00 f1 f0 d7 9f 9a  f0 07 00 f9 f1 03 40 f9 
  000000e0  f0 23 40 39 30 02 00 39  f0 03 40 f9 11 02 40 39 
  000000f0  f1 0f 00 f9 e0 63 40 39  bf 03 00 91 fd 7b 43 a9 
  00000100  ff 03 01 91 c0 03 5f d6  ff 03 01 d1 fd 7b 03 a9 
  00000110  fd 03 00 91 1f 20 03 d5  f0 03 00 91 10 a2 00 91 
  00000120  f0 03 00 f9 50 01 80 d2  1f 16 00 f1 f0 d7 9f 9a 
  00000130  f0 07 00 f9 f1 03 40 f9  f0 23 40 39 30 02 00 39 
  00000140  f0 03 40 f9 11 02 40 39  f1 0f 00 f9 e0 63 40 39 
  00000150  bf 03 00 91 fd 7b 43 a9  ff 03 01 91 c0 03 5f d6 
  00000160  ff 03 02 d1 fd 7b 07 a9  fd 03 00 91 1f 20 03 d5 
  00000170  f0 03 00 91 10 62 01 91  f0 03 00 f9 f0 03 00 91 
  00000180  10 82 01 91 f0 07 00 f9  50 01 80 d2 1f 22 00 f1 
  00000190  f0 d7 9f 9a f0 0b 00 f9  f1 07 40 f9 f0 43 40 39 
  000001a0  30 02 00 39 f0 07 40 f9  11 02 40 39 f1 13 00 f9 
  000001b0  f0 83 40 39 1f 06 00 f1  f0 17 9f 9a f0 17 00 f9 
  000001c0  f0 17 40 f9 1f 02 00 f1  41 00 00 54 05 00 00 14 
  000001d0  f1 03 40 f9 10 20 80 d2  30 02 00 f9 05 00 00 14 
  000001e0  f1 03 40 f9 10 10 80 d2  30 02 00 f9 01 00 00 14 
  000001f0  f0 03 40 f9 11 02 40 f9  f1 23 00 f9 e0 23 40 f9 
  00000200  bf 03 00 91 fd 7b 47 a9  ff 03 02 91 c0 03 5f d6 
  00000210  ff 03 02 d1 fd 7b 07 a9  fd 03 00 91 1f 20 03 d5 
  00000220  f0 03 00 91 10 62 01 91  f0 03 00 f9 f0 03 00 91 
  00000230  10 82 01 91 f0 07 00 f9  50 01 80 d2 1f 22 00 f1 
  00000240  f0 d7 9f 9a f0 0b 00 f9  f1 07 40 f9 f0 43 40 39 
  00000250  30 02 00 39 f0 07 40 f9  11 02 40 39 f1 13 00 f9 
  00000260  f0 83 40 39 1f 06 00 f1  f0 17 9f 9a f0 17 00 f9 
  00000270  f0 17 40 f9 1f 02 00 f1  41 00 00 54 05 00 00 14 
  00000280  f1 03 40 f9 10 20 80 d2  30 02 00 f9 05 00 00 14 
  00000290  f1 03 40 f9 10 10 80 d2  30 02 00 f9 01 00 00 14 
  000002a0  f0 03 40 f9 11 02 40 f9  f1 23 00 f9 e0 23 40 f9 
  000002b0  bf 03 00 91 fd 7b 47 a9  ff 03 02 91 c0 03 5f d6 
  000002c0  ff 03 0d d1 f0 03 00 91  10 c2 0c 91 1d 7a 00 a9 
  000002d0  fd 03 00 91 1f 20 03 d5  f0 03 00 91 10 a2 0a 91 
  000002e0  f0 13 00 f9 00 00 00 90  00 00 00 91 00 40 03 91 
  000002f0  00 00 00 94 00 00 00 90  00 00 00 91 00 e0 03 91 
  00000300  00 00 00 94 00 00 00 90  00 00 00 91 00 e0 04 91 
  00000310  00 00 00 94 00 00 00 90  00 00 00 91 00 a0 05 91 
  00000320  00 00 00 94 00 00 00 90  00 00 00 91 00 40 06 91 
  00000330  00 00 00 94 00 00 00 90  00 00 00 91 00 60 06 91 
  00000340  01 00 00 90 21 00 00 91  10 00 00 90 10 02 00 91 
  00000350  f0 03 00 f9 42 01 80 d2  50 01 80 d2 f0 07 00 f9 
  00000360  00 00 00 94 00 00 00 90  00 00 00 91 00 c0 06 91 
  00000370  01 00 00 90 21 00 00 91  10 00 00 90 10 02 00 91 
  00000380  f0 03 00 f9 a2 00 80 d2  b0 00 80 d2 f0 07 00 f9 
  00000390  00 00 00 94 00 00 00 90  00 00 00 91 00 20 07 91 
  000003a0  21 00 80 d2 30 00 80 d2  f0 03 00 f9 22 00 80 d2 
  000003b0  30 00 80 d2 f0 07 00 f9  23 00 80 d2 30 00 80 d2 
  000003c0  f0 0b 00 f9 00 00 00 94  00 00 00 90 00 00 00 91 
  000003d0  00 e0 07 91 01 00 00 90  21 00 00 91 10 00 00 90 
  000003e0  10 02 00 91 f0 03 00 f9  02 00 00 90 42 00 00 91 
  000003f0  10 00 00 90 10 02 00 91  f0 07 00 f9 00 00 00 94 
  00000400  00 00 00 90 00 00 00 91  00 60 08 91 00 00 00 94 
  00000410  f1 13 40 f9 10 00 80 d2  30 02 00 f9 01 00 00 14 
  00000420  f0 03 00 91 10 c2 0a 91  f0 43 00 f9 f0 13 40 f9 
  00000430  11 02 40 f9 f1 47 00 f9  f0 47 40 f9 1f 12 00 f1 
  00000440  f0 a7 9f 9a f0 4b 00 f9  f1 43 40 f9 f0 43 42 39 
  00000450  30 02 00 39 f0 43 40 f9  11 02 40 39 f1 53 00 f9 
  00000460  f0 83 42 39 1f 06 00 f1  f0 17 9f 9a f0 57 00 f9 
  00000470  f0 57 40 f9 1f 02 00 f1  41 00 00 54 9b 00 00 14 
  00000480  f0 03 00 91 10 e2 0a 91  f0 5b 00 f9 f0 13 40 f9 
  00000490  11 02 40 f9 f1 5f 00 f9  f1 5b 40 f9 f0 5f 40 f9 
  000004a0  30 02 00 f9 f0 03 00 91  10 02 0b 91 f0 67 00 f9 
  000004b0  f1 67 40 f9 e9 03 11 aa  10 00 00 90 10 02 00 91 
  000004c0  30 01 00 f9 b0 00 80 d2  10 00 a0 f2 10 00 c0 f2 
  000004d0  10 00 e0 f2 29 21 00 91  30 01 00 f9 e9 03 11 aa 
  000004e0  29 41 00 91 10 00 00 90  10 02 00 91 30 01 00 f9 
  000004f0  90 00 80 d2 10 00 a0 f2  10 00 c0 f2 10 00 e0 f2 
  00000500  29 21 00 91 30 01 00 f9  e9 03 11 aa 29 81 00 91 
  00000510  10 00 00 90 10 02 00 91  30 01 00 f9 b0 00 80 d2 
  00000520  10 00 a0 f2 10 00 c0 f2  10 00 e0 f2 29 21 00 91 
  00000530  30 01 00 f9 e9 03 11 aa  29 c1 00 91 10 00 00 90 
  00000540  10 02 00 91 30 01 00 f9  b0 00 80 d2 10 00 a0 f2 
  00000550  10 00 c0 f2 10 00 e0 f2  29 21 00 91 30 01 00 f9 
  00000560  f0 03 00 91 10 02 0c 91  f0 6f 00 f9 f0 13 40 f9 
  00000570  11 02 40 f9 f1 73 00 f9  f1 6f 40 f9 f0 73 40 f9 
  00000580  30 02 00 f9 f0 03 00 91  10 22 0c 91 f0 7b 00 f9 
  00000590  f1 7b 40 f9 b0 00 80 d2  10 00 a0 f2 10 00 c0 f2 
  000005a0  10 00 e0 f2 e9 03 11 aa  30 01 00 f9 90 00 80 d2 
  000005b0  10 00 a0 f2 10 00 c0 f2  10 00 e0 f2 e9 03 11 aa 
  000005c0  29 21 00 91 30 01 00 f9  b0 00 80 d2 10 00 a0 f2 
  000005d0  10 00 c0 f2 10 00 e0 f2  e9 03 11 aa 29 41 00 91 
  000005e0  30 01 00 f9 b0 00 80 d2  10 00 a0 f2 10 00 c0 f2 
  000005f0  10 00 e0 f2 e9 03 11 aa  29 61 00 91 30 01 00 f9 
  00000600  f0 5b 40 f9 11 02 40 f9  f1 83 00 f9 f0 83 40 f9 
  00000610  11 02 80 d2 10 7e 11 9b  f0 87 00 f9 f0 67 40 f9 
  00000620  f0 8b 00 f9 f0 8b 40 f9  f1 87 40 f9 10 02 11 8b 
  00000630  f0 8f 00 f9 f0 8f 40 f9  f0 93 00 f9 f0 93 40 f9 
  00000640  f0 97 00 f9 f0 97 40 f9  11 02 40 f9 f1 9b 00 f9 
  00000650  f0 6f 40 f9 11 02 40 f9  f1 9f 00 f9 f0 9f 40 f9 
  00000660  11 01 80 d2 10 7e 11 9b  f0 a3 00 f9 f0 7b 40 f9 
  00000670  f0 a7 00 f9 f0 a7 40 f9  f1 a3 40 f9 10 02 11 8b 
  00000680  f0 ab 00 f9 f0 ab 40 f9  f0 af 00 f9 f0 af 40 f9 
  00000690  11 02 40 f9 f1 b3 00 f9  00 00 00 90 00 00 00 91 
  000006a0  00 80 08 91 e1 9b 40 f9  f0 9b 40 f9 f0 03 00 f9 
  000006b0  e2 b3 40 f9 f0 b3 40 f9  f0 07 00 f9 00 00 00 94 
  000006c0  f0 13 40 f9 11 02 40 f9  f1 bb 00 f9 f0 bb 40 f9 
  000006d0  10 06 00 91 f0 bf 00 f9  f1 13 40 f9 f0 bf 40 f9 
  000006e0  30 02 00 f9 4f ff ff 17  00 00 00 90 00 00 00 91 
  000006f0  00 e0 08 91 61 02 80 d2  70 02 80 d2 f0 03 00 f9 
  00000700  00 00 00 94 00 00 00 90  00 00 00 91 00 40 09 91 
  00000710  01 00 00 90 21 00 40 f9  10 00 00 90 10 02 40 f9 
  00000720  f0 03 00 f9 02 00 00 90  42 00 40 f9 10 00 00 90 
  00000730  10 02 40 f9 f0 07 00 f9  00 00 00 94 00 00 00 90 
  00000740  00 00 00 91 00 a0 09 91  01 00 00 90 21 00 00 91 
  00000750  10 00 00 90 10 02 00 91  f0 03 00 f9 00 00 00 94 
  00000760  00 00 00 90 00 00 00 91  00 e0 09 91 01 00 00 90 
  00000770  21 00 40 f9 10 00 00 90  10 02 40 f9 f0 03 00 f9 
  00000780  00 00 00 94 bf 03 00 91  f0 03 00 91 10 c2 0c 91 
  00000790  1d 7a 40 a9 ff 03 0d 91  00 00 80 d2 c0 03 5f d6 

.rodata (650 bytes):
  00000000  46 65 72 72 6f 50 68 61  73 65 00 30 2e 31 2e 30 
  00000010  00 00 00 00 00 00 00 00  0a 00 00 00 00 00 00 00 
  00000020  0a 00 00 00 00 00 00 00  05 00 00 00 00 00 00 00 
  00000030  05 00 00 00 00 00 00 00  01 01 01 01 01 01 46 65 
  00000040  72 72 6f 00 50 68 61 73  65 00 61 6c 70 68 61 00 
  00000050  62 65 74 61 00 67 61 6d  6d 61 00 64 65 6c 74 61 
  00000060  00 00 00 00 00 00 00 00  05 00 00 00 00 00 00 00 
  00000070  04 00 00 00 00 00 00 00  05 00 00 00 00 00 00 00 
  00000080  05 00 00 00 00 00 00 00  05 00 00 00 00 00 00 00 
  00000090  04 00 00 00 00 00 00 00  05 00 00 00 00 00 00 00 
  000000a0  05 00 00 00 00 00 00 00  13 00 00 00 00 00 00 00 
  000000b0  13 00 00 00 00 00 00 00  46 65 72 72 6f 50 68 61 
  000000c0  73 65 20 76 30 2e 31 2e  30 00 00 00 00 00 00 00 
  000000d0  f0 9f 93 98 20 54 75 74  6f 72 69 61 6c 3a 20 30 
  000000e0  32 5f 73 74 72 69 6e 67  5f 70 72 6f 63 65 73 73 
  000000f0  69 6e 67 2e 66 70 0a 00  f0 9f a7 ad 20 46 6f 63 
  00000100  75 73 3a 20 43 6f 6d 70  69 6c 65 2d 74 69 6d 65 
  00000110  20 73 74 72 69 6e 67 20  6f 70 65 72 61 74 69 6f 
  00000120  6e 73 20 61 6e 64 20 69  6e 74 72 69 6e 73 69 63 
  00000130  73 0a 00 00 00 00 00 00  f0 9f a7 aa 20 57 68 61 
  00000140  74 20 74 6f 20 6c 6f 6f  6b 20 66 6f 72 3a 20 6c 
  00000150  61 62 65 6c 65 64 20 6f  75 74 70 75 74 73 20 62 
  00000160  65 6c 6f 77 0a 00 00 00  e2 9c 85 20 45 78 70 65 
  00000170  63 74 61 74 69 6f 6e 3a  20 6f 75 74 70 75 74 73 
  00000180  20 6d 61 74 63 68 20 6c  61 62 65 6c 73 0a 00 00 
  00000190  0a 00 00 00 00 00 00 00  6e 61 6d 65 3d 27 25 73 
  000001a0  27 20 6c 65 6e 3d 25 6c  6c 75 0a 00 00 00 00 00 
  000001b0  76 65 72 73 69 6f 6e 3d  27 25 73 27 20 6c 65 6e 
  000001c0  3d 25 6c 6c 75 0a 00 00  70 72 65 66 69 78 5f 6f 
  000001d0  6b 3d 25 64 2c 20 73 75  66 66 69 78 5f 6f 6b 3d 
  000001e0  25 64 2c 20 63 6f 6e 74  61 69 6e 73 5f 70 68 61 
  000001f0  73 65 3d 25 64 0a 00 00  73 6c 69 63 65 73 3a 20 
  00000200  73 68 6f 72 74 3d 27 25  73 27 20 74 61 69 6c 3d 
  00000210  27 25 73 27 0a 00 00 00  77 6f 72 64 73 3a 0a 00 
  00000220  20 20 25 73 20 2d 3e 20  6c 65 6e 3d 25 6c 6c 75 
  00000230  0a 00 00 00 00 00 00 00  74 6f 74 61 6c 20 77 6f 
  00000240  72 64 20 6c 65 6e 67 74  68 3d 25 6c 6c 75 0a 00 
  00000250  65 6d 70 74 79 3d 25 64  2c 20 6c 6f 6e 67 3d 25 
  00000260  64 0a 00 00 00 00 00 00  62 61 6e 6e 65 72 3d 27 
  00000270  25 73 27 0a 00 00 00 00  62 75 66 66 65 72 5f 73 
  00000280  69 7a 65 3d 25 6c 6c 75  0a 00 
