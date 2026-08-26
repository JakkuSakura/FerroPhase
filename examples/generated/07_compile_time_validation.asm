fp-native dump: format=MachO arch=Aarch64 entry=0x0

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
global __const_data_0 ty=Array(I8, 5) constant=true initializer=Some(Bytes([68, 97, 116, 97, 0]))
global DATA_TYPE_NAME ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0]))
global __const_data_1 ty=Array(I8, 4) constant=true initializer=Some(Bytes([105, 54, 52, 0]))
global DATA_FIELD_A_TYPE ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0]))
global __const_data_2 ty=Array(I8, 3) constant=true initializer=Some(Bytes([117, 56, 0]))
global HEADER_FIELD_VERSION_TYPE ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0]))
global MAX_SIZE ty=I64 constant=true initializer=Some(Bytes([64, 0, 0, 0, 0, 0, 0, 0]))
fn mach_memory_object_memory_entry_64
fn getgrent
fn getpwnam_r
fn vfork
fn shm_unlink
fn task_set_policy
fn task_map_corpse_info_64
fn posix_spawn_file_actions_adddup2
fn vm_remap
fn dlopen
fn opendir
fn task_set_emulation
fn munmap
fn gmtime
fn iswspecial
fn task_suspend2
fn task_set_mach_voucher
fn mach_port_assert_attributes
fn strcspn
fn vm_region_recurse
fn _Exit
fn setsid
fn feupdateenv
fn _OSReadInt16
fn freopen
fn semop
fn kext_request
fn gethostbyname
fn tempnam
fn __assert_rtn
fn memcpy
fn fstatvfs
fn posix_spawnattr_init
fn host_get_boot_info
fn task_assign
fn vm_write
fn macx_swapon
fn NSAddLibrary
fn wcschr
fn task_register_dyld_image_infos
fn fclose
fn vsscanf
fn iswphonogram
fn strnlen
fn processor_set_tasks
fn sigrelse
fn host_processors
fn vfwscanf
fn vm_inherit
fn vm_copy
fn NXSwapHostLongLongToLittle
fn ctime
fn mach_port_request_notification
fn host_kernel_version
fn getdelim
fn msgrcv
fn fchown
fn putwc
fn wcpncpy
fn if_indextoname
fn kmod_get_info
fn posix_spawn_file_actions_destroy
fn duplocale
fn lcong48
fn setservent
fn ffs
fn gethostname
fn __darwin_fd_clr
fn host_set_special_port
fn clonefileat
fn wcpcpy
fn sched_get_priority_max
fn mach_msg
fn posix_spawnattr_getsigmask
fn gai_strerror
fn setstate
fn aio_write
fn mach_port_get_refs
fn creat
fn setreuid
fn iswascii
fn swtch_pri
fn wmemcmp
fn NXSwapHostLongLongToBig
fn setpgrp
fn utimes
fn NXSwapBigLongLongToHost
fn isxdigit
fn ___tolower
fn remove
fn posix_spawnattr_setsigmask
fn localeconv
fn getaddrinfo
fn sighold
fn tcsendbreak
fn close
fn getpid
fn __vsprintf_chk
fn pause
fn __NDR_convert__mig_reply_error_t
fn mach_port_guard
fn seed48
fn voucher_mach_msg_set
fn mach_port_names
fn iswideogram
fn initstate
fn regfree
fn NXSwapHostIntToBig
fn NXSwapHostLongToBig
fn getgrnam
fn fsetxattr
fn posix_spawn_file_actions_init
fn dup2
fn mach_port_construct
fn iswalpha
fn fputws
fn mach_port_move_member
fn fflush
fn vswscanf
fn listen
fn readlinkat
fn localtime_r
fn wcrtomb
fn endgrent
fn fesetround
fn newlocale
fn mktemp
fn iswnumber
fn getgid
fn getuid
fn labs
fn srand
fn munlockall
fn shmdt
fn task_swap_exception_ports
fn mach_voucher_deallocate
fn iswlower
fn shmctl
fn fchownat
fn unlinkat
fn getc_unlocked
fn __vsnprintf_chk
fn fstatat
fn thread_wire
fn host_get_special_port
fn regcomp
fn task_get_exception_ports
fn task_set_emulation_vector
fn thread_swap_mach_voucher
fn host_create_mach_voucher
fn etap_trace_thread
fn task_purgable_info
fn wait
fn lock_set_create
fn _kernelrpc_mach_port_extract_member_trap
fn NSIsSymbolNameDefined
fn task_policy_get
fn task_dyld_process_info_notify_register
fn thread_assign
fn socket
fn sem_trywait
fn iswblank
fn vscanf
fn strndup
fn getrlimit
fn fread
fn aio_error
fn closedir
fn mktime
fn send
fn pclose
fn posix_spawn
fn select
fn processor_set_stack_usage
fn getwc
fn task_test_async_upcall_propagation
fn islower
fn iswpunct
fn fileno
fn setjmp
fn sigpending
fn getsid
fn vm_mapped_pages_info
fn mach_port_set_mscount
fn thread_policy
fn processor_info
fn aligned_alloc
fn raise
fn _kernelrpc_mach_port_insert_right_trap
fn task_dyld_process_info_notify_get
fn setregid
fn wcscoll
fn getc
fn lseek
fn futimens
fn __isctype
fn host_create_mach_voucher_trap
fn mach_port_destroy
fn processor_control
fn thread_policy_set
fn strtol
fn _OSWriteSwapInt64
fn getgrnam_r
fn regexec
fn _OSReadSwapInt32
fn wcsnlen
fn vm_map
fn malloc
fn ctermid
fn cfgetispeed
fn processor_set_threads
fn sched_yield
fn task_register_dyld_shared_cache_image_info
fn mach_port_allocate_qos
fn vsprintf
fn posix_memalign
fn strtoll
fn sigemptyset
fn strcpy
fn wcslen
fn tcsetattr
fn lrand48
fn utime
fn ftok
fn host_set_UNDServer
fn ualarm
fn vm_machine_attribute
fn mach_port_space_info
fn getwchar
fn mach_vm_region_info
fn _kernelrpc_mach_port_mod_refs_trap
fn ptsname
fn msgctl
fn mach_port_kobject
fn clock_settime
fn if_nametoindex
fn clock_gettime
fn toupper
fn getservbyport
fn _kernelrpc_mach_vm_protect_trap
fn _kernelrpc_mach_port_guard_trap
fn task_self_trap
fn unsetenv
fn pselect
fn mkfifo
fn vm_map_page_query
fn processor_set_create
fn host_lockgroup_info
fn mig_reply_setup
fn NSGetSectionDataInObjectFileImage
fn lockf
fn _kernelrpc_mach_vm_deallocate_trap
fn unlink
fn mbrlen
fn ferror
fn setbuf
fn iswcntrl
fn remque
fn vwscanf
fn chown
fn encrypt
fn feclearexcept
fn fegetround
fn feof
fn pthread_kill
fn strtoull
fn fgetwc
fn getpeername
fn mach_port_get_service_port_info
fn _kernelrpc_mach_port_deallocate_trap
fn feraiseexcept
fn shutdown
fn alarm
fn wcscat
fn ftello
fn l64a
fn clock_getres
fn sysconf
fn atoi
fn mach_error_string
fn _dyld_launched_prebound
fn socketpair
fn semaphore_signal
fn getsubopt
fn getgrgid
fn NSVersionOfRunTimeLibrary
fn task_threads
fn NSDestroyObjectFileImage
fn clock_set_res
fn NSNameOfModule
fn NSLinkEditError
fn _dyld_get_image_vmaddr_slide
fn setgrent
fn wcsncmp
fn pathconf
fn strtok
fn NSIsSymbolNameDefinedWithHint
fn ___runetype
fn getpwent
fn wcsncpy
fn strdup
fn killpg
fn posix_spawnattr_destroy
fn readlink
fn _OSWriteSwapInt32
fn mach_port_get_context
fn mach_port_destruct
fn clock
fn wcscspn
fn task_set_phys_footprint_limit
fn mach_port_guard_with_flags
fn NSAddImage
fn __darwin_check_fd_set
fn fsetpos
fn mach_msg_receive
fn NXSwapFloat
fn NSLookupAndBindSymbolWithHint
fn crypt
fn thread_get_exception_ports_info
fn task_policy
fn _kernelrpc_mach_vm_allocate_trap
fn mach_port_set_context
fn fputwc
fn NSIsSymbolNameDefinedInImage
fn _dyld_image_containing_address
fn strcasecmp
fn wcspbrk
fn hsearch
fn linkat
fn memccpy
fn isprint
fn recv
fn pthread_key_delete
fn sched_get_priority_min
fn tcsetpgrp
fn semaphore_wait_signal
fn thread_adopt_exception_handler
fn mach_error
fn waitpid
fn iconv
fn open_memstream
fn mig_deallocate
fn fputc
fn wmemmove
fn strcmp
fn strcat
fn posix_spawnattr_getflags
fn vm_purgable_control
fn kqueue
fn kevent64
fn sendmsg
fn _dyld_lookup_and_bind_with_hint
fn getpwuid
fn mach_port_mod_refs
fn sigprocmask
fn random
fn hdestroy
fn posix_spawn_file_actions_addchdir
fn vfwprintf
fn ctime_r
fn posix_spawnattr_setflags
fn fetestexcept
fn fgetws
fn iscntrl
fn execve
fn wcstoul
fn vm_wire
fn mach_port_deallocate
fn NXSwapBigLongToHost
fn NSCreateObjectFileImageFromFile
fn host_security_create_task_token
fn munlock
fn psignal
fn mig_dealloc_reply_port
fn host_get_clock_control
fn thread_abort_safely
fn mach_port_set_seqno
fn vm_allocate_cpm
fn wcstok
fn mach_msg_destroy
fn kill
fn pthread_sigmask
fn mach_port_peek
fn mbrtowc
fn getentropy
fn __maskrune
fn grantpt
fn posix_spawnp
fn _dyld_lookup_and_bind_fully
fn vprintf
fn strsignal
fn setnetent
fn sem_close
fn wcsspn
fn putchar_unlocked
fn srandom
fn vwprintf
fn sem_unlink
fn endpwent
fn posix_spawnattr_getpgroup
fn debug_control_port_for_pid
fn task_create
fn getservent
fn _OSReadSwapInt64
fn getnameinfo
fn processor_start
fn act_get_state
fn wcsftime
fn NSSymbolReferenceCountInObjectFileImage
fn globfree
fn mach_port_get_srights
fn towctrans
fn _longjmp
fn mach_port_insert_member
fn umask
fn getchar_unlocked
fn __math_errhandling
fn rand_r
fn wctomb
fn host_get_multiuser_config_flags
fn fclonefileat
fn _kernelrpc_mach_port_allocate_trap
fn memchr
fn iswprint
fn strncat
fn task_resume
fn mig_get_reply_port
fn mach_port_kobject_description
fn _OSSwapInt16
fn putenv
fn processor_assign
fn wcstombs
fn inet_ntoa
fn getlogin_r
fn task_set_exception_ports
fn task_get_exception_ports_info
fn mach_port_allocate_name
fn mach_port_extract_member
fn NXSwapLong
fn _dyld_get_image_header
fn vm_region
fn memmove
fn strtoimax
fn _OSReadInt64
fn NXSwapLittleIntToHost
fn cfsetospeed
fn NSSymbolDefinitionCountInObjectFileImage
fn clock_get_res
fn unlockpt
fn NSCreateObjectFileImageFromMemory
fn isblank
fn realloc
fn setvbuf
fn strerror
fn wctype
fn mkdir
fn act_set_state
fn ldiv
fn iswrune
fn fork
fn pthread_getconcurrency
fn closelog
fn toascii
fn nrand48
fn sem_init
fn _OSWriteInt64
fn host_priv_statistics
fn exit
fn mkdirat
fn posix_spawn_file_actions_addclose
fn wcscpy
fn processor_set_destroy
fn iswgraph
fn semaphore_wait
fn vm_read_overwrite
fn realpath
fn getitimer
fn mach_port_type
fn mach_port_extract_right
fn _setjmp
fn fseeko
fn mach_port_unguard
fn semaphore_timedwait_signal
fn posix_madvise
fn processor_set_max_priority
fn inet_addr
fn semaphore_signal_all
fn asctime_r
fn mach_port_space_basic_info
fn task_inspect
fn host_get_io_main
fn _kernelrpc_mach_port_destruct_trap
fn semget
fn task_map_kcdata_object_64
fn ungetc
fn iswupper
fn wctob
fn wmemcpy
fn connect
fn hcreate
fn fchdir
fn OSHostByteOrder
fn thread_get_assignment
fn isspace
fn host_request_notification
fn NSLookupAndBindSymbol
fn popen
fn getgrgid_r
fn getservbyname
fn link
fn NXSwapLongLong
fn NXSwapHostShortToLittle
fn wcstoll
fn fgetxattr
fn _dyld_bind_fully_image_containing_address
fn swtch
fn host_check_multiuser_mode
fn strcoll
fn getpwuid_r
fn mach_msg_send
fn _OSSwapInt32
fn endnetent
fn execv
fn setpwent
fn getpgrp
fn isatty
fn getpgid
fn kmod_control
fn thread_suspend
fn thread_info
fn thread_assign_default
fn mach_vm_region_info_64
fn macx_triggers
fn sigdelset
fn atol
fn flockfile
fn mig_allocate
fn fseek
fn getnetent
fn tcgetsid
fn thread_set_exception_ports
fn thread_sample
fn vm_read
fn host_processor_sets
fn regerror
fn vm_map_64
fn NXSwapBigShortToHost
fn sem_getvalue
fn times
fn sigaction
fn fdopendir
fn geteuid
fn getrusage
fn __svfscanf
fn posix_spawnattr_setpgroup
fn llabs
fn pread
fn open_wmemstream
fn thread_get_exception_ports
fn _kernelrpc_mach_port_request_notification_trap
fn NSUnLinkModule
fn putchar
fn endprotoent
fn tmpnam
fn setsockopt
fn putc_unlocked
fn readdir_r
fn fesetenv
fn __sigbits
fn div
fn dlerror
fn pwrite
fn NSLookupSymbolInImage
fn setgrfile
fn funlockfile
fn tcdrain
fn mbtowc
fn sem_post
fn mmap
fn mach_error_type
fn _dyld_all_twolevel_modules_prebound
fn task_swap_mach_voucher
fn getlogin
fn tolower
fn mach_generate_activity_id
fn setxattr
fn mach_task_is_self
fn isalnum
fn isdigit
fn _kernelrpc_mach_port_unguard_trap
fn mach_vm_reclaim_update_kernel_accounting_trap
fn vdprintf
fn wcsstr
fn confstr
fn aio_return
fn task_get_emulation_vector
fn lchown
fn getxattr
fn getppid
fn sigismember
fn voucher_mach_msg_clear
fn fpathconf
fn mig_put_reply_port
fn fputs
fn strlen
fn setlogmask
fn thread_get_special_port
fn thread_swap_exception_ports
fn fgetpos
fn clearerr
fn jrand48
fn getenv
fn strstr
fn if_nameindex
fn access
fn processor_exit
fn setitimer
fn thread_resume
fn wcstol
fn iswhexnumber
fn iconv_close
fn dup
fn getegid
fn _OSWriteInt16
fn mach_ports_lookup
fn mach_port_insert_right
fn host_statistics64
fn iswalnum
fn macx_backing_store_recovery
fn task_name_for_pid
fn atomic_flag_test_and_set_explicit
fn wcswidth
fn sigfillset
fn wcsncat
fn setuid
fn thread_get_mach_voucher
fn fmemopen
fn vm_behavior_set
fn atomic_flag_clear
fn mach_port_is_connection_for_service
fn mach_zone_info_for_zone
fn sigsetjmp
fn mbsnrtowcs
fn abort
fn recvmsg
fn setlocale
fn gethostent
fn task_suspend
fn NXSwapBigIntToHost
fn vm_protect
fn processor_set_tasks_with_flavor
fn freelocale
fn fwrite
fn _OSReadInt32
fn sockatmark
fn iconv_open
fn slot_name
fn task_policy_set
fn clock_sleep_trap
fn _NSGetExecutablePath
fn longjmp
fn fgetc
fn rename
fn asctime
fn ttyname
fn semaphore_destroy
fn ftell
fn a64l
fn vm_region_recurse_64
fn __toupper
fn _OSSwapInt64
fn task_sample
fn task_set_special_port
fn task_set_state
fn thread_set_policy
fn rmdir
fn host_statistics
fn tcflow
fn vswprintf
fn NXSwapDouble
fn NSAddressOfSymbol
fn chdir
fn host_security_set_task_token
fn _kernelrpc_mach_vm_purgable_control_trap
fn ungetwc
fn host_page_size
fn _kernelrpc_mach_port_move_member_trap
fn mach_memory_info
fn panic_init
fn tcflush
fn NSInstallLinkEditErrorHandlers
fn pipe
fn sync
fn getchar
fn perror
fn getline
fn setpriority
fn strxfrm
fn host_processor_set_priv
fn thread_set_mach_voucher
fn getsockname
fn strftime
fn sendto
fn truncate
fn task_set_port_space
fn processor_set_default
fn bind
fn _tlv_bootstrap
fn pid_for_task
fn wcsncasecmp
fn processor_get_assignment
fn task_register_dyld_set_dyld_state
fn thread_set_state
fn _dyld_shared_cache_contains_path
fn __wcwidth
fn nice
fn thread_switch
fn setenv
fn read
fn mach_zone_info
fn _dyld_lookup_and_bind
fn mig_strncpy_zerofill
fn strtok_r
fn clock_sleep
fn NSLookupSymbolInModule
fn listxattr
fn task_get_dyld_image_infos
fn macx_backing_store_suspend
fn lstat
fn task_for_pid
fn strtoul
fn host_processor_info
fn __sputc
fn strncmp
fn strtoumax
fn getgroups
fn task_resume2
fn NXSwapLittleShortToHost
fn NSLibraryNameForModule
fn removexattr
fn endservent
fn freeaddrinfo
fn mprotect
fn task_get_assignment
fn putwchar
fn poll
fn tcgetpgrp
fn host_default_memory_manager
fn mlock
fn utimensat
fn strrchr
fn putc
fn clock_set_attributes
fn __tolower
fn wcstoimax
fn atomic_signal_fence
fn processor_set_policy_enable
fn semaphore_signal_thread
fn msgsnd
fn task_terminate
fn thread_terminate
fn imaxabs
fn ___toupper
fn __istype
fn wmemset
fn __darwin_fd_set
fn setgroupent
fn NSAddLibraryWithSearching
fn sigwait
fn voucher_mach_msg_adopt
fn gethostid
fn towupper
fn isgraph
fn wcsnrtombs
fn _dyld_get_image_header_containing_address
fn fchmodat
fn sethostent
fn task_set_exc_guard_behavior
fn siglongjmp
fn mbsrtowcs
fn host_virtual_physical_table_info
fn NSSymbolDefinitionNameInObjectFileImage
fn telldir
fn task_get_mach_voucher
fn thread_policy_get
fn free
fn atomic_flag_test_and_set
fn aio_suspend
fn wcsrtombs
fn stpcpy
fn setgid
fn inet_pton
fn kmod_create
fn sigaddset
fn host_get_UNDServer
fn swab
fn vm_msync
fn strspn
fn setegid
fn mach_make_memory_entry_64
fn _kernelrpc_mach_vm_map_trap
fn host_set_multiuser_config_flags
fn NSLinkModule
fn NXHostByteOrder
fn abs
fn thread_abort
fn NXSwapHostLongToLittle
fn localtime
fn mach_make_memory_entry
fn _dyld_get_image_name
fn strpbrk
fn atoll
fn strncasecmp
fn sem_wait
fn cfsetispeed
fn task_set_info
fn task_identity_token_get_task_port
fn clonefile
fn posix_spawnattr_getsigdefault
fn seteuid
fn mrand48
fn lldiv
fn aio_fsync
fn endhostent
fn mbstowcs
fn nanosleep
fn host_swap_exception_ports
fn _kernelrpc_mach_port_type_trap
fn msync
fn ftrylockfile
fn fopen
fn getcwd
fn host_info
fn task_get_state
fn vsnprintf
fn wcsdup
fn fgets
fn processor_set_info
fn NSVersionOfLinkTimeLibrary
fn mach_port_kernel_object
fn posix_spawnattr_setsigdefault
fn wcsxfrm
fn NSIsSymbolDefinedInObjectFileImage
fn btowc
fn vfscanf
fn fchmod
fn getdate
fn thread_create
fn openlog
fn gethostbyaddr
fn sigsuspend
fn _dyld_image_count
fn mknod
fn statvfs
fn isascii
fn fdopen
fn wcsrchr
fn setpgid
fn task_register_dyld_get_process_state
fn getpwnam
fn strerror_r
fn gmtime_r
fn tzset
fn sigignore
fn dlsym
fn mach_voucher_extract_attr_recipe_trap
fn vm_allocate
fn task_info
fn _OSWriteInt32
fn inet_ntop
fn fstat
fn mknodat
fn NSSymbolReferenceNameInObjectFileImage
fn wcstoull
fn task_assign_default
fn imaxdiv
fn tmpfile
fn cfgetospeed
fn thread_create_running
fn atomic_thread_fence
fn host_register_well_known_mach_voucher_attr_manager
fn NXSwapLittleLongToHost
fn __srget
fn getnetbyaddr
fn voucher_mach_msg_revert
fn recvfrom
fn insque
fn _OSWriteSwapInt16
fn thread_depress_abort
fn host_get_clock_service
fn ispunct
fn fnmatch
fn task_set_ras_pc
fn lio_listio
fn task_dyld_process_info_notify_deregister
fn seekdir
fn ftruncate
fn task_get_special_port
fn kevent
fn vm_read_list
fn fsync
fn gets
fn shmget
fn siginterrupt
fn iswdigit
fn sigpause
fn dirfd
fn strchr
fn _OSReadSwapInt16
fn task_unregister_dyld_image_infos
fn thread_convert_thread_state
fn mach_port_get_attributes
fn macx_swapoff
fn setprotoent
fn uname
fn wcscasecmp
fn vm_stats
fn iswspace
fn task_map_corpse_info
fn vm_deallocate
fn mach_port_allocate_full
fn system
fn setrlimit
fn _exit
fn setkey
fn quick_exit
fn calloc
fn wcwidth
fn getsockopt
fn readdir
fn rewinddir
fn getprotoent
fn tcgetattr
fn if_freenameindex
fn write
fn chmod
fn processor_set_statistics
fn mblen
fn sem_destroy
fn posix_spawn_file_actions_addfchdir
fn msgget
fn strncpy
fn task_get_exc_guard_behavior
fn rewind
fn dlclose
fn uselocale
fn getprotobyname
fn pthread_setconcurrency
fn aio_cancel
fn mach_port_allocate
fn host_set_atm_diagnostic_flag
fn mach_memory_object_memory_entry
fn NXSwapHostShortToBig
fn getopt
fn fegetexceptflag
fn task_create_identity_token
fn flistxattr
fn semaphore_timedwait
fn isupper
fn _kernelrpc_mach_port_construct_trap
fn fegetenv
fn mbsinit
fn __error
fn usleep
fn processor_set_policy_control
fn mach_port_swap_guard
fn ttyname_r
fn alphasort
fn NXSwapShort
fn memset
fn NXSwapLittleLongLongToHost
fn time
fn processor_set_policy_disable
fn aio_read
fn feholdexcept
fn fwide
fn sleep
fn mlockall
fn sigaltstack
fn _dyld_present
fn fremovexattr
fn _kernelrpc_mach_port_insert_member_trap
fn execvp
fn timespec_get
fn mach_msg_overwrite
fn mach_vm_wire
fn wctrans
fn task_test_sync_upcall
fn getnetbyname
fn rand
fn symlink
fn stat
fn __darwin_check_fd_set_overflow
fn towlower
fn getpriority
fn thread_set_special_port
fn posix_spawn_file_actions_addopen
fn faccessat
fn stpncpy
fn shmat
fn NXSwapInt
fn accept
fn posix_openpt
fn puts
fn mig_strncpy
fn getprotobynumber
fn memcmp
fn waitid
fn strptime
fn mach_port_rename
fn _host_page_size
fn host_get_atm_diagnostic_flag
fn mach_port_dnrequest_info
fn dirname
fn isalpha
fn iswxdigit
fn NSModuleForSymbol
fn thread_get_state
fn mach_host_self
fn semaphore_create
fn lock_set_destroy
fn vfprintf
fn task_register_hardened_exception_handler
fn mach_port_set_attributes
fn wcscmp
fn wmemchr
fn mkstemp
fn kmod_destroy
fn task_set_corpse_forking_behavior
fn mach_thread_self
fn NSNameOfSymbol
fn atomic_flag_clear_explicit
fn __swbuf
fn mkfifoat
fn clock_set_time
fn basename
fn symlinkat
fn host_get_exception_ports
fn task_wire
fn iswctype
fn host_reboot
fn NXSwapHostIntToLittle
fn wcstoumax
fn gettimeofday
fn pthread_testcancel
fn task_zone_info
fn host_register_mach_voucher_attr_manager
fn task_generate_corpse
fn srand48
fn host_set_exception_ports
fn vm_region_64
fn fesetexceptflag
fn __darwin_fd_isset
fn mach_ports_register
fn vm_remap_new
fn nl_langinfo
fn _kernelrpc_mach_port_get_attributes_trap
fn vm_map_exec_lockdown
fn renameat
fn mach_port_get_set_status
fn main
  bb0 bb0
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    alloca Virtual { id: 85, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 85, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 32
    alloca Virtual { id: 87, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 87, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 3
    load Virtual { id: 89, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 85, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 90, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 87, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 89, bank: General, size_bits: 64 }, Virtual { id: 90, bank: General, size_bits: 64 }
    alloca Virtual { id: 92, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 92, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 1
    alloca Virtual { id: 94, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 94, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 0
    load Virtual { id: 96, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 92, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 97, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 94, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 96, bank: General, size_bits: 8 }, Virtual { id: 97, bank: General, size_bits: 8 }
    alloca Virtual { id: 99, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 99, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 16
    alloca Virtual { id: 101, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 101, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 4
    alloca Virtual { id: 103, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 103, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 1
    load Virtual { id: 105, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 99, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 106, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 101, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 107, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 103, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 105, bank: General, size_bits: 64 }, Virtual { id: 106, bank: General, size_bits: 64 }, Virtual { id: 107, bank: General, size_bits: 8 }
    intrinsic.call symbol(intrinsic.println), symbol(__const_data_0), symbol(__const_data_1), symbol(__const_data_2)
    alloca Virtual { id: 110, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 110, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 0
    load Virtual { id: 112, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 110, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 112, bank: General, size_bits: 8 }
    alloca Virtual { id: 114, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 114, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 32
    alloca Virtual { id: 116, bank: General, size_bits: 64 }, 1
    load Virtual { id: 117, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 114, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    le Virtual { id: 118, bank: General, size_bits: 8 }, Virtual { id: 117, bank: General, size_bits: 64 }, 64
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 116, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 118, bank: General, size_bits: 8 }
    alloca Virtual { id: 120, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 120, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 16
    alloca Virtual { id: 122, bank: General, size_bits: 64 }, 1
    load Virtual { id: 123, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 120, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    le Virtual { id: 124, bank: General, size_bits: 8 }, Virtual { id: 123, bank: General, size_bits: 64 }, 64
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 122, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 124, bank: General, size_bits: 8 }
    alloca Virtual { id: 126, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 126, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 32
    alloca Virtual { id: 128, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 128, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 16
    alloca Virtual { id: 130, bank: General, size_bits: 64 }, 1
    load Virtual { id: 131, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 126, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 132, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 128, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 133, bank: General, size_bits: 64 }, Virtual { id: 131, bank: General, size_bits: 64 }, Virtual { id: 132, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 130, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 133, bank: General, size_bits: 64 }
    alloca Virtual { id: 135, bank: General, size_bits: 64 }, 1
    load Virtual { id: 136, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 130, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    le Virtual { id: 137, bank: General, size_bits: 8 }, Virtual { id: 136, bank: General, size_bits: 64 }, 96
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 135, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 137, bank: General, size_bits: 8 }
    alloca Virtual { id: 139, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 139, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 32
    alloca Virtual { id: 141, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 141, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 16
    alloca Virtual { id: 143, bank: General, size_bits: 64 }, 1
    load Virtual { id: 144, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 139, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 145, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 141, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 146, bank: General, size_bits: 64 }, Virtual { id: 144, bank: General, size_bits: 64 }, Virtual { id: 145, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 143, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 146, bank: General, size_bits: 64 }
    load Virtual { id: 148, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 116, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 149, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 122, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 150, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 135, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 151, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 143, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 148, bank: General, size_bits: 8 }, Virtual { id: 149, bank: General, size_bits: 8 }, Virtual { id: 150, bank: General, size_bits: 8 }, Virtual { id: 151, bank: General, size_bits: 64 }
    ret


Symbols:
  main                             0x00000000

Text relocations:
  offset=0x00000018 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000024 kind=CallRel32 symbol=printf addend=0
  offset=0x00000028 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000034 kind=CallRel32 symbol=printf addend=0
  offset=0x00000038 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000044 kind=CallRel32 symbol=printf addend=0
  offset=0x00000048 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000054 kind=CallRel32 symbol=printf addend=0
  offset=0x00000058 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000064 kind=CallRel32 symbol=printf addend=0
  offset=0x000000b0 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000000d4 kind=CallRel32 symbol=printf addend=0
  offset=0x00000120 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000144 kind=CallRel32 symbol=printf addend=0
  offset=0x000001b4 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000001e4 kind=CallRel32 symbol=printf addend=0
  offset=0x000001e8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000001f4 kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x000001fc kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x00000208 kind=Aarch64AdrpAdd symbol=__const_data_1 addend=0
  offset=0x00000210 kind=Aarch64AdrpAdd symbol=__const_data_1 addend=0
  offset=0x0000021c kind=Aarch64AdrpAdd symbol=__const_data_2 addend=0
  offset=0x00000224 kind=Aarch64AdrpAdd symbol=__const_data_2 addend=0
  offset=0x00000230 kind=CallRel32 symbol=printf addend=0
  offset=0x00000258 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000270 kind=CallRel32 symbol=printf addend=0
  offset=0x00000450 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000048c kind=CallRel32 symbol=printf addend=0

Section relocations:
  section=Data offset=0x00000000 kind=Abs64 symbol=__const_data_0 addend=0
  section=Data offset=0x00000010 kind=Abs64 symbol=__const_data_1 addend=0
  section=Data offset=0x00000020 kind=Abs64 symbol=__const_data_2 addend=0

.text (1196 bytes):
  00000000  ff 43 10 d1 f0 03 00 91  10 02 10 91 1d 7a 00 a9 
  00000010  fd 03 00 91 1f 20 03 d5  00 00 00 90 00 00 00 91 
  00000020  00 60 00 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000030  00 20 01 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000040  00 60 02 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000050  00 20 03 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000060  00 c0 03 91 00 00 00 94  f0 03 00 91 10 82 0d 91 
  00000070  f0 27 00 f9 f1 27 40 f9  10 04 80 d2 30 02 00 f9 
  00000080  f0 03 00 91 10 a2 0d 91  f0 2f 00 f9 f1 2f 40 f9 
  00000090  70 00 80 d2 30 02 00 f9  f0 27 40 f9 11 02 40 f9 
  000000a0  f1 37 00 f9 f0 2f 40 f9  11 02 40 f9 f1 3b 00 f9 
  000000b0  00 00 00 90 00 00 00 91  00 e0 03 91 e1 37 40 f9 
  000000c0  f0 37 40 f9 f0 03 00 f9  e2 3b 40 f9 f0 3b 40 f9 
  000000d0  f0 07 00 f9 00 00 00 94  f0 03 00 91 10 c2 0d 91 
  000000e0  f0 43 00 f9 f1 43 40 f9  30 00 80 d2 30 02 00 39 
  000000f0  f0 03 00 91 10 e2 0d 91  f0 4b 00 f9 f1 4b 40 f9 
  00000100  10 00 80 d2 30 02 00 39  f0 43 40 f9 11 02 40 39 
  00000110  f1 53 00 f9 f0 4b 40 f9  11 02 40 39 f1 57 00 f9 
  00000120  00 00 00 90 00 00 00 91  00 60 04 91 e1 83 42 39 
  00000130  f0 83 42 39 f0 03 00 f9  e2 a3 42 39 f0 a3 42 39 
  00000140  f0 07 00 f9 00 00 00 94  f0 03 00 91 10 02 0e 91 
  00000150  f0 5f 00 f9 f1 5f 40 f9  10 02 80 d2 30 02 00 f9 
  00000160  f0 03 00 91 10 22 0e 91  f0 67 00 f9 f1 67 40 f9 
  00000170  90 00 80 d2 30 02 00 f9  f0 03 00 91 10 42 0e 91 
  00000180  f0 6f 00 f9 f1 6f 40 f9  30 00 80 d2 30 02 00 39 
  00000190  f0 5f 40 f9 11 02 40 f9  f1 77 00 f9 f0 67 40 f9 
  000001a0  11 02 40 f9 f1 7b 00 f9  f0 6f 40 f9 11 02 40 39 
  000001b0  f1 7f 00 f9 00 00 00 90  00 00 00 91 00 e0 04 91 
  000001c0  e1 77 40 f9 f0 77 40 f9  f0 03 00 f9 e2 7b 40 f9 
  000001d0  f0 7b 40 f9 f0 07 00 f9  e3 e3 43 39 f0 e3 43 39 
  000001e0  f0 0b 00 f9 00 00 00 94  00 00 00 90 00 00 00 91 
  000001f0  00 c0 05 91 01 00 00 90  21 00 00 91 10 00 00 90 
  00000200  10 02 00 91 f0 03 00 f9  02 00 00 90 42 00 00 91 
  00000210  10 00 00 90 10 02 00 91  f0 07 00 f9 03 00 00 90 
  00000220  63 00 00 91 10 00 00 90  10 02 00 91 f0 0b 00 f9 
  00000230  00 00 00 94 f0 03 00 91  10 62 0e 91 f0 8b 00 f9 
  00000240  f1 8b 40 f9 10 00 80 d2  30 02 00 39 f0 8b 40 f9 
  00000250  11 02 40 39 f1 93 00 f9  00 00 00 90 00 00 00 91 
  00000260  00 60 06 91 e1 83 44 39  f0 83 44 39 f0 03 00 f9 
  00000270  00 00 00 94 f0 03 00 91  10 82 0e 91 f0 9b 00 f9 
  00000280  f1 9b 40 f9 10 04 80 d2  30 02 00 f9 f0 03 00 91 
  00000290  10 a2 0e 91 f0 a3 00 f9  f0 9b 40 f9 11 02 40 f9 
  000002a0  f1 a7 00 f9 f0 a7 40 f9  1f 02 01 f1 f0 c7 9f 9a 
  000002b0  f0 ab 00 f9 f1 a3 40 f9  f0 43 45 39 30 02 00 39 
  000002c0  f0 03 00 91 10 c2 0e 91  f0 b3 00 f9 f1 b3 40 f9 
  000002d0  10 02 80 d2 30 02 00 f9  f0 03 00 91 10 e2 0e 91 
  000002e0  f0 bb 00 f9 f0 b3 40 f9  11 02 40 f9 f1 bf 00 f9 
  000002f0  f0 bf 40 f9 1f 02 01 f1  f0 c7 9f 9a f0 c3 00 f9 
  00000300  f1 bb 40 f9 f0 03 46 39  30 02 00 39 f0 03 00 91 
  00000310  10 02 0f 91 f0 cb 00 f9  f1 cb 40 f9 10 04 80 d2 
  00000320  30 02 00 f9 f0 03 00 91  10 22 0f 91 f0 d3 00 f9 
  00000330  f1 d3 40 f9 10 02 80 d2  30 02 00 f9 f0 03 00 91 
  00000340  10 42 0f 91 f0 db 00 f9  f0 cb 40 f9 11 02 40 f9 
  00000350  f1 df 00 f9 f0 d3 40 f9  11 02 40 f9 f1 e3 00 f9 
  00000360  f0 df 40 f9 f1 e3 40 f9  10 02 11 8b f0 e7 00 f9 
  00000370  f1 db 40 f9 f0 e7 40 f9  30 02 00 f9 f0 03 00 91 
  00000380  10 62 0f 91 f0 ef 00 f9  f0 db 40 f9 11 02 40 f9 
  00000390  f1 f3 00 f9 f0 f3 40 f9  1f 82 01 f1 f0 c7 9f 9a 
  000003a0  f0 f7 00 f9 f1 ef 40 f9  f0 a3 47 39 30 02 00 39 
  000003b0  f0 03 00 91 10 82 0f 91  f0 ff 00 f9 f1 ff 40 f9 
  000003c0  10 04 80 d2 30 02 00 f9  f0 03 00 91 10 a2 0f 91 
  000003d0  f0 07 01 f9 f1 07 41 f9  10 02 80 d2 30 02 00 f9 
  000003e0  f0 03 00 91 10 c2 0f 91  f0 0f 01 f9 f0 ff 40 f9 
  000003f0  11 02 40 f9 f1 13 01 f9  f0 07 41 f9 11 02 40 f9 
  00000400  f1 17 01 f9 f0 13 41 f9  f1 17 41 f9 10 02 11 8b 
  00000410  f0 1b 01 f9 f1 0f 41 f9  f0 1b 41 f9 30 02 00 f9 
  00000420  f0 a3 40 f9 11 02 40 39  f1 23 01 f9 f0 bb 40 f9 
  00000430  11 02 40 39 f1 27 01 f9  f0 ef 40 f9 11 02 40 39 
  00000440  f1 2b 01 f9 f0 0f 41 f9  11 02 40 f9 f1 2f 01 f9 
  00000450  00 00 00 90 00 00 00 91  00 c0 06 91 e1 03 49 39 
  00000460  f0 03 49 39 f0 03 00 f9  e2 23 49 39 f0 23 49 39 
  00000470  f0 07 00 f9 e3 43 49 39  f0 43 49 39 f0 0b 00 f9 
  00000480  e4 2f 41 f9 f0 2f 41 f9  f0 0f 00 f9 00 00 00 94 
  00000490  bf 03 00 91 f0 03 00 91  10 02 10 91 1d 7a 40 a9 
  000004a0  ff 43 10 91 00 00 80 d2  c0 03 5f d6 

.rodata (496 bytes):
  00000000  44 61 74 61 00 69 36 34  00 75 38 00 00 00 00 00 
  00000010  40 00 00 00 00 00 00 00  f0 9f 93 98 20 54 75 74 
  00000020  6f 72 69 61 6c 3a 20 30  37 5f 63 6f 6d 70 69 6c 
  00000030  65 5f 74 69 6d 65 5f 76  61 6c 69 64 61 74 69 6f 
  00000040  6e 2e 66 70 0a 00 00 00  f0 9f a7 ad 20 46 6f 63 
  00000050  75 73 3a 20 43 6f 6d 70  69 6c 65 2d 74 69 6d 65 
  00000060  20 76 61 6c 69 64 61 74  69 6f 6e 20 75 73 69 6e 
  00000070  67 20 63 6f 6e 73 74 20  65 78 70 72 65 73 73 69 
  00000080  6f 6e 73 20 61 6e 64 20  69 6e 74 72 6f 73 70 65 
  00000090  63 74 69 6f 6e 0a 00 00  f0 9f a7 aa 20 57 68 61 
  000000a0  74 20 74 6f 20 6c 6f 6f  6b 20 66 6f 72 3a 20 6c 
  000000b0  61 62 65 6c 65 64 20 6f  75 74 70 75 74 73 20 62 
  000000c0  65 6c 6f 77 0a 00 00 00  e2 9c 85 20 45 78 70 65 
  000000d0  63 74 61 74 69 6f 6e 3a  20 6f 75 74 70 75 74 73 
  000000e0  20 6d 61 74 63 68 20 6c  61 62 65 6c 73 0a 00 00 
  000000f0  0a 00 00 00 00 00 00 00  64 61 74 61 3a 20 73 69 
  00000100  7a 65 6f 66 3d 25 6c 6c  75 2c 20 66 69 65 6c 64 
  00000110  73 3d 25 6c 6c 64 0a 00  64 61 74 61 3a 20 68 61 
  00000120  73 5f 61 3d 25 64 2c 20  68 61 73 5f 78 3d 25 64 
  00000130  0a 00 00 00 00 00 00 00  68 65 61 64 65 72 3a 20 
  00000140  73 69 7a 65 6f 66 3d 25  6c 6c 75 2c 20 66 69 65 
  00000150  6c 64 73 3d 25 6c 6c 64  2c 20 68 61 73 5f 76 65 
  00000160  72 73 69 6f 6e 3d 25 64  0a 00 00 00 00 00 00 00 
  00000170  74 79 70 65 73 3a 20 64  61 74 61 3d 27 25 73 27 
  00000180  20 61 3d 27 25 73 27 20  76 65 72 73 69 6f 6e 3d 
  00000190  27 25 73 27 0a 00 00 00  64 61 74 61 20 68 61 73 
  000001a0  20 74 6f 5f 73 74 72 69  6e 67 3a 20 25 64 0a 00 
  000001b0  6c 61 79 6f 75 74 3a 20  64 61 74 61 5f 6f 6b 3d 
  000001c0  25 64 2c 20 68 65 61 64  65 72 5f 6f 6b 3d 25 64 
  000001d0  2c 20 74 6f 74 61 6c 5f  6f 6b 3d 25 64 2c 20 74 
  000001e0  6f 74 61 6c 5f 73 69 7a  65 3d 25 6c 6c 75 0a 00 
