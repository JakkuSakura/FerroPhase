fp-native dump: format=MachO arch=Aarch64 entry=0x0

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
global BUFFER_SIZE ty=I64 constant=true initializer=Some(Bytes([0, 16, 0, 0, 0, 0, 0, 0]))
global MAX_CONNECTIONS ty=I64 constant=true initializer=Some(Bytes([150, 0, 0, 0, 0, 0, 0, 0]))
global FACTORIAL_5 ty=I64 constant=true initializer=Some(Bytes([120, 0, 0, 0, 0, 0, 0, 0]))
global IS_LARGE ty=I1 constant=true initializer=Some(Bytes([1]))
global DEFAULT_CONFIG ty=Struct { fields: [I64, I64], packed: false, name: None } constant=true initializer=Some(Bytes([0, 16, 0, 0, 0, 0, 0, 0, 150, 0, 0, 0, 0, 0, 0, 0]))
global __const_data_0 ty=Array(I8, 6) constant=true initializer=Some(Bytes([108, 97, 114, 103, 101, 0]))
global __const_data_1 ty=Array(I8, 6) constant=true initializer=Some(Bytes([115, 109, 97, 108, 108, 0]))
fn task_register_dyld_image_infos
fn NXSwapLittleShortToHost
fn NSSymbolReferenceCountInObjectFileImage
fn _dyld_launched_prebound
fn __NDR_convert__mig_reply_error_t
fn sendto
fn mach_port_is_connection_for_service
fn feraiseexcept
fn towlower
fn fchdir
fn _dyld_image_count
fn processor_set_stack_usage
fn wcschr
fn fchown
fn stpcpy
fn vsscanf
fn host_get_UNDServer
fn mach_port_peek
fn mach_port_guard_with_flags
fn mach_generate_activity_id
fn NXSwapShort
fn processor_control
fn fmemopen
fn longjmp
fn mach_port_allocate
fn host_reboot
fn mach_port_get_set_status
fn mach_port_dnrequest_info
fn faccessat
fn semaphore_wait
fn wcscoll
fn getc
fn task_for_pid
fn wctrans
fn gethostent
fn semop
fn host_processors
fn host_get_special_port
fn vm_region_64
fn mach_port_request_notification
fn _kernelrpc_mach_port_guard_trap
fn mach_memory_info
fn NXSwapHostLongToBig
fn aio_cancel
fn processor_set_policy_control
fn __swbuf
fn _kernelrpc_mach_vm_allocate_trap
fn getrlimit
fn mktime
fn execvp
fn shmget
fn pthread_kill
fn regcomp
fn feupdateenv
fn imaxdiv
fn getchar_unlocked
fn system
fn strptime
fn wcscpy
fn posix_spawnattr_getflags
fn posix_spawnattr_setsigmask
fn pipe
fn mach_msg
fn vm_region
fn abort
fn _dyld_bind_fully_image_containing_address
fn feclearexcept
fn sigdelset
fn flockfile
fn wcscspn
fn clonefileat
fn closelog
fn getpwuid_r
fn memccpy
fn pthread_sigmask
fn isgraph
fn pwrite
fn calloc
fn NSSymbolReferenceNameInObjectFileImage
fn NSAddLibrary
fn vfork
fn posix_spawn_file_actions_addfchdir
fn initstate
fn strpbrk
fn unlockpt
fn wcsncmp
fn getlogin
fn cfgetospeed
fn getgrgid_r
fn __svfscanf
fn processor_set_threads
fn remove
fn iswprint
fn vswprintf
fn wcsxfrm
fn getuid
fn munmap
fn _OSReadInt32
fn thread_create
fn NXSwapLittleLongLongToHost
fn NXSwapLong
fn mach_port_names
fn mach_voucher_extract_attr_recipe_trap
fn _OSReadSwapInt16
fn recvmsg
fn ualarm
fn gets
fn vm_behavior_set
fn posix_spawnattr_getpgroup
fn feof
fn atol
fn tcsetpgrp
fn kmod_get_info
fn mach_port_extract_member
fn mach_thread_self
fn kqueue
fn isprint
fn thread_resume
fn mkfifoat
fn thread_policy
fn NXSwapBigShortToHost
fn iswblank
fn task_self_trap
fn _tlv_bootstrap
fn mig_deallocate
fn fputws
fn sigpause
fn setprotoent
fn ctime
fn semaphore_wait_signal
fn ttyname_r
fn fremovexattr
fn thread_get_exception_ports
fn clonefile
fn ctime_r
fn __darwin_check_fd_set_overflow
fn task_swap_mach_voucher
fn _kernelrpc_mach_vm_deallocate_trap
fn mach_memory_object_memory_entry
fn unlinkat
fn posix_spawn_file_actions_addchdir
fn host_get_boot_info
fn vm_read
fn host_get_io_main
fn task_set_ras_pc
fn getpriority
fn getnetbyaddr
fn mach_msg_overwrite
fn host_register_mach_voucher_attr_manager
fn getc_unlocked
fn clock
fn host_get_multiuser_config_flags
fn sysconf
fn getpwnam
fn isblank
fn sync
fn task_get_exception_ports
fn getentropy
fn vswscanf
fn mach_port_kernel_object
fn asctime_r
fn task_register_dyld_shared_cache_image_info
fn task_set_phys_footprint_limit
fn mach_port_guard
fn mach_vm_region_info_64
fn vm_allocate
fn setpgrp
fn rand_r
fn mach_port_insert_right
fn processor_start
fn macx_swapon
fn fseek
fn isspace
fn getpwnam_r
fn link
fn task_policy_get
fn NXHostByteOrder
fn NSAddressOfSymbol
fn getxattr
fn inet_ntoa
fn getpgid
fn wcstoull
fn vm_wire
fn gmtime_r
fn getpwuid
fn vscanf
fn mknod
fn lseek
fn setservent
fn act_get_state
fn task_set_port_space
fn NSLookupAndBindSymbolWithHint
fn task_policy_set
fn tzset
fn thread_suspend
fn _kernelrpc_mach_port_allocate_trap
fn dlclose
fn fgetc
fn task_inspect
fn getnetent
fn task_set_exception_ports
fn mach_msg_send
fn _longjmp
fn memcmp
fn atoll
fn strncat
fn llabs
fn mig_strncpy_zerofill
fn getdate
fn thread_set_special_port
fn wcsftime
fn fileno
fn task_create
fn strcpy
fn mach_port_construct
fn NXSwapBigIntToHost
fn __darwin_fd_isset
fn task_terminate
fn mach_port_allocate_full
fn vsnprintf
fn fegetround
fn readlinkat
fn _OSReadInt64
fn task_zone_info
fn vfprintf
fn thread_swap_mach_voucher
fn __isctype
fn tcgetattr
fn wcstol
fn posix_spawnp
fn alarm
fn getpwent
fn kevent64
fn mbrtowc
fn sem_post
fn sigignore
fn newlocale
fn aligned_alloc
fn strerror
fn iconv_close
fn mkdirat
fn getsubopt
fn task_test_sync_upcall
fn endservent
fn vm_read_overwrite
fn dlerror
fn posix_spawn_file_actions_destroy
fn _OSWriteInt64
fn strtok
fn setsid
fn renameat
fn putchar_unlocked
fn localtime
fn getgrent
fn vm_remap
fn task_suspend
fn vm_protect
fn task_create_identity_token
fn nrand48
fn macx_swapoff
fn host_statistics
fn NSDestroyObjectFileImage
fn ___runetype
fn mach_error
fn mkfifo
fn _Exit
fn endgrent
fn mach_port_mod_refs
fn task_dyld_process_info_notify_get
fn ldiv
fn OSHostByteOrder
fn thread_wire
fn siginterrupt
fn iswdigit
fn tcgetsid
fn task_get_state
fn host_processor_info
fn atomic_flag_clear
fn strcoll
fn iswpunct
fn fstat
fn tcsetattr
fn slot_name
fn getwchar
fn mach_port_unguard
fn thread_set_policy
fn fegetexceptflag
fn free
fn alphasort
fn posix_spawnattr_getsigmask
fn NXSwapHostIntToLittle
fn fdopen
fn putc_unlocked
fn wctomb
fn posix_openpt
fn strncasecmp
fn strtoul
fn isatty
fn _OSWriteInt16
fn processor_set_policy_disable
fn mach_error_string
fn strftime
fn vwprintf
fn sem_close
fn posix_spawnattr_setflags
fn statvfs
fn vfscanf
fn __wcwidth
fn processor_set_policy_enable
fn task_policy
fn posix_spawnattr_setpgroup
fn voucher_mach_msg_set
fn __error
fn srand
fn isalpha
fn quick_exit
fn setgrent
fn setgid
fn thread_get_mach_voucher
fn thread_adopt_exception_handler
fn stpncpy
fn _host_page_size
fn waitid
fn host_request_notification
fn host_set_multiuser_config_flags
fn NSGetSectionDataInObjectFileImage
fn iswupper
fn insque
fn mbstowcs
fn feholdexcept
fn wcscmp
fn __sigbits
fn iconv
fn fputs
fn getgroups
fn lstat
fn NSLibraryNameForModule
fn sigsetjmp
fn vm_write
fn realloc
fn fstatvfs
fn mig_get_reply_port
fn duplocale
fn vm_mapped_pages_info
fn iswalpha
fn task_set_emulation_vector
fn NSVersionOfLinkTimeLibrary
fn host_priv_statistics
fn host_security_set_task_token
fn fsync
fn mrand48
fn fgetxattr
fn getwc
fn fnmatch
fn msync
fn mach_port_allocate_name
fn mach_port_destruct
fn fesetround
fn ftello
fn _kernelrpc_mach_vm_purgable_control_trap
fn NSSymbolDefinitionCountInObjectFileImage
fn setxattr
fn utime
fn mach_make_memory_entry_64
fn wctype
fn semaphore_timedwait_signal
fn wmemset
fn strncmp
fn cfsetospeed
fn setbuf
fn remque
fn openlog
fn rand
fn rewinddir
fn endhostent
fn munlockall
fn munlock
fn exit
fn getlogin_r
fn fsetpos
fn select
fn task_info
fn strcspn
fn host_security_create_task_token
fn closedir
fn posix_spawn_file_actions_addclose
fn iswctype
fn sigismember
fn _OSSwapInt64
fn stat
fn clock_set_time
fn mknodat
fn mach_port_rename
fn host_get_atm_diagnostic_flag
fn send
fn NXSwapLongLong
fn NSLinkEditError
fn host_set_special_port
fn NXSwapBigLongLongToHost
fn vm_inherit
fn getitimer
fn sigfillset
fn waitpid
fn accept
fn freopen
fn mblen
fn sockatmark
fn tcflush
fn task_assign
fn strsignal
fn ispunct
fn freelocale
fn processor_set_tasks
fn pid_for_task
fn cfgetispeed
fn mach_port_set_mscount
fn mach_port_deallocate
fn sigprocmask
fn towupper
fn setrlimit
fn getgrnam_r
fn ftok
fn semaphore_destroy
fn task_set_policy
fn mach_make_memory_entry
fn clock_sleep
fn task_generate_corpse
fn __math_errhandling
fn strtol
fn strndup
fn getopt
fn mlockall
fn sem_getvalue
fn strtoumax
fn puts
fn setlocale
fn __vsprintf_chk
fn strcmp
fn wcsnlen
fn sigwait
fn fputwc
fn getgrnam
fn symlinkat
fn labs
fn unsetenv
fn inet_addr
fn getservbyport
fn setpwent
fn dup2
fn vfwscanf
fn truncate
fn seteuid
fn msgsnd
fn fstatat
fn _setjmp
fn clock_getres
fn jrand48
fn mbsinit
fn posix_spawn_file_actions_addopen
fn confstr
fn clock_gettime
fn setegid
fn mach_voucher_deallocate
fn _kernelrpc_mach_port_insert_right_trap
fn NXSwapDouble
fn wcsdup
fn killpg
fn mach_vm_reclaim_update_kernel_accounting_trap
fn __maskrune
fn sigaltstack
fn clock_settime
fn task_register_dyld_set_dyld_state
fn NSIsSymbolNameDefined
fn dlopen
fn tmpnam
fn aio_suspend
fn thread_abort
fn vm_map_exec_lockdown
fn siglongjmp
fn mig_dealloc_reply_port
fn creat
fn ctermid
fn mach_port_allocate_qos
fn mach_port_get_service_port_info
fn wcstombs
fn ftrylockfile
fn wcsnrtombs
fn _dyld_lookup_and_bind_with_hint
fn mkdir
fn fclonefileat
fn NSCreateObjectFileImageFromFile
fn wcslen
fn endpwent
fn thread_swap_exception_ports
fn _dyld_lookup_and_bind
fn wmemcpy
fn setgroupent
fn lock_set_destroy
fn thread_abort_safely
fn mach_memory_object_memory_entry_64
fn fsetxattr
fn sigaction
fn rewind
fn isupper
fn random
fn sem_destroy
fn thread_set_state
fn vsprintf
fn pthread_setconcurrency
fn _OSWriteSwapInt64
fn task_suspend2
fn socket
fn wcwidth
fn getaddrinfo
fn _OSSwapInt32
fn vm_read_list
fn _kernelrpc_mach_port_type_trap
fn ptsname
fn dirfd
fn mach_port_set_context
fn processor_set_default
fn task_map_kcdata_object_64
fn NSLookupSymbolInModule
fn _dyld_image_containing_address
fn sigsuspend
fn vm_allocate_cpm
fn strnlen
fn poll
fn sigaddset
fn wcstok
fn wcstoul
fn freeaddrinfo
fn task_name_for_pid
fn semaphore_create
fn sigemptyset
fn getsockopt
fn iswnumber
fn sem_unlink
fn getrusage
fn kill
fn shmdt
fn wcrtomb
fn host_processor_set_priv
fn dlsym
fn fgetpos
fn mig_reply_setup
fn clock_get_res
fn NXSwapHostShortToBig
fn kevent
fn endprotoent
fn localeconv
fn pthread_testcancel
fn gethostid
fn asctime
fn iswspace
fn hsearch
fn task_set_corpse_forking_behavior
fn getnameinfo
fn voucher_mach_msg_adopt
fn wmemcmp
fn thread_sample
fn vm_map_64
fn mach_error_type
fn lldiv
fn perror
fn __vsnprintf_chk
fn strstr
fn recv
fn sched_get_priority_max
fn __sputc
fn tcsendbreak
fn times
fn mig_put_reply_port
fn host_swap_exception_ports
fn task_get_mach_voucher
fn wcswidth
fn vm_deallocate
fn vm_remap_new
fn lockf
fn kext_request
fn lio_listio
fn fetestexcept
fn islower
fn posix_spawnattr_getsigdefault
fn mach_port_move_member
fn mach_port_space_basic_info
fn sleep
fn mach_port_kobject_description
fn _kernelrpc_mach_port_extract_member_trap
fn hcreate
fn task_dyld_process_info_notify_register
fn task_register_dyld_get_process_state
fn __toupper
fn putc
fn sem_trywait
fn mmap
fn thread_set_exception_ports
fn inet_ntop
fn clock_set_attributes
fn NSModuleForSymbol
fn _kernelrpc_mach_vm_protect_trap
fn ungetwc
fn tcdrain
fn mlock
fn processor_assign
fn ffs
fn processor_set_statistics
fn mach_port_kobject
fn host_lockgroup_info
fn NXSwapInt
fn gethostname
fn mach_port_get_attributes
fn _kernelrpc_mach_port_move_member_trap
fn setitimer
fn clock_set_res
fn getnetbyname
fn grantpt
fn gai_strerror
fn NSVersionOfRunTimeLibrary
fn __assert_rtn
fn mbtowc
fn open_wmemstream
fn lchown
fn task_set_exc_guard_behavior
fn semaphore_timedwait
fn macx_backing_store_recovery
fn srand48
fn memcpy
fn wcsstr
fn aio_fsync
fn read
fn msgrcv
fn shmat
fn NSNameOfModule
fn ftell
fn iswcntrl
fn fgetws
fn wcsncpy
fn iswideogram
fn regerror
fn mach_msg_receive
fn _dyld_present
fn vdprintf
fn getservent
fn macx_triggers
fn getppid
fn gethostbyname
fn sched_yield
fn NSIsSymbolNameDefinedInImage
fn _dyld_get_image_header_containing_address
fn iswrune
fn _kernelrpc_mach_port_construct_trap
fn getcwd
fn setpriority
fn open_memstream
fn lcong48
fn host_set_exception_ports
fn NSAddLibraryWithSearching
fn strxfrm
fn bind
fn globfree
fn host_set_atm_diagnostic_flag
fn strncpy
fn execv
fn __darwin_fd_set
fn atomic_signal_fence
fn clock_sleep_trap
fn NSLookupSymbolInImage
fn atomic_flag_test_and_set
fn listxattr
fn fegetenv
fn time
fn mach_port_set_seqno
fn inet_pton
fn host_get_clock_control
fn sem_wait
fn act_set_state
fn NXSwapBigLongToHost
fn abs
fn _dyld_all_twolevel_modules_prebound
fn task_register_hardened_exception_handler
fn mach_zone_info_for_zone
fn getline
fn iconv_open
fn removexattr
fn funlockfile
fn thread_get_state
fn thread_get_special_port
fn symlink
fn task_dyld_process_info_notify_deregister
fn readdir
fn _dyld_get_image_name
fn iswlower
fn seed48
fn close
fn thread_get_exception_ports_info
fn strcat
fn vm_map_page_query
fn thread_policy_get
fn setnetent
fn wcscat
fn thread_set_mach_voucher
fn setkey
fn strlen
fn regfree
fn vm_stats
fn swtch
fn setpgid
fn dirname
fn recvfrom
fn isxdigit
fn swab
fn fputc
fn setenv
fn memchr
fn wcscasecmp
fn getgid
fn thread_info
fn strcasecmp
fn fpathconf
fn _kernelrpc_mach_port_request_notification_trap
fn putwc
fn a64l
fn task_get_exc_guard_behavior
fn etap_trace_thread
fn vm_copy
fn _kernelrpc_mach_port_mod_refs_trap
fn _dyld_get_image_vmaddr_slide
fn NSNameOfSymbol
fn NSInstallLinkEditErrorHandlers
fn getenv
fn swtch_pri
fn getsockname
fn fwrite
fn fgetwc
fn wcsrtombs
fn shutdown
fn task_assign_default
fn thread_policy_set
fn processor_set_create
fn msgget
fn iswspecial
fn chown
fn _OSReadSwapInt32
fn setreuid
fn host_processor_sets
fn strdup
fn sighold
fn listen
fn posix_spawnattr_destroy
fn getpid
fn kmod_create
fn __srget
fn _exit
fn mbsrtowcs
fn mktemp
fn posix_spawnattr_setsigdefault
fn NXSwapHostLongLongToBig
fn debug_control_port_for_pid
fn seekdir
fn __darwin_fd_clr
fn task_swap_exception_ports
fn wcstoumax
fn isdigit
fn nice
fn NXSwapHostLongToLittle
fn thread_assign
fn NSCreateObjectFileImageFromMemory
fn strtoull
fn strerror_r
fn iswxdigit
fn wctob
fn sendmsg
fn posix_memalign
fn tolower
fn getprotoent
fn aio_write
fn basename
fn sched_get_priority_min
fn chmod
fn uname
fn iscntrl
fn semaphore_signal_thread
fn getprotobynumber
fn atoi
fn tempnam
fn lrand48
fn strspn
fn gethostbyaddr
fn fseeko
fn aio_error
fn getgrgid
fn pathconf
fn realpath
fn vm_map
fn mach_task_is_self
fn _kernelrpc_mach_vm_map_trap
fn fclose
fn setsockopt
fn nl_langinfo
fn posix_spawnattr_init
fn setlogmask
fn host_create_mach_voucher_trap
fn host_kernel_version
fn getegid
fn task_resume2
fn task_get_dyld_image_infos
fn iswalnum
fn atomic_flag_clear_explicit
fn getpeername
fn wcsncasecmp
fn pause
fn __tolower
fn wcpcpy
fn sigrelse
fn atomic_thread_fence
fn l64a
fn readlink
fn tcgetpgrp
fn _OSWriteSwapInt32
fn task_get_special_port
fn ___tolower
fn unlink
fn regexec
fn futimens
fn gettimeofday
fn mig_strncpy
fn vm_machine_attribute
fn thread_depress_abort
fn _kernelrpc_mach_port_insert_member_trap
fn opendir
fn shmctl
fn host_statistics64
fn voucher_mach_msg_clear
fn isalnum
fn execve
fn nanosleep
fn clearerr
fn wcpncpy
fn telldir
fn posix_spawn_file_actions_init
fn fork
fn setuid
fn setgrfile
fn host_default_memory_manager
fn mach_msg_destroy
fn _kernelrpc_mach_port_unguard_trap
fn shm_unlink
fn wcsspn
fn wmemchr
fn mach_ports_register
fn mach_port_space_info
fn memmove
fn _dyld_shared_cache_contains_path
fn voucher_mach_msg_revert
fn NXSwapHostLongLongToLittle
fn msgctl
fn _OSWriteSwapInt16
fn mbrlen
fn if_nametoindex
fn vwscanf
fn aio_return
fn posix_spawn
fn getsid
fn fwide
fn semget
fn semaphore_signal
fn div
fn geteuid
fn iswhexnumber
fn chdir
fn wmemmove
fn putwchar
fn processor_set_max_priority
fn task_identity_token_get_task_port
fn vm_msync
fn _kernelrpc_mach_port_deallocate_trap
fn NSIsSymbolDefinedInObjectFileImage
fn NSLookupAndBindSymbol
fn ttyname
fn host_page_size
fn vfwprintf
fn strtoimax
fn thread_get_assignment
fn utimes
fn connect
fn mach_ports_lookup
fn malloc
fn _dyld_lookup_and_bind_fully
fn ___toupper
fn semaphore_signal_all
fn host_create_mach_voucher
fn processor_set_destroy
fn flistxattr
fn task_set_info
fn panic_init
fn NSIsSymbolNameDefinedWithHint
fn mach_port_get_context
fn NSUnLinkModule
fn getservbyname
fn utimensat
fn mkstemp
fn task_threads
fn isascii
fn fopen
fn readdir_r
fn wait
fn task_set_emulation
fn toascii
fn fread
fn if_nameindex
fn write
fn fchmodat
fn localtime_r
fn host_get_exception_ports
fn task_purgable_info
fn endnetent
fn task_get_exception_ports_info
fn fflush
fn strtoll
fn fdopendir
fn getchar
fn rmdir
fn thread_assign_default
fn __darwin_check_fd_set
fn vm_purgable_control
fn mach_port_extract_right
fn mach_port_destroy
fn sethostent
fn host_info
fn mig_allocate
fn towctrans
fn NXSwapLittleIntToHost
fn fchownat
fn NSLinkModule
fn NSSymbolDefinitionNameInObjectFileImage
fn mach_host_self
fn _OSWriteInt32
fn ungetc
fn thread_create_running
fn tmpfile
fn ftruncate
fn umask
fn toupper
fn sigpending
fn mach_port_get_refs
fn task_get_assignment
fn task_get_emulation_vector
fn strrchr
fn processor_set_info
fn setstate
fn processor_set_tasks_with_flavor
fn task_unregister_dyld_image_infos
fn aio_read
fn task_set_special_port
fn thread_convert_thread_state
fn kmod_destroy
fn task_set_state
fn _kernelrpc_mach_port_get_attributes_trap
fn getprotobyname
fn fgets
fn encrypt
fn if_indextoname
fn raise
fn task_set_mach_voucher
fn wcstoll
fn gmtime
fn mprotect
fn vm_region_recurse
fn mach_zone_info
fn cfsetispeed
fn host_register_well_known_mach_voucher_attr_manager
fn setjmp
fn mach_port_get_srights
fn pselect
fn host_check_multiuser_mode
fn iswphonogram
fn mbsnrtowcs
fn popen
fn iswascii
fn atomic_flag_test_and_set_explicit
fn wcsncat
fn pthread_key_delete
fn crypt
fn timespec_get
fn mach_vm_wire
fn mach_vm_region_info
fn posix_madvise
fn NXSwapHostIntToBig
fn thread_terminate
fn task_resume
fn srandom
fn NSAddImage
fn getpgrp
fn task_test_async_upcall_propagation
fn putenv
fn btowc
fn fchmod
fn _NSGetExecutablePath
fn setvbuf
fn imaxabs
fn uselocale
fn task_wire
fn mach_port_set_attributes
fn _dyld_get_image_header
fn usleep
fn getdelim
fn _OSSwapInt16
fn iswgraph
fn hdestroy
fn processor_info
fn NXSwapHostShortToLittle
fn mach_port_type
fn mach_port_assert_attributes
fn ferror
fn vprintf
fn strtok_r
fn psignal
fn wcsrchr
fn socketpair
fn pthread_getconcurrency
fn access
fn pread
fn fesetenv
fn processor_get_assignment
fn putchar
fn task_map_corpse_info_64
fn mach_port_insert_member
fn mach_port_swap_guard
fn pclose
fn host_virtual_physical_table_info
fn setregid
fn NXSwapFloat
fn posix_spawn_file_actions_adddup2
fn sem_init
fn kmod_control
fn if_freenameindex
fn task_sample
fn macx_backing_store_suspend
fn thread_switch
fn processor_exit
fn host_get_clock_service
fn rename
fn tcflow
fn task_map_corpse_info
fn NXSwapLittleLongToHost
fn host_set_UNDServer
fn dup
fn fesetexceptflag
fn wcstoimax
fn __istype
fn memset
fn _OSReadSwapInt64
fn _OSReadInt16
fn vm_region_recurse_64
fn _kernelrpc_mach_port_destruct_trap
fn strchr
fn linkat
fn wcspbrk
fn lock_set_create
fn main
  bb0 bb0
    alloca Virtual { id: 0, bank: General, size_bits: 64 }, 1
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    alloca Virtual { id: 6, bank: General, size_bits: 64 }, 1
    div Virtual { id: 7, bank: General, size_bits: 64 }, 4096, 1024
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 6, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 7, bank: General, size_bits: 64 }
    load Virtual { id: 9, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 6, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 9, bank: General, size_bits: 64 }, 120, 1
    alloca Virtual { id: 11, bank: General, size_bits: 64 }, 1
    div Virtual { id: 12, bank: General, size_bits: 64 }, 4096, 1024
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 11, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 12, bank: General, size_bits: 64 }
    load Virtual { id: 14, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 11, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 14, bank: General, size_bits: 64 }, 150
    alloca Virtual { id: 16, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 16, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 3
    alloca Virtual { id: 18, bank: General, size_bits: 64 }, 1
    mul Virtual { id: 19, bank: General, size_bits: 64 }, 4096, 2
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 18, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 19, bank: General, size_bits: 64 }
    alloca Virtual { id: 21, bank: General, size_bits: 64 }, 1
    load Virtual { id: 22, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 18, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 21, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 22, bank: General, size_bits: 64 }
    alloca Virtual { id: 24, bank: General, size_bits: 64 }, 1
    gt Virtual { id: 25, bank: General, size_bits: 8 }, 4096, 2048
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 24, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 25, bank: General, size_bits: 8 }
    load Virtual { id: 27, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 24, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 28, bank: General, size_bits: 8 }, Virtual { id: 27, bank: General, size_bits: 8 }, 1
    condbr
  bb1 bb1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb2 bb2
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb3 bb3
    alloca Virtual { id: 31, bank: General, size_bits: 64 }, 1
    load Virtual { id: 32, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(16), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 31, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 32, bank: General, size_bits: 64 }
    alloca Virtual { id: 34, bank: General, size_bits: 64 }, 1
    mul Virtual { id: 35, bank: General, size_bits: 64 }, 4096, 150
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 34, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 35, bank: General, size_bits: 64 }
    alloca Virtual { id: 37, bank: General, size_bits: 64 }, 1
    load Virtual { id: 38, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 16, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 39, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 34, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    mul Virtual { id: 40, bank: General, size_bits: 64 }, Virtual { id: 38, bank: General, size_bits: 64 }, Virtual { id: 39, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 37, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 40, bank: General, size_bits: 64 }
    alloca Virtual { id: 42, bank: General, size_bits: 64 }, 1
    load Virtual { id: 43, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 37, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 42, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 43, bank: General, size_bits: 64 }
    load Virtual { id: 45, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 21, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 46, bank: General, size_bits: 64 }, Virtual { id: 31, bank: General, size_bits: 64 }
    load Virtual { id: 47, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 46, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 48, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 42, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 45, bank: General, size_bits: 64 }, Virtual { id: 47, bank: General, size_bits: 64 }, Virtual { id: 48, bank: General, size_bits: 64 }
    ret


Symbols:
  main                             0x00000000

Text relocations:
  offset=0x00000024 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000030 kind=CallRel32 symbol=printf addend=0
  offset=0x00000034 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000040 kind=CallRel32 symbol=printf addend=0
  offset=0x00000044 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000050 kind=CallRel32 symbol=printf addend=0
  offset=0x00000054 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000060 kind=CallRel32 symbol=printf addend=0
  offset=0x00000064 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000070 kind=CallRel32 symbol=printf addend=0
  offset=0x000000ac kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000000dc kind=CallRel32 symbol=printf addend=0
  offset=0x00000118 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000013c kind=CallRel32 symbol=printf addend=0
  offset=0x00000200 kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x00000238 kind=Aarch64AdrpAdd symbol=__const_data_1 addend=0
  offset=0x00000378 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000003a8 kind=CallRel32 symbol=printf addend=0

.text (968 bytes):
  00000000  ff c3 0b d1 f0 03 00 91  10 82 0b 91 1d 7a 00 a9 
  00000010  fd 03 00 91 1f 20 03 d5  f0 03 00 91 10 c2 09 91 
  00000020  f0 13 00 f9 00 00 00 90  00 00 00 91 00 00 01 91 
  00000030  00 00 00 94 00 00 00 90  00 00 00 91 00 a0 01 91 
  00000040  00 00 00 94 00 00 00 90  00 00 00 91 00 00 03 91 
  00000050  00 00 00 94 00 00 00 90  00 00 00 91 00 c0 03 91 
  00000060  00 00 00 94 00 00 00 90  00 00 00 91 00 60 04 91 
  00000070  00 00 00 94 f0 03 00 91  10 02 0a 91 f0 2b 00 f9 
  00000080  10 00 82 d2 11 80 80 d2  09 0e d1 9a f0 03 09 aa 
  00000090  f0 2f 00 f9 f1 2b 40 f9  f0 2f 40 f9 30 02 00 f9 
  000000a0  f0 2b 40 f9 11 02 40 f9  f1 37 00 f9 00 00 00 90 
  000000b0  00 00 00 91 00 80 04 91  e1 37 40 f9 f0 37 40 f9 
  000000c0  f0 03 00 f9 02 0f 80 d2  10 0f 80 d2 f0 07 00 f9 
  000000d0  23 00 80 d2 30 00 80 d2  f0 0b 00 f9 00 00 00 94 
  000000e0  f0 03 00 91 10 22 0a 91  f0 3f 00 f9 10 00 82 d2 
  000000f0  11 80 80 d2 09 0e d1 9a  f0 03 09 aa f0 43 00 f9 
  00000100  f1 3f 40 f9 f0 43 40 f9  30 02 00 f9 f0 3f 40 f9 
  00000110  11 02 40 f9 f1 4b 00 f9  00 00 00 90 00 00 00 91 
  00000120  00 40 05 91 e1 4b 40 f9  f0 4b 40 f9 f0 03 00 f9 
  00000130  c2 12 80 d2 d0 12 80 d2  f0 07 00 f9 00 00 00 94 
  00000140  f0 03 00 91 10 42 0a 91  f0 53 00 f9 f1 53 40 f9 
  00000150  70 00 80 d2 30 02 00 f9  f0 03 00 91 10 62 0a 91 
  00000160  f0 5b 00 f9 10 00 82 d2  51 00 80 d2 10 7e 11 9b 
  00000170  f0 5f 00 f9 f1 5b 40 f9  f0 5f 40 f9 30 02 00 f9 
  00000180  f0 03 00 91 10 82 0a 91  f0 67 00 f9 f0 5b 40 f9 
  00000190  11 02 40 f9 f1 6b 00 f9  f1 67 40 f9 f0 6b 40 f9 
  000001a0  30 02 00 f9 f0 03 00 91  10 a2 0a 91 f0 73 00 f9 
  000001b0  10 00 82 d2 1f 02 20 f1  f0 d7 9f 9a f0 77 00 f9 
  000001c0  f1 73 40 f9 f0 a3 43 39  30 02 00 39 f0 73 40 f9 
  000001d0  11 02 40 39 f1 7f 00 f9  f0 e3 43 39 1f 06 00 f1 
  000001e0  f0 17 9f 9a f0 83 00 f9  f0 83 40 f9 1f 02 00 f1 
  000001f0  41 00 00 54 0f 00 00 14  f1 13 40 f9 eb 03 11 aa 
  00000200  10 00 00 90 10 02 00 91  ea 03 0b aa 50 01 00 f9 
  00000210  b0 00 80 d2 10 00 a0 f2  10 00 c0 f2 10 00 e0 f2 
  00000220  ea 03 0b aa 4a 21 00 91  50 01 00 f9 0f 00 00 14 
  00000230  f1 13 40 f9 eb 03 11 aa  10 00 00 90 10 02 00 91 
  00000240  ea 03 0b aa 50 01 00 f9  b0 00 80 d2 10 00 a0 f2 
  00000250  10 00 c0 f2 10 00 e0 f2  ea 03 0b aa 4a 21 00 91 
  00000260  50 01 00 f9 01 00 00 14  f0 03 00 91 10 c2 0a 91 
  00000270  f0 8f 00 f9 f1 13 40 f9  e9 03 11 aa 30 01 40 f9 
  00000280  f0 33 01 f9 e9 03 11 aa  29 21 00 91 30 01 40 f9 
  00000290  f0 37 01 f9 f0 03 00 91  10 82 09 91 f0 93 00 f9 
  000002a0  f1 8f 40 f9 f0 33 41 f9  e9 03 11 aa 30 01 00 f9 
  000002b0  f0 37 41 f9 e9 03 11 aa  29 21 00 91 30 01 00 f9 
  000002c0  f0 03 00 91 10 02 0b 91  f0 9b 00 f9 10 00 82 d2 
  000002d0  d1 12 80 d2 10 7e 11 9b  f0 9f 00 f9 f1 9b 40 f9 
  000002e0  f0 9f 40 f9 30 02 00 f9  f0 03 00 91 10 22 0b 91 
  000002f0  f0 a7 00 f9 f0 53 40 f9  11 02 40 f9 f1 ab 00 f9 
  00000300  f0 9b 40 f9 11 02 40 f9  f1 af 00 f9 f0 ab 40 f9 
  00000310  f1 af 40 f9 10 7e 11 9b  f0 b3 00 f9 f1 a7 40 f9 
  00000320  f0 b3 40 f9 30 02 00 f9  f0 03 00 91 10 42 0b 91 
  00000330  f0 bb 00 f9 f0 a7 40 f9  11 02 40 f9 f1 bf 00 f9 
  00000340  f1 bb 40 f9 f0 bf 40 f9  30 02 00 f9 f0 67 40 f9 
  00000350  11 02 40 f9 f1 c7 00 f9  f0 8f 40 f9 f0 cb 00 f9 
  00000360  f0 cb 40 f9 11 02 40 f9  f1 cf 00 f9 f0 bb 40 f9 
  00000370  11 02 40 f9 f1 d3 00 f9  00 00 00 90 00 00 00 91 
  00000380  00 00 06 91 e1 c7 40 f9  f0 c7 40 f9 f0 03 00 f9 
  00000390  e2 cf 40 f9 f0 cf 40 f9  f0 07 00 f9 e3 d3 40 f9 
  000003a0  f0 d3 40 f9 f0 0b 00 f9  00 00 00 94 bf 03 00 91 
  000003b0  f0 03 00 91 10 82 0b 91  1d 7a 40 a9 ff c3 0b 91 
  000003c0  00 00 80 d2 c0 03 5f d6 

.rodata (435 bytes):
  00000000  00 10 00 00 00 00 00 00  96 00 00 00 00 00 00 00 
  00000010  78 00 00 00 00 00 00 00  01 00 00 00 00 00 00 00 
  00000020  00 10 00 00 00 00 00 00  96 00 00 00 00 00 00 00 
  00000030  6c 61 72 67 65 00 73 6d  61 6c 6c 00 00 00 00 00 
  00000040  f0 9f 93 98 20 54 75 74  6f 72 69 61 6c 3a 20 30 
  00000050  31 5f 63 6f 6e 73 74 5f  65 76 61 6c 5f 62 61 73 
  00000060  69 63 73 2e 66 70 0a 00  f0 9f a7 ad 20 46 6f 63 
  00000070  75 73 3a 20 42 61 73 69  63 20 63 6f 6e 73 74 20 
  00000080  65 76 61 6c 75 61 74 69  6f 6e 20 77 69 74 68 20 
  00000090  63 6f 6d 70 69 6c 65 2d  74 69 6d 65 20 61 72 69 
  000000a0  74 68 6d 65 74 69 63 20  61 6e 64 20 63 6f 6e 73 
  000000b0  74 20 62 6c 6f 63 6b 73  0a 00 00 00 00 00 00 00 
  000000c0  f0 9f a7 aa 20 57 68 61  74 20 74 6f 20 6c 6f 6f 
  000000d0  6b 20 66 6f 72 3a 20 6c  61 62 65 6c 65 64 20 6f 
  000000e0  75 74 70 75 74 73 20 62  65 6c 6f 77 0a 00 00 00 
  000000f0  e2 9c 85 20 45 78 70 65  63 74 61 74 69 6f 6e 3a 
  00000100  20 6f 75 74 70 75 74 73  20 6d 61 74 63 68 20 6c 
  00000110  61 62 65 6c 73 0a 00 00  0a 00 00 00 00 00 00 00 
  00000120  42 75 66 66 65 72 3a 20  25 6c 6c 64 4b 42 2c 20 
  00000130  66 61 63 74 6f 72 69 61  6c 28 35 29 3d 25 6c 6c 
  00000140  64 2c 20 6c 61 72 67 65  3d 25 64 0a 00 00 00 00 
  00000150  43 6f 6e 66 69 67 3a 20  25 6c 6c 64 4b 42 20 62 
  00000160  75 66 66 65 72 2c 20 25  6c 6c 64 20 63 6f 6e 6e 
  00000170  65 63 74 69 6f 6e 73 0a  00 00 00 00 00 00 00 00 
  00000180  43 6f 6e 73 74 20 62 6c  6f 63 6b 73 3a 20 73 69 
  00000190  7a 65 3d 25 6c 6c 64 2c  20 73 74 72 61 74 65 67 
  000001a0  79 3d 25 73 2c 20 6d 65  6d 6f 72 79 3d 25 6c 6c 
  000001b0  64 0a 00 
