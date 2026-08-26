fp-native dump: format=MachO arch=Aarch64 entry=0x0

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
global BUFFER_SIZE ty=I64 constant=true initializer=Some(Bytes([0, 16, 0, 0, 0, 0, 0, 0]))
global BUFFER_SIZE ty=I64 constant=true initializer=Some(Bytes([0, 16, 0, 0, 0, 0, 0, 0]))
global MAX_CONNECTIONS ty=I64 constant=true initializer=Some(Bytes([150, 0, 0, 0, 0, 0, 0, 0]))
global MAX_CONNECTIONS ty=I64 constant=true initializer=Some(Bytes([150, 0, 0, 0, 0, 0, 0, 0]))
global FACTORIAL_5 ty=I64 constant=true initializer=Some(Bytes([120, 0, 0, 0, 0, 0, 0, 0]))
global FACTORIAL_5 ty=I64 constant=true initializer=Some(Bytes([120, 0, 0, 0, 0, 0, 0, 0]))
global IS_LARGE ty=I1 constant=true initializer=Some(Bytes([1]))
global IS_LARGE ty=I1 constant=true initializer=Some(Bytes([1]))
global DEFAULT_CONFIG ty=Struct { fields: [I64, I64], packed: false, name: None } constant=true initializer=Some(Bytes([0, 16, 0, 0, 0, 0, 0, 0, 150, 0, 0, 0, 0, 0, 0, 0]))
global DEFAULT_CONFIG ty=Struct { fields: [I64, I64], packed: false, name: None } constant=true initializer=Some(Bytes([0, 16, 0, 0, 0, 0, 0, 0, 150, 0, 0, 0, 0, 0, 0, 0]))
global __const_data_0 ty=Array(I8, 6) constant=true initializer=Some(Bytes([108, 97, 114, 103, 101, 0]))
global __const_data_1 ty=Array(I8, 6) constant=true initializer=Some(Bytes([115, 109, 97, 108, 108, 0]))
fn sigaddset
fn shmdt
fn vfscanf
fn statvfs
fn mach_port_get_refs
fn processor_set_destroy
fn getpeername
fn NSGetSectionDataInObjectFileImage
fn perror
fn __darwin_fd_isset
fn isupper
fn isgraph
fn pthread_testcancel
fn mkdirat
fn grantpt
fn getnameinfo
fn sendto
fn isspace
fn posix_spawn_file_actions_destroy
fn slot_name
fn atomic_flag_test_and_set_explicit
fn gmtime
fn strspn
fn kevent64
fn shm_unlink
fn mach_port_kernel_object
fn flistxattr
fn usleep
fn vm_purgable_control
fn thread_create_running
fn dlerror
fn task_set_emulation
fn task_dyld_process_info_notify_register
fn globfree
fn fgetc
fn getpwnam_r
fn putc_unlocked
fn host_set_atm_diagnostic_flag
fn processor_set_tasks_with_flavor
fn aio_error
fn __vsprintf_chk
fn NSModuleForSymbol
fn mkfifo
fn getsockname
fn getline
fn host_get_io_main
fn vm_region
fn waitpid
fn task_set_policy
fn tolower
fn __svfscanf
fn strtok
fn NSUnLinkModule
fn _dyld_present
fn hsearch
fn mig_allocate
fn _kernelrpc_mach_port_deallocate_trap
fn NSDestroyObjectFileImage
fn siginterrupt
fn calloc
fn unsetenv
fn rand
fn host_processor_set_priv
fn mkstemp
fn utimes
fn wcsncat
fn mach_port_insert_member
fn renameat
fn dup2
fn mach_error_string
fn strtoll
fn getprotobynumber
fn wcslen
fn wcscoll
fn setbuf
fn task_generate_corpse
fn initstate
fn ldiv
fn asctime_r
fn hcreate
fn closelog
fn getpgrp
fn confstr
fn lstat
fn kext_request
fn host_security_create_task_token
fn mach_vm_reclaim_update_kernel_accounting_trap
fn getgid
fn socketpair
fn task_sample
fn atomic_flag_test_and_set
fn clock_set_res
fn _dyld_get_image_vmaddr_slide
fn clonefileat
fn msgget
fn closedir
fn feraiseexcept
fn setnetent
fn close
fn vm_wire
fn mach_memory_object_memory_entry
fn host_processor_info
fn fetestexcept
fn localtime_r
fn iswpunct
fn setpwent
fn strndup
fn wcrtomb
fn wmemcpy
fn host_check_multiuser_mode
fn uselocale
fn NXSwapHostIntToLittle
fn NSAddLibraryWithSearching
fn write
fn dlopen
fn NSIsSymbolNameDefined
fn setreuid
fn mach_vm_region_info_64
fn getdelim
fn munlockall
fn kmod_control
fn macx_swapoff
fn isprint
fn NSLookupAndBindSymbolWithHint
fn vm_read_overwrite
fn sigaction
fn nanosleep
fn thread_policy_get
fn clock_get_res
fn iswlower
fn kmod_get_info
fn wcscasecmp
fn wcsstr
fn wcsncpy
fn setuid
fn task_swap_mach_voucher
fn strtoimax
fn vdprintf
fn mach_port_extract_member
fn getegid
fn isxdigit
fn toupper
fn vfwprintf
fn readlink
fn _kernelrpc_mach_vm_map_trap
fn etap_trace_thread
fn shutdown
fn iswxdigit
fn vm_machine_attribute
fn pid_for_task
fn iswspace
fn l64a
fn strncat
fn mbsrtowcs
fn wcstoul
fn __toupper
fn vwprintf
fn send
fn _OSReadSwapInt16
fn mach_port_get_context
fn swtch
fn NXSwapBigShortToHost
fn NXSwapBigLongLongToHost
fn newlocale
fn iswcntrl
fn sigpause
fn rename
fn wctob
fn symlink
fn task_threads
fn task_get_emulation_vector
fn _dyld_lookup_and_bind_with_hint
fn ftello
fn clock_getres
fn processor_set_policy_disable
fn ftrylockfile
fn _dyld_bind_fully_image_containing_address
fn strerror
fn shmctl
fn _OSWriteSwapInt16
fn strcmp
fn kqueue
fn __wcwidth
fn getpwent
fn removexattr
fn if_freenameindex
fn _OSSwapInt32
fn task_test_sync_upcall
fn mach_port_type
fn ftell
fn strtok_r
fn fputws
fn vswprintf
fn sched_yield
fn task_get_dyld_image_infos
fn thread_policy
fn vm_region_recurse
fn wcschr
fn mach_port_dnrequest_info
fn _dyld_lookup_and_bind_fully
fn getxattr
fn memccpy
fn chmod
fn getchar
fn strcoll
fn remque
fn posix_spawnp
fn sigignore
fn strptime
fn setgid
fn thread_sample
fn vm_map_exec_lockdown
fn mach_error_type
fn msgrcv
fn wcpncpy
fn unlink
fn mlock
fn task_dyld_process_info_notify_deregister
fn posix_spawnattr_init
fn feholdexcept
fn unlockpt
fn posix_spawnattr_setsigmask
fn thread_suspend
fn mach_port_allocate_qos
fn mach_msg_receive
fn vm_stats
fn _dyld_image_count
fn mach_port_allocate_full
fn mach_port_deallocate
fn vprintf
fn putwc
fn NSSymbolDefinitionNameInObjectFileImage
fn freelocale
fn kmod_create
fn sighold
fn setsockopt
fn sigsuspend
fn task_zone_info
fn mach_port_destroy
fn NSLookupAndBindSymbol
fn NSAddImage
fn thread_assign_default
fn fsetxattr
fn act_set_state
fn stat
fn fchown
fn vsscanf
fn aio_return
fn vm_copy
fn nrand48
fn processor_set_max_priority
fn NXSwapLongLong
fn _dyld_lookup_and_bind
fn timespec_get
fn posix_spawnattr_setpgroup
fn setpgid
fn NSIsSymbolDefinedInObjectFileImage
fn host_set_UNDServer
fn NSLinkModule
fn readlinkat
fn _kernelrpc_mach_port_get_attributes_trap
fn setstate
fn abort
fn fwide
fn setservent
fn mach_zone_info
fn sem_destroy
fn truncate
fn towlower
fn semaphore_signal_all
fn task_get_state
fn clock_settime
fn wmemmove
fn puts
fn sendmsg
fn getprotoent
fn sethostent
fn mig_put_reply_port
fn strcspn
fn bind
fn task_policy_get
fn fstatat
fn vm_read_list
fn NSLookupSymbolInModule
fn __math_errhandling
fn wcspbrk
fn freeaddrinfo
fn lseek
fn mach_memory_object_memory_entry_64
fn ungetc
fn getservbyname
fn ffs
fn tcsetpgrp
fn getlogin_r
fn thread_create
fn posix_spawnattr_getflags
fn __darwin_fd_clr
fn wcsnlen
fn sigdelset
fn task_set_port_space
fn tmpnam
fn processor_info
fn iconv
fn tcflush
fn memcmp
fn _dyld_get_image_header_containing_address
fn vwscanf
fn endhostent
fn _Exit
fn vm_mapped_pages_info
fn execve
fn task_resume2
fn host_get_special_port
fn utime
fn sync
fn processor_set_policy_enable
fn fchownat
fn mach_port_mod_refs
fn host_kernel_version
fn getpriority
fn __swbuf
fn processor_set_statistics
fn thread_info
fn NXSwapHostShortToLittle
fn strcasecmp
fn wcwidth
fn fseek
fn memchr
fn asctime
fn fileno
fn fpathconf
fn mmap
fn sigaltstack
fn freopen
fn if_nameindex
fn rmdir
fn mach_msg
fn _OSWriteSwapInt64
fn host_get_boot_info
fn mach_port_construct
fn wait
fn uname
fn fgetws
fn task_assign
fn task_name_for_pid
fn _dyld_all_twolevel_modules_prebound
fn div
fn lrand48
fn wctomb
fn dirname
fn sysconf
fn free
fn iswhexnumber
fn posix_spawnattr_destroy
fn thread_set_special_port
fn thread_set_policy
fn getc_unlocked
fn host_lockgroup_info
fn strlen
fn srand48
fn _kernelrpc_mach_port_extract_member_trap
fn wcstoumax
fn pthread_key_delete
fn semaphore_timedwait
fn encrypt
fn task_set_emulation_vector
fn readdir_r
fn mach_port_guard
fn setxattr
fn imaxdiv
fn if_indextoname
fn sem_close
fn sem_getvalue
fn unlinkat
fn execv
fn task_set_corpse_forking_behavior
fn host_virtual_physical_table_info
fn towctrans
fn getcwd
fn setprotoent
fn _dyld_shared_cache_contains_path
fn task_set_special_port
fn msgsnd
fn fclonefileat
fn lcong48
fn select
fn processor_set_tasks
fn OSHostByteOrder
fn getnetent
fn pselect
fn _kernelrpc_mach_port_unguard_trap
fn _kernelrpc_mach_vm_protect_trap
fn task_policy
fn setpriority
fn memcpy
fn iswalnum
fn open_wmemstream
fn atol
fn getservent
fn poll
fn isatty
fn setegid
fn NXSwapHostShortToBig
fn __srget
fn wcscspn
fn linkat
fn mbstowcs
fn clock_set_time
fn task_suspend2
fn NXSwapLittleLongToHost
fn gettimeofday
fn NXSwapHostLongLongToLittle
fn NSLinkEditError
fn pthread_kill
fn wcscpy
fn host_processor_sets
fn macx_triggers
fn time
fn aio_read
fn dlsym
fn vm_remap_new
fn mig_strncpy_zerofill
fn task_get_exception_ports_info
fn localeconv
fn realpath
fn wmemcmp
fn setgrent
fn endnetent
fn ctime_r
fn mbrlen
fn wcsrchr
fn posix_spawnattr_setflags
fn gethostbyname
fn getservbyport
fn posix_spawnattr_getpgroup
fn task_info
fn task_inspect
fn host_get_clock_service
fn regcomp
fn vm_read
fn NXSwapShort
fn isdigit
fn host_get_atm_diagnostic_flag
fn thread_convert_thread_state
fn mach_task_is_self
fn llabs
fn getlogin
fn mknodat
fn basename
fn endprotoent
fn cfgetispeed
fn host_set_exception_ports
fn fsetpos
fn tcdrain
fn fgetwc
fn __darwin_check_fd_set
fn processor_exit
fn task_wire
fn NXSwapBigLongToHost
fn host_get_exception_ports
fn fesetexceptflag
fn strchr
fn pthread_setconcurrency
fn waitid
fn mach_port_get_set_status
fn NXSwapHostLongLongToBig
fn umask
fn getpgid
fn task_register_dyld_set_dyld_state
fn isascii
fn strtoull
fn sem_wait
fn setvbuf
fn NSIsSymbolNameDefinedInImage
fn NSInstallLinkEditErrorHandlers
fn mach_port_set_attributes
fn strtol
fn _NSGetExecutablePath
fn readdir
fn vfork
fn endservent
fn thread_set_mach_voucher
fn _OSReadInt64
fn funlockfile
fn getppid
fn NSIsSymbolNameDefinedWithHint
fn towupper
fn inet_pton
fn mach_port_kobject_description
fn voucher_mach_msg_adopt
fn clock_set_attributes
fn task_set_exc_guard_behavior
fn getchar_unlocked
fn atomic_thread_fence
fn gmtime_r
fn fseeko
fn wcsncmp
fn wcpcpy
fn getgroups
fn tcgetpgrp
fn getc
fn task_get_mach_voucher
fn task_create_identity_token
fn NXHostByteOrder
fn posix_spawn_file_actions_addopen
fn NXSwapLittleLongLongToHost
fn swtch_pri
fn thread_terminate
fn ___tolower
fn mig_get_reply_port
fn atoi
fn wcsftime
fn islower
fn fputwc
fn dlclose
fn sched_get_priority_min
fn host_default_memory_manager
fn putwchar
fn srandom
fn system
fn inet_addr
fn iswblank
fn feclearexcept
fn iswideogram
fn vm_protect
fn clearerr
fn posix_spawnattr_setsigdefault
fn vm_inherit
fn posix_spawn_file_actions_addchdir
fn sleep
fn thread_get_mach_voucher
fn thread_get_exception_ports
fn setlocale
fn shmget
fn host_statistics
fn feupdateenv
fn fesetenv
fn putc
fn lio_listio
fn task_map_corpse_info_64
fn mkfifoat
fn setgroupent
fn task_map_kcdata_object_64
fn sem_unlink
fn semaphore_signal
fn quick_exit
fn host_swap_exception_ports
fn mach_port_swap_guard
fn panic_init
fn fesetround
fn voucher_mach_msg_set
fn ftruncate
fn NSAddressOfSymbol
fn NXSwapLittleShortToHost
fn siglongjmp
fn atomic_flag_clear
fn mach_msg_overwrite
fn _kernelrpc_mach_vm_allocate_trap
fn _kernelrpc_mach_port_type_trap
fn longjmp
fn getgrnam
fn getgrnam_r
fn ftok
fn strrchr
fn pclose
fn gethostent
fn setrlimit
fn thread_depress_abort
fn mach_port_insert_right
fn mach_port_is_connection_for_service
fn _dyld_get_image_name
fn getrlimit
fn wcscmp
fn mach_port_extract_right
fn open_memstream
fn task_for_pid
fn _exit
fn aio_cancel
fn isalpha
fn getopt
fn processor_set_threads
fn lldiv
fn iswctype
fn faccessat
fn dup
fn mprotect
fn mach_port_get_service_port_info
fn mach_port_names
fn task_register_dyld_image_infos
fn kill
fn mach_ports_lookup
fn mblen
fn fopen
fn seed48
fn lockf
fn mig_strncpy
fn task_policy_set
fn task_set_phys_footprint_limit
fn thread_get_assignment
fn geteuid
fn clock
fn processor_set_default
fn NXSwapInt
fn task_register_hardened_exception_handler
fn sigprocmask
fn posix_spawn
fn posix_memalign
fn task_assign_default
fn tempnam
fn msgctl
fn strerror_r
fn srand
fn mlockall
fn kmod_destroy
fn semaphore_destroy
fn pthread_sigmask
fn setkey
fn thread_get_exception_ports_info
fn NSLibraryNameForModule
fn _dyld_launched_prebound
fn getentropy
fn iswnumber
fn sem_post
fn getaddrinfo
fn endpwent
fn __darwin_fd_set
fn fchdir
fn host_set_special_port
fn crypt
fn _OSReadInt16
fn semaphore_timedwait_signal
fn symlinkat
fn semaphore_wait_signal
fn mach_error
fn tcsendbreak
fn hdestroy
fn random
fn getsockopt
fn dirfd
fn __assert_rtn
fn task_set_state
fn thread_get_special_port
fn gethostbyaddr
fn vswscanf
fn setsid
fn mach_vm_region_info
fn memset
fn __error
fn imaxabs
fn psignal
fn atoll
fn jrand48
fn wcstoull
fn fnmatch
fn pwrite
fn host_reboot
fn task_get_exc_guard_behavior
fn mach_thread_self
fn mbrtowc
fn task_create
fn task_get_assignment
fn fegetround
fn thread_set_exception_ports
fn posix_spawnattr_getsigdefault
fn fork
fn host_page_size
fn _kernelrpc_mach_vm_deallocate_trap
fn strncasecmp
fn ualarm
fn strdup
fn ungetwc
fn vm_allocate
fn mach_port_allocate
fn mach_port_space_basic_info
fn mach_port_assert_attributes
fn mach_generate_activity_id
fn mach_msg_destroy
fn processor_set_stack_usage
fn vm_deallocate
fn wcstoll
fn thread_get_state
fn popen
fn lchown
fn mach_voucher_deallocate
fn setpgrp
fn task_map_corpse_info
fn lock_set_create
fn getgrgid_r
fn mktemp
fn debug_control_port_for_pid
fn _host_page_size
fn mach_port_request_notification
fn _kernelrpc_mach_port_move_member_trap
fn voucher_mach_msg_clear
fn _tlv_bootstrap
fn _OSWriteSwapInt32
fn ttyname_r
fn getpwnam
fn _OSReadInt32
fn sigismember
fn host_statistics64
fn host_get_multiuser_config_flags
fn mach_port_rename
fn NSVersionOfLinkTimeLibrary
fn btowc
fn tcsetattr
fn mbsinit
fn killpg
fn mrand48
fn __sputc
fn _OSWriteInt16
fn recv
fn task_resume
fn semaphore_create
fn fmemopen
fn semaphore_signal_thread
fn host_get_UNDServer
fn mach_port_guard_with_flags
fn wcsxfrm
fn iswspecial
fn aligned_alloc
fn processor_assign
fn fstat
fn task_terminate
fn clock_sleep_trap
fn host_set_multiuser_config_flags
fn NSSymbolDefinitionCountInObjectFileImage
fn mach_port_set_mscount
fn _kernelrpc_mach_vm_purgable_control_trap
fn host_create_mach_voucher
fn times
fn NSNameOfSymbol
fn __isctype
fn tzset
fn iswdigit
fn _OSSwapInt16
fn pthread_getconcurrency
fn seteuid
fn posix_spawn_file_actions_addfchdir
fn access
fn sigpending
fn mach_make_memory_entry
fn _kernelrpc_mach_port_insert_member_trap
fn setlogmask
fn vm_region_recurse_64
fn host_get_clock_control
fn processor_control
fn _dyld_image_containing_address
fn host_create_mach_voucher_trap
fn macx_backing_store_recovery
fn task_identity_token_get_task_port
fn iswrune
fn utimensat
fn setgrfile
fn isalnum
fn gets
fn fremovexattr
fn getdate
fn getnetbyaddr
fn atomic_signal_fence
fn host_priv_statistics
fn NSLookupSymbolInImage
fn strpbrk
fn munmap
fn __istype
fn wcstoimax
fn fsync
fn vfprintf
fn execvp
fn task_test_async_upcall_propagation
fn _kernelrpc_mach_port_insert_right_trap
fn NXSwapFloat
fn link
fn task_get_special_port
fn thread_abort
fn NXSwapHostLongToBig
fn NSCreateObjectFileImageFromMemory
fn thread_adopt_exception_handler
fn setenv
fn insque
fn vm_map
fn memmove
fn wcsdup
fn mach_port_unguard
fn posix_madvise
fn fflush
fn NXSwapBigIntToHost
fn host_security_set_task_token
fn getnetbyname
fn processor_set_create
fn posix_spawn_file_actions_adddup2
fn chdir
fn pipe
fn mach_zone_info_for_zone
fn wcsnrtombs
fn _OSSwapInt64
fn fclose
fn vscanf
fn swab
fn munlock
fn wmemchr
fn mach_ports_register
fn __tolower
fn getwc
fn rewinddir
fn shmat
fn _kernelrpc_mach_port_guard_trap
fn host_register_mach_voucher_attr_manager
fn rand_r
fn wcswidth
fn raise
fn _OSWriteInt32
fn vm_behavior_set
fn host_register_well_known_mach_voucher_attr_manager
fn mach_host_self
fn mach_msg_send
fn chown
fn ptsname
fn host_info
fn vm_msync
fn voucher_mach_msg_revert
fn NXSwapLong
fn _OSReadSwapInt64
fn vfwscanf
fn getuid
fn regexec
fn posix_openpt
fn task_self_trap
fn stpncpy
fn clonefile
fn abs
fn strncmp
fn __darwin_check_fd_set_overflow
fn mknod
fn task_register_dyld_get_process_state
fn thread_resume
fn mach_vm_wire
fn recvmsg
fn listxattr
fn mach_port_kobject
fn setjmp
fn thread_swap_mach_voucher
fn a64l
fn iswgraph
fn task_set_mach_voucher
fn malloc
fn mach_port_get_attributes
fn getpwuid_r
fn semop
fn vsprintf
fn iscntrl
fn sigrelse
fn vm_map_64
fn sigfillset
fn semaphore_wait
fn strtoumax
fn getsubopt
fn fputs
fn fdopen
fn nl_langinfo
fn __NDR_convert__mig_reply_error_t
fn fgetpos
fn fread
fn tmpfile
fn strcat
fn strftime
fn wctrans
fn duplocale
fn if_nametoindex
fn posix_spawnattr_getsigmask
fn atomic_flag_clear_explicit
fn cfsetospeed
fn cfsetispeed
fn fchmodat
fn iswalpha
fn inet_ntoa
fn vm_remap
fn task_unregister_dyld_image_infos
fn mach_port_allocate_name
fn opendir
fn regerror
fn gethostid
fn pathconf
fn sigsetjmp
fn pread
fn vm_region_64
fn NXSwapLittleIntToHost
fn wctype
fn ___toupper
fn accept
fn setitimer
fn fwrite
fn ferror
fn regfree
fn creat
fn cfgetospeed
fn getpid
fn ttyname
fn processor_get_assignment
fn thread_switch
fn mach_voucher_extract_attr_recipe_trap
fn iconv_close
fn rewind
fn thread_set_state
fn mig_reply_setup
fn _kernelrpc_mach_port_mod_refs_trap
fn fdopendir
fn connect
fn NSSymbolReferenceNameInObjectFileImage
fn mach_port_get_srights
fn feof
fn recvfrom
fn openlog
fn mig_deallocate
fn wcsspn
fn msync
fn aio_write
fn gethostname
fn getitimer
fn fstatvfs
fn processor_set_policy_control
fn task_suspend
fn task_set_exception_ports
fn mach_port_destruct
fn __sigbits
fn NXSwapDouble
fn NSVersionOfRunTimeLibrary
fn getgrgid
fn getpwuid
fn alarm
fn task_register_dyld_shared_cache_image_info
fn mkdir
fn vsnprintf
fn _OSReadSwapInt32
fn wcstol
fn mach_port_space_info
fn _kernelrpc_mach_port_allocate_trap
fn iswprint
fn strxfrm
fn fgets
fn wcscat
fn NSSymbolReferenceCountInObjectFileImage
fn getgrent
fn mach_make_memory_entry_64
fn fputc
fn getrusage
fn putenv
fn thread_swap_exception_ports
fn tcgetattr
fn mach_port_set_seqno
fn wmemset
fn task_set_info
fn stpcpy
fn putchar
fn iswascii
fn thread_wire
fn __maskrune
fn setregid
fn vm_allocate_cpm
fn fchmod
fn sched_get_priority_max
fn mach_port_set_context
fn NXSwapHostIntToBig
fn _dyld_get_image_header
fn _OSWriteInt64
fn NSCreateObjectFileImageFromFile
fn macx_swapon
fn NSAddLibrary
fn socket
fn kevent
fn fgetxattr
fn remove
fn sockatmark
fn isblank
fn sigwait
fn exit
fn strtoul
fn iswupper
fn wcsrtombs
fn wcstombs
fn telldir
fn ctermid
fn sem_init
fn strnlen
fn tcflow
fn mig_dealloc_reply_port
fn host_processors
fn getprotobyname
fn seekdir
fn task_get_exception_ports
fn localtime
fn inet_ntop
fn act_get_state
fn _longjmp
fn sem_trywait
fn ispunct
fn processor_start
fn getsid
fn iconv_open
fn aio_fsync
fn task_purgable_info
fn sigemptyset
fn pause
fn fegetenv
fn semget
fn ___runetype
fn toascii
fn listen
fn flockfile
fn strncpy
fn mbsnrtowcs
fn fegetexceptflag
fn getenv
fn labs
fn wcsncasecmp
fn posix_spawn_file_actions_addclose
fn posix_spawn_file_actions_init
fn thread_abort_safely
fn task_set_ras_pc
fn thread_assign
fn mbtowc
fn iswphonogram
fn lock_set_destroy
fn thread_policy_set
fn alphasort
fn task_dyld_process_info_notify_get
fn _kernelrpc_mach_port_destruct_trap
fn processor_set_info
fn _kernelrpc_mach_port_request_notification_trap
fn NXSwapHostLongToLittle
fn NSNameOfModule
fn clock_sleep
fn clock_gettime
fn getwchar
fn futimens
fn macx_backing_store_suspend
fn vm_write
fn strsignal
fn mktime
fn endgrent
fn _kernelrpc_mach_port_construct_trap
fn _setjmp
fn tcgetsid
fn read
fn nice
fn ctime
fn task_swap_exception_ports
fn strstr
fn realloc
fn wcstok
fn mach_port_move_member
fn putchar_unlocked
fn aio_suspend
fn __vsnprintf_chk
fn gai_strerror
fn strcpy
fn host_request_notification
fn vm_map_page_query
fn mach_port_peek
fn mach_memory_info
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
  00000020  f0 13 00 f9 00 00 00 90  00 00 00 91 00 a0 01 91 
  00000030  00 00 00 94 00 00 00 90  00 00 00 91 00 40 02 91 
  00000040  00 00 00 94 00 00 00 90  00 00 00 91 00 a0 03 91 
  00000050  00 00 00 94 00 00 00 90  00 00 00 91 00 60 04 91 
  00000060  00 00 00 94 00 00 00 90  00 00 00 91 00 00 05 91 
  00000070  00 00 00 94 f0 03 00 91  10 02 0a 91 f0 2b 00 f9 
  00000080  10 00 82 d2 11 80 80 d2  09 0e d1 9a f0 03 09 aa 
  00000090  f0 2f 00 f9 f1 2b 40 f9  f0 2f 40 f9 30 02 00 f9 
  000000a0  f0 2b 40 f9 11 02 40 f9  f1 37 00 f9 00 00 00 90 
  000000b0  00 00 00 91 00 20 05 91  e1 37 40 f9 f0 37 40 f9 
  000000c0  f0 03 00 f9 02 0f 80 d2  10 0f 80 d2 f0 07 00 f9 
  000000d0  23 00 80 d2 30 00 80 d2  f0 0b 00 f9 00 00 00 94 
  000000e0  f0 03 00 91 10 22 0a 91  f0 3f 00 f9 10 00 82 d2 
  000000f0  11 80 80 d2 09 0e d1 9a  f0 03 09 aa f0 43 00 f9 
  00000100  f1 3f 40 f9 f0 43 40 f9  30 02 00 f9 f0 3f 40 f9 
  00000110  11 02 40 f9 f1 4b 00 f9  00 00 00 90 00 00 00 91 
  00000120  00 e0 05 91 e1 4b 40 f9  f0 4b 40 f9 f0 03 00 f9 
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
  00000380  00 a0 06 91 e1 c7 40 f9  f0 c7 40 f9 f0 03 00 f9 
  00000390  e2 cf 40 f9 f0 cf 40 f9  f0 07 00 f9 e3 d3 40 f9 
  000003a0  f0 d3 40 f9 f0 0b 00 f9  00 00 00 94 bf 03 00 91 
  000003b0  f0 03 00 91 10 82 0b 91  1d 7a 40 a9 ff c3 0b 91 
  000003c0  00 00 80 d2 c0 03 5f d6 

.rodata (475 bytes):
  00000000  00 10 00 00 00 00 00 00  00 10 00 00 00 00 00 00 
  00000010  96 00 00 00 00 00 00 00  96 00 00 00 00 00 00 00 
  00000020  78 00 00 00 00 00 00 00  78 00 00 00 00 00 00 00 
  00000030  01 01 00 00 00 00 00 00  00 10 00 00 00 00 00 00 
  00000040  96 00 00 00 00 00 00 00  00 10 00 00 00 00 00 00 
  00000050  96 00 00 00 00 00 00 00  6c 61 72 67 65 00 73 6d 
  00000060  61 6c 6c 00 00 00 00 00  f0 9f 93 98 20 54 75 74 
  00000070  6f 72 69 61 6c 3a 20 30  31 5f 63 6f 6e 73 74 5f 
  00000080  65 76 61 6c 5f 62 61 73  69 63 73 2e 66 70 0a 00 
  00000090  f0 9f a7 ad 20 46 6f 63  75 73 3a 20 42 61 73 69 
  000000a0  63 20 63 6f 6e 73 74 20  65 76 61 6c 75 61 74 69 
  000000b0  6f 6e 20 77 69 74 68 20  63 6f 6d 70 69 6c 65 2d 
  000000c0  74 69 6d 65 20 61 72 69  74 68 6d 65 74 69 63 20 
  000000d0  61 6e 64 20 63 6f 6e 73  74 20 62 6c 6f 63 6b 73 
  000000e0  0a 00 00 00 00 00 00 00  f0 9f a7 aa 20 57 68 61 
  000000f0  74 20 74 6f 20 6c 6f 6f  6b 20 66 6f 72 3a 20 6c 
  00000100  61 62 65 6c 65 64 20 6f  75 74 70 75 74 73 20 62 
  00000110  65 6c 6f 77 0a 00 00 00  e2 9c 85 20 45 78 70 65 
  00000120  63 74 61 74 69 6f 6e 3a  20 6f 75 74 70 75 74 73 
  00000130  20 6d 61 74 63 68 20 6c  61 62 65 6c 73 0a 00 00 
  00000140  0a 00 00 00 00 00 00 00  42 75 66 66 65 72 3a 20 
  00000150  25 6c 6c 64 4b 42 2c 20  66 61 63 74 6f 72 69 61 
  00000160  6c 28 35 29 3d 25 6c 6c  64 2c 20 6c 61 72 67 65 
  00000170  3d 25 64 0a 00 00 00 00  43 6f 6e 66 69 67 3a 20 
  00000180  25 6c 6c 64 4b 42 20 62  75 66 66 65 72 2c 20 25 
  00000190  6c 6c 64 20 63 6f 6e 6e  65 63 74 69 6f 6e 73 0a 
  000001a0  00 00 00 00 00 00 00 00  43 6f 6e 73 74 20 62 6c 
  000001b0  6f 63 6b 73 3a 20 73 69  7a 65 3d 25 6c 6c 64 2c 
  000001c0  20 73 74 72 61 74 65 67  79 3d 25 73 2c 20 6d 65 
  000001d0  6d 6f 72 79 3d 25 6c 6c  64 0a 00 
