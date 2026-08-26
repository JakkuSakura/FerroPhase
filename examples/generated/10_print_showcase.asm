fp-native dump: format=MachO arch=Aarch64 entry=0x0

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
global __const_data_0 ty=Array(I8, 12) constant=true initializer=Some(Bytes([108, 105, 110, 101, 49, 10, 108, 105, 110, 101, 50, 0]))
global __const_data_1 ty=Array(I8, 8) constant=true initializer=Some(Bytes([116, 97, 98, 9, 101, 110, 100, 0]))
fn mach_port_peek
fn grantpt
fn mach_msg
fn readlink
fn pread
fn _kernelrpc_mach_port_request_notification_trap
fn thread_convert_thread_state
fn NXSwapLittleIntToHost
fn processor_set_default
fn NXSwapHostIntToBig
fn NSGetSectionDataInObjectFileImage
fn __swbuf
fn _OSSwapInt16
fn atomic_flag_test_and_set_explicit
fn abs
fn getservent
fn setnetent
fn iswalnum
fn fputs
fn strtok_r
fn aio_suspend
fn fegetenv
fn quick_exit
fn task_get_state
fn fwrite
fn processor_set_max_priority
fn task_set_mach_voucher
fn mach_port_get_attributes
fn mig_reply_setup
fn sockatmark
fn mkdirat
fn strnlen
fn getsockopt
fn _OSWriteSwapInt64
fn task_set_emulation_vector
fn removexattr
fn hcreate
fn creat
fn getgroups
fn vscanf
fn task_policy_get
fn task_sample
fn wcscoll
fn thread_get_exception_ports_info
fn NSIsSymbolDefinedInObjectFileImage
fn toupper
fn _OSReadInt32
fn iswgraph
fn shutdown
fn dirname
fn iswcntrl
fn mach_error
fn waitpid
fn strftime
fn btowc
fn wcstoll
fn getlogin
fn _OSReadSwapInt64
fn clock_getres
fn task_set_exc_guard_behavior
fn vswscanf
fn alarm
fn NXSwapShort
fn NSLookupAndBindSymbol
fn NXSwapHostLongToBig
fn getdelim
fn asctime
fn fgetc
fn putenv
fn wcstol
fn getgrnam_r
fn __darwin_fd_isset
fn mig_put_reply_port
fn task_for_pid
fn wcsstr
fn mblen
fn setkey
fn panic_init
fn mach_msg_destroy
fn fclonefileat
fn ungetc
fn host_page_size
fn wcrtomb
fn rewinddir
fn strtoumax
fn ctermid
fn getgid
fn getpgrp
fn swtch
fn __srget
fn openlog
fn execve
fn _OSWriteInt32
fn vm_mapped_pages_info
fn mach_port_extract_member
fn fesetround
fn srand48
fn wmemchr
fn rmdir
fn ftok
fn msync
fn processor_set_tasks_with_flavor
fn sched_get_priority_max
fn mach_port_request_notification
fn host_lockgroup_info
fn sigprocmask
fn cfsetospeed
fn strerror
fn semaphore_wait
fn memcmp
fn ftruncate
fn sethostent
fn regfree
fn posix_spawnattr_setflags
fn iswspace
fn iconv_open
fn macx_swapon
fn clock_set_res
fn setxattr
fn NSLookupSymbolInModule
fn task_wire
fn task_set_port_space
fn srandom
fn setpgrp
fn shmat
fn _kernelrpc_mach_port_insert_member_trap
fn unsetenv
fn mach_msg_overwrite
fn cfgetospeed
fn OSHostByteOrder
fn mach_memory_object_memory_entry
fn memmove
fn hdestroy
fn wcslen
fn setgid
fn vm_protect
fn NXSwapBigIntToHost
fn uselocale
fn mig_get_reply_port
fn _kernelrpc_mach_port_type_trap
fn lldiv
fn strtoimax
fn iswlower
fn rename
fn iswprint
fn alphasort
fn task_get_special_port
fn vm_allocate
fn NXSwapHostLongLongToLittle
fn NSSymbolReferenceCountInObjectFileImage
fn random
fn llabs
fn fesetenv
fn posix_spawnattr_getsigdefault
fn task_inspect
fn __wcwidth
fn stpcpy
fn mach_port_get_refs
fn host_get_exception_ports
fn recvfrom
fn getnetbyname
fn iswdigit
fn mktime
fn thread_create
fn task_map_corpse_info_64
fn vsnprintf
fn strspn
fn mbsrtowcs
fn wcscasecmp
fn strncasecmp
fn processor_start
fn thread_depress_abort
fn mach_port_destruct
fn pathconf
fn ldiv
fn task_get_assignment
fn duplocale
fn thread_swap_mach_voucher
fn chmod
fn isgraph
fn _kernelrpc_mach_port_unguard_trap
fn send
fn closedir
fn pthread_testcancel
fn longjmp
fn fetestexcept
fn getchar_unlocked
fn poll
fn munlockall
fn getcwd
fn mach_port_insert_right
fn ftell
fn dlsym
fn fgetxattr
fn strtoul
fn unlockpt
fn endprotoent
fn task_map_corpse_info
fn task_resume
fn thread_policy_set
fn stat
fn NSSymbolDefinitionNameInObjectFileImage
fn NSIsSymbolNameDefinedInImage
fn task_suspend2
fn wcsrtombs
fn task_suspend
fn wmemmove
fn getprotobynumber
fn tmpnam
fn mbstowcs
fn nl_langinfo
fn execvp
fn vm_inherit
fn mach_make_memory_entry_64
fn aio_read
fn NXSwapHostShortToLittle
fn getpgid
fn fseeko
fn strsignal
fn wcstoumax
fn mach_port_destroy
fn thread_switch
fn wcsftime
fn iswphonogram
fn tcsetattr
fn wmemset
fn __isctype
fn atomic_signal_fence
fn posix_spawnattr_init
fn task_get_exception_ports
fn setstate
fn mprotect
fn thread_set_special_port
fn NXSwapHostShortToBig
fn siglongjmp
fn sendto
fn fsync
fn mach_port_assert_attributes
fn vm_allocate_cpm
fn wcsnrtombs
fn task_dyld_process_info_notify_get
fn kext_request
fn setjmp
fn ___toupper
fn aio_error
fn getservbyport
fn realloc
fn exit
fn getaddrinfo
fn pause
fn mknodat
fn clock
fn getegid
fn processor_set_policy_enable
fn task_get_dyld_image_infos
fn readlinkat
fn iswnumber
fn fesetexceptflag
fn killpg
fn semaphore_signal_all
fn gmtime
fn fputws
fn iscntrl
fn host_set_special_port
fn semaphore_wait_signal
fn voucher_mach_msg_set
fn setgroupent
fn NXSwapLittleLongLongToHost
fn thread_info
fn free
fn iswascii
fn accept
fn task_resume2
fn mach_port_guard
fn mach_port_kobject_description
fn crypt
fn thread_sample
fn toascii
fn __error
fn __svfscanf
fn strrchr
fn wcsncpy
fn dlclose
fn setlogmask
fn isupper
fn gets
fn abort
fn imaxdiv
fn nrand48
fn iswhexnumber
fn inet_pton
fn sem_unlink
fn tcgetsid
fn aio_return
fn processor_set_info
fn posix_spawnattr_getflags
fn mach_port_is_connection_for_service
fn mach_thread_self
fn host_register_mach_voucher_attr_manager
fn mach_error_string
fn mach_port_kobject
fn NXSwapLongLong
fn NSAddLibraryWithSearching
fn kqueue
fn vdprintf
fn vm_map_64
fn mach_port_space_info
fn tempnam
fn task_assign
fn getentropy
fn aio_fsync
fn pwrite
fn _OSWriteInt64
fn thread_policy_get
fn host_statistics
fn host_processor_info
fn _kernelrpc_mach_port_move_member_trap
fn mach_port_move_member
fn _dyld_lookup_and_bind_with_hint
fn atol
fn setgrfile
fn vm_region
fn stpncpy
fn wcscmp
fn rand
fn rewind
fn strtol
fn if_nametoindex
fn malloc
fn sem_post
fn host_security_set_task_token
fn mach_port_mod_refs
fn freeaddrinfo
fn unlink
fn symlink
fn thread_get_mach_voucher
fn vm_behavior_set
fn host_info
fn _dyld_shared_cache_contains_path
fn _dyld_image_containing_address
fn wcstoull
fn mach_voucher_deallocate
fn iswrune
fn fgets
fn fdopen
fn putwc
fn pthread_sigmask
fn msgctl
fn vsprintf
fn iswpunct
fn memcpy
fn atomic_flag_clear
fn strcoll
fn wcsdup
fn gai_strerror
fn fchdir
fn mach_port_allocate_full
fn NXSwapLong
fn _setjmp
fn mknod
fn task_swap_mach_voucher
fn strxfrm
fn semaphore_destroy
fn remove
fn NXSwapHostLongToLittle
fn NSDestroyObjectFileImage
fn sem_init
fn posix_spawnattr_getpgroup
fn mach_port_get_context
fn getxattr
fn vm_read_overwrite
fn kevent
fn sigfillset
fn nanosleep
fn NSNameOfModule
fn processor_set_statistics
fn vm_region_recurse_64
fn localtime_r
fn _kernelrpc_mach_port_get_attributes_trap
fn seekdir
fn wcsnlen
fn task_terminate
fn sighold
fn vfscanf
fn mbrtowc
fn sleep
fn processor_set_tasks
fn _kernelrpc_mach_port_allocate_trap
fn vswprintf
fn mkfifoat
fn thread_terminate
fn ttyname
fn _dyld_get_image_header_containing_address
fn task_set_corpse_forking_behavior
fn NSModuleForSymbol
fn task_info
fn task_dyld_process_info_notify_register
fn labs
fn host_virtual_physical_table_info
fn getpwuid
fn wmemcmp
fn geteuid
fn pipe
fn semget
fn kmod_create
fn gethostbyaddr
fn getgrgid
fn thread_suspend
fn vm_copy
fn mach_port_construct
fn __sputc
fn fremovexattr
fn sem_close
fn putchar
fn dup2
fn mach_port_set_seqno
fn strcmp
fn vm_msync
fn tcsetpgrp
fn host_get_io_main
fn host_statistics64
fn lock_set_create
fn __vsprintf_chk
fn mbtowc
fn kill
fn iswspecial
fn recv
fn posix_spawn_file_actions_addchdir
fn swab
fn fstat
fn mach_generate_activity_id
fn sigismember
fn setbuf
fn mbrlen
fn aligned_alloc
fn wmemcpy
fn getprotoent
fn __maskrune
fn lockf
fn fstatat
fn wcschr
fn vm_read
fn mach_error_type
fn vm_purgable_control
fn sem_wait
fn _dyld_image_count
fn strcpy
fn thread_resume
fn NSLookupSymbolInImage
fn NSLibraryNameForModule
fn task_generate_corpse
fn setenv
fn sigsetjmp
fn wcspbrk
fn _Exit
fn processor_set_threads
fn vsscanf
fn towupper
fn task_unregister_dyld_image_infos
fn sigsuspend
fn mach_port_deallocate
fn mach_voucher_extract_attr_recipe_trap
fn mig_deallocate
fn ferror
fn thread_set_state
fn host_check_multiuser_mode
fn umask
fn macx_backing_store_suspend
fn wcsncmp
fn _kernelrpc_mach_vm_protect_trap
fn sendmsg
fn fchownat
fn tcgetpgrp
fn socketpair
fn mach_port_type
fn __tolower
fn strpbrk
fn semaphore_create
fn strncat
fn mbsnrtowcs
fn kevent64
fn _dyld_lookup_and_bind
fn endgrent
fn endservent
fn getgrnam
fn vfork
fn task_set_policy
fn getpwnam
fn wait
fn strtoull
fn time
fn processor_set_policy_control
fn posix_spawnattr_setpgroup
fn fputc
fn setregid
fn iconv_close
fn lcong48
fn setpriority
fn isalnum
fn sigaddset
fn NXSwapLittleLongToHost
fn inet_ntop
fn debug_control_port_for_pid
fn __vsnprintf_chk
fn inet_ntoa
fn waitid
fn posix_spawn_file_actions_addfchdir
fn linkat
fn strerror_r
fn _exit
fn fegetexceptflag
fn posix_spawn_file_actions_destroy
fn if_nameindex
fn localeconv
fn select
fn mkdir
fn mach_port_allocate_qos
fn srand
fn msgsnd
fn clock_gettime
fn getppid
fn aio_write
fn fread
fn iswblank
fn _OSWriteSwapInt32
fn _kernelrpc_mach_port_extract_member_trap
fn usleep
fn faccessat
fn host_set_UNDServer
fn iswctype
fn fputwc
fn wctrans
fn lseek
fn task_set_ras_pc
fn mach_zone_info_for_zone
fn NXSwapHostIntToLittle
fn getpid
fn host_get_special_port
fn thread_get_exception_ports
fn wcstoul
fn posix_spawn_file_actions_init
fn popen
fn _dyld_launched_prebound
fn NSAddressOfSymbol
fn host_get_atm_diagnostic_flag
fn listxattr
fn getrlimit
fn getpwnam_r
fn times
fn getwchar
fn ___tolower
fn mlock
fn host_priv_statistics
fn thread_get_special_port
fn thread_set_policy
fn dlerror
fn vm_machine_attribute
fn lrand48
fn sigemptyset
fn wcwidth
fn setvbuf
fn sem_getvalue
fn lock_set_destroy
fn open_memstream
fn thread_create_running
fn processor_exit
fn task_register_hardened_exception_handler
fn mach_port_guard_with_flags
fn mach_vm_wire
fn NXSwapBigLongLongToHost
fn asctime_r
fn vm_remap_new
fn _NSGetExecutablePath
fn strndup
fn strncpy
fn wcscspn
fn gethostent
fn _kernelrpc_mach_vm_map_trap
fn kmod_control
fn lio_listio
fn localtime
fn initstate
fn posix_spawnattr_getsigmask
fn host_get_clock_control
fn thread_abort_safely
fn fdopendir
fn thread_wire
fn feof
fn ftello
fn host_default_memory_manager
fn task_set_exception_ports
fn thread_get_state
fn mach_vm_region_info
fn putc_unlocked
fn wctob
fn setsockopt
fn getnetent
fn task_create_identity_token
fn NSLinkModule
fn processor_set_policy_disable
fn fstatvfs
fn sigaltstack
fn endhostent
fn mach_msg_receive
fn clock_get_res
fn read
fn _kernelrpc_mach_port_construct_trap
fn posix_spawn_file_actions_addclose
fn encrypt
fn vfprintf
fn newlocale
fn unlinkat
fn task_dyld_process_info_notify_deregister
fn NSSymbolDefinitionCountInObjectFileImage
fn mach_port_insert_member
fn host_swap_exception_ports
fn getc
fn _kernelrpc_mach_port_destruct_trap
fn host_create_mach_voucher
fn mach_port_get_srights
fn dlopen
fn vm_remap
fn kmod_get_info
fn NXSwapBigLongToHost
fn getsockname
fn setlocale
fn if_indextoname
fn __darwin_check_fd_set_overflow
fn globfree
fn _OSReadInt16
fn isalpha
fn fileno
fn vwprintf
fn sched_get_priority_min
fn host_get_clock_service
fn mach_msg_send
fn NSLinkEditError
fn posix_spawnattr_setsigdefault
fn mach_port_set_attributes
fn mach_port_rename
fn feraiseexcept
fn mach_port_kernel_object
fn task_get_emulation_vector
fn processor_assign
fn ualarm
fn __NDR_convert__mig_reply_error_t
fn clock_sleep_trap
fn NXSwapFloat
fn pthread_kill
fn wcsncasecmp
fn posix_spawn
fn posix_spawnp
fn isdigit
fn symlinkat
fn _OSReadInt64
fn pthread_getconcurrency
fn utimes
fn task_register_dyld_image_infos
fn clock_set_time
fn jrand48
fn sigpause
fn vprintf
fn msgget
fn getitimer
fn vm_map_exec_lockdown
fn mach_port_set_mscount
fn getline
fn getpriority
fn task_policy_set
fn mach_port_get_service_port_info
fn posix_memalign
fn fclose
fn _longjmp
fn sigwait
fn readdir
fn write
fn vm_write
fn bind
fn tolower
fn tcflow
fn clock_set_attributes
fn basename
fn mach_port_extract_right
fn __istype
fn _kernelrpc_mach_port_deallocate_trap
fn task_register_dyld_set_dyld_state
fn towlower
fn getgrent
fn task_set_phys_footprint_limit
fn task_get_mach_voucher
fn mach_port_space_basic_info
fn host_get_multiuser_config_flags
fn NSSymbolReferenceNameInObjectFileImage
fn NSUnLinkModule
fn semaphore_timedwait
fn _OSSwapInt64
fn tcflush
fn getenv
fn NXSwapLittleShortToHost
fn mach_port_swap_guard
fn slot_name
fn NXSwapBigShortToHost
fn getprotobyname
fn clonefileat
fn utime
fn NSIsSymbolNameDefinedWithHint
fn memset
fn mig_dealloc_reply_port
fn macx_swapoff
fn host_set_multiuser_config_flags
fn flistxattr
fn mbsinit
fn task_threads
fn fopen
fn _OSSwapInt32
fn freelocale
fn mrand48
fn posix_openpt
fn fchmod
fn mig_strncpy
fn truncate
fn host_processor_set_priv
fn semaphore_signal
fn putc
fn task_create
fn wcpcpy
fn tmpfile
fn task_swap_exception_ports
fn connect
fn wcscat
fn processor_get_assignment
fn thread_adopt_exception_handler
fn thread_abort
fn _dyld_get_image_header
fn _tlv_bootstrap
fn NSLookupAndBindSymbolWithHint
fn NSAddImage
fn getgrgid_r
fn pthread_setconcurrency
fn mach_task_is_self
fn voucher_mach_msg_clear
fn voucher_mach_msg_adopt
fn clonefile
fn vm_deallocate
fn thread_set_mach_voucher
fn vfwprintf
fn psignal
fn _OSWriteSwapInt16
fn voucher_mach_msg_revert
fn setreuid
fn isspace
fn __darwin_check_fd_set
fn mach_port_names
fn wcstoimax
fn sigignore
fn strcasecmp
fn getlogin_r
fn ___runetype
fn seteuid
fn putchar_unlocked
fn getpwuid_r
fn opendir
fn execv
fn task_assign_default
fn ffs
fn task_name_for_pid
fn processor_control
fn setuid
fn processor_set_stack_usage
fn tcdrain
fn macx_backing_store_recovery
fn setpwent
fn close
fn sem_trywait
fn host_kernel_version
fn atoll
fn task_set_emulation
fn fsetxattr
fn mkstemp
fn sched_yield
fn isprint
fn timespec_get
fn fmemopen
fn nice
fn tcgetattr
fn a64l
fn NSCreateObjectFileImageFromFile
fn sigaction
fn getsubopt
fn shm_unlink
fn gmtime_r
fn strptime
fn tzset
fn telldir
fn getpwent
fn __toupper
fn endpwent
fn __darwin_fd_set
fn isxdigit
fn clearerr
fn regcomp
fn iswupper
fn wcsspn
fn posix_spawn_file_actions_addopen
fn posix_spawn_file_actions_adddup2
fn puts
fn __darwin_fd_clr
fn atoi
fn ttyname_r
fn getuid
fn gethostbyname
fn mlockall
fn semop
fn _OSReadSwapInt32
fn inet_addr
fn sem_destroy
fn cfgetispeed
fn host_get_boot_info
fn setrlimit
fn fchown
fn _OSReadSwapInt16
fn processor_info
fn setgrent
fn processor_set_destroy
fn mach_port_get_set_status
fn mach_port_dnrequest_info
fn link
fn lchown
fn task_register_dyld_get_process_state
fn _kernelrpc_mach_vm_allocate_trap
fn towctrans
fn processor_set_create
fn act_set_state
fn swtch_pri
fn task_purgable_info
fn regerror
fn msgrcv
fn semaphore_timedwait_signal
fn thread_assign
fn open_wmemstream
fn fpathconf
fn thread_assign_default
fn ctime
fn feupdateenv
fn strdup
fn getdate
fn endnetent
fn host_create_mach_voucher_trap
fn __assert_rtn
fn mach_host_self
fn isblank
fn fgetwc
fn sync
fn host_request_notification
fn NXSwapInt
fn getsid
fn chown
fn thread_policy
fn fsetpos
fn div
fn NSAddLibrary
fn setsid
fn uname
fn realpath
fn getnameinfo
fn _kernelrpc_mach_port_insert_right_trap
fn mach_vm_reclaim_update_kernel_accounting_trap
fn _dyld_get_image_vmaddr_slide
fn vfwscanf
fn _dyld_bind_fully_image_containing_address
fn remque
fn mig_strncpy_zerofill
fn NSInstallLinkEditErrorHandlers
fn atomic_flag_clear_explicit
fn wcsxfrm
fn fgetws
fn fnmatch
fn task_register_dyld_shared_cache_image_info
fn iswalpha
fn strchr
fn listen
fn clock_settime
fn task_self_trap
fn seed48
fn feholdexcept
fn hsearch
fn act_get_state
fn posix_spawnattr_setsigmask
fn mach_memory_info
fn _dyld_present
fn wcpncpy
fn getpeername
fn if_freenameindex
fn iswideogram
fn raise
fn getwc
fn getservbyname
fn mmap
fn posix_madvise
fn task_map_kcdata_object_64
fn clock_sleep
fn funlockfile
fn flockfile
fn task_zone_info
fn task_get_exc_guard_behavior
fn futimens
fn pselect
fn isatty
fn strtoll
fn cfsetispeed
fn host_set_exception_ports
fn siginterrupt
fn task_test_async_upcall_propagation
fn _host_page_size
fn wcscpy
fn strcspn
fn fchmodat
fn getopt
fn wctype
fn sigrelse
fn mig_allocate
fn pclose
fn strcat
fn fegetround
fn getnetbyaddr
fn recvmsg
fn access
fn chdir
fn wctomb
fn strlen
fn vm_wire
fn l64a
fn strtok
fn vm_map
fn mach_ports_register
fn vm_region_64
fn iswxdigit
fn wcsrchr
fn shmget
fn fseek
fn freopen
fn ispunct
fn _OSWriteInt16
fn mach_make_memory_entry
fn lstat
fn system
fn setegid
fn rand_r
fn memchr
fn pthread_key_delete
fn ctime_r
fn confstr
fn kmod_destroy
fn host_security_create_task_token
fn fgetpos
fn thread_set_exception_ports
fn thread_swap_exception_ports
fn task_set_state
fn aio_cancel
fn socket
fn vm_map_page_query
fn _kernelrpc_mach_vm_deallocate_trap
fn mach_zone_info
fn NXSwapDouble
fn NSVersionOfRunTimeLibrary
fn NSIsSymbolNameDefined
fn iconv
fn _dyld_all_twolevel_modules_prebound
fn host_set_atm_diagnostic_flag
fn macx_triggers
fn closelog
fn NSCreateObjectFileImageFromMemory
fn host_processors
fn _kernelrpc_mach_port_mod_refs_trap
fn insque
fn _dyld_lookup_and_bind_fully
fn sigdelset
fn regexec
fn thread_get_assignment
fn strncmp
fn mach_port_unguard
fn NXSwapHostLongLongToBig
fn host_get_UNDServer
fn mach_port_allocate
fn imaxabs
fn putwchar
fn setprotoent
fn setservent
fn gethostname
fn ungetwc
fn atomic_thread_fence
fn renameat
fn __math_errhandling
fn setpgid
fn host_reboot
fn semaphore_signal_thread
fn setitimer
fn sigpending
fn calloc
fn munlock
fn host_processor_sets
fn task_get_exception_ports_info
fn ptsname
fn wcstombs
fn fwide
fn wcswidth
fn shmctl
fn task_test_sync_upcall
fn _kernelrpc_mach_port_guard_trap
fn NXHostByteOrder
fn NSVersionOfLinkTimeLibrary
fn NSNameOfSymbol
fn mach_port_allocate_name
fn _kernelrpc_mach_vm_purgable_control_trap
fn __sigbits
fn shmdt
fn gettimeofday
fn getc_unlocked
fn vm_read_list
fn host_register_well_known_mach_voucher_attr_manager
fn ftrylockfile
fn fflush
fn mach_port_set_context
fn wcsncat
fn dirfd
fn task_identity_token_get_task_port
fn mach_memory_object_memory_entry_64
fn mkfifo
fn readdir_r
fn etap_trace_thread
fn munmap
fn tcsendbreak
fn perror
fn gethostid
fn mach_vm_region_info_64
fn _dyld_get_image_name
fn task_set_special_port
fn feclearexcept
fn pid_for_task
fn islower
fn wcstok
fn mach_ports_lookup
fn atomic_flag_test_and_set
fn fork
fn getrusage
fn mktemp
fn getchar
fn vwscanf
fn posix_spawnattr_destroy
fn dup
fn utimensat
fn statvfs
fn vm_stats
fn memccpy
fn strstr
fn sysconf
fn isascii
fn task_policy
fn vm_region_recurse
fn task_set_info
fn main
  bb0 bb0
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.print)
    intrinsic.call symbol(intrinsic.print)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.print)
    intrinsic.call symbol(intrinsic.print)
    intrinsic.call symbol(intrinsic.print)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.print)
    intrinsic.call symbol(intrinsic.println)
    alloca Virtual { id: 14, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 14, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 7
    load Virtual { id: 16, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 14, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 16, bank: General, size_bits: 64 }
    intrinsic.call symbol(intrinsic.println), 2, 3, 5
    intrinsic.call symbol(intrinsic.println), 4614256650576692846
    intrinsic.call symbol(intrinsic.println), 97, 90
    intrinsic.call symbol(intrinsic.println), 1, 2
    intrinsic.call symbol(intrinsic.println), 1, 0
    intrinsic.call symbol(intrinsic.print)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.print)
    intrinsic.call symbol(intrinsic.print)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.print)
    intrinsic.call symbol(intrinsic.print)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println), symbol(__const_data_0), symbol(__const_data_1)
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
  offset=0x00000068 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000074 kind=CallRel32 symbol=printf addend=0
  offset=0x00000078 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000084 kind=CallRel32 symbol=printf addend=0
  offset=0x00000088 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000094 kind=CallRel32 symbol=printf addend=0
  offset=0x00000098 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000000a4 kind=CallRel32 symbol=printf addend=0
  offset=0x000000a8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000000b4 kind=CallRel32 symbol=printf addend=0
  offset=0x000000b8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000000c4 kind=CallRel32 symbol=printf addend=0
  offset=0x000000c8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000000d4 kind=CallRel32 symbol=printf addend=0
  offset=0x000000d8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000000e4 kind=CallRel32 symbol=printf addend=0
  offset=0x000000e8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000000f4 kind=CallRel32 symbol=printf addend=0
  offset=0x0000011c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000134 kind=CallRel32 symbol=printf addend=0
  offset=0x00000138 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000168 kind=CallRel32 symbol=printf addend=0
  offset=0x0000016c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000001a4 kind=CallRel32 symbol=printf addend=0
  offset=0x000001a8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000001cc kind=CallRel32 symbol=printf addend=0
  offset=0x000001d0 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000001f4 kind=CallRel32 symbol=printf addend=0
  offset=0x000001f8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000021c kind=CallRel32 symbol=printf addend=0
  offset=0x00000220 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000022c kind=CallRel32 symbol=printf addend=0
  offset=0x00000230 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000023c kind=CallRel32 symbol=printf addend=0
  offset=0x00000240 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000024c kind=CallRel32 symbol=printf addend=0
  offset=0x00000250 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000025c kind=CallRel32 symbol=printf addend=0
  offset=0x00000260 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000026c kind=CallRel32 symbol=printf addend=0
  offset=0x00000270 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000027c kind=CallRel32 symbol=printf addend=0
  offset=0x00000280 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000028c kind=CallRel32 symbol=printf addend=0
  offset=0x00000290 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000029c kind=CallRel32 symbol=printf addend=0
  offset=0x000002a0 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000002ac kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x000002b4 kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x000002c0 kind=Aarch64AdrpAdd symbol=__const_data_1 addend=0
  offset=0x000002c8 kind=Aarch64AdrpAdd symbol=__const_data_1 addend=0
  offset=0x000002d4 kind=CallRel32 symbol=printf addend=0

.text (756 bytes):
  00000000  ff 03 09 d1 f0 03 00 91  10 c2 08 91 1d 7a 00 a9 
  00000010  fd 03 00 91 1f 20 03 d5  00 00 00 90 00 00 00 91 
  00000020  00 60 00 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000030  00 00 01 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000040  00 a0 02 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000050  00 60 03 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000060  00 00 04 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000070  00 20 04 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000080  00 40 04 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000090  00 00 04 91 00 00 00 94  00 00 00 90 00 00 00 91 
  000000a0  00 a0 04 91 00 00 00 94  00 00 00 90 00 00 00 91 
  000000b0  00 c0 04 91 00 00 00 94  00 00 00 90 00 00 00 91 
  000000c0  00 00 05 91 00 00 00 94  00 00 00 90 00 00 00 91 
  000000d0  00 00 04 91 00 00 00 94  00 00 00 90 00 00 00 91 
  000000e0  00 20 05 91 00 00 00 94  00 00 00 90 00 00 00 91 
  000000f0  00 00 04 91 00 00 00 94  f0 03 00 91 10 82 08 91 
  00000100  f0 4b 00 f9 f1 4b 40 f9  f0 00 80 d2 30 02 00 f9 
  00000110  f0 4b 40 f9 11 02 40 f9  f1 53 00 f9 00 00 00 90 
  00000120  00 00 00 91 00 60 05 91  e1 53 40 f9 f0 53 40 f9 
  00000130  f0 03 00 f9 00 00 00 94  00 00 00 90 00 00 00 91 
  00000140  00 a0 05 91 41 00 80 d2  50 00 80 d2 f0 03 00 f9 
  00000150  62 00 80 d2 70 00 80 d2  f0 07 00 f9 a3 00 80 d2 
  00000160  b0 00 80 d2 f0 0b 00 f9  00 00 00 94 00 00 00 90 
  00000170  00 00 00 91 00 20 06 91  d0 cd 90 d2 70 03 be f2 
  00000180  30 3f c4 f2 30 01 e8 f2  00 02 67 9e d0 cd 90 d2 
  00000190  70 03 be f2 30 3f c4 f2  30 01 e8 f2 00 02 67 9e 
  000001a0  e0 03 00 fd 00 00 00 94  00 00 00 90 00 00 00 91 
  000001b0  00 60 06 91 21 0c 80 d2  30 0c 80 d2 f0 03 00 f9 
  000001c0  42 0b 80 d2 50 0b 80 d2  f0 07 00 f9 00 00 00 94 
  000001d0  00 00 00 90 00 00 00 91  00 a0 06 91 21 00 80 d2 
  000001e0  30 00 80 d2 f0 03 00 f9  42 00 80 d2 50 00 80 d2 
  000001f0  f0 07 00 f9 00 00 00 94  00 00 00 90 00 00 00 91 
  00000200  00 00 07 91 21 00 80 d2  30 00 80 d2 f0 03 00 f9 
  00000210  02 00 80 d2 10 00 80 d2  f0 07 00 f9 00 00 00 94 
  00000220  00 00 00 90 00 00 00 91  00 40 07 91 00 00 00 94 
  00000230  00 00 00 90 00 00 00 91  00 00 04 91 00 00 00 94 
  00000240  00 00 00 90 00 00 00 91  00 60 07 91 00 00 00 94 
  00000250  00 00 00 90 00 00 00 91  00 e0 07 91 00 00 00 94 
  00000260  00 00 00 90 00 00 00 91  00 00 04 91 00 00 00 94 
  00000270  00 00 00 90 00 00 00 91  00 40 08 91 00 00 00 94 
  00000280  00 00 00 90 00 00 00 91  00 60 08 91 00 00 00 94 
  00000290  00 00 00 90 00 00 00 91  00 00 04 91 00 00 00 94 
  000002a0  00 00 00 90 00 00 00 91  00 80 08 91 01 00 00 90 
  000002b0  21 00 00 91 10 00 00 90  10 02 00 91 f0 03 00 f9 
  000002c0  02 00 00 90 42 00 00 91  10 00 00 90 10 02 00 91 
  000002d0  f0 07 00 f9 00 00 00 94  bf 03 00 91 f0 03 00 91 
  000002e0  10 c2 08 91 1d 7a 40 a9  ff 03 09 91 00 00 80 d2 
  000002f0  c0 03 5f d6 

.rodata (560 bytes):
  00000000  6c 69 6e 65 31 0a 6c 69  6e 65 32 00 74 61 62 09 
  00000010  65 6e 64 00 00 00 00 00  f0 9f 93 98 20 54 75 74 
  00000020  6f 72 69 61 6c 3a 20 31  30 5f 70 72 69 6e 74 5f 
  00000030  73 68 6f 77 63 61 73 65  2e 66 70 0a 00 00 00 00 
  00000040  f0 9f a7 ad 20 46 6f 63  75 73 3a 20 43 6f 6d 70 
  00000050  72 65 68 65 6e 73 69 76  65 20 70 72 69 6e 74 6c 
  00000060  6e 21 2f 70 72 69 6e 74  20 73 68 6f 77 63 61 73 
  00000070  65 20 63 6f 76 65 72 69  6e 67 20 76 61 72 69 61 
  00000080  64 69 63 20 61 72 67 75  6d 65 6e 74 73 20 61 6e 
  00000090  64 20 72 75 6e 74 69 6d  65 20 66 6f 72 6d 61 74 
  000000a0  74 69 6e 67 0a 00 00 00  f0 9f a7 aa 20 57 68 61 
  000000b0  74 20 74 6f 20 6c 6f 6f  6b 20 66 6f 72 3a 20 6c 
  000000c0  61 62 65 6c 65 64 20 6f  75 74 70 75 74 73 20 62 
  000000d0  65 6c 6f 77 0a 00 00 00  e2 9c 85 20 45 78 70 65 
  000000e0  63 74 61 74 69 6f 6e 3a  20 6f 75 74 70 75 74 73 
  000000f0  20 6d 61 74 63 68 20 6c  61 62 65 6c 73 0a 00 00 
  00000100  0a 00 00 00 00 00 00 00  48 65 6c 6c 6f 00 00 00 
  00000110  57 6f 72 6c 64 20 77 69  74 68 20 6e 65 77 6c 69 
  00000120  6e 65 73 00 00 00 00 00  4e 75 6d 62 65 72 3a 00 
  00000130  42 6f 6f 6c 65 61 6e 3a  00 00 00 00 00 00 00 00 
  00000140  4d 69 78 65 64 3a 00 00  4e 61 6d 65 73 70 61 63 
  00000150  65 20 74 65 73 74 00 00  76 61 6c 75 65 20 3d 20 
  00000160  25 6c 6c 64 0a 00 00 00  6d 61 74 68 3a 20 25 6c 
  00000170  6c 64 20 2b 20 25 6c 6c  64 20 3d 20 25 6c 6c 64 
  00000180  0a 00 00 00 00 00 00 00  66 6c 6f 61 74 3a 20 25 
  00000190  66 0a 00 00 00 00 00 00  63 68 61 72 73 3a 20 25 
  000001a0  64 20 25 64 0a 00 00 00  74 75 70 6c 65 3a 20 28 
  000001b0  25 6c 6c 64 2c 20 25 6c  6c 64 29 0a 00 00 00 00 
  000001c0  62 6f 6f 6c 73 3a 20 25  64 20 25 64 0a 00 00 00 
  000001d0  54 68 69 73 00 00 00 00  43 6f 6e 74 69 6e 75 69 
  000001e0  6e 67 20 77 69 74 68 6f  75 74 20 6e 65 77 6c 69 
  000001f0  6e 65 00 00 00 00 00 00  20 2d 20 61 70 70 65 6e 
  00000200  64 65 64 20 63 6f 6e 74  65 6e 74 00 00 00 00 00 
  00000210  55 6e 69 74 3a 00 00 00  4e 75 6c 6c 3a 00 00 00 
  00000220  65 73 63 61 70 65 64 3a  20 25 73 20 25 73 0a 00 
