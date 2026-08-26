fp-native dump: format=MachO arch=Aarch64 entry=0x3cc

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
global FACTORIAL_CONST ty=I64 constant=true initializer=Some(Bytes([120, 0, 0, 0, 0, 0, 0, 0]))
fn processor_set_default
fn sigemptyset
fn msgget
fn fegetround
fn sendmsg
fn __math_errhandling
fn setstate
fn NSLookupAndBindSymbol
fn ttyname
fn pthread_getconcurrency
fn getpwnam
fn thread_abort
fn strptime
fn iswspace
fn processor_get_assignment
fn processor_set_tasks
fn thread_swap_exception_ports
fn recvfrom
fn munmap
fn __sigbits
fn regerror
fn _kernelrpc_mach_port_allocate_trap
fn wcscspn
fn abs
fn lldiv
fn towlower
fn __srget
fn symlink
fn vm_remap
fn host_get_clock_service
fn feholdexcept
fn mach_port_request_notification
fn NXSwapHostLongToLittle
fn open_memstream
fn sem_post
fn sched_yield
fn times
fn vm_allocate
fn lock_set_create
fn gettimeofday
fn task_test_sync_upcall
fn lrand48
fn encrypt
fn NSSymbolReferenceCountInObjectFileImage
fn thread_set_mach_voucher
fn mach_port_type
fn NXSwapLong
fn strspn
fn ___tolower
fn utimensat
fn thread_abort_safely
fn thread_get_exception_ports_info
fn _host_page_size
fn getxattr
fn task_get_state
fn vm_read_overwrite
fn puts
fn mach_port_set_context
fn endservent
fn fchmod
fn utime
fn iswupper
fn _kernelrpc_mach_port_insert_member_trap
fn wcsstr
fn task_suspend2
fn NXSwapInt
fn _kernelrpc_mach_vm_map_trap
fn NSSymbolDefinitionCountInObjectFileImage
fn pselect
fn NSSymbolDefinitionNameInObjectFileImage
fn task_threads
fn _kernelrpc_mach_vm_purgable_control_trap
fn thread_assign_default
fn getuid
fn fsetpos
fn llabs
fn lchown
fn semaphore_create
fn getc
fn thread_get_assignment
fn _kernelrpc_mach_port_guard_trap
fn host_set_multiuser_config_flags
fn mach_zone_info_for_zone
fn mktime
fn wcsftime
fn _OSReadSwapInt16
fn rand_r
fn wcsdup
fn pwrite
fn mach_port_names
fn shutdown
fn iswalpha
fn NXSwapHostLongLongToBig
fn NSSymbolReferenceNameInObjectFileImage
fn shmdt
fn mbsrtowcs
fn thread_info
fn lock_set_destroy
fn task_policy_set
fn utimes
fn task_set_mach_voucher
fn fegetenv
fn vm_protect
fn pid_for_task
fn getentropy
fn wcpcpy
fn usleep
fn mach_port_set_mscount
fn regexec
fn thread_get_exception_ports
fn panic_init
fn slot_name
fn mach_msg_receive
fn aio_fsync
fn processor_control
fn _kernelrpc_mach_port_insert_right_trap
fn mach_port_allocate_name
fn getsockopt
fn getaddrinfo
fn _dyld_image_containing_address
fn mktemp
fn asctime_r
fn dirfd
fn task_set_emulation
fn execve
fn mach_port_destruct
fn killpg
fn mlockall
fn putchar
fn fputwc
fn pthread_key_delete
fn task_register_hardened_exception_handler
fn wmemcpy
fn thread_policy_set
fn memcmp
fn srandom
fn clock
fn task_set_ras_pc
fn _kernelrpc_mach_port_get_attributes_trap
fn task_name_for_pid
fn wcslen
fn remque
fn setgid
fn voucher_mach_msg_revert
fn gai_strerror
fn dirname
fn geteuid
fn setitimer
fn _dyld_image_count
fn task_get_assignment
fn mach_port_allocate_full
fn task_create
fn sigsetjmp
fn getpgid
fn getc_unlocked
fn vprintf
fn wcstombs
fn mmap
fn NSLookupAndBindSymbolWithHint
fn mach_ports_register
fn sigfillset
fn memchr
fn globfree
fn thread_get_state
fn vm_inherit
fn task_info
fn vm_region_64
fn processor_exit
fn task_get_exc_guard_behavior
fn ferror
fn mach_port_is_connection_for_service
fn wcsncasecmp
fn longjmp
fn tcdrain
fn NSDestroyObjectFileImage
fn kevent
fn iscntrl
fn mbsnrtowcs
fn posix_openpt
fn mprotect
fn mach_port_space_info
fn getpgrp
fn _setjmp
fn getpriority
fn mbstowcs
fn getgroups
fn clonefile
fn unlockpt
fn write
fn vwscanf
fn getline
fn mach_port_get_service_port_info
fn NSUnLinkModule
fn realpath
fn mig_get_reply_port
fn host_register_mach_voucher_attr_manager
fn setsid
fn setuid
fn mig_allocate
fn processor_set_info
fn task_get_special_port
fn mach_vm_wire
fn task_register_dyld_get_process_state
fn system
fn crypt
fn ftok
fn getitimer
fn processor_set_statistics
fn task_set_exception_ports
fn sighold
fn vm_deallocate
fn readlink
fn __NDR_convert__mig_reply_error_t
fn telldir
fn toascii
fn mig_put_reply_port
fn confstr
fn _OSSwapInt16
fn basename
fn removexattr
fn free
fn task_register_dyld_image_infos
fn ttyname_r
fn getprotobynumber
fn mkdir
fn vm_allocate_cpm
fn wctomb
fn getnetbyaddr
fn getservbyname
fn mach_memory_info
fn _kernelrpc_mach_port_move_member_trap
fn mach_port_get_srights
fn task_set_corpse_forking_behavior
fn strncasecmp
fn mach_port_kernel_object
fn fdopen
fn sockatmark
fn gmtime
fn isblank
fn grantpt
fn mkstemp
fn link
fn calloc
fn imaxdiv
fn strtoull
fn getprotobyname
fn swab
fn __swbuf
fn fileno
fn hcreate
fn task_policy_get
fn task_set_emulation_vector
fn vm_map_page_query
fn _kernelrpc_mach_port_type_trap
fn sethostent
fn NXSwapHostIntToBig
fn _dyld_get_image_header
fn task_swap_exception_ports
fn inet_ntop
fn mach_error
fn NSIsSymbolNameDefined
fn sigpause
fn fstatvfs
fn feraiseexcept
fn thread_depress_abort
fn task_dyld_process_info_notify_get
fn listen
fn memccpy
fn wmemchr
fn gets
fn strxfrm
fn if_indextoname
fn atoll
fn unlinkat
fn gmtime_r
fn tcflush
fn getsockname
fn posix_spawn_file_actions_init
fn stat
fn mblen
fn fgetc
fn host_get_clock_control
fn putc_unlocked
fn NXSwapBigLongToHost
fn _dyld_present
fn processor_set_policy_disable
fn _OSWriteSwapInt64
fn host_processor_set_priv
fn mach_ports_lookup
fn inet_ntoa
fn setgrfile
fn kmod_destroy
fn thread_convert_thread_state
fn mach_vm_region_info_64
fn vm_remap_new
fn clock_settime
fn mach_port_kobject_description
fn creat
fn mach_host_self
fn host_virtual_physical_table_info
fn NXSwapHostLongLongToLittle
fn setgroupent
fn host_processors
fn _dyld_all_twolevel_modules_prebound
fn fgetwc
fn readdir_r
fn vm_wire
fn task_get_dyld_image_infos
fn funlockfile
fn flockfile
fn host_set_special_port
fn putwc
fn kill
fn random
fn processor_set_policy_enable
fn macx_backing_store_suspend
fn setlocale
fn iswspecial
fn msgrcv
fn task_test_async_upcall_propagation
fn NSModuleForSymbol
fn fsync
fn sigaltstack
fn ptsname
fn ffs
fn mig_dealloc_reply_port
fn getprotoent
fn mach_port_mod_refs
fn ftruncate
fn strcat
fn flistxattr
fn fmemopen
fn host_processor_info
fn pthread_sigmask
fn wcsncmp
fn pipe
fn _kernelrpc_mach_port_mod_refs_trap
fn host_statistics
fn posix_madvise
fn NSLibraryNameForModule
fn strncpy
fn chown
fn unsetenv
fn isatty
fn __toupper
fn _OSReadInt16
fn sigpending
fn div
fn fnmatch
fn posix_spawnattr_setsigmask
fn setgrent
fn pause
fn task_suspend
fn thread_set_special_port
fn task_assign_default
fn act_get_state
fn macx_triggers
fn NSAddLibrary
fn wcspbrk
fn pthread_testcancel
fn listxattr
fn mach_msg_overwrite
fn sigismember
fn atomic_flag_clear_explicit
fn strlen
fn asctime
fn setkey
fn waitpid
fn aio_suspend
fn fflush
fn __darwin_check_fd_set_overflow
fn wcrtomb
fn abort
fn hsearch
fn getpwnam_r
fn fchdir
fn fchmodat
fn isalnum
fn wcstoull
fn alphasort
fn mkdirat
fn _OSWriteSwapInt32
fn closelog
fn vm_read_list
fn ldiv
fn clock_getres
fn if_nametoindex
fn sync
fn __darwin_fd_clr
fn raise
fn processor_set_threads
fn task_resume
fn mach_make_memory_entry
fn semget
fn host_set_UNDServer
fn host_priv_statistics
fn _kernelrpc_mach_vm_protect_trap
fn task_set_special_port
fn _kernelrpc_mach_port_extract_member_trap
fn _exit
fn islower
fn wmemcmp
fn endpwent
fn NXSwapLittleLongLongToHost
fn wcstoll
fn wctype
fn chmod
fn mach_port_set_attributes
fn kqueue
fn tempnam
fn fegetexceptflag
fn task_set_exc_guard_behavior
fn atomic_signal_fence
fn iswctype
fn siglongjmp
fn sem_init
fn rand
fn stpncpy
fn cfgetispeed
fn getegid
fn getwchar
fn getservbyport
fn clock_gettime
fn setpgrp
fn munlockall
fn shmctl
fn strrchr
fn thread_wire
fn tcsetattr
fn host_get_UNDServer
fn task_register_dyld_set_dyld_state
fn fstatat
fn vm_mapped_pages_info
fn atomic_flag_test_and_set_explicit
fn jrand48
fn getgrgid
fn strcmp
fn __vsprintf_chk
fn iswphonogram
fn vm_purgable_control
fn mach_port_assert_attributes
fn swtch
fn NXSwapLittleShortToHost
fn NSVersionOfRunTimeLibrary
fn fetestexcept
fn NSNameOfModule
fn getopt
fn mach_port_construct
fn thread_switch
fn NSVersionOfLinkTimeLibrary
fn fputs
fn getlogin_r
fn sigignore
fn socket
fn chdir
fn towupper
fn mach_zone_info
fn sigaction
fn if_nameindex
fn isascii
fn strpbrk
fn wcstoumax
fn mach_port_allocate
fn iswpunct
fn dlopen
fn atomic_flag_clear
fn munlock
fn NSCreateObjectFileImageFromMemory
fn _dyld_launched_prebound
fn NSAddressOfSymbol
fn wcscasecmp
fn sched_get_priority_max
fn linkat
fn semop
fn strtoll
fn host_default_memory_manager
fn inet_pton
fn strtoimax
fn iswgraph
fn rmdir
fn feupdateenv
fn fread
fn mach_msg
fn host_reboot
fn mach_port_unguard
fn ispunct
fn _kernelrpc_mach_port_destruct_trap
fn getgrnam
fn posix_spawn_file_actions_addfchdir
fn task_dyld_process_info_notify_register
fn host_lockgroup_info
fn gethostid
fn host_check_multiuser_mode
fn pthread_kill
fn wcsncpy
fn __svfscanf
fn NXSwapFloat
fn NSLinkEditError
fn posix_memalign
fn wctrans
fn aio_cancel
fn _kernelrpc_mach_vm_allocate_trap
fn wcstoimax
fn uselocale
fn quick_exit
fn time
fn sendto
fn getlogin
fn host_security_create_task_token
fn semaphore_signal
fn posix_spawn_file_actions_addclose
fn wcsxfrm
fn task_policy
fn mach_port_guard
fn regcomp
fn freeaddrinfo
fn close
fn fgets
fn iswlower
fn wcsncat
fn mach_port_get_set_status
fn if_freenameindex
fn task_purgable_info
fn host_get_multiuser_config_flags
fn tolower
fn vm_map
fn ctime
fn wcsspn
fn task_generate_corpse
fn sem_close
fn posix_spawn
fn vdprintf
fn posix_spawn_file_actions_addchdir
fn fchownat
fn setregid
fn __assert_rtn
fn getgid
fn __darwin_fd_set
fn wmemset
fn thread_set_state
fn mach_port_insert_right
fn posix_spawn_file_actions_adddup2
fn _kernelrpc_mach_vm_deallocate_trap
fn mach_voucher_deallocate
fn tzset
fn recv
fn _kernelrpc_mach_port_construct_trap
fn fork
fn mkfifo
fn mkfifoat
fn mach_port_swap_guard
fn vswscanf
fn task_dyld_process_info_notify_deregister
fn mach_msg_destroy
fn _dyld_get_image_name
fn vsscanf
fn thread_suspend
fn iswxdigit
fn vm_copy
fn fclonefileat
fn socketpair
fn _dyld_lookup_and_bind_with_hint
fn host_set_exception_ports
fn labs
fn thread_resume
fn malloc
fn putc
fn wcswidth
fn shmget
fn _dyld_lookup_and_bind_fully
fn siginterrupt
fn iswblank
fn wctob
fn lockf
fn semaphore_timedwait_signal
fn task_sample
fn wcsrtombs
fn vm_write
fn msgctl
fn swtch_pri
fn task_set_state
fn NSIsSymbolDefinedInObjectFileImage
fn NSInstallLinkEditErrorHandlers
fn sleep
fn aio_read
fn fopen
fn vsnprintf
fn vfwprintf
fn wcschr
fn mach_port_rename
fn kmod_get_info
fn getservent
fn _OSReadInt32
fn semaphore_signal_all
fn macx_swapon
fn strncat
fn mach_memory_object_memory_entry_64
fn setpwent
fn NXSwapDouble
fn host_get_io_main
fn atoi
fn mach_port_deallocate
fn posix_spawnp
fn NXHostByteOrder
fn __isctype
fn getpwuid
fn fstat
fn fwrite
fn setvbuf
fn NXSwapHostLongToBig
fn wcpncpy
fn openlog
fn task_get_emulation_vector
fn macx_backing_store_recovery
fn mach_error_type
fn NXSwapBigIntToHost
fn iconv_open
fn NSIsSymbolNameDefinedWithHint
fn nl_langinfo
fn gethostbyaddr
fn setprotoent
fn task_inspect
fn aio_error
fn posix_spawn_file_actions_destroy
fn recvmsg
fn getchar
fn cfsetospeed
fn sysconf
fn isdigit
fn localtime_r
fn _OSReadSwapInt64
fn mig_strncpy_zerofill
fn kmod_control
fn getsid
fn endgrent
fn semaphore_destroy
fn vswprintf
fn dlclose
fn host_swap_exception_ports
fn seed48
fn pclose
fn thread_terminate
fn wcscoll
fn thread_get_special_port
fn ___runetype
fn fseeko
fn etap_trace_thread
fn vm_read
fn aligned_alloc
fn mach_port_set_seqno
fn host_page_size
fn NXSwapLittleLongToHost
fn mach_port_peek
fn setbuf
fn strtol
fn NSLinkModule
fn task_map_corpse_info_64
fn task_identity_token_get_task_port
fn strftime
fn fgetws
fn clock_set_res
fn fclose
fn getpwuid_r
fn strtoumax
fn posix_spawnattr_getsigdefault
fn _kernelrpc_mach_port_deallocate_trap
fn futimens
fn host_kernel_version
fn thread_policy_get
fn strcspn
fn _dyld_bind_fully_image_containing_address
fn ungetwc
fn getnetent
fn a64l
fn mach_port_get_refs
fn regfree
fn _OSSwapInt64
fn freopen
fn clock_set_time
fn mach_port_dnrequest_info
fn voucher_mach_msg_set
fn _OSWriteInt32
fn strtoul
fn vm_machine_attribute
fn wait
fn mach_port_kobject
fn processor_info
fn inet_addr
fn thread_adopt_exception_handler
fn atomic_flag_test_and_set
fn fgetpos
fn ftell
fn setpriority
fn getgrnam_r
fn isxdigit
fn setreuid
fn _dyld_shared_cache_contains_path
fn seekdir
fn hdestroy
fn sem_destroy
fn send
fn processor_set_destroy
fn NSGetSectionDataInObjectFileImage
fn wcsrchr
fn _dyld_lookup_and_bind
fn task_get_mach_voucher
fn fgetxattr
fn wcsnlen
fn vm_region_recurse_64
fn task_wire
fn mbrlen
fn clock_sleep
fn debug_control_port_for_pid
fn fesetround
fn rewinddir
fn sched_get_priority_min
fn host_statistics64
fn _OSReadInt64
fn task_register_dyld_shared_cache_image_info
fn _dyld_get_image_vmaddr_slide
fn NSLookupSymbolInModule
fn uname
fn strdup
fn isspace
fn host_processor_sets
fn _OSWriteInt64
fn mach_port_get_context
fn setnetent
fn setpgid
fn __maskrune
fn feof
fn fputws
fn vwprintf
fn localtime
fn lseek
fn umask
fn mach_port_insert_member
fn putchar_unlocked
fn iconv
fn fputc
fn host_info
fn dlerror
fn read
fn __vsnprintf_chk
fn task_swap_mach_voucher
fn _tlv_bootstrap
fn fesetexceptflag
fn wcscmp
fn readdir
fn task_set_policy
fn getppid
fn iswideogram
fn ualarm
fn nice
fn iswcntrl
fn getgrent
fn exit
fn alarm
fn getrusage
fn l64a
fn mach_memory_object_memory_entry
fn sem_getvalue
fn lio_listio
fn insque
fn clock_get_res
fn _NSGetExecutablePath
fn sigprocmask
fn sem_unlink
fn setegid
fn gethostname
fn task_unregister_dyld_image_infos
fn task_for_pid
fn popen
fn towctrans
fn vm_msync
fn msgsnd
fn thread_policy
fn task_set_port_space
fn host_get_atm_diagnostic_flag
fn cfgetospeed
fn initstate
fn isalpha
fn iswalnum
fn host_set_atm_diagnostic_flag
fn isupper
fn _Exit
fn mig_deallocate
fn thread_sample
fn mach_vm_reclaim_update_kernel_accounting_trap
fn lcong48
fn mrand48
fn perror
fn setenv
fn memmove
fn stpcpy
fn iswdigit
fn iswhexnumber
fn iswrune
fn isprint
fn __error
fn fpathconf
fn tcgetattr
fn wcsnrtombs
fn memset
fn strnlen
fn mlock
fn ungetc
fn select
fn psignal
fn iswnumber
fn mknodat
fn task_map_kcdata_object_64
fn mach_make_memory_entry_64
fn host_create_mach_voucher_trap
fn remove
fn thread_get_mach_voucher
fn vm_region_recurse
fn NXSwapBigShortToHost
fn NXSwapLittleIntToHost
fn tcflow
fn wcscat
fn NSAddLibraryWithSearching
fn msync
fn execv
fn NXSwapHostShortToBig
fn NXSwapShort
fn NSNameOfSymbol
fn mach_port_space_basic_info
fn wcstok
fn clock_set_attributes
fn connect
fn __sputc
fn fchown
fn getenv
fn tcsetpgrp
fn seteuid
fn fwide
fn posix_spawnattr_init
fn symlinkat
fn getwc
fn processor_assign
fn thread_set_exception_ports
fn wcstol
fn vm_behavior_set
fn renameat
fn processor_set_policy_control
fn strcasecmp
fn lstat
fn _kernelrpc_mach_port_unguard_trap
fn mach_msg_send
fn unlink
fn atomic_thread_fence
fn task_self_trap
fn strtok
fn fdopendir
fn host_get_exception_ports
fn accept
fn getpeername
fn mach_thread_self
fn strerror_r
fn posix_spawn_file_actions_addopen
fn setxattr
fn thread_create_running
fn ctermid
fn open_wmemstream
fn endnetent
fn dup2
fn vm_map_64
fn mach_voucher_extract_attr_recipe_trap
fn gethostent
fn toupper
fn isgraph
fn mach_port_get_attributes
fn memcpy
fn _OSWriteInt16
fn gethostbyname
fn faccessat
fn host_security_set_task_token
fn access
fn thread_create
fn kmod_create
fn thread_assign
fn posix_spawnattr_destroy
fn mach_port_destroy
fn macx_swapoff
fn ___toupper
fn mach_port_guard_with_flags
fn host_register_well_known_mach_voucher_attr_manager
fn tcgetpgrp
fn putenv
fn setservent
fn _OSWriteSwapInt16
fn kext_request
fn imaxabs
fn posix_spawnattr_setpgroup
fn strncmp
fn fremovexattr
fn aio_return
fn poll
fn voucher_mach_msg_clear
fn atol
fn nanosleep
fn mbtowc
fn mbrtowc
fn posix_spawnattr_setflags
fn execvp
fn sigdelset
fn mbsinit
fn vscanf
fn task_set_phys_footprint_limit
fn statvfs
fn rewind
fn freelocale
fn tcsendbreak
fn mknod
fn vm_region
fn task_map_corpse_info
fn timespec_get
fn mach_error_string
fn getchar_unlocked
fn sigsuspend
fn OSHostByteOrder
fn mach_port_extract_member
fn wcwidth
fn endprotoent
fn task_set_info
fn clock_sleep_trap
fn NSCreateObjectFileImageFromFile
fn sigrelse
fn mach_port_extract_right
fn duplocale
fn _dyld_get_image_header_containing_address
fn ctime_r
fn clonefileat
fn strerror
fn tmpfile
fn strtok_r
fn iswprint
fn putwchar
fn processor_set_tasks_with_flavor
fn cfsetispeed
fn setsockopt
fn task_get_exception_ports_info
fn processor_set_max_priority
fn readlinkat
fn tcgetsid
fn setrlimit
fn posix_spawnattr_getpgroup
fn task_terminate
fn getnameinfo
fn endhostent
fn getrlimit
fn setjmp
fn kevent64
fn __wcwidth
fn task_assign
fn closedir
fn fsetxattr
fn NXSwapLongLong
fn fseek
fn wcstoul
fn NSIsSymbolNameDefinedInImage
fn srand48
fn strcoll
fn strsignal
fn feclearexcept
fn strstr
fn sem_wait
fn _OSReadSwapInt32
fn posix_spawnattr_setsigdefault
fn opendir
fn vfwscanf
fn posix_spawnattr_getsigmask
fn pread
fn task_get_exception_ports
fn mach_port_allocate_qos
fn iswascii
fn _kernelrpc_mach_port_request_notification_trap
fn voucher_mach_msg_adopt
fn task_zone_info
fn vsprintf
fn srand
fn pathconf
fn realloc
fn btowc
fn strcpy
fn wcscpy
fn getnetbyname
fn mig_strncpy
fn semaphore_signal_thread
fn getcwd
fn NXSwapHostIntToLittle
fn shm_unlink
fn mach_vm_region_info
fn NXSwapHostShortToLittle
fn nrand48
fn dup
fn NSAddImage
fn __istype
fn processor_set_create
fn host_request_notification
fn strchr
fn dlsym
fn host_get_special_port
fn getdate
fn task_resume2
fn __tolower
fn setlogmask
fn getpid
fn vfork
fn mach_task_is_self
fn __darwin_check_fd_set
fn rename
fn localeconv
fn _longjmp
fn waitid
fn _OSSwapInt32
fn pthread_setconcurrency
fn __darwin_fd_isset
fn sigaddset
fn truncate
fn tmpnam
fn processor_start
fn semaphore_wait
fn semaphore_wait_signal
fn act_set_state
fn vm_map_exec_lockdown
fn mach_generate_activity_id
fn host_create_mach_voucher
fn vm_stats
fn processor_set_stack_usage
fn semaphore_timedwait
fn mig_reply_setup
fn fesetenv
fn bind
fn getgrgid_r
fn shmat
fn clearerr
fn newlocale
fn ftrylockfile
fn getsubopt
fn task_create_identity_token
fn wmemmove
fn thread_set_policy
fn host_get_boot_info
fn sigwait
fn ftello
fn posix_spawnattr_getflags
fn vfscanf
fn getdelim
fn iconv_close
fn getpwent
fn NXSwapBigLongLongToHost
fn mach_port_move_member
fn NSLookupSymbolInImage
fn aio_write
fn sem_trywait
fn thread_swap_mach_voucher
fn vfprintf
fn strndup
fn find_first_divisor
  bb0 bb0
    alloca Virtual { id: 0, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 1, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 1, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 2
    br
  bb1 bb1
    br
  bb2 bb2
    alloca Virtual { id: 3, bank: General, size_bits: 64 }, 1
    load Virtual { id: 4, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 1, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 5, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 1, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    mul Virtual { id: 6, bank: General, size_bits: 64 }, Virtual { id: 4, bank: General, size_bits: 64 }, Virtual { id: 5, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 3, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 6, bank: General, size_bits: 64 }
    alloca Virtual { id: 8, bank: General, size_bits: 64 }, 1
    load Virtual { id: 9, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 3, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    gt Virtual { id: 10, bank: General, size_bits: 8 }, Virtual { id: 9, bank: General, size_bits: 64 }, symbol(local.1)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 8, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 10, bank: General, size_bits: 8 }
    load Virtual { id: 12, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 8, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 13, bank: General, size_bits: 8 }, Virtual { id: 12, bank: General, size_bits: 8 }, 1
    condbr
  bb4 bb4
    alloca Virtual { id: 14, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 14, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(local.1)
    load Virtual { id: 16, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 14, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 16, bank: General, size_bits: 64 }
    br
  bb5 bb5
    br
  bb3 bb3
    load Virtual { id: 18, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
  bb6 bb6
    alloca Virtual { id: 19, bank: General, size_bits: 64 }, 1
    load Virtual { id: 20, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 1, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    rem Virtual { id: 21, bank: General, size_bits: 64 }, symbol(local.1), Virtual { id: 20, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 19, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 21, bank: General, size_bits: 64 }
    alloca Virtual { id: 23, bank: General, size_bits: 64 }, 1
    load Virtual { id: 24, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 19, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 25, bank: General, size_bits: 8 }, Virtual { id: 24, bank: General, size_bits: 64 }, 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 23, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 25, bank: General, size_bits: 8 }
    load Virtual { id: 27, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 23, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 28, bank: General, size_bits: 8 }, Virtual { id: 27, bank: General, size_bits: 8 }, 1
    condbr
  bb8 bb8
    alloca Virtual { id: 29, bank: General, size_bits: 64 }, 1
    load Virtual { id: 30, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 1, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 29, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 30, bank: General, size_bits: 64 }
    load Virtual { id: 32, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 29, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 32, bank: General, size_bits: 64 }
    br
  bb9 bb9
    br
  bb10 bb10
    load Virtual { id: 34, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 1, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 35, bank: General, size_bits: 64 }, Virtual { id: 34, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 1, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 35, bank: General, size_bits: 64 }
    br
  bb7 bb7
    load Virtual { id: 37, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
  bb11 bb11
    load Virtual { id: 38, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn factorial
  bb0 bb0
    alloca Virtual { id: 39, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 40, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 41, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 39, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 41, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 1
    br
  bb1 bb1
    alloca Virtual { id: 44, bank: General, size_bits: 64 }, 1
    load Virtual { id: 45, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 41, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    le Virtual { id: 46, bank: General, size_bits: 8 }, Virtual { id: 45, bank: General, size_bits: 64 }, symbol(local.1)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 44, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 46, bank: General, size_bits: 8 }
    load Virtual { id: 48, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 44, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 49, bank: General, size_bits: 8 }, Virtual { id: 48, bank: General, size_bits: 8 }, 1
    condbr
  bb2 bb2
    load Virtual { id: 50, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 39, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 51, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 41, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    mul Virtual { id: 52, bank: General, size_bits: 64 }, Virtual { id: 50, bank: General, size_bits: 64 }, Virtual { id: 51, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 39, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 52, bank: General, size_bits: 64 }
    load Virtual { id: 54, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 41, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 55, bank: General, size_bits: 64 }, Virtual { id: 54, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 41, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 55, bank: General, size_bits: 64 }
    br
  bb3 bb3
    load Virtual { id: 57, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 39, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 40, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 57, bank: General, size_bits: 64 }
    load Virtual { id: 59, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 40, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn main
  bb0 bb0
    alloca Virtual { id: 60, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 61, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 62, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 63, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 64, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 65, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 66, bank: General, size_bits: 64 }, 1
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    call symbol(factorial)(5) cc=C tail=false
    br
  bb1 bb1
    intrinsic.call symbol(intrinsic.println), Virtual { id: 74, bank: General, size_bits: 64 }
    call symbol(factorial)(7) cc=C tail=false
    br
  bb2 bb2
    intrinsic.call symbol(intrinsic.println), Virtual { id: 76, bank: General, size_bits: 64 }
    intrinsic.call symbol(intrinsic.println)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 66, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 60, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 1
    br
  bb3 bb3
    alloca Virtual { id: 81, bank: General, size_bits: 64 }, 1
    load Virtual { id: 82, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 60, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    lt Virtual { id: 83, bank: General, size_bits: 8 }, Virtual { id: 82, bank: General, size_bits: 64 }, 10
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 81, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 83, bank: General, size_bits: 8 }
    load Virtual { id: 85, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 81, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 86, bank: General, size_bits: 8 }, Virtual { id: 85, bank: General, size_bits: 8 }, 1
    condbr
  bb4 bb4
    alloca Virtual { id: 87, bank: General, size_bits: 64 }, 1
    load Virtual { id: 88, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 60, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 89, bank: General, size_bits: 64 }, Virtual { id: 88, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 87, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 89, bank: General, size_bits: 64 }
    load Virtual { id: 91, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 66, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 92, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 87, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 93, bank: General, size_bits: 64 }, Virtual { id: 91, bank: General, size_bits: 64 }, Virtual { id: 92, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 66, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 93, bank: General, size_bits: 64 }
    load Virtual { id: 95, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 60, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 96, bank: General, size_bits: 64 }, Virtual { id: 95, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 60, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 96, bank: General, size_bits: 64 }
    br
  bb5 bb5
    load Virtual { id: 98, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 66, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 98, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 63, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 64, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 5
    br
  bb6 bb6
    alloca Virtual { id: 102, bank: General, size_bits: 64 }, 1
    load Virtual { id: 103, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 64, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    lt Virtual { id: 104, bank: General, size_bits: 8 }, Virtual { id: 103, bank: General, size_bits: 64 }, 15
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 102, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 104, bank: General, size_bits: 8 }
    load Virtual { id: 106, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 102, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 107, bank: General, size_bits: 8 }, Virtual { id: 106, bank: General, size_bits: 8 }, 1
    condbr
  bb7 bb7
    alloca Virtual { id: 108, bank: General, size_bits: 64 }, 1
    load Virtual { id: 109, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 64, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 110, bank: General, size_bits: 64 }, Virtual { id: 109, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 108, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 110, bank: General, size_bits: 64 }
    load Virtual { id: 112, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 63, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 113, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 108, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 114, bank: General, size_bits: 64 }, Virtual { id: 112, bank: General, size_bits: 64 }, Virtual { id: 113, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 63, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 114, bank: General, size_bits: 64 }
    load Virtual { id: 116, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 64, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 117, bank: General, size_bits: 64 }, Virtual { id: 116, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 64, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 117, bank: General, size_bits: 64 }
    br
  bb8 bb8
    load Virtual { id: 119, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 63, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 119, bank: General, size_bits: 64 }
    intrinsic.call symbol(intrinsic.println)
    call symbol(find_first_divisor)(24) cc=C tail=false
    br
  bb9 bb9
    intrinsic.call symbol(intrinsic.println), Virtual { id: 122, bank: General, size_bits: 64 }
    call symbol(find_first_divisor)(17) cc=C tail=false
    br
  bb10 bb10
    intrinsic.call symbol(intrinsic.println), Virtual { id: 124, bank: General, size_bits: 64 }
    intrinsic.call symbol(intrinsic.println)
    call symbol(sum_even_numbers)(10) cc=C tail=false
    br
  bb11 bb11
    intrinsic.call symbol(intrinsic.println), Virtual { id: 127, bank: General, size_bits: 64 }
    intrinsic.call symbol(intrinsic.println)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 62, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 65, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 1
    br
  bb12 bb12
    alloca Virtual { id: 132, bank: General, size_bits: 64 }, 1
    load Virtual { id: 133, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 65, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    lt Virtual { id: 134, bank: General, size_bits: 8 }, Virtual { id: 133, bank: General, size_bits: 64 }, 4
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 132, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 134, bank: General, size_bits: 8 }
    load Virtual { id: 136, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 132, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 137, bank: General, size_bits: 8 }, Virtual { id: 136, bank: General, size_bits: 8 }, 1
    condbr
  bb13 bb13
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 61, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 1
    br
  bb14 bb14
    load Virtual { id: 139, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 62, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 139, bank: General, size_bits: 64 }
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println), 120
    intrinsic.call symbol(intrinsic.println)
    ret
  bb15 bb15
    alloca Virtual { id: 144, bank: General, size_bits: 64 }, 1
    load Virtual { id: 145, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 61, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    lt Virtual { id: 146, bank: General, size_bits: 8 }, Virtual { id: 145, bank: General, size_bits: 64 }, 4
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 144, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 146, bank: General, size_bits: 8 }
    load Virtual { id: 148, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 144, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 149, bank: General, size_bits: 8 }, Virtual { id: 148, bank: General, size_bits: 8 }, 1
    condbr
  bb16 bb16
    load Virtual { id: 150, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 62, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 151, bank: General, size_bits: 64 }, Virtual { id: 150, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 62, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 151, bank: General, size_bits: 64 }
    alloca Virtual { id: 153, bank: General, size_bits: 64 }, 1
    load Virtual { id: 154, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 65, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 155, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 61, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 156, bank: General, size_bits: 8 }, Virtual { id: 154, bank: General, size_bits: 64 }, Virtual { id: 155, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 153, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 156, bank: General, size_bits: 8 }
    load Virtual { id: 158, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 153, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 159, bank: General, size_bits: 8 }, Virtual { id: 158, bank: General, size_bits: 8 }, 1
    condbr
  bb17 bb17
    load Virtual { id: 160, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 65, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 161, bank: General, size_bits: 64 }, Virtual { id: 160, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 65, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 161, bank: General, size_bits: 64 }
    br
  bb18 bb18
    load Virtual { id: 163, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 65, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.print), Virtual { id: 163, bank: General, size_bits: 64 }
    br
  bb19 bb19
    br
  bb20 bb20
    load Virtual { id: 165, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 61, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 166, bank: General, size_bits: 64 }, Virtual { id: 165, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 61, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 166, bank: General, size_bits: 64 }
    br
fn sum_even_numbers
  bb0 bb0
    alloca Virtual { id: 168, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 169, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 170, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 168, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 170, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 0
    br
  bb1 bb1
    alloca Virtual { id: 173, bank: General, size_bits: 64 }, 1
    load Virtual { id: 174, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 170, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    lt Virtual { id: 175, bank: General, size_bits: 8 }, Virtual { id: 174, bank: General, size_bits: 64 }, symbol(local.1)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 173, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 175, bank: General, size_bits: 8 }
    load Virtual { id: 177, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 173, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 178, bank: General, size_bits: 8 }, Virtual { id: 177, bank: General, size_bits: 8 }, 1
    condbr
  bb2 bb2
    load Virtual { id: 179, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 170, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 180, bank: General, size_bits: 64 }, Virtual { id: 179, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 170, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 180, bank: General, size_bits: 64 }
    alloca Virtual { id: 182, bank: General, size_bits: 64 }, 1
    load Virtual { id: 183, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 170, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    rem Virtual { id: 184, bank: General, size_bits: 64 }, Virtual { id: 183, bank: General, size_bits: 64 }, 2
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 182, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 184, bank: General, size_bits: 64 }
    alloca Virtual { id: 186, bank: General, size_bits: 64 }, 1
    load Virtual { id: 187, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 182, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ne Virtual { id: 188, bank: General, size_bits: 8 }, Virtual { id: 187, bank: General, size_bits: 64 }, 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 186, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 188, bank: General, size_bits: 8 }
    load Virtual { id: 190, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 186, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 191, bank: General, size_bits: 8 }, Virtual { id: 190, bank: General, size_bits: 8 }, 1
    condbr
  bb3 bb3
    load Virtual { id: 192, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 168, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 169, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 192, bank: General, size_bits: 64 }
    load Virtual { id: 194, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 169, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
  bb4 bb4
    br
  bb5 bb5
    br
  bb6 bb6
    load Virtual { id: 195, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 168, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 196, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 170, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 197, bank: General, size_bits: 64 }, Virtual { id: 195, bank: General, size_bits: 64 }, Virtual { id: 196, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 168, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 197, bank: General, size_bits: 64 }
    br
  bb7 bb7
    load Virtual { id: 199, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 169, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret


Symbols:
  find_first_divisor               0x00000000
  factorial                        0x00000280
  main                             0x000003cc
  sum_even_numbers                 0x00000a9c

Text relocations:
  offset=0x00000438 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000444 kind=CallRel32 symbol=printf addend=0
  offset=0x00000448 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000454 kind=CallRel32 symbol=printf addend=0
  offset=0x00000458 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000464 kind=CallRel32 symbol=printf addend=0
  offset=0x00000468 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000474 kind=CallRel32 symbol=printf addend=0
  offset=0x00000478 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000484 kind=CallRel32 symbol=printf addend=0
  offset=0x00000488 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000494 kind=CallRel32 symbol=printf addend=0
  offset=0x00000498 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000004a4 kind=CallRel32 symbol=printf addend=0
  offset=0x000004b8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000004d0 kind=CallRel32 symbol=printf addend=0
  offset=0x000004e4 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000004fc kind=CallRel32 symbol=printf addend=0
  offset=0x00000500 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000050c kind=CallRel32 symbol=printf addend=0
  offset=0x00000620 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000638 kind=CallRel32 symbol=printf addend=0
  offset=0x0000074c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000764 kind=CallRel32 symbol=printf addend=0
  offset=0x00000768 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000774 kind=CallRel32 symbol=printf addend=0
  offset=0x00000788 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000007a0 kind=CallRel32 symbol=printf addend=0
  offset=0x000007b4 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000007cc kind=CallRel32 symbol=printf addend=0
  offset=0x000007d0 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000007dc kind=CallRel32 symbol=printf addend=0
  offset=0x000007f0 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000808 kind=CallRel32 symbol=printf addend=0
  offset=0x0000080c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000818 kind=CallRel32 symbol=printf addend=0
  offset=0x000008b4 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000008cc kind=CallRel32 symbol=printf addend=0
  offset=0x000008d0 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000008dc kind=CallRel32 symbol=printf addend=0
  offset=0x000008e0 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000008f8 kind=CallRel32 symbol=printf addend=0
  offset=0x000008fc kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000908 kind=CallRel32 symbol=printf addend=0
  offset=0x00000a50 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000a68 kind=CallRel32 symbol=printf addend=0

.text (3240 bytes):
  00000000  ff 43 07 d1 fd 7b 1c a9  fd 03 00 91 e0 a3 00 f9 
  00000010  1f 20 03 d5 f0 03 00 91  10 02 06 91 f0 03 00 f9 
  00000020  f0 03 00 91 10 22 06 91  f0 07 00 f9 f1 07 40 f9 
  00000030  50 00 80 d2 30 02 00 f9  01 00 00 14 01 00 00 14 
  00000040  f0 03 00 91 10 42 06 91  f0 0f 00 f9 f0 07 40 f9 
  00000050  11 02 40 f9 f1 13 00 f9  f0 07 40 f9 11 02 40 f9 
  00000060  f1 17 00 f9 f0 13 40 f9  f1 17 40 f9 10 7e 11 9b 
  00000070  f0 1b 00 f9 f1 0f 40 f9  f0 1b 40 f9 30 02 00 f9 
  00000080  f0 03 00 91 10 62 06 91  f0 23 00 f9 f0 0f 40 f9 
  00000090  11 02 40 f9 f1 27 00 f9  f0 27 40 f9 f1 a3 40 f9 
  000000a0  1f 02 11 eb f0 d7 9f 9a  f0 2b 00 f9 f1 23 40 f9 
  000000b0  f0 43 41 39 30 02 00 39  f0 23 40 f9 11 02 40 39 
  000000c0  f1 33 00 f9 f0 83 41 39  1f 06 00 f1 f0 17 9f 9a 
  000000d0  f0 37 00 f9 f0 37 40 f9  1f 02 00 f1 41 00 00 54 
  000000e0  0e 00 00 14 f0 03 00 91  10 82 06 91 f0 3b 00 f9 
  000000f0  f1 3b 40 f9 f0 a3 40 f9  30 02 00 f9 f0 3b 40 f9 
  00000100  11 02 40 f9 f1 43 00 f9  f1 03 40 f9 f0 43 40 f9 
  00000110  30 02 00 f9 02 00 00 14  09 00 00 14 f0 03 40 f9 
  00000120  11 02 40 f9 f1 4b 00 f9  e0 4b 40 f9 bf 03 00 91 
  00000130  fd 7b 5c a9 ff 43 07 91  c0 03 5f d6 f0 03 00 91 
  00000140  10 a2 06 91 f0 4f 00 f9  f0 07 40 f9 11 02 40 f9 
  00000150  f1 53 00 f9 f0 a3 40 f9  f1 53 40 f9 09 0e d1 9a 
  00000160  30 c1 11 9b f0 57 00 f9  f1 4f 40 f9 f0 57 40 f9 
  00000170  30 02 00 f9 f0 03 00 91  10 c2 06 91 f0 5f 00 f9 
  00000180  f0 4f 40 f9 11 02 40 f9  f1 63 00 f9 f0 63 40 f9 
  00000190  1f 02 00 f1 f0 17 9f 9a  f0 67 00 f9 f1 5f 40 f9 
  000001a0  f0 23 43 39 30 02 00 39  f0 5f 40 f9 11 02 40 39 
  000001b0  f1 6f 00 f9 f0 63 43 39  1f 06 00 f1 f0 17 9f 9a 
  000001c0  f0 73 00 f9 f0 73 40 f9  1f 02 00 f1 41 00 00 54 
  000001d0  11 00 00 14 f0 03 00 91  10 e2 06 91 f0 77 00 f9 
  000001e0  f0 07 40 f9 11 02 40 f9  f1 7b 00 f9 f1 77 40 f9 
  000001f0  f0 7b 40 f9 30 02 00 f9  f0 77 40 f9 11 02 40 f9 
  00000200  f1 83 00 f9 f1 03 40 f9  f0 83 40 f9 30 02 00 f9 
  00000210  c3 ff ff 17 01 00 00 14  f0 07 40 f9 11 02 40 f9 
  00000220  f1 8b 00 f9 f0 8b 40 f9  10 06 00 91 f0 8f 00 f9 
  00000230  f1 07 40 f9 f0 8f 40 f9  30 02 00 f9 80 ff ff 17 
  00000240  f0 03 40 f9 11 02 40 f9  f1 97 00 f9 e0 97 40 f9 
  00000250  bf 03 00 91 fd 7b 5c a9  ff 43 07 91 c0 03 5f d6 
  00000260  f0 03 40 f9 11 02 40 f9  f1 9b 00 f9 e0 9b 40 f9 
  00000270  bf 03 00 91 fd 7b 5c a9  ff 43 07 91 c0 03 5f d6 
  00000280  ff 03 04 d1 fd 7b 0f a9  fd 03 00 91 e0 5b 00 f9 
  00000290  1f 20 03 d5 f0 03 00 91  10 42 03 91 f0 03 00 f9 
  000002a0  f0 03 00 91 10 62 03 91  f0 07 00 f9 f0 03 00 91 
  000002b0  10 82 03 91 f0 0b 00 f9  f1 03 40 f9 30 00 80 d2 
  000002c0  30 02 00 f9 f1 0b 40 f9  30 00 80 d2 30 02 00 f9 
  000002d0  01 00 00 14 f0 03 00 91  10 a2 03 91 f0 17 00 f9 
  000002e0  f0 0b 40 f9 11 02 40 f9  f1 1b 00 f9 f0 1b 40 f9 
  000002f0  f1 5b 40 f9 1f 02 11 eb  f0 c7 9f 9a f0 1f 00 f9 
  00000300  f1 17 40 f9 f0 e3 40 39  30 02 00 39 f0 17 40 f9 
  00000310  11 02 40 39 f1 27 00 f9  f0 23 41 39 1f 06 00 f1 
  00000320  f0 17 9f 9a f0 2b 00 f9  f0 2b 40 f9 1f 02 00 f1 
  00000330  41 00 00 54 18 00 00 14  f0 03 40 f9 11 02 40 f9 
  00000340  f1 2f 00 f9 f0 0b 40 f9  11 02 40 f9 f1 33 00 f9 
  00000350  f0 2f 40 f9 f1 33 40 f9  10 7e 11 9b f0 37 00 f9 
  00000360  f1 03 40 f9 f0 37 40 f9  30 02 00 f9 f0 0b 40 f9 
  00000370  11 02 40 f9 f1 3f 00 f9  f0 3f 40 f9 10 06 00 91 
  00000380  f0 43 00 f9 f1 0b 40 f9  f0 43 40 f9 30 02 00 f9 
  00000390  d1 ff ff 17 f0 03 40 f9  11 02 40 f9 f1 4b 00 f9 
  000003a0  f1 07 40 f9 f0 4b 40 f9  30 02 00 f9 f0 07 40 f9 
  000003b0  11 02 40 f9 f1 53 00 f9  e0 53 40 f9 bf 03 00 91 
  000003c0  fd 7b 4f a9 ff 03 04 91  c0 03 5f d6 ff 43 15 d1 
  000003d0  f0 03 00 91 10 02 15 91  1d 7a 00 a9 fd 03 00 91 
  000003e0  1f 20 03 d5 f0 03 00 91  10 22 13 91 f0 0b 00 f9 
  000003f0  f0 03 00 91 10 42 13 91  f0 0f 00 f9 f0 03 00 91 
  00000400  10 62 13 91 f0 13 00 f9  f0 03 00 91 10 82 13 91 
  00000410  f0 17 00 f9 f0 03 00 91  10 a2 13 91 f0 1b 00 f9 
  00000420  f0 03 00 91 10 c2 13 91  f0 1f 00 f9 f0 03 00 91 
  00000430  10 e2 13 91 f0 23 00 f9  00 00 00 90 00 00 00 91 
  00000440  00 20 00 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000450  00 a0 00 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000460  00 80 01 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000470  00 40 02 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000480  00 e0 02 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000490  00 00 03 91 00 00 00 94  00 00 00 90 00 00 00 91 
  000004a0  00 80 03 91 00 00 00 94  a0 00 80 d2 75 ff ff 97 
  000004b0  e0 43 00 f9 01 00 00 14  00 00 00 90 00 00 00 91 
  000004c0  00 00 04 91 e1 43 40 f9  f0 43 40 f9 f0 03 00 f9 
  000004d0  00 00 00 94 e0 00 80 d2  6a ff ff 97 e0 4b 00 f9 
  000004e0  01 00 00 14 00 00 00 90  00 00 00 91 00 40 04 91 
  000004f0  e1 4b 40 f9 f0 4b 40 f9  f0 03 00 f9 00 00 00 94 
  00000500  00 00 00 90 00 00 00 91  00 80 04 91 00 00 00 94 
  00000510  f1 23 40 f9 10 00 80 d2  30 02 00 f9 f1 0b 40 f9 
  00000520  30 00 80 d2 30 02 00 f9  01 00 00 14 f0 03 00 91 
  00000530  10 02 14 91 f0 5f 00 f9  f0 0b 40 f9 11 02 40 f9 
  00000540  f1 63 00 f9 f0 63 40 f9  1f 2a 00 f1 f0 a7 9f 9a 
  00000550  f0 67 00 f9 f1 5f 40 f9  f0 23 43 39 30 02 00 39 
  00000560  f0 5f 40 f9 11 02 40 39  f1 6f 00 f9 f0 63 43 39 
  00000570  1f 06 00 f1 f0 17 9f 9a  f0 73 00 f9 f0 73 40 f9 
  00000580  1f 02 00 f1 41 00 00 54  23 00 00 14 f0 03 00 91 
  00000590  10 22 14 91 f0 77 00 f9  f0 0b 40 f9 11 02 40 f9 
  000005a0  f1 7b 00 f9 f0 7b 40 f9  f0 7f 00 f9 f1 77 40 f9 
  000005b0  f0 7f 40 f9 30 02 00 f9  f0 23 40 f9 11 02 40 f9 
  000005c0  f1 87 00 f9 f0 77 40 f9  11 02 40 f9 f1 8b 00 f9 
  000005d0  f0 87 40 f9 f1 8b 40 f9  10 02 11 8b f0 8f 00 f9 
  000005e0  f1 23 40 f9 f0 8f 40 f9  30 02 00 f9 f0 0b 40 f9 
  000005f0  11 02 40 f9 f1 97 00 f9  f0 97 40 f9 10 06 00 91 
  00000600  f0 9b 00 f9 f1 0b 40 f9  f0 9b 40 f9 30 02 00 f9 
  00000610  c7 ff ff 17 f0 23 40 f9  11 02 40 f9 f1 a3 00 f9 
  00000620  00 00 00 90 00 00 00 91  00 00 05 91 e1 a3 40 f9 
  00000630  f0 a3 40 f9 f0 03 00 f9  00 00 00 94 f1 17 40 f9 
  00000640  10 00 80 d2 30 02 00 f9  f1 1b 40 f9 b0 00 80 d2 
  00000650  30 02 00 f9 01 00 00 14  f0 03 00 91 10 42 14 91 
  00000660  f0 b3 00 f9 f0 1b 40 f9  11 02 40 f9 f1 b7 00 f9 
  00000670  f0 b7 40 f9 1f 3e 00 f1  f0 a7 9f 9a f0 bb 00 f9 
  00000680  f1 b3 40 f9 f0 c3 45 39  30 02 00 39 f0 b3 40 f9 
  00000690  11 02 40 39 f1 c3 00 f9  f0 03 46 39 1f 06 00 f1 
  000006a0  f0 17 9f 9a f0 c7 00 f9  f0 c7 40 f9 1f 02 00 f1 
  000006b0  41 00 00 54 23 00 00 14  f0 03 00 91 10 62 14 91 
  000006c0  f0 cb 00 f9 f0 1b 40 f9  11 02 40 f9 f1 cf 00 f9 
  000006d0  f0 cf 40 f9 f0 d3 00 f9  f1 cb 40 f9 f0 d3 40 f9 
  000006e0  30 02 00 f9 f0 17 40 f9  11 02 40 f9 f1 db 00 f9 
  000006f0  f0 cb 40 f9 11 02 40 f9  f1 df 00 f9 f0 db 40 f9 
  00000700  f1 df 40 f9 10 02 11 8b  f0 e3 00 f9 f1 17 40 f9 
  00000710  f0 e3 40 f9 30 02 00 f9  f0 1b 40 f9 11 02 40 f9 
  00000720  f1 eb 00 f9 f0 eb 40 f9  10 06 00 91 f0 ef 00 f9 
  00000730  f1 1b 40 f9 f0 ef 40 f9  30 02 00 f9 c7 ff ff 17 
  00000740  f0 17 40 f9 11 02 40 f9  f1 f7 00 f9 00 00 00 90 
  00000750  00 00 00 91 00 60 05 91  e1 f7 40 f9 f0 f7 40 f9 
  00000760  f0 03 00 f9 00 00 00 94  00 00 00 90 00 00 00 91 
  00000770  00 c0 05 91 00 00 00 94  00 03 80 d2 21 fe ff 97 
  00000780  e0 03 01 f9 01 00 00 14  00 00 00 90 00 00 00 91 
  00000790  00 60 06 91 e1 03 41 f9  f0 03 41 f9 f0 03 00 f9 
  000007a0  00 00 00 94 20 02 80 d2  16 fe ff 97 e0 0b 01 f9 
  000007b0  01 00 00 14 00 00 00 90  00 00 00 91 00 e0 06 91 
  000007c0  e1 0b 41 f9 f0 0b 41 f9  f0 03 00 f9 00 00 00 94 
  000007d0  00 00 00 90 00 00 00 91  00 60 07 91 00 00 00 94 
  000007e0  40 01 80 d2 ae 00 00 94  e0 17 01 f9 01 00 00 14 
  000007f0  00 00 00 90 00 00 00 91  00 e0 07 91 e1 17 41 f9 
  00000800  f0 17 41 f9 f0 03 00 f9  00 00 00 94 00 00 00 90 
  00000810  00 00 00 91 00 80 08 91  00 00 00 94 f1 13 40 f9 
  00000820  10 00 80 d2 30 02 00 f9  f1 1f 40 f9 30 00 80 d2 
  00000830  30 02 00 f9 01 00 00 14  f0 03 00 91 10 82 14 91 
  00000840  f0 2b 01 f9 f0 1f 40 f9  11 02 40 f9 f1 2f 01 f9 
  00000850  f0 2f 41 f9 1f 12 00 f1  f0 a7 9f 9a f0 33 01 f9 
  00000860  f1 2b 41 f9 f0 83 49 39  30 02 00 39 f0 2b 41 f9 
  00000870  11 02 40 39 f1 3b 01 f9  f0 c3 49 39 1f 06 00 f1 
  00000880  f0 17 9f 9a f0 3f 01 f9  f0 3f 41 f9 1f 02 00 f1 
  00000890  41 00 00 54 05 00 00 14  f1 0f 40 f9 30 00 80 d2 
  000008a0  30 02 00 f9 21 00 00 14  f0 13 40 f9 11 02 40 f9 
  000008b0  f1 47 01 f9 00 00 00 90  00 00 00 91 00 e0 08 91 
  000008c0  e1 47 41 f9 f0 47 41 f9  f0 03 00 f9 00 00 00 94 
  000008d0  00 00 00 90 00 00 00 91  00 40 09 91 00 00 00 94 
  000008e0  00 00 00 90 00 00 00 91  00 c0 09 91 01 0f 80 d2 
  000008f0  10 0f 80 d2 f0 03 00 f9  00 00 00 94 00 00 00 90 
  00000900  00 00 00 91 00 20 0a 91  00 00 00 94 bf 03 00 91 
  00000910  f0 03 00 91 10 02 15 91  1d 7a 40 a9 ff 43 15 91 
  00000920  00 00 80 d2 c0 03 5f d6  f0 03 00 91 10 a2 14 91 
  00000930  f0 5b 01 f9 f0 0f 40 f9  11 02 40 f9 f1 5f 01 f9 
  00000940  f0 5f 41 f9 1f 12 00 f1  f0 a7 9f 9a f0 63 01 f9 
  00000950  f1 5b 41 f9 f0 03 4b 39  30 02 00 39 f0 5b 41 f9 
  00000960  11 02 40 39 f1 6b 01 f9  f0 43 4b 39 1f 06 00 f1 
  00000970  f0 17 9f 9a f0 6f 01 f9  f0 6f 41 f9 1f 02 00 f1 
  00000980  41 00 00 54 26 00 00 14  f0 13 40 f9 11 02 40 f9 
  00000990  f1 73 01 f9 f0 73 41 f9  10 06 00 91 f0 77 01 f9 
  000009a0  f1 13 40 f9 f0 77 41 f9  30 02 00 f9 f0 03 00 91 
  000009b0  10 c2 14 91 f0 7f 01 f9  f0 1f 40 f9 11 02 40 f9 
  000009c0  f1 83 01 f9 f0 0f 40 f9  11 02 40 f9 f1 87 01 f9 
  000009d0  f0 83 41 f9 f1 87 41 f9  1f 02 11 eb f0 17 9f 9a 
  000009e0  f0 8b 01 f9 f1 7f 41 f9  f0 43 4c 39 30 02 00 39 
  000009f0  f0 7f 41 f9 11 02 40 39  f1 93 01 f9 f0 83 4c 39 
  00000a00  1f 06 00 f1 f0 17 9f 9a  f0 97 01 f9 f0 97 41 f9 
  00000a10  1f 02 00 f1 81 01 00 54  16 00 00 14 f0 1f 40 f9 
  00000a20  11 02 40 f9 f1 9b 01 f9  f0 9b 41 f9 10 06 00 91 
  00000a30  f0 9f 01 f9 f1 1f 40 f9  f0 9f 41 f9 30 02 00 f9 
  00000a40  7e ff ff 17 f0 1f 40 f9  11 02 40 f9 f1 a7 01 f9 
  00000a50  00 00 00 90 00 00 00 91  00 c0 0a 91 e1 a7 41 f9 
  00000a60  f0 a7 41 f9 f0 03 00 f9  00 00 00 94 02 00 00 14 
  00000a70  01 00 00 14 f0 0f 40 f9  11 02 40 f9 f1 af 01 f9 
  00000a80  f0 af 41 f9 10 06 00 91  f0 b3 01 f9 f1 0f 40 f9 
  00000a90  f0 b3 41 f9 30 02 00 f9  a4 ff ff 17 ff 03 06 d1 
  00000aa0  fd 7b 17 a9 fd 03 00 91  e0 87 00 f9 1f 20 03 d5 
  00000ab0  f0 03 00 91 10 e2 04 91  f0 03 00 f9 f0 03 00 91 
  00000ac0  10 02 05 91 f0 07 00 f9  f0 03 00 91 10 22 05 91 
  00000ad0  f0 0b 00 f9 f1 03 40 f9  10 00 80 d2 30 02 00 f9 
  00000ae0  f1 0b 40 f9 10 00 80 d2  30 02 00 f9 01 00 00 14 
  00000af0  f0 03 00 91 10 42 05 91  f0 17 00 f9 f0 0b 40 f9 
  00000b00  11 02 40 f9 f1 1b 00 f9  f0 1b 40 f9 f1 87 40 f9 
  00000b10  1f 02 11 eb f0 a7 9f 9a  f0 1f 00 f9 f1 17 40 f9 
  00000b20  f0 e3 40 39 30 02 00 39  f0 17 40 f9 11 02 40 39 
  00000b30  f1 27 00 f9 f0 23 41 39  1f 06 00 f1 f0 17 9f 9a 
  00000b40  f0 2b 00 f9 f0 2b 40 f9  1f 02 00 f1 41 00 00 54 
  00000b50  30 00 00 14 f0 0b 40 f9  11 02 40 f9 f1 2f 00 f9 
  00000b60  f0 2f 40 f9 10 06 00 91  f0 33 00 f9 f1 0b 40 f9 
  00000b70  f0 33 40 f9 30 02 00 f9  f0 03 00 91 10 62 05 91 
  00000b80  f0 3b 00 f9 f0 0b 40 f9  11 02 40 f9 f1 3f 00 f9 
  00000b90  f0 3f 40 f9 51 00 80 d2  09 0e d1 9a 30 c1 11 9b 
  00000ba0  f0 43 00 f9 f1 3b 40 f9  f0 43 40 f9 30 02 00 f9 
  00000bb0  f0 03 00 91 10 82 05 91  f0 4b 00 f9 f0 3b 40 f9 
  00000bc0  11 02 40 f9 f1 4f 00 f9  f0 4f 40 f9 1f 02 00 f1 
  00000bd0  f0 07 9f 9a f0 53 00 f9  f1 4b 40 f9 f0 83 42 39 
  00000be0  30 02 00 39 f0 4b 40 f9  11 02 40 39 f1 5b 00 f9 
  00000bf0  f0 c3 42 39 1f 06 00 f1  f0 17 9f 9a f0 5f 00 f9 
  00000c00  f0 5f 40 f9 1f 02 00 f1  01 02 00 54 10 00 00 14 
  00000c10  f0 03 40 f9 11 02 40 f9  f1 63 00 f9 f1 07 40 f9 
  00000c20  f0 63 40 f9 30 02 00 f9  f0 07 40 f9 11 02 40 f9 
  00000c30  f1 6b 00 f9 e0 6b 40 f9  bf 03 00 91 fd 7b 57 a9 
  00000c40  ff 03 06 91 c0 03 5f d6  aa ff ff 17 01 00 00 14 
  00000c50  f0 03 40 f9 11 02 40 f9  f1 6f 00 f9 f0 0b 40 f9 
  00000c60  11 02 40 f9 f1 73 00 f9  f0 6f 40 f9 f1 73 40 f9 
  00000c70  10 02 11 8b f0 77 00 f9  f1 03 40 f9 f0 77 40 f9 
  00000c80  30 02 00 f9 9b ff ff 17  f0 07 40 f9 11 02 40 f9 
  00000c90  f1 7f 00 f9 e0 7f 40 f9  bf 03 00 91 fd 7b 57 a9 
  00000ca0  ff 03 06 91 c0 03 5f d6 

.rodata (696 bytes):
  00000000  78 00 00 00 00 00 00 00  f0 9f 93 98 20 54 75 74 
  00000010  6f 72 69 61 6c 3a 20 31  33 5f 6c 6f 6f 70 73 2e 
  00000020  66 70 0a 00 00 00 00 00  f0 9f a7 ad 20 46 6f 63 
  00000030  75 73 3a 20 4c 6f 6f 70  20 63 6f 6e 73 74 72 75 
  00000040  63 74 73 3a 20 77 68 69  6c 65 2c 20 66 6f 72 2c 
  00000050  20 61 6e 64 20 6c 6f 6f  70 2e 0a 00 00 00 00 00 
  00000060  f0 9f a7 aa 20 57 68 61  74 20 74 6f 20 6c 6f 6f 
  00000070  6b 20 66 6f 72 3a 20 6c  61 62 65 6c 65 64 20 6f 
  00000080  75 74 70 75 74 73 20 62  65 6c 6f 77 0a 00 00 00 
  00000090  e2 9c 85 20 45 78 70 65  63 74 61 74 69 6f 6e 3a 
  000000a0  20 6f 75 74 70 75 74 73  20 6d 61 74 63 68 20 6c 
  000000b0  61 62 65 6c 73 0a 00 00  0a 00 00 00 00 00 00 00 
  000000c0  3d 3d 3d 20 4c 6f 6f 70  20 43 6f 6e 73 74 72 75 
  000000d0  63 74 73 20 3d 3d 3d 0a  0a 00 00 00 00 00 00 00 
  000000e0  31 2e 20 57 68 69 6c 65  20 6c 6f 6f 70 20 2d 20 
  000000f0  66 61 63 74 6f 72 69 61  6c 3a 0a 00 00 00 00 00 
  00000100  20 20 35 21 20 3d 20 25  6c 6c 64 0a 00 00 00 00 
  00000110  20 20 37 21 20 3d 20 25  6c 6c 64 0a 00 00 00 00 
  00000120  0a 32 2e 20 46 6f 72 20  6c 6f 6f 70 20 2d 20 73 
  00000130  75 6d 20 72 61 6e 67 65  3a 0a 00 00 00 00 00 00 
  00000140  20 20 73 75 6d 28 31 2e  2e 31 30 29 20 3d 20 25 
  00000150  6c 6c 64 0a 00 00 00 00  20 20 73 75 6d 28 35 2e 
  00000160  2e 31 35 29 20 3d 20 25  6c 6c 64 0a 00 00 00 00 
  00000170  0a 33 2e 20 4c 6f 6f 70  20 77 69 74 68 20 62 72 
  00000180  65 61 6b 20 65 78 70 72  65 73 73 69 6f 6e 3a 0a 
  00000190  00 00 00 00 00 00 00 00  20 20 46 69 72 73 74 20 
  000001a0  64 69 76 69 73 6f 72 20  6f 66 20 32 34 3a 20 25 
  000001b0  6c 6c 64 0a 00 00 00 00  20 20 46 69 72 73 74 20 
  000001c0  64 69 76 69 73 6f 72 20  6f 66 20 31 37 3a 20 25 
  000001d0  6c 6c 64 0a 00 00 00 00  0a 34 2e 20 4c 6f 6f 70 
  000001e0  20 77 69 74 68 20 63 6f  6e 74 69 6e 75 65 3a 0a 
  000001f0  00 00 00 00 00 00 00 00  20 20 53 75 6d 20 6f 66 
  00000200  20 65 76 65 6e 20 6e 75  6d 62 65 72 73 20 3c 20 
  00000210  31 30 3a 20 25 6c 6c 64  0a 00 00 00 00 00 00 00 
  00000220  0a 35 2e 20 4e 65 73 74  65 64 20 6c 6f 6f 70 73 
  00000230  3a 0a 00 00 00 00 00 00  0a 20 20 49 74 65 72 61 
  00000240  74 69 6f 6e 73 3a 20 25  6c 6c 64 0a 00 00 00 00 
  00000250  0a 36 2e 20 43 6f 6d 70  69 6c 65 2d 74 69 6d 65 
  00000260  20 63 6f 6e 73 74 61 6e  74 3a 0a 00 00 00 00 00 
  00000270  20 20 63 6f 6e 73 74 20  35 21 20 3d 20 25 6c 6c 
  00000280  64 0a 00 00 00 00 00 00  0a e2 9c 93 20 4c 6f 6f 
  00000290  70 20 63 6f 6e 73 74 72  75 63 74 73 20 64 65 6d 
  000002a0  6f 6e 73 74 72 61 74 65  64 21 0a 00 00 00 00 00 
  000002b0  5b 25 6c 6c 75 5d 20 00 
