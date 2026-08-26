fp-native dump: format=MachO arch=Aarch64 entry=0x0

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
global __const_data_0 ty=Array(I8, 6) constant=true initializer=Some(Bytes([97, 108, 112, 104, 97, 0]))
global __const_data_1 ty=Array(I8, 5) constant=true initializer=Some(Bytes([98, 101, 116, 97, 0]))
fn sigsetjmp
fn mach_msg_overwrite
fn thread_resume
fn mbrlen
fn close
fn getgrgid
fn thread_policy_get
fn mach_port_allocate
fn mach_error_type
fn __vsprintf_chk
fn _dyld_get_image_name
fn fputwc
fn strtok
fn strndup
fn opendir
fn aio_error
fn semop
fn mach_msg
fn isalpha
fn kmod_destroy
fn thread_swap_exception_ports
fn strtoull
fn endhostent
fn sched_get_priority_max
fn macx_backing_store_recovery
fn slot_name
fn host_set_multiuser_config_flags
fn remque
fn feholdexcept
fn fstatat
fn _kernelrpc_mach_port_guard_trap
fn thread_depress_abort
fn _kernelrpc_mach_vm_allocate_trap
fn strtok_r
fn iswprint
fn mig_get_reply_port
fn atomic_flag_clear_explicit
fn voucher_mach_msg_clear
fn getgrnam
fn NXSwapShort
fn nl_langinfo
fn getppid
fn fdopendir
fn sleep
fn mach_port_type
fn strcmp
fn getsubopt
fn sched_yield
fn tcgetpgrp
fn write
fn gethostname
fn mktemp
fn host_processor_sets
fn wctob
fn sem_init
fn mach_ports_register
fn NSLookupSymbolInImage
fn usleep
fn mach_port_extract_member
fn mach_port_guard
fn task_get_special_port
fn _dyld_lookup_and_bind
fn setenv
fn raise
fn gets
fn wcwidth
fn task_map_corpse_info_64
fn mach_port_kobject
fn tcdrain
fn task_dyld_process_info_notify_register
fn iconv_close
fn macx_triggers
fn clock
fn dup2
fn _OSWriteSwapInt32
fn task_policy_get
fn listen
fn task_set_exc_guard_behavior
fn vm_inherit
fn vm_map
fn mach_port_mod_refs
fn mach_zone_info
fn task_dyld_process_info_notify_deregister
fn clock_get_res
fn poll
fn voucher_mach_msg_revert
fn longjmp
fn fstatvfs
fn NXSwapBigLongLongToHost
fn posix_memalign
fn setprotoent
fn atomic_flag_clear
fn __darwin_fd_isset
fn fchmod
fn isgraph
fn setgid
fn setuid
fn utimes
fn imaxabs
fn task_get_assignment
fn atol
fn wcrtomb
fn wcstol
fn tcgetattr
fn getgrgid_r
fn cfgetispeed
fn feraiseexcept
fn iswdigit
fn setkey
fn wcspbrk
fn getline
fn iswrune
fn dlerror
fn regerror
fn atomic_thread_fence
fn posix_spawnattr_init
fn execv
fn mach_port_extract_right
fn processor_assign
fn NSAddLibrary
fn mach_port_request_notification
fn vsscanf
fn task_policy_set
fn host_get_io_main
fn mig_allocate
fn dup
fn read
fn thread_set_exception_ports
fn rand
fn vm_read_overwrite
fn wcschr
fn mach_host_self
fn wcstoumax
fn __sigbits
fn putchar
fn iswalpha
fn inet_addr
fn fchown
fn endpwent
fn task_set_emulation_vector
fn abs
fn posix_spawnattr_getsigdefault
fn __error
fn strdup
fn sysconf
fn gettimeofday
fn pthread_key_delete
fn sem_wait
fn getpgid
fn kmod_control
fn _OSReadSwapInt32
fn atoll
fn cfsetispeed
fn host_create_mach_voucher
fn processor_set_create
fn mach_port_set_mscount
fn psignal
fn remove
fn strncat
fn thread_adopt_exception_handler
fn wcscmp
fn aio_fsync
fn shutdown
fn __NDR_convert__mig_reply_error_t
fn processor_start
fn NSInstallLinkEditErrorHandlers
fn host_set_special_port
fn strlen
fn mach_vm_wire
fn _dyld_get_image_vmaddr_slide
fn iswlower
fn NXSwapHostShortToLittle
fn mlock
fn NXSwapDouble
fn mkfifoat
fn getentropy
fn lldiv
fn mach_voucher_deallocate
fn vm_behavior_set
fn getpwuid
fn NXSwapHostIntToLittle
fn thread_set_mach_voucher
fn wcsxfrm
fn stat
fn semaphore_signal_thread
fn NSNameOfModule
fn getgrent
fn nanosleep
fn strrchr
fn setgroupent
fn _OSWriteSwapInt16
fn vm_map_exec_lockdown
fn dlsym
fn vfwscanf
fn wcsrchr
fn __darwin_check_fd_set_overflow
fn task_set_emulation
fn wcsncmp
fn vm_purgable_control
fn NSIsSymbolDefinedInObjectFileImage
fn sem_trywait
fn NSLinkEditError
fn setgrent
fn host_security_create_task_token
fn setsockopt
fn nice
fn seekdir
fn task_register_dyld_get_process_state
fn fgetc
fn wmemchr
fn _kernelrpc_mach_port_mod_refs_trap
fn fesetround
fn _kernelrpc_mach_port_extract_member_trap
fn mach_port_is_connection_for_service
fn NXSwapHostLongLongToBig
fn chdir
fn rmdir
fn vfprintf
fn posix_spawnattr_getpgroup
fn NSIsSymbolNameDefined
fn wcsnlen
fn host_swap_exception_ports
fn processor_get_assignment
fn pipe
fn NSNameOfSymbol
fn mach_task_is_self
fn gai_strerror
fn strcspn
fn sem_destroy
fn posix_madvise
fn munlockall
fn getpwnam
fn lockf
fn puts
fn aio_cancel
fn ttyname
fn semaphore_wait_signal
fn toascii
fn processor_set_policy_disable
fn task_map_kcdata_object_64
fn mach_port_unguard
fn mbstowcs
fn mach_port_space_basic_info
fn siginterrupt
fn telldir
fn task_suspend2
fn NSModuleForSymbol
fn vfscanf
fn feupdateenv
fn endgrent
fn processor_set_info
fn mbsrtowcs
fn readlinkat
fn vscanf
fn vm_region_recurse_64
fn access
fn basename
fn kext_request
fn vm_remap
fn globfree
fn isascii
fn mach_port_names
fn thread_terminate
fn open_wmemstream
fn mig_dealloc_reply_port
fn fgets
fn thread_suspend
fn strtol
fn _kernelrpc_mach_port_construct_trap
fn labs
fn wcscat
fn getpgrp
fn task_wire
fn processor_info
fn host_statistics64
fn isblank
fn free
fn wcsstr
fn readdir_r
fn fsync
fn task_register_dyld_image_infos
fn fopen
fn _Exit
fn NXSwapHostLongToBig
fn recvmsg
fn pselect
fn _OSReadSwapInt16
fn sethostent
fn thread_wire
fn task_resume2
fn mach_port_peek
fn mach_port_insert_right
fn localtime
fn semaphore_timedwait
fn pid_for_task
fn system
fn putwc
fn imaxdiv
fn mbrtowc
fn tcgetsid
fn siglongjmp
fn vprintf
fn getgroups
fn _OSReadInt16
fn task_set_ras_pc
fn semaphore_destroy
fn sigdelset
fn host_set_atm_diagnostic_flag
fn getsockname
fn getpeername
fn vm_allocate_cpm
fn _kernelrpc_mach_port_move_member_trap
fn task_get_exception_ports_info
fn fchmodat
fn mach_port_insert_member
fn host_get_clock_service
fn getegid
fn mach_port_get_service_port_info
fn calloc
fn task_set_exception_ports
fn rewind
fn task_get_exception_ports
fn sighold
fn getopt
fn task_unregister_dyld_image_infos
fn localtime_r
fn if_freenameindex
fn vm_region_64
fn mach_make_memory_entry_64
fn shmget
fn host_processor_set_priv
fn host_get_multiuser_config_flags
fn setrlimit
fn NSLookupAndBindSymbolWithHint
fn mach_port_get_context
fn _dyld_get_image_header_containing_address
fn putchar_unlocked
fn strcoll
fn strxfrm
fn isdigit
fn hsearch
fn act_get_state
fn towlower
fn setreuid
fn _kernelrpc_mach_vm_protect_trap
fn _kernelrpc_mach_port_request_notification_trap
fn _dyld_get_image_header
fn strncmp
fn towupper
fn NSDestroyObjectFileImage
fn if_nameindex
fn ftell
fn insque
fn posix_spawnattr_setpgroup
fn mach_port_get_refs
fn mach_port_dnrequest_info
fn semaphore_signal
fn feclearexcept
fn wcstok
fn fegetenv
fn getpid
fn __toupper
fn _longjmp
fn iswspace
fn wctrans
fn bind
fn getprotobynumber
fn task_register_dyld_shared_cache_image_info
fn strftime
fn isxdigit
fn getchar_unlocked
fn task_inspect
fn vm_read
fn host_get_special_port
fn thread_get_exception_ports_info
fn mach_port_swap_guard
fn wcsncpy
fn wcsncasecmp
fn utime
fn host_virtual_physical_table_info
fn atomic_flag_test_and_set
fn iconv_open
fn _OSWriteSwapInt64
fn processor_set_threads
fn mach_memory_info
fn NSCreateObjectFileImageFromFile
fn processor_set_statistics
fn send
fn wcslen
fn _host_page_size
fn clock_sleep
fn NXSwapHostIntToBig
fn pread
fn stpncpy
fn gethostbyaddr
fn thread_get_mach_voucher
fn mach_port_rename
fn swtch_pri
fn wcscasecmp
fn fputc
fn ctime
fn link
fn ualarm
fn ___toupper
fn waitpid
fn getrlimit
fn fwrite
fn pclose
fn mkstemp
fn memcmp
fn wcpcpy
fn dirname
fn vswprintf
fn setgrfile
fn sync
fn getnetbyaddr
fn mach_vm_region_info
fn gethostid
fn host_reboot
fn endservent
fn setbuf
fn wmemset
fn getpwnam_r
fn strcasecmp
fn exit
fn alarm
fn getpriority
fn thread_create_running
fn strtoimax
fn setvbuf
fn thread_swap_mach_voucher
fn _kernelrpc_mach_port_destruct_trap
fn thread_switch
fn sigemptyset
fn symlink
fn ___runetype
fn tcflush
fn _exit
fn _OSReadInt64
fn kmod_create
fn processor_set_policy_control
fn wcsncat
fn mach_port_allocate_name
fn mach_port_assert_attributes
fn _kernelrpc_mach_port_insert_right_trap
fn host_register_mach_voucher_attr_manager
fn unlinkat
fn fchdir
fn mach_port_construct
fn fclose
fn feof
fn nrand48
fn _OSWriteInt64
fn task_set_special_port
fn getlogin
fn NXSwapBigLongToHost
fn waitid
fn timespec_get
fn freeaddrinfo
fn ftok
fn task_assign_default
fn thread_get_assignment
fn _dyld_bind_fully_image_containing_address
fn fsetxattr
fn mach_error
fn stpcpy
fn task_map_corpse_info
fn getdelim
fn strptime
fn __assert_rtn
fn ispunct
fn geteuid
fn _kernelrpc_mach_vm_purgable_control_trap
fn posix_spawnattr_destroy
fn unlockpt
fn killpg
fn fdopen
fn wcsrtombs
fn setpwent
fn shmat
fn mach_error_string
fn if_nametoindex
fn processor_set_policy_enable
fn localeconv
fn setegid
fn umask
fn vm_stats
fn OSHostByteOrder
fn host_kernel_version
fn posix_openpt
fn fremovexattr
fn clonefile
fn setservent
fn NSIsSymbolNameDefinedWithHint
fn strcpy
fn aio_read
fn isalnum
fn rename
fn __istype
fn iswhexnumber
fn ferror
fn semaphore_wait
fn semaphore_create
fn getrusage
fn fseek
fn iswctype
fn mach_port_destruct
fn mach_port_kobject_description
fn thread_policy
fn task_dyld_process_info_notify_get
fn _dyld_lookup_and_bind_with_hint
fn kevent
fn NXSwapHostShortToBig
fn thread_assign
fn __maskrune
fn tempnam
fn connect
fn btowc
fn getservbyname
fn getpwent
fn statvfs
fn vm_map_64
fn ctermid
fn newlocale
fn strstr
fn getwc
fn a64l
fn jrand48
fn getwchar
fn gethostent
fn host_get_clock_control
fn task_get_mach_voucher
fn NXSwapLittleShortToHost
fn NSAddLibraryWithSearching
fn mach_memory_object_memory_entry
fn vm_deallocate
fn strtoul
fn task_get_state
fn sigaltstack
fn strerror
fn _kernelrpc_mach_port_allocate_trap
fn kevent64
fn msgctl
fn task_generate_corpse
fn memchr
fn vsnprintf
fn task_register_dyld_set_dyld_state
fn tcflow
fn __isctype
fn isspace
fn flockfile
fn perror
fn seed48
fn iswalnum
fn iswupper
fn sigfillset
fn iswideogram
fn sched_get_priority_min
fn lcong48
fn sem_unlink
fn unlink
fn getlogin_r
fn munmap
fn host_security_set_task_token
fn vdprintf
fn host_processors
fn thread_get_special_port
fn task_test_sync_upcall
fn __wcwidth
fn mach_port_allocate_qos
fn getuid
fn host_priv_statistics
fn vfork
fn processor_set_tasks_with_flavor
fn quick_exit
fn posix_spawnp
fn processor_set_destroy
fn vm_region_recurse
fn vm_copy
fn NSLookupAndBindSymbol
fn NSSymbolDefinitionNameInObjectFileImage
fn atomic_flag_test_and_set_explicit
fn host_check_multiuser_mode
fn thread_set_special_port
fn _setjmp
fn task_suspend
fn NSVersionOfRunTimeLibrary
fn setstate
fn getnetent
fn iswpunct
fn uname
fn task_terminate
fn task_threads
fn rand_r
fn host_info
fn _OSSwapInt32
fn memmove
fn strchr
fn fwide
fn pthread_getconcurrency
fn posix_spawn_file_actions_init
fn mknod
fn wctomb
fn mkdirat
fn realloc
fn strspn
fn islower
fn clock_settime
fn wctype
fn socketpair
fn alphasort
fn sem_close
fn vm_region
fn vm_allocate
fn fetestexcept
fn mktime
fn iconv
fn crypt
fn fread
fn srandom
fn uselocale
fn inet_ntoa
fn getitimer
fn processor_set_tasks
fn task_test_async_upcall_propagation
fn mach_generate_activity_id
fn getservbyport
fn processor_set_default
fn mach_zone_info_for_zone
fn NXSwapLittleIntToHost
fn getsockopt
fn random
fn getxattr
fn processor_set_stack_usage
fn vm_read_list
fn tolower
fn wcscspn
fn sem_getvalue
fn flistxattr
fn ungetc
fn tcsetattr
fn task_purgable_info
fn ffs
fn gmtime
fn NSSymbolDefinitionCountInObjectFileImage
fn clock_getres
fn closedir
fn wcsftime
fn faccessat
fn lchown
fn mach_port_kernel_object
fn clock_set_time
fn rewinddir
fn mach_make_memory_entry
fn thread_sample
fn task_resume
fn fputws
fn openlog
fn sigwait
fn mig_strncpy
fn setxattr
fn open_memstream
fn NSSymbolReferenceCountInObjectFileImage
fn _OSWriteInt32
fn tmpnam
fn setregid
fn iswphonogram
fn thread_abort_safely
fn lstat
fn shmdt
fn thread_create
fn realpath
fn mkdir
fn pathconf
fn select
fn posix_spawn_file_actions_addopen
fn utimensat
fn task_identity_token_get_task_port
fn semaphore_timedwait_signal
fn NSVersionOfLinkTimeLibrary
fn gethostbyname
fn towctrans
fn funlockfile
fn mach_port_allocate_full
fn ftruncate
fn recvfrom
fn processor_control
fn fgetxattr
fn _dyld_present
fn act_set_state
fn clonefileat
fn __darwin_fd_set
fn panic_init
fn mach_msg_receive
fn mbsinit
fn ldiv
fn sigrelse
fn isupper
fn atomic_signal_fence
fn ftrylockfile
fn _OSSwapInt64
fn socket
fn grantpt
fn posix_spawn
fn vm_protect
fn vm_write
fn _kernelrpc_mach_port_deallocate_trap
fn task_swap_exception_ports
fn swtch
fn mach_msg_destroy
fn setlocale
fn getenv
fn lrand48
fn task_set_policy
fn _kernelrpc_mach_vm_map_trap
fn thread_set_state
fn ftello
fn cfsetospeed
fn voucher_mach_msg_adopt
fn NXHostByteOrder
fn NSUnLinkModule
fn _dyld_lookup_and_bind_fully
fn execvp
fn mach_port_guard_with_flags
fn vm_mapped_pages_info
fn wcsnrtombs
fn mmap
fn NSCreateObjectFileImageFromMemory
fn getc
fn sigpending
fn strpbrk
fn setsid
fn task_info
fn clock_set_attributes
fn task_name_for_pid
fn asctime
fn popen
fn shm_unlink
fn NXSwapInt
fn setnetent
fn fputs
fn wait
fn vfwprintf
fn wmemcmp
fn chown
fn _OSSwapInt16
fn recv
fn getsid
fn mach_port_get_attributes
fn posix_spawnattr_setsigdefault
fn div
fn removexattr
fn freelocale
fn task_set_port_space
fn mig_strncpy_zerofill
fn endnetent
fn sockatmark
fn thread_info
fn regexec
fn chmod
fn mach_port_move_member
fn thread_get_state
fn getprotobyname
fn regfree
fn task_assign
fn sigsuspend
fn iswascii
fn shmctl
fn _OSWriteInt16
fn fchownat
fn task_create
fn host_request_notification
fn vm_msync
fn host_lockgroup_info
fn NSAddImage
fn __math_errhandling
fn dlclose
fn getgrnam_r
fn NSSymbolReferenceNameInObjectFileImage
fn macx_swapon
fn setjmp
fn srand48
fn gmtime_r
fn pthread_sigmask
fn if_indextoname
fn _kernelrpc_mach_port_insert_member_trap
fn NSLookupSymbolInModule
fn _kernelrpc_mach_vm_deallocate_trap
fn toupper
fn NXSwapLittleLongToHost
fn NXSwapHostLongToLittle
fn tcsetpgrp
fn tcsendbreak
fn setpgrp
fn host_get_atm_diagnostic_flag
fn getnameinfo
fn semaphore_signal_all
fn _dyld_shared_cache_contains_path
fn fgetwc
fn _kernelrpc_mach_port_unguard_trap
fn fesetenv
fn getnetbyname
fn encrypt
fn mach_thread_self
fn host_page_size
fn clock_sleep_trap
fn fegetexceptflag
fn unsetenv
fn strncpy
fn _dyld_image_containing_address
fn iswblank
fn _dyld_all_twolevel_modules_prebound
fn strtoll
fn putenv
fn iswgraph
fn pthread_kill
fn wmemcpy
fn task_set_corpse_forking_behavior
fn __sputc
fn truncate
fn mach_port_deallocate
fn macx_swapoff
fn llabs
fn getaddrinfo
fn aio_return
fn getpwuid_r
fn debug_control_port_for_pid
fn mig_deallocate
fn wcstoimax
fn wmemmove
fn kqueue
fn _NSGetExecutablePath
fn NXSwapLongLong
fn creat
fn posix_spawn_file_actions_addfchdir
fn posix_spawnattr_getflags
fn listxattr
fn strsignal
fn setlogmask
fn getc_unlocked
fn fegetround
fn vm_machine_attribute
fn aligned_alloc
fn mach_ports_lookup
fn vm_remap_new
fn mach_port_space_info
fn ttyname_r
fn tzset
fn iswnumber
fn NXSwapFloat
fn NSIsSymbolNameDefinedInImage
fn fclonefileat
fn getchar
fn ___tolower
fn l64a
fn iscntrl
fn malloc
fn posix_spawnattr_setsigmask
fn lio_listio
fn abort
fn sigpause
fn regcomp
fn host_set_exception_ports
fn sendto
fn duplocale
fn wcsspn
fn mig_put_reply_port
fn task_zone_info
fn mbsnrtowcs
fn fgetpos
fn wcscpy
fn _OSReadInt32
fn mrand48
fn wcstoll
fn ungetwc
fn iswspecial
fn task_set_state
fn thread_get_exception_ports
fn strcat
fn confstr
fn thread_set_policy
fn task_set_phys_footprint_limit
fn NSLibraryNameForModule
fn __tolower
fn execve
fn host_get_UNDServer
fn pthread_testcancel
fn _kernelrpc_mach_port_get_attributes_trap
fn NXSwapHostLongLongToLittle
fn __svfscanf
fn getservent
fn __srget
fn isatty
fn task_for_pid
fn futimens
fn mach_vm_region_info_64
fn sigaction
fn wcpncpy
fn cfgetospeed
fn fork
fn fileno
fn task_sample
fn pthread_setconcurrency
fn mach_memory_object_memory_entry_64
fn processor_exit
fn mach_port_get_srights
fn thread_assign_default
fn etap_trace_thread
fn macx_backing_store_suspend
fn mig_reply_setup
fn fseeko
fn task_swap_mach_voucher
fn clock_set_res
fn fflush
fn mlockall
fn freopen
fn fstat
fn sigignore
fn _OSReadSwapInt64
fn host_set_UNDServer
fn fesetexceptflag
fn mach_voucher_extract_attr_recipe_trap
fn asctime_r
fn mblen
fn voucher_mach_msg_set
fn NXSwapLittleLongLongToHost
fn times
fn lock_set_create
fn memcpy
fn fmemopen
fn host_get_boot_info
fn getdate
fn task_policy
fn task_self_trap
fn time
fn host_statistics
fn mbtowc
fn sigaddset
fn putwchar
fn vswscanf
fn accept
fn pause
fn setpgid
fn wcstoul
fn initstate
fn posix_spawnattr_getsigmask
fn clearerr
fn ptsname
fn wcswidth
fn atoi
fn posix_spawn_file_actions_addchdir
fn symlinkat
fn getcwd
fn wcsdup
fn posix_spawn_file_actions_destroy
fn getprotoent
fn aio_write
fn inet_pton
fn aio_suspend
fn seteuid
fn host_register_well_known_mach_voucher_attr_manager
fn clock_gettime
fn fsetpos
fn endprotoent
fn getgid
fn msync
fn fgetws
fn sendmsg
fn vsprintf
fn task_get_emulation_vector
fn vm_wire
fn sem_post
fn task_set_mach_voucher
fn mach_port_set_attributes
fn munlock
fn msgsnd
fn processor_set_max_priority
fn vm_map_page_query
fn NXSwapLong
fn strnlen
fn mach_port_destroy
fn mach_port_set_seqno
fn _kernelrpc_mach_port_type_trap
fn putc_unlocked
fn readlink
fn thread_policy_set
fn _dyld_launched_prebound
fn task_create_identity_token
fn _dyld_image_count
fn strtoumax
fn posix_spawnattr_setflags
fn mprotect
fn sigprocmask
fn posix_spawn_file_actions_addclose
fn NXSwapBigShortToHost
fn task_get_dyld_image_infos
fn NSAddressOfSymbol
fn dlopen
fn lseek
fn sigismember
fn __darwin_fd_clr
fn mkfifo
fn fnmatch
fn swab
fn ctime_r
fn inet_ntop
fn task_register_hardened_exception_handler
fn kmod_get_info
fn vwscanf
fn wcscoll
fn readdir
fn fpathconf
fn mach_vm_reclaim_update_kernel_accounting_trap
fn task_set_info
fn iswcntrl
fn semget
fn NSLinkModule
fn wcstombs
fn strerror_r
fn vwprintf
fn _tlv_bootstrap
fn mach_port_set_context
fn host_create_mach_voucher_trap
fn kill
fn __vsnprintf_chk
fn msgrcv
fn mach_msg_send
fn __swbuf
fn NSGetSectionDataInObjectFileImage
fn putc
fn tmpfile
fn setitimer
fn host_get_exception_ports
fn lock_set_destroy
fn NXSwapBigIntToHost
fn hcreate
fn setpriority
fn posix_spawn_file_actions_adddup2
fn __darwin_check_fd_set
fn mknodat
fn host_default_memory_manager
fn memset
fn dirfd
fn thread_abort
fn hdestroy
fn linkat
fn task_get_exc_guard_behavior
fn closelog
fn thread_convert_thread_state
fn wcstoull
fn renameat
fn srand
fn iswxdigit
fn memccpy
fn strncasecmp
fn mach_port_get_set_status
fn pwrite
fn msgget
fn host_processor_info
fn isprint
fn main
  bb0 bb0
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    call symbol(summarize)(struct(len=2), 3, true) cc=C tail=false
    alloca Virtual { id: 5, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 5, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 4, bank: General, size_bits: 64 }
    br
  bb1 bb1
    bitcast Virtual { id: 7, bank: General, size_bits: 64 }, Virtual { id: 5, bank: General, size_bits: 64 }
    load Virtual { id: 8, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 7, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 8, bank: General, size_bits: 64 }
    call symbol(summarize)(struct(len=2), 7, false) cc=C tail=false
    alloca Virtual { id: 11, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 11, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 10, bank: General, size_bits: 64 }
    br
  bb2 bb2
    bitcast Virtual { id: 13, bank: General, size_bits: 64 }, Virtual { id: 11, bank: General, size_bits: 64 }
    load Virtual { id: 14, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 13, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 14, bank: General, size_bits: 64 }
    call symbol(add)(5, 2) cc=C tail=false
    br
  bb3 bb3
    intrinsic.call symbol(intrinsic.println), Virtual { id: 16, bank: General, size_bits: 64 }
    ret
fn add
  bb0 bb0
    alloca Virtual { id: 18, bank: General, size_bits: 64 }, 1
    add Virtual { id: 19, bank: General, size_bits: 64 }, symbol(local.1), symbol(local.2)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 18, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 19, bank: General, size_bits: 64 }
    load Virtual { id: 21, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 18, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn summarize
  bb0 bb0
    alloca Virtual { id: 22, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 23, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 23, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(local.1)
    bitcast Virtual { id: 25, bank: General, size_bits: 64 }, Virtual { id: 23, bank: General, size_bits: 64 }
    load Virtual { id: 26, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 25, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.format), Virtual { id: 26, bank: General, size_bits: 64 }, symbol(local.2), symbol(local.3)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 22, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 27, bank: General, size_bits: 64 }
    load Virtual { id: 29, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 22, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(16), address_space: None, pre_indexed: false, post_indexed: false })
    ret


Symbols:
  main                             0x00000000
  add                              0x00000204
  summarize                        0x00000264

Text relocations:
  offset=0x00000010 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000001c kind=CallRel32 symbol=printf addend=0
  offset=0x00000020 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000002c kind=CallRel32 symbol=printf addend=0
  offset=0x00000030 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000003c kind=CallRel32 symbol=printf addend=0
  offset=0x00000040 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000004c kind=CallRel32 symbol=printf addend=0
  offset=0x00000060 kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x000000ec kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000104 kind=CallRel32 symbol=printf addend=0
  offset=0x00000118 kind=Aarch64AdrpAdd symbol=__const_data_1 addend=0
  offset=0x000001a4 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000001bc kind=CallRel32 symbol=printf addend=0
  offset=0x000001d4 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000001ec kind=CallRel32 symbol=printf addend=0
  offset=0x000002f0 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000320 kind=CallRel32 symbol=snprintf addend=0
  offset=0x00000338 kind=CallRel32 symbol=malloc addend=0
  offset=0x0000034c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000037c kind=CallRel32 symbol=snprintf addend=0

.text (1028 bytes):
  00000000  ff c3 05 d1 fd 7b 16 a9  fd 03 00 91 1f 20 03 d5 
  00000010  00 00 00 90 00 00 00 91  00 40 00 91 00 00 00 94 
  00000020  00 00 00 90 00 00 00 91  00 c0 00 91 00 00 00 94 
  00000030  00 00 00 90 00 00 00 91  00 80 01 91 00 00 00 94 
  00000040  00 00 00 90 00 00 00 91  00 60 02 91 00 00 00 94 
  00000050  e0 03 00 91 00 60 04 91  f1 03 00 91 31 22 04 91 
  00000060  10 00 00 90 10 02 00 91  e9 03 11 aa 30 01 00 f9 
  00000070  b0 00 80 d2 10 00 a0 f2  10 00 c0 f2 10 00 e0 f2 
  00000080  e9 03 11 aa 29 21 00 91  30 01 00 f9 e1 03 11 aa 
  00000090  62 00 80 d2 23 00 80 d2  73 00 00 94 f0 03 00 91 
  000000a0  10 62 04 91 f0 1b 00 f9  f0 03 00 91 10 e2 04 91 
  000000b0  f0 1f 00 f9 f1 1f 40 f9  f0 8f 40 f9 e9 03 11 aa 
  000000c0  30 01 00 f9 f0 93 40 f9  e9 03 11 aa 29 21 00 91 
  000000d0  30 01 00 f9 01 00 00 14  f0 1f 40 f9 f0 27 00 f9 
  000000e0  f0 27 40 f9 11 02 40 f9  f1 2b 00 f9 00 00 00 90 
  000000f0  00 00 00 91 00 80 02 91  e1 2b 40 f9 f0 2b 40 f9 
  00000100  f0 03 00 f9 00 00 00 94  e0 03 00 91 00 a0 04 91 
  00000110  f1 03 00 91 31 22 04 91  10 00 00 90 10 02 00 91 
  00000120  e9 03 11 aa 30 01 00 f9  90 00 80 d2 10 00 a0 f2 
  00000130  10 00 c0 f2 10 00 e0 f2  e9 03 11 aa 29 21 00 91 
  00000140  30 01 00 f9 e1 03 11 aa  e2 00 80 d2 03 00 80 d2 
  00000150  45 00 00 94 f0 03 00 91  10 a2 04 91 f0 33 00 f9 
  00000160  f0 03 00 91 10 22 05 91  f0 37 00 f9 f1 37 40 f9 
  00000170  f0 97 40 f9 e9 03 11 aa  30 01 00 f9 f0 9b 40 f9 
  00000180  e9 03 11 aa 29 21 00 91  30 01 00 f9 01 00 00 14 
  00000190  f0 37 40 f9 f0 3f 00 f9  f0 3f 40 f9 11 02 40 f9 
  000001a0  f1 43 00 f9 00 00 00 90  00 00 00 91 00 c0 02 91 
  000001b0  e1 43 40 f9 f0 43 40 f9  f0 03 00 f9 00 00 00 94 
  000001c0  a0 00 80 d2 41 00 80 d2  0f 00 00 94 e0 4b 00 f9 
  000001d0  01 00 00 14 00 00 00 90  00 00 00 91 00 00 03 91 
  000001e0  e1 4b 40 f9 f0 4b 40 f9  f0 03 00 f9 00 00 00 94 
  000001f0  bf 03 00 91 fd 7b 56 a9  ff c3 05 91 00 00 80 d2 
  00000200  c0 03 5f d6 ff 43 01 d1  fd 7b 04 a9 fd 03 00 91 
  00000210  e0 17 00 f9 e1 1b 00 f9  1f 20 03 d5 f0 03 00 91 
  00000220  10 e2 00 91 f0 03 00 f9  f0 17 40 f9 f1 1b 40 f9 
  00000230  10 02 11 8b f0 07 00 f9  f1 03 40 f9 f0 07 40 f9 
  00000240  30 02 00 f9 f0 03 40 f9  11 02 40 f9 f1 0f 00 f9 
  00000250  e0 0f 40 f9 bf 03 00 91  fd 7b 44 a9 ff 43 01 91 
  00000260  c0 03 5f d6 ff c3 03 d1  fd 7b 0e a9 fd 03 00 91 
  00000270  e0 4b 00 f9 e9 03 01 aa  30 01 40 f9 f0 3b 00 f9 
  00000280  e9 03 01 aa 29 21 00 91  30 01 40 f9 f0 3f 00 f9 
  00000290  e2 43 00 f9 e3 23 02 39  1f 20 03 d5 f0 03 00 91 
  000002a0  10 e2 02 91 f0 13 00 f9  f0 03 00 91 10 22 03 91 
  000002b0  f0 17 00 f9 f1 17 40 f9  f0 3b 40 f9 e9 03 11 aa 
  000002c0  30 01 00 f9 f0 3f 40 f9  e9 03 11 aa 29 21 00 91 
  000002d0  30 01 00 f9 f0 17 40 f9  f0 1f 00 f9 f0 1f 40 f9 
  000002e0  11 02 40 f9 f1 23 00 f9  00 00 80 d2 01 00 80 d2 
  000002f0  02 00 00 90 42 00 00 91  42 40 03 91 e3 23 40 f9 
  00000300  f0 23 40 f9 f0 03 00 f9  e4 43 40 f9 f0 43 40 f9 
  00000310  f0 07 00 f9 e5 23 42 39  f0 23 42 39 f0 0b 00 f9 
  00000320  00 00 00 94 f0 03 00 aa  f0 53 00 f9 10 06 00 91 
  00000330  f0 27 00 f9 e0 03 10 aa  00 00 00 94 e9 03 00 aa 
  00000340  e0 03 09 aa e1 27 40 f9  e9 27 00 f9 02 00 00 90 
  00000350  42 00 00 91 42 40 03 91  e3 23 40 f9 f0 23 40 f9 
  00000360  f0 03 00 f9 e4 43 40 f9  f0 43 40 f9 f0 07 00 f9 
  00000370  e5 23 42 39 f0 23 42 39  f0 0b 00 f9 00 00 00 94 
  00000380  e9 27 40 f9 e9 4f 00 f9  f1 13 40 f9 f0 4f 40 f9 
  00000390  e9 03 11 aa 30 01 00 f9  f0 53 40 f9 e9 03 11 aa 
  000003a0  29 21 00 91 30 01 00 f9  f1 13 40 f9 e9 03 11 aa 
  000003b0  30 01 40 f9 f0 57 00 f9  e9 03 11 aa 29 21 00 91 
  000003c0  30 01 40 f9 f0 5b 00 f9  f0 03 00 91 10 a2 02 91 
  000003d0  f0 2f 00 f9 f1 4b 40 f9  f0 57 40 f9 e9 03 11 aa 
  000003e0  30 01 00 f9 f0 5b 40 f9  e9 03 11 aa 29 21 00 91 
  000003f0  30 01 00 f9 bf 03 00 91  fd 7b 4e a9 ff c3 03 91 
  00000400  c0 03 5f d6 

.rodata (238 bytes):
  00000000  61 6c 70 68 61 00 62 65  74 61 00 00 00 00 00 00 
  00000010  54 75 74 6f 72 69 61 6c  3a 20 33 33 5f 6e 61 6d 
  00000020  65 64 5f 61 72 67 73 2e  66 70 0a 00 00 00 00 00 
  00000030  46 6f 63 75 73 3a 20 4e  61 6d 65 64 20 61 72 67 
  00000040  75 6d 65 6e 74 73 20 69  6e 20 66 75 6e 63 74 69 
  00000050  6f 6e 20 63 61 6c 6c 73  0a 00 00 00 00 00 00 00 
  00000060  45 78 70 65 63 74 61 74  69 6f 6e 3a 20 6b 65 79 
  00000070  77 6f 72 64 20 61 72 67  75 6d 65 6e 74 73 20 63 
  00000080  61 6e 20 62 65 20 72 65  6f 72 64 65 72 65 64 0a 
  00000090  00 00 00 00 00 00 00 00  0a 00 00 00 00 00 00 00 
  000000a0  66 69 72 73 74 3a 20 25  73 0a 00 00 00 00 00 00 
  000000b0  73 65 63 6f 6e 64 3a 20  25 73 0a 00 00 00 00 00 
  000000c0  61 64 64 3a 20 25 6c 6c  64 0a 00 00 00 00 00 00 
  000000d0  6c 61 62 65 6c 3d 25 73  20 63 6f 75 6e 74 3d 25 
  000000e0  6c 6c 64 20 61 63 74 69  76 65 3d 25 64 00 
