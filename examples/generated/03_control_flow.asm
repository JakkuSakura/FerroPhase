fp-native dump: format=MachO arch=Aarch64 entry=0x0

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
global TEMP ty=I64 constant=true initializer=Some(Bytes([25, 0, 0, 0, 0, 0, 0, 0]))
global __const_data_0 ty=Array(I8, 4) constant=true initializer=Some(Bytes([104, 111, 116, 0]))
global __const_data_1 ty=Array(I8, 5) constant=true initializer=Some(Bytes([119, 97, 114, 109, 0]))
global __const_data_2 ty=Array(I8, 5) constant=true initializer=Some(Bytes([99, 111, 108, 100, 0]))
global IS_SUNNY ty=I1 constant=true initializer=Some(Bytes([1]))
global IS_WARM ty=I1 constant=true initializer=Some(Bytes([1]))
global __const_data_3 ty=Array(I8, 8) constant=true initializer=Some(Bytes([111, 117, 116, 100, 111, 111, 114, 0]))
global __const_data_4 ty=Array(I8, 7) constant=true initializer=Some(Bytes([105, 110, 100, 111, 111, 114, 0]))
global SCORE ty=I64 constant=true initializer=Some(Bytes([85, 0, 0, 0, 0, 0, 0, 0]))
global __const_data_5 ty=Array(I8, 2) constant=true initializer=Some(Bytes([65, 0]))
global __const_data_6 ty=Array(I8, 2) constant=true initializer=Some(Bytes([66, 0]))
global __const_data_7 ty=Array(I8, 2) constant=true initializer=Some(Bytes([67, 0]))
global __const_data_8 ty=Array(I8, 2) constant=true initializer=Some(Bytes([70, 0]))
global __const_data_9 ty=Array(I8, 5) constant=true initializer=Some(Bytes([104, 105, 103, 104, 0]))
global __const_data_10 ty=Array(I8, 7) constant=true initializer=Some(Bytes([109, 101, 100, 105, 117, 109, 0]))
global __const_data_11 ty=Array(I8, 4) constant=true initializer=Some(Bytes([108, 111, 119, 0]))
fn getentropy
fn processor_set_statistics
fn nrand48
fn task_inspect
fn mach_port_request_notification
fn mach_port_set_context
fn _kernelrpc_mach_port_move_member_trap
fn fileno
fn host_register_mach_voucher_attr_manager
fn thread_assign
fn strtoll
fn getsockname
fn getpgid
fn mach_msg_overwrite
fn memcmp
fn putwc
fn task_policy_set
fn task_get_exception_ports_info
fn task_map_kcdata_object_64
fn vm_mapped_pages_info
fn _kernelrpc_mach_port_guard_trap
fn munmap
fn isgraph
fn task_set_emulation
fn debug_control_port_for_pid
fn _kernelrpc_mach_port_type_trap
fn fegetexceptflag
fn ___toupper
fn fmemopen
fn memmove
fn insque
fn sem_getvalue
fn close
fn munlockall
fn semget
fn mach_port_swap_guard
fn ptsname
fn nanosleep
fn wcsncat
fn vdprintf
fn wctrans
fn sem_init
fn encrypt
fn ualarm
fn thread_get_assignment
fn mkdirat
fn host_create_mach_voucher_trap
fn strpbrk
fn __srget
fn fread
fn NSNameOfSymbol
fn fputc
fn renameat
fn globfree
fn ttyname
fn msync
fn strcpy
fn getnetbyaddr
fn ferror
fn ctermid
fn realpath
fn iconv_close
fn host_get_exception_ports
fn voucher_mach_msg_adopt
fn strchr
fn iswrune
fn mknod
fn tcsendbreak
fn msgctl
fn setxattr
fn thread_depress_abort
fn iswspace
fn labs
fn fgetpos
fn nl_langinfo
fn _OSReadInt64
fn task_set_special_port
fn setpriority
fn processor_control
fn host_security_create_task_token
fn sched_get_priority_max
fn task_get_assignment
fn mach_port_is_connection_for_service
fn getdate
fn statvfs
fn host_get_io_main
fn putchar_unlocked
fn getcwd
fn wcscmp
fn host_get_special_port
fn mach_port_set_seqno
fn cfgetospeed
fn thread_get_exception_ports
fn NXSwapBigLongLongToHost
fn __assert_rtn
fn imaxdiv
fn popen
fn semaphore_wait
fn NXSwapLittleLongToHost
fn getpwnam_r
fn NXSwapBigShortToHost
fn setsid
fn aio_error
fn NSLibraryNameForModule
fn mach_port_deallocate
fn mach_memory_object_memory_entry_64
fn NSLookupAndBindSymbol
fn _dyld_bind_fully_image_containing_address
fn atoi
fn fwide
fn chmod
fn _kernelrpc_mach_port_unguard_trap
fn task_assign_default
fn clonefile
fn __isctype
fn mach_port_assert_attributes
fn wcscspn
fn setstate
fn memccpy
fn endhostent
fn mach_vm_region_info_64
fn basename
fn strerror_r
fn atomic_thread_fence
fn mach_port_kobject
fn host_info
fn kill
fn inet_ntop
fn clock_get_res
fn socket
fn __darwin_fd_isset
fn unsetenv
fn _OSReadSwapInt16
fn vm_write
fn wcscat
fn getgid
fn stat
fn getgrgid
fn gai_strerror
fn execvp
fn sigrelse
fn quick_exit
fn ___runetype
fn lldiv
fn towctrans
fn msgrcv
fn processor_set_policy_disable
fn vm_region_recurse_64
fn mach_port_get_srights
fn wcschr
fn localeconv
fn srandom
fn gmtime
fn _OSSwapInt64
fn lseek
fn _exit
fn memset
fn __error
fn setpgrp
fn munlock
fn mkfifo
fn strcasecmp
fn getgroups
fn symlink
fn asctime_r
fn aligned_alloc
fn strtoul
fn sockatmark
fn host_security_set_task_token
fn posix_spawnattr_getflags
fn task_get_special_port
fn hdestroy
fn clock_set_attributes
fn getpgrp
fn thread_get_special_port
fn putc_unlocked
fn strcat
fn wcsspn
fn connect
fn fseeko
fn wcstoull
fn thread_set_special_port
fn strcspn
fn wcsrtombs
fn task_assign
fn mach_port_destruct
fn lrand48
fn vm_deallocate
fn dup
fn towupper
fn mach_port_guard
fn mach_port_get_service_port_info
fn getpriority
fn ctime_r
fn getchar
fn regfree
fn OSHostByteOrder
fn mach_port_kobject_description
fn putc
fn rename
fn host_lockgroup_info
fn NXSwapHostShortToLittle
fn _dyld_all_twolevel_modules_prebound
fn __vsnprintf_chk
fn kqueue
fn fork
fn clock
fn iswascii
fn wmemcpy
fn creat
fn wcstombs
fn kmod_get_info
fn host_virtual_physical_table_info
fn bind
fn wcrtomb
fn tmpfile
fn getprotoent
fn ffs
fn mig_get_reply_port
fn task_purgable_info
fn pread
fn _kernelrpc_mach_vm_allocate_trap
fn macx_triggers
fn _kernelrpc_mach_port_request_notification_trap
fn task_register_hardened_exception_handler
fn NXSwapFloat
fn NSVersionOfRunTimeLibrary
fn __NDR_convert__mig_reply_error_t
fn iconv
fn host_request_notification
fn NXSwapHostIntToBig
fn NXSwapHostLongLongToLittle
fn NSUnLinkModule
fn abort
fn kext_request
fn fegetround
fn processor_set_policy_enable
fn semaphore_timedwait
fn mach_msg_send
fn NXSwapLittleShortToHost
fn semaphore_signal_all
fn removexattr
fn voucher_mach_msg_revert
fn _kernelrpc_mach_port_extract_member_trap
fn processor_exit
fn _dyld_get_image_header
fn NSInstallLinkEditErrorHandlers
fn NSAddLibrary
fn mblen
fn __toupper
fn task_identity_token_get_task_port
fn strtok
fn getsid
fn wcsxfrm
fn macx_backing_store_suspend
fn flistxattr
fn task_get_dyld_image_infos
fn thread_get_state
fn posix_spawnattr_setflags
fn umask
fn processor_set_max_priority
fn thread_adopt_exception_handler
fn sigemptyset
fn wcsftime
fn tcgetattr
fn wmemchr
fn processor_set_threads
fn __darwin_check_fd_set_overflow
fn mach_port_extract_member
fn kmod_create
fn NXSwapLittleIntToHost
fn task_suspend2
fn dlerror
fn sighold
fn macx_swapon
fn mbsinit
fn getrlimit
fn task_terminate
fn host_register_well_known_mach_voucher_attr_manager
fn freopen
fn islower
fn posix_spawn_file_actions_addchdir
fn div
fn __darwin_fd_set
fn iswctype
fn getpwent
fn task_generate_corpse
fn host_kernel_version
fn raise
fn getsockopt
fn setservent
fn task_zone_info
fn cfsetospeed
fn NSLookupSymbolInImage
fn _dyld_present
fn vm_msync
fn macx_backing_store_recovery
fn kevent
fn clonefileat
fn mach_make_memory_entry_64
fn getrusage
fn srand
fn task_set_info
fn srand48
fn task_set_port_space
fn NSCreateObjectFileImageFromFile
fn task_unregister_dyld_image_infos
fn sigsuspend
fn cfgetispeed
fn atomic_signal_fence
fn stpcpy
fn dlsym
fn faccessat
fn btowc
fn posix_spawn_file_actions_addclose
fn read
fn __istype
fn mlockall
fn shmctl
fn posix_spawnattr_getsigmask
fn setuid
fn mknodat
fn strftime
fn mach_msg
fn mig_put_reply_port
fn vm_remap_new
fn fsetpos
fn atomic_flag_test_and_set
fn if_indextoname
fn task_set_ras_pc
fn NSIsSymbolDefinedInObjectFileImage
fn recv
fn openlog
fn strncat
fn fopen
fn getgrnam
fn sysconf
fn processor_get_assignment
fn mach_memory_info
fn vswprintf
fn getservent
fn atoll
fn utimensat
fn processor_set_tasks
fn mach_memory_object_memory_entry
fn waitid
fn write
fn mkdir
fn mach_port_set_attributes
fn setlocale
fn _OSWriteInt64
fn strdup
fn thread_set_policy
fn host_get_boot_info
fn sem_close
fn mbrtowc
fn lchown
fn host_processor_sets
fn mach_thread_self
fn NSIsSymbolNameDefined
fn semaphore_wait_signal
fn times
fn vm_behavior_set
fn mach_port_allocate_full
fn NSModuleForSymbol
fn __sigbits
fn act_set_state
fn __darwin_check_fd_set
fn seed48
fn fwrite
fn setgrfile
fn aio_cancel
fn getservbyport
fn sendmsg
fn __swbuf
fn seteuid
fn thread_switch
fn fchown
fn NXSwapDouble
fn tcsetpgrp
fn task_suspend
fn sched_yield
fn mach_port_insert_right
fn clock_sleep_trap
fn sigprocmask
fn abs
fn posix_spawnp
fn NSGetSectionDataInObjectFileImage
fn iswprint
fn __svfscanf
fn vsscanf
fn strncasecmp
fn wcsstr
fn _longjmp
fn iswlower
fn shm_unlink
fn vm_stats
fn _OSWriteInt32
fn task_set_phys_footprint_limit
fn endgrent
fn getenv
fn mktime
fn isprint
fn localtime
fn tempnam
fn posix_spawnattr_setsigmask
fn fesetexceptflag
fn getpwuid_r
fn tcflush
fn vm_region_recurse
fn mach_port_kernel_object
fn mach_port_construct
fn puts
fn pthread_kill
fn vsprintf
fn clock_getres
fn uname
fn thread_assign_default
fn thread_set_mach_voucher
fn mach_zone_info_for_zone
fn mkstemp
fn wcpncpy
fn task_get_exc_guard_behavior
fn _dyld_image_containing_address
fn task_policy
fn host_get_clock_service
fn free
fn vm_copy
fn _tlv_bootstrap
fn feclearexcept
fn toascii
fn newlocale
fn NXSwapHostLongToBig
fn NSSymbolReferenceNameInObjectFileImage
fn seekdir
fn posix_spawnattr_destroy
fn shmat
fn task_self_trap
fn task_resume
fn sigaction
fn getgrent
fn mach_port_space_info
fn NSCreateObjectFileImageFromMemory
fn act_get_state
fn NSSymbolDefinitionCountInObjectFileImage
fn localtime_r
fn ftruncate
fn task_dyld_process_info_notify_deregister
fn voucher_mach_msg_set
fn aio_write
fn setkey
fn nice
fn vswscanf
fn NXSwapLongLong
fn closedir
fn listxattr
fn strncpy
fn aio_read
fn llabs
fn isdigit
fn isblank
fn setenv
fn socketpair
fn mbstowcs
fn sem_trywait
fn sleep
fn getlogin_r
fn fesetenv
fn mbtowc
fn wcsncasecmp
fn mprotect
fn opendir
fn _OSReadInt16
fn mig_allocate
fn host_default_memory_manager
fn processor_set_info
fn task_set_emulation_vector
fn mach_vm_wire
fn vm_region_64
fn task_register_dyld_get_process_state
fn mach_host_self
fn fclose
fn task_dyld_process_info_notify_get
fn stpncpy
fn task_swap_mach_voucher
fn mach_vm_region_info
fn pthread_setconcurrency
fn tcgetsid
fn _OSWriteSwapInt32
fn clearerr
fn mach_port_rename
fn _kernelrpc_mach_port_get_attributes_trap
fn vwscanf
fn wmemcmp
fn task_info
fn NXSwapShort
fn NXSwapHostLongToLittle
fn wcstoimax
fn getwchar
fn clock_gettime
fn host_get_multiuser_config_flags
fn mach_port_set_mscount
fn sem_post
fn vfprintf
fn wcstoul
fn confstr
fn vm_map_exec_lockdown
fn fsetxattr
fn wcscpy
fn task_register_dyld_shared_cache_image_info
fn dlclose
fn NSLinkModule
fn mbsrtowcs
fn dirfd
fn NSLookupAndBindSymbolWithHint
fn strndup
fn time
fn inet_addr
fn getnetbyname
fn mig_dealloc_reply_port
fn mach_ports_lookup
fn task_set_policy
fn thread_suspend
fn NXSwapInt
fn rand_r
fn aio_return
fn fstatvfs
fn msgsnd
fn macx_swapoff
fn sigaltstack
fn mach_port_unguard
fn host_set_atm_diagnostic_flag
fn _dyld_lookup_and_bind_with_hint
fn ispunct
fn setjmp
fn processor_set_tasks_with_flavor
fn task_swap_exception_ports
fn lio_listio
fn wcscasecmp
fn host_page_size
fn futimens
fn alphasort
fn listen
fn host_processor_set_priv
fn mach_port_dnrequest_info
fn tolower
fn remque
fn killpg
fn mach_port_mod_refs
fn strtol
fn _OSWriteSwapInt16
fn dlopen
fn tmpnam
fn task_threads
fn task_set_exc_guard_behavior
fn fegetenv
fn vfwscanf
fn pathconf
fn task_test_async_upcall_propagation
fn mach_port_extract_right
fn _kernelrpc_mach_port_allocate_trap
fn posix_spawnattr_setsigdefault
fn pipe
fn fchownat
fn getpid
fn _OSWriteSwapInt64
fn task_create_identity_token
fn inet_pton
fn ungetwc
fn vm_protect
fn vm_read_overwrite
fn strrchr
fn host_reboot
fn fputs
fn ftell
fn strspn
fn mach_port_get_attributes
fn thread_wire
fn putchar
fn ftrylockfile
fn vprintf
fn mach_voucher_deallocate
fn thread_convert_thread_state
fn posix_spawnattr_getsigdefault
fn thread_swap_exception_ports
fn fgets
fn ldiv
fn clock_settime
fn posix_spawnattr_init
fn pause
fn mbrlen
fn readdir_r
fn task_resume2
fn mach_task_is_self
fn mbsnrtowcs
fn mig_deallocate
fn thread_info
fn panic_init
fn NSVersionOfLinkTimeLibrary
fn setgroupent
fn getgrgid_r
fn posix_spawnattr_setpgroup
fn toupper
fn _setjmp
fn strsignal
fn getpwnam
fn clock_set_time
fn endpwent
fn rand
fn posix_openpt
fn posix_spawn_file_actions_addfchdir
fn swtch
fn realloc
fn getservbyname
fn mach_port_names
fn host_statistics
fn task_test_sync_upcall
fn _dyld_image_count
fn host_processor_info
fn uselocale
fn sigpause
fn getdelim
fn _Exit
fn symlinkat
fn timespec_get
fn unlinkat
fn processor_set_policy_control
fn hsearch
fn random
fn open_wmemstream
fn _kernelrpc_mach_port_insert_member_trap
fn calloc
fn getline
fn memchr
fn inet_ntoa
fn tcdrain
fn readdir
fn endnetent
fn fpathconf
fn iswcntrl
fn utimes
fn task_sample
fn iswideogram
fn getpwuid
fn shmget
fn _kernelrpc_mach_port_deallocate_trap
fn mach_port_insert_member
fn remove
fn rewind
fn open_memstream
fn grantpt
fn wcscoll
fn getpeername
fn sem_unlink
fn pthread_key_delete
fn readlinkat
fn feholdexcept
fn posix_madvise
fn lock_set_create
fn getprotobynumber
fn putwchar
fn lstat
fn thread_resume
fn clock_set_res
fn aio_fsync
fn mach_port_allocate_name
fn semop
fn isupper
fn dirname
fn atol
fn vm_region
fn __math_errhandling
fn iswspecial
fn strncmp
fn chdir
fn fnmatch
fn thread_create_running
fn mach_error_string
fn kevent64
fn iswgraph
fn iswpunct
fn lockf
fn _kernelrpc_mach_vm_map_trap
fn telldir
fn host_get_atm_diagnostic_flag
fn mig_reply_setup
fn fclonefileat
fn recvmsg
fn utime
fn getxattr
fn thread_terminate
fn vm_wire
fn processor_start
fn isxdigit
fn ctime
fn getc
fn iswupper
fn host_processors
fn wcsnlen
fn fstat
fn host_priv_statistics
fn wcsdup
fn mach_voucher_extract_attr_recipe_trap
fn gethostent
fn _kernelrpc_mach_port_construct_trap
fn getc_unlocked
fn rewinddir
fn readlink
fn sigdelset
fn memcpy
fn setrlimit
fn recvfrom
fn sendto
fn aio_suspend
fn unlockpt
fn access
fn execv
fn fchmod
fn wmemset
fn task_get_state
fn task_map_corpse_info_64
fn vm_map
fn vm_map_page_query
fn _OSSwapInt16
fn host_set_special_port
fn gethostname
fn semaphore_timedwait_signal
fn iswdigit
fn task_set_exception_ports
fn _kernelrpc_mach_port_mod_refs_trap
fn voucher_mach_msg_clear
fn pclose
fn NXSwapBigLongToHost
fn _dyld_lookup_and_bind
fn getlogin
fn ftok
fn _dyld_get_image_header_containing_address
fn mach_port_type
fn getnetent
fn NXSwapLittleLongLongToHost
fn iscntrl
fn waitpid
fn closelog
fn setbuf
fn sigaddset
fn wcwidth
fn accept
fn regexec
fn send
fn getegid
fn posix_memalign
fn sem_destroy
fn vfork
fn setregid
fn mach_port_allocate
fn freeaddrinfo
fn iswhexnumber
fn getuid
fn host_get_clock_control
fn task_set_state
fn NSAddImage
fn setnetent
fn thread_create
fn fdopendir
fn setegid
fn sem_wait
fn strtoumax
fn wcstok
fn __sputc
fn vm_read
fn wcspbrk
fn fesetround
fn poll
fn posix_spawn_file_actions_destroy
fn semaphore_create
fn thread_set_exception_ports
fn vm_purgable_control
fn execve
fn _dyld_lookup_and_bind_fully
fn posix_spawn
fn processor_set_stack_usage
fn _dyld_shared_cache_contains_path
fn fsync
fn processor_set_create
fn fgetc
fn getppid
fn wcslen
fn isascii
fn atomic_flag_clear
fn strlen
fn regcomp
fn setlogmask
fn wcstoumax
fn swtch_pri
fn NXSwapHostIntToLittle
fn NSLookupSymbolInModule
fn wcstol
fn getaddrinfo
fn mkfifoat
fn posix_spawn_file_actions_addopen
fn fgetwc
fn vscanf
fn malloc
fn hcreate
fn wcstoll
fn ttyname_r
fn strtoimax
fn dup2
fn mlock
fn isatty
fn shmdt
fn host_get_UNDServer
fn vm_machine_attribute
fn __wcwidth
fn strerror
fn regerror
fn setpgid
fn fchmodat
fn wcsncpy
fn mach_make_memory_entry
fn isspace
fn wctomb
fn vm_allocate_cpm
fn wcsnrtombs
fn lock_set_destroy
fn getgrnam_r
fn setgrent
fn _kernelrpc_mach_vm_protect_trap
fn _kernelrpc_mach_vm_purgable_control_trap
fn __tolower
fn fetestexcept
fn if_nametoindex
fn setgid
fn gettimeofday
fn _OSWriteInt16
fn mach_port_allocate_qos
fn fstatat
fn mach_error_type
fn host_set_UNDServer
fn feof
fn sigsetjmp
fn mach_msg_receive
fn host_swap_exception_ports
fn thread_abort_safely
fn clock_sleep
fn thread_policy_get
fn NSSymbolReferenceCountInObjectFileImage
fn setprotoent
fn if_nameindex
fn task_dyld_process_info_notify_register
fn posix_spawnattr_getpgroup
fn wctype
fn pthread_testcancel
fn cfsetispeed
fn mmap
fn ___tolower
fn chown
fn crypt
fn pselect
fn lcong48
fn mach_port_space_basic_info
fn processor_set_default
fn thread_policy
fn mach_port_destroy
fn NXHostByteOrder
fn NXSwapHostLongLongToBig
fn task_register_dyld_set_dyld_state
fn task_set_corpse_forking_behavior
fn getsubopt
fn posix_spawn_file_actions_init
fn NXSwapHostShortToBig
fn NSAddressOfSymbol
fn initstate
fn atomic_flag_test_and_set_explicit
fn putenv
fn endservent
fn duplocale
fn iswnumber
fn gethostbyaddr
fn ungetc
fn tzset
fn iswphonogram
fn getprotobyname
fn tcsetattr
fn strstr
fn shutdown
fn fseek
fn feupdateenv
fn siglongjmp
fn getchar_unlocked
fn posix_spawn_file_actions_adddup2
fn sigignore
fn tcgetpgrp
fn getitimer
fn kmod_control
fn processor_assign
fn thread_abort
fn longjmp
fn setitimer
fn strxfrm
fn geteuid
fn _OSReadSwapInt32
fn semaphore_destroy
fn thread_swap_mach_voucher
fn isalpha
fn task_get_exception_ports
fn _OSSwapInt32
fn fchdir
fn sethostent
fn _kernelrpc_mach_port_destruct_trap
fn host_create_mach_voucher
fn setpwent
fn NSAddLibraryWithSearching
fn perror
fn host_set_multiuser_config_flags
fn mach_error
fn mach_port_move_member
fn gmtime_r
fn iswalnum
fn sched_get_priority_min
fn tcflow
fn select
fn vm_map_64
fn mach_port_get_refs
fn mig_strncpy_zerofill
fn task_name_for_pid
fn NSSymbolDefinitionNameInObjectFileImage
fn vfwprintf
fn NXSwapBigIntToHost
fn wait
fn NSDestroyObjectFileImage
fn flockfile
fn kmod_destroy
fn _kernelrpc_mach_port_insert_right_trap
fn _dyld_launched_prebound
fn mach_msg_destroy
fn getnameinfo
fn atomic_flag_clear_explicit
fn fremovexattr
fn isalnum
fn setreuid
fn setsockopt
fn mach_zone_info
fn fputws
fn getopt
fn strptime
fn gethostbyname
fn vsnprintf
fn linkat
fn pwrite
fn task_policy_get
fn slot_name
fn thread_sample
fn mrand48
fn fgetxattr
fn NSLinkEditError
fn feraiseexcept
fn sigpending
fn gets
fn mktemp
fn strcoll
fn wcsncmp
fn strnlen
fn wcswidth
fn fdopen
fn alarm
fn task_map_corpse_info
fn vm_allocate
fn _host_page_size
fn NSNameOfModule
fn sigwait
fn sync
fn truncate
fn iswblank
fn task_for_pid
fn pid_for_task
fn NSIsSymbolNameDefinedWithHint
fn wcpcpy
fn pthread_sigmask
fn mach_port_peek
fn mach_vm_reclaim_update_kernel_accounting_trap
fn NXSwapLong
fn NSIsSymbolNameDefinedInImage
fn mach_ports_register
fn thread_set_state
fn _dyld_get_image_vmaddr_slide
fn etap_trace_thread
fn __darwin_fd_clr
fn towlower
fn _OSReadSwapInt64
fn mach_port_get_context
fn fgetws
fn strtok_r
fn getwc
fn pthread_getconcurrency
fn link
fn thread_policy_set
fn freelocale
fn asctime
fn if_freenameindex
fn funlockfile
fn strtoull
fn wctob
fn swab
fn psignal
fn mach_port_guard_with_flags
fn host_statistics64
fn _dyld_get_image_name
fn setvbuf
fn siginterrupt
fn iswxdigit
fn task_get_mach_voucher
fn imaxabs
fn thread_get_exception_ports_info
fn vm_read_list
fn msgget
fn task_wire
fn host_check_multiuser_mode
fn fflush
fn l64a
fn fputwc
fn mach_port_get_set_status
fn sigfillset
fn usleep
fn vfscanf
fn processor_set_destroy
fn system
fn rmdir
fn semaphore_signal_thread
fn wcsrchr
fn task_get_emulation_vector
fn __vsprintf_chk
fn exit
fn vm_remap
fn thread_get_mach_voucher
fn _kernelrpc_mach_vm_deallocate_trap
fn unlink
fn vm_inherit
fn wmemmove
fn semaphore_signal
fn processor_info
fn mig_strncpy
fn task_set_mach_voucher
fn __maskrune
fn host_set_exception_ports
fn task_create
fn task_register_dyld_image_infos
fn mach_generate_activity_id
fn gethostid
fn iswalpha
fn sigismember
fn jrand48
fn iconv_open
fn _OSReadInt32
fn vwprintf
fn endprotoent
fn _NSGetExecutablePath
fn ftello
fn a64l
fn strcmp
fn main
  bb0 bb0
    alloca Virtual { id: 90, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 91, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 92, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 93, bank: General, size_bits: 64 }, 1
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    alloca Virtual { id: 99, bank: General, size_bits: 64 }, 1
    gt Virtual { id: 100, bank: General, size_bits: 8 }, 25, 30
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 99, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 100, bank: General, size_bits: 8 }
    load Virtual { id: 102, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 99, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 103, bank: General, size_bits: 8 }, Virtual { id: 102, bank: General, size_bits: 8 }, 1
    condbr
  bb1 bb1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 90, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb2 bb2
    alloca Virtual { id: 105, bank: General, size_bits: 64 }, 1
    gt Virtual { id: 106, bank: General, size_bits: 8 }, 25, 20
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 105, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 106, bank: General, size_bits: 8 }
    load Virtual { id: 108, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 105, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 109, bank: General, size_bits: 8 }, Virtual { id: 108, bank: General, size_bits: 8 }, 1
    condbr
  bb3 bb3
    bitcast Virtual { id: 110, bank: General, size_bits: 64 }, Virtual { id: 90, bank: General, size_bits: 64 }
    load Virtual { id: 111, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 110, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), 25, Virtual { id: 111, bank: General, size_bits: 64 }
    alloca Virtual { id: 113, bank: General, size_bits: 64 }, 1
    and Virtual { id: 114, bank: General, size_bits: 8 }, 1, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 113, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 114, bank: General, size_bits: 8 }
    load Virtual { id: 116, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 113, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 117, bank: General, size_bits: 8 }, Virtual { id: 116, bank: General, size_bits: 8 }, 1
    condbr
  bb4 bb4
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 90, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb5 bb5
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 90, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb7 bb7
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 91, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb8 bb8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 91, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb6 bb6
    br
  bb9 bb9
    bitcast Virtual { id: 122, bank: General, size_bits: 64 }, Virtual { id: 91, bank: General, size_bits: 64 }
    load Virtual { id: 123, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 122, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 123, bank: General, size_bits: 64 }
    alloca Virtual { id: 125, bank: General, size_bits: 64 }, 1
    ge Virtual { id: 126, bank: General, size_bits: 8 }, 85, 90
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 125, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 126, bank: General, size_bits: 8 }
    load Virtual { id: 128, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 125, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 129, bank: General, size_bits: 8 }, Virtual { id: 128, bank: General, size_bits: 8 }, 1
    condbr
  bb10 bb10
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 93, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb11 bb11
    alloca Virtual { id: 131, bank: General, size_bits: 64 }, 1
    ge Virtual { id: 132, bank: General, size_bits: 8 }, 85, 80
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 131, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 132, bank: General, size_bits: 8 }
    load Virtual { id: 134, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 131, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 135, bank: General, size_bits: 8 }, Virtual { id: 134, bank: General, size_bits: 8 }, 1
    condbr
  bb12 bb12
    bitcast Virtual { id: 136, bank: General, size_bits: 64 }, Virtual { id: 93, bank: General, size_bits: 64 }
    load Virtual { id: 137, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 136, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), 85, Virtual { id: 137, bank: General, size_bits: 64 }
    alloca Virtual { id: 139, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 139, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 42
    alloca Virtual { id: 141, bank: General, size_bits: 64 }, 1
    load Virtual { id: 142, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 139, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    gt Virtual { id: 143, bank: General, size_bits: 8 }, Virtual { id: 142, bank: General, size_bits: 64 }, 50
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 141, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 143, bank: General, size_bits: 8 }
    load Virtual { id: 145, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 141, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 146, bank: General, size_bits: 8 }, Virtual { id: 145, bank: General, size_bits: 8 }, 1
    condbr
  bb13 bb13
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 93, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb14 bb14
    alloca Virtual { id: 148, bank: General, size_bits: 64 }, 1
    ge Virtual { id: 149, bank: General, size_bits: 8 }, 85, 70
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 148, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 149, bank: General, size_bits: 8 }
    load Virtual { id: 151, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 148, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 152, bank: General, size_bits: 8 }, Virtual { id: 151, bank: General, size_bits: 8 }, 1
    condbr
  bb19 bb19
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 92, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb20 bb20
    alloca Virtual { id: 154, bank: General, size_bits: 64 }, 1
    load Virtual { id: 155, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 139, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    gt Virtual { id: 156, bank: General, size_bits: 8 }, Virtual { id: 155, bank: General, size_bits: 64 }, 25
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 154, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 156, bank: General, size_bits: 8 }
    load Virtual { id: 158, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 154, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 159, bank: General, size_bits: 8 }, Virtual { id: 158, bank: General, size_bits: 8 }, 1
    condbr
  bb15 bb15
    br
  bb16 bb16
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 93, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb17 bb17
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 93, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb21 bb21
    alloca Virtual { id: 162, bank: General, size_bits: 64 }, 1
    load Virtual { id: 163, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 92, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(16), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 162, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 163, bank: General, size_bits: 64 }
    load Virtual { id: 165, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 139, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 166, bank: General, size_bits: 64 }, Virtual { id: 162, bank: General, size_bits: 64 }
    load Virtual { id: 167, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 166, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 165, bank: General, size_bits: 64 }, Virtual { id: 167, bank: General, size_bits: 64 }
    ret
  bb22 bb22
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 92, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb23 bb23
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 92, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb18 bb18
    br
  bb24 bb24
    br


Symbols:
  main                             0x00000000

Text relocations:
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
  offset=0x000000f4 kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x0000018c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000001b0 kind=CallRel32 symbol=printf addend=0
  offset=0x00000210 kind=Aarch64AdrpAdd symbol=__const_data_1 addend=0
  offset=0x00000248 kind=Aarch64AdrpAdd symbol=__const_data_2 addend=0
  offset=0x00000280 kind=Aarch64AdrpAdd symbol=__const_data_3 addend=0
  offset=0x000002b8 kind=Aarch64AdrpAdd symbol=__const_data_4 addend=0
  offset=0x00000300 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000318 kind=CallRel32 symbol=printf addend=0
  offset=0x00000378 kind=Aarch64AdrpAdd symbol=__const_data_5 addend=0
  offset=0x00000410 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000434 kind=CallRel32 symbol=printf addend=0
  offset=0x000004b8 kind=Aarch64AdrpAdd symbol=__const_data_6 addend=0
  offset=0x00000544 kind=Aarch64AdrpAdd symbol=__const_data_9 addend=0
  offset=0x000005e0 kind=Aarch64AdrpAdd symbol=__const_data_7 addend=0
  offset=0x00000618 kind=Aarch64AdrpAdd symbol=__const_data_8 addend=0
  offset=0x000006c0 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000006e4 kind=CallRel32 symbol=printf addend=0
  offset=0x0000070c kind=Aarch64AdrpAdd symbol=__const_data_10 addend=0
  offset=0x00000744 kind=Aarch64AdrpAdd symbol=__const_data_11 addend=0

.text (1916 bytes):
  00000000  ff 03 11 d1 f0 03 00 91  10 c2 10 91 1d 7a 00 a9 
  00000010  fd 03 00 91 1f 20 03 d5  f0 03 00 91 10 42 0e 91 
  00000020  f0 0b 00 f9 f0 03 00 91  10 82 0e 91 f0 0f 00 f9 
  00000030  f0 03 00 91 10 c2 0e 91  f0 13 00 f9 f0 03 00 91 
  00000040  10 02 0f 91 f0 17 00 f9  00 00 00 90 00 00 00 91 
  00000050  00 20 01 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000060  00 c0 01 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000070  00 20 03 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000080  00 e0 03 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000090  00 80 04 91 00 00 00 94  f0 03 00 91 10 42 0f 91 
  000000a0  f0 2f 00 f9 30 03 80 d2  1f 7a 00 f1 f0 d7 9f 9a 
  000000b0  f0 33 00 f9 f1 2f 40 f9  f0 83 41 39 30 02 00 39 
  000000c0  f0 2f 40 f9 11 02 40 39  f1 3b 00 f9 f0 c3 41 39 
  000000d0  1f 06 00 f1 f0 17 9f 9a  f0 3f 00 f9 f0 3f 40 f9 
  000000e0  1f 02 00 f1 41 00 00 54  0f 00 00 14 f1 0b 40 f9 
  000000f0  eb 03 11 aa 10 00 00 90  10 02 00 91 ea 03 0b aa 
  00000100  50 01 00 f9 70 00 80 d2  10 00 a0 f2 10 00 c0 f2 
  00000110  10 00 e0 f2 ea 03 0b aa  4a 21 00 91 50 01 00 f9 
  00000120  16 00 00 14 f0 03 00 91  10 62 0f 91 f0 47 00 f9 
  00000130  30 03 80 d2 1f 52 00 f1  f0 d7 9f 9a f0 4b 00 f9 
  00000140  f1 47 40 f9 f0 43 42 39  30 02 00 39 f0 47 40 f9 
  00000150  11 02 40 39 f1 53 00 f9  f0 83 42 39 1f 06 00 f1 
  00000160  f0 17 9f 9a f0 57 00 f9  f0 57 40 f9 1f 02 00 f1 
  00000170  c1 04 00 54 33 00 00 14  f0 0b 40 f9 f0 5b 00 f9 
  00000180  f0 5b 40 f9 11 02 40 f9  f1 5f 00 f9 00 00 00 90 
  00000190  00 00 00 91 00 a0 04 91  21 03 80 d2 30 03 80 d2 
  000001a0  f0 03 00 f9 e2 5f 40 f9  f0 5f 40 f9 f0 07 00 f9 
  000001b0  00 00 00 94 f0 03 00 91  10 82 0f 91 f0 67 00 f9 
  000001c0  30 00 80 d2 31 00 80 d2  10 02 11 8a f0 6b 00 f9 
  000001d0  f1 67 40 f9 f0 43 43 39  30 02 00 39 f0 67 40 f9 
  000001e0  11 02 40 39 f1 73 00 f9  f0 83 43 39 1f 06 00 f1 
  000001f0  f0 17 9f 9a f0 77 00 f9  f0 77 40 f9 1f 02 00 f1 
  00000200  c1 03 00 54 2b 00 00 14  f1 0b 40 f9 eb 03 11 aa 
  00000210  10 00 00 90 10 02 00 91  ea 03 0b aa 50 01 00 f9 
  00000220  90 00 80 d2 10 00 a0 f2  10 00 c0 f2 10 00 e0 f2 
  00000230  ea 03 0b aa 4a 21 00 91  50 01 00 f9 2b 00 00 14 
  00000240  f1 0b 40 f9 eb 03 11 aa  10 00 00 90 10 02 00 91 
  00000250  ea 03 0b aa 50 01 00 f9  90 00 80 d2 10 00 a0 f2 
  00000260  10 00 c0 f2 10 00 e0 f2  ea 03 0b aa 4a 21 00 91 
  00000270  50 01 00 f9 1d 00 00 14  f1 0f 40 f9 eb 03 11 aa 
  00000280  10 00 00 90 10 02 00 91  ea 03 0b aa 50 01 00 f9 
  00000290  f0 00 80 d2 10 00 a0 f2  10 00 c0 f2 10 00 e0 f2 
  000002a0  ea 03 0b aa 4a 21 00 91  50 01 00 f9 10 00 00 14 
  000002b0  f1 0f 40 f9 eb 03 11 aa  10 00 00 90 10 02 00 91 
  000002c0  ea 03 0b aa 50 01 00 f9  d0 00 80 d2 10 00 a0 f2 
  000002d0  10 00 c0 f2 10 00 e0 f2  ea 03 0b aa 4a 21 00 91 
  000002e0  50 01 00 f9 02 00 00 14  a4 ff ff 17 f0 0f 40 f9 
  000002f0  f0 8b 00 f9 f0 8b 40 f9  11 02 40 f9 f1 8f 00 f9 
  00000300  00 00 00 90 00 00 00 91  00 e0 04 91 e1 8f 40 f9 
  00000310  f0 8f 40 f9 f0 03 00 f9  00 00 00 94 f0 03 00 91 
  00000320  10 a2 0f 91 f0 97 00 f9  b0 0a 80 d2 1f 6a 01 f1 
  00000330  f0 b7 9f 9a f0 9b 00 f9  f1 97 40 f9 f0 c3 44 39 
  00000340  30 02 00 39 f0 97 40 f9  11 02 40 39 f1 a3 00 f9 
  00000350  f0 03 45 39 1f 06 00 f1  f0 17 9f 9a f0 a7 00 f9 
  00000360  f0 a7 40 f9 1f 02 00 f1  41 00 00 54 0f 00 00 14 
  00000370  f1 17 40 f9 eb 03 11 aa  10 00 00 90 10 02 00 91 
  00000380  ea 03 0b aa 50 01 00 f9  30 00 80 d2 10 00 a0 f2 
  00000390  10 00 c0 f2 10 00 e0 f2  ea 03 0b aa 4a 21 00 91 
  000003a0  50 01 00 f9 16 00 00 14  f0 03 00 91 10 c2 0f 91 
  000003b0  f0 af 00 f9 b0 0a 80 d2  1f 42 01 f1 f0 b7 9f 9a 
  000003c0  f0 b3 00 f9 f1 af 40 f9  f0 83 45 39 30 02 00 39 
  000003d0  f0 af 40 f9 11 02 40 39  f1 bb 00 f9 f0 c3 45 39 
  000003e0  1f 06 00 f1 f0 17 9f 9a  f0 bf 00 f9 f0 bf 40 f9 
  000003f0  1f 02 00 f1 e1 05 00 54  3c 00 00 14 f0 17 40 f9 
  00000400  f0 c3 00 f9 f0 c3 40 f9  11 02 40 f9 f1 c7 00 f9 
  00000410  00 00 00 90 00 00 00 91  00 20 05 91 a1 0a 80 d2 
  00000420  b0 0a 80 d2 f0 03 00 f9  e2 c7 40 f9 f0 c7 40 f9 
  00000430  f0 07 00 f9 00 00 00 94  f0 03 00 91 10 e2 0f 91 
  00000440  f0 cf 00 f9 f1 cf 40 f9  50 05 80 d2 30 02 00 f9 
  00000450  f0 03 00 91 10 02 10 91  f0 d7 00 f9 f0 cf 40 f9 
  00000460  11 02 40 f9 f1 db 00 f9  f0 db 40 f9 1f ca 00 f1 
  00000470  f0 d7 9f 9a f0 df 00 f9  f1 d7 40 f9 f0 e3 46 39 
  00000480  30 02 00 39 f0 d7 40 f9  11 02 40 39 f1 e7 00 f9 
  00000490  f0 23 47 39 1f 06 00 f1  f0 17 9f 9a f0 eb 00 f9 
  000004a0  f0 eb 40 f9 1f 02 00 f1  a1 04 00 54 32 00 00 14 
  000004b0  f1 17 40 f9 eb 03 11 aa  10 00 00 90 10 02 00 91 
  000004c0  ea 03 0b aa 50 01 00 f9  30 00 80 d2 10 00 a0 f2 
  000004d0  10 00 c0 f2 10 00 e0 f2  ea 03 0b aa 4a 21 00 91 
  000004e0  50 01 00 f9 3c 00 00 14  f0 03 00 91 10 22 10 91 
  000004f0  f0 f3 00 f9 b0 0a 80 d2  1f 1a 01 f1 f0 b7 9f 9a 
  00000500  f0 f7 00 f9 f1 f3 40 f9  f0 a3 47 39 30 02 00 39 
  00000510  f0 f3 40 f9 11 02 40 39  f1 ff 00 f9 f0 e3 47 39 
  00000520  1f 06 00 f1 f0 17 9f 9a  f0 03 01 f9 f0 03 41 f9 
  00000530  1f 02 00 f1 21 05 00 54  36 00 00 14 f1 13 40 f9 
  00000540  eb 03 11 aa 10 00 00 90  10 02 00 91 ea 03 0b aa 
  00000550  50 01 00 f9 90 00 80 d2  10 00 a0 f2 10 00 c0 f2 
  00000560  10 00 e0 f2 ea 03 0b aa  4a 21 00 91 50 01 00 f9 
  00000570  36 00 00 14 f0 03 00 91  10 42 10 91 f0 0b 01 f9 
  00000580  f0 cf 40 f9 11 02 40 f9  f1 0f 01 f9 f0 0f 41 f9 
  00000590  1f 66 00 f1 f0 d7 9f 9a  f0 13 01 f9 f1 0b 41 f9 
  000005a0  f0 83 48 39 30 02 00 39  f0 0b 41 f9 11 02 40 39 
  000005b0  f1 1b 01 f9 f0 c3 48 39  1f 06 00 f1 f0 17 9f 9a 
  000005c0  f0 1f 01 f9 f0 1f 41 f9  1f 02 00 f1 c1 09 00 54 
  000005d0  5b 00 00 14 8a ff ff 17  f1 17 40 f9 eb 03 11 aa 
  000005e0  10 00 00 90 10 02 00 91  ea 03 0b aa 50 01 00 f9 
  000005f0  30 00 80 d2 10 00 a0 f2  10 00 c0 f2 10 00 e0 f2 
  00000600  ea 03 0b aa 4a 21 00 91  50 01 00 f9 5a 00 00 14 
  00000610  f1 17 40 f9 eb 03 11 aa  10 00 00 90 10 02 00 91 
  00000620  ea 03 0b aa 50 01 00 f9  30 00 80 d2 10 00 a0 f2 
  00000630  10 00 c0 f2 10 00 e0 f2  ea 03 0b aa 4a 21 00 91 
  00000640  50 01 00 f9 4c 00 00 14  f0 03 00 91 10 62 10 91 
  00000650  f0 2b 01 f9 f1 13 40 f9  e9 03 11 aa 30 01 40 f9 
  00000660  f0 c3 01 f9 e9 03 11 aa  29 21 00 91 30 01 40 f9 
  00000670  f0 c7 01 f9 f0 03 00 91  10 02 0e 91 f0 2f 01 f9 
  00000680  f1 2b 41 f9 f0 c3 41 f9  e9 03 11 aa 30 01 00 f9 
  00000690  f0 c7 41 f9 e9 03 11 aa  29 21 00 91 30 01 00 f9 
  000006a0  f0 cf 40 f9 11 02 40 f9  f1 37 01 f9 f0 2b 41 f9 
  000006b0  f0 3b 01 f9 f0 3b 41 f9  11 02 40 f9 f1 3f 01 f9 
  000006c0  00 00 00 90 00 00 00 91  00 80 05 91 e1 37 41 f9 
  000006d0  f0 37 41 f9 f0 03 00 f9  e2 3f 41 f9 f0 3f 41 f9 
  000006e0  f0 07 00 f9 00 00 00 94  bf 03 00 91 f0 03 00 91 
  000006f0  10 c2 10 91 1d 7a 40 a9  ff 03 11 91 00 00 80 d2 
  00000700  c0 03 5f d6 f1 13 40 f9  eb 03 11 aa 10 00 00 90 
  00000710  10 02 00 91 ea 03 0b aa  50 01 00 f9 d0 00 80 d2 
  00000720  10 00 a0 f2 10 00 c0 f2  10 00 e0 f2 ea 03 0b aa 
  00000730  4a 21 00 91 50 01 00 f9  10 00 00 14 f1 13 40 f9 
  00000740  eb 03 11 aa 10 00 00 90  10 02 00 91 ea 03 0b aa 
  00000750  50 01 00 f9 70 00 80 d2  10 00 a0 f2 10 00 c0 f2 
  00000760  10 00 e0 f2 ea 03 0b aa  4a 21 00 91 50 01 00 f9 
  00000770  02 00 00 14 98 ff ff 17  b4 ff ff 17 

.rodata (370 bytes):
  00000000  19 00 00 00 00 00 00 00  68 6f 74 00 77 61 72 6d 
  00000010  00 63 6f 6c 64 00 01 01  6f 75 74 64 6f 6f 72 00 
  00000020  69 6e 64 6f 6f 72 00 00  55 00 00 00 00 00 00 00 
  00000030  41 00 42 00 43 00 46 00  68 69 67 68 00 6d 65 64 
  00000040  69 75 6d 00 6c 6f 77 00  f0 9f 93 98 20 54 75 74 
  00000050  6f 72 69 61 6c 3a 20 30  33 5f 63 6f 6e 74 72 6f 
  00000060  6c 5f 66 6c 6f 77 2e 66  70 0a 00 00 00 00 00 00 
  00000070  f0 9f a7 ad 20 46 6f 63  75 73 3a 20 43 6f 6e 74 
  00000080  72 6f 6c 20 66 6c 6f 77  3a 20 69 66 2f 65 6c 73 
  00000090  65 20 65 78 70 72 65 73  73 69 6f 6e 73 20 77 69 
  000000a0  74 68 20 63 6f 6e 73 74  20 61 6e 64 20 72 75 6e 
  000000b0  74 69 6d 65 20 65 76 61  6c 75 61 74 69 6f 6e 0a 
  000000c0  00 00 00 00 00 00 00 00  f0 9f a7 aa 20 57 68 61 
  000000d0  74 20 74 6f 20 6c 6f 6f  6b 20 66 6f 72 3a 20 6c 
  000000e0  61 62 65 6c 65 64 20 6f  75 74 70 75 74 73 20 62 
  000000f0  65 6c 6f 77 0a 00 00 00  e2 9c 85 20 45 78 70 65 
  00000100  63 74 61 74 69 6f 6e 3a  20 6f 75 74 70 75 74 73 
  00000110  20 6d 61 74 63 68 20 6c  61 62 65 6c 73 0a 00 00 
  00000120  0a 00 00 00 00 00 00 00  25 6c 6c 64 c2 b0 43 20 
  00000130  69 73 20 25 73 0a 00 00  53 75 67 67 65 73 74 65 
  00000140  64 3a 20 25 73 0a 00 00  53 63 6f 72 65 20 25 6c 
  00000150  6c 64 20 3d 20 67 72 61  64 65 20 25 73 0a 00 00 
  00000160  56 61 6c 75 65 20 25 6c  6c 64 20 69 73 20 25 73 
  00000170  0a 00 
